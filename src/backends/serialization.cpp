// SPDX-License-Identifier: GPL-3.0-or-later
#include "serialization.hpp"

#include <algorithm>
#include <optional>
#include <span>
#include <stdexcept>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace evrostos::backends {
namespace {

enum class Dialect { nusmv, spin };

// NuSMV 2.6 manual, section 2, and core/parser/input.l.2.{50,51}.
// Include lexer keywords omitted from the manual's identifier list.
constexpr std::string_view nusmv_keywords[] = {
    "MODULE", "DEFINE", "MDEFINE", "CONSTANTS", "VAR", "IVAR", "FROZENVAR",
    "INIT", "TRANS", "INVAR", "SPEC", "CTLSPEC", "LTLSPEC", "PSLSPEC", "COMPUTE",
    "NAME", "INVARSPEC", "FAIRNESS", "JUSTICE", "COMPASSION", "ISA", "ASSIGN",
    "CONSTRAINT", "SIMPWFF", "CTLWFF", "LTLWFF", "PSLWFF", "COMPWFF", "IN",
    "MIN", "MAX", "MIRROR", "PRED", "PREDICATES", "FUN", "ITYPE", "NEXTWFF",
    "COMPID", "READ", "WRITE", "CONSTARRAY", "typeof", "process", "array", "of",
    "boolean", "integer", "real", "word", "Word", "word1", "bool", "signed",
    "unsigned", "extend", "resize", "sizeof", "toint", "uwconst", "swconst",
    "EX", "AX", "EF", "AF", "EG", "AG", "E", "F", "O", "G", "H", "X", "Y",
    "Z", "A", "U", "S", "V", "T", "BU", "EBF", "ABF", "EBG", "ABG", "case",
    "esac", "mod", "next", "init", "union", "in", "xor", "xnor", "self",
    "TRUE", "FALSE", "count", "abs", "max", "min"
};

// SPIN's inline-LTL lexer: LTL_syms and Names in Src/spinlex.c.
// Keywords containing underscores are outside the accepted atom alphabet.
constexpr std::string_view spin_keywords[] = {
    "U", "V", "W", "X", "always", "eventually", "until", "stronguntil",
    "weakuntil", "release", "next", "implies", "equivalent", "active", "assert",
    "atomic", "bit", "bool", "break", "byte", "do", "chan", "else", "empty",
    "enabled", "eval", "false", "fi", "for", "full", "goto", "hidden", "if",
    "in", "init", "inline", "int", "len", "local", "ltl", "mtype", "nempty",
    "never", "nfull", "notrace", "od", "of", "pid", "printf", "printm",
    "priority", "proctype", "provided", "return", "run", "select", "short",
    "skip", "timeout", "trace", "true", "show", "typedef", "unless", "unsigned",
    "xr", "xs"
};

bool valid_identifier(std::string_view atom, Dialect dialect) {
    const auto letter = [](char c) {
        return ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z'));
    };

    if (atom.empty() || !letter(atom.front())) {
        return false;
    }

    for (char c : atom) {
        if (!letter(c) && !((c >= '0') && (c <= '9'))) {
            return false;
        }
    }

    std::span<const std::string_view> keywords;
    switch (dialect) {
    case Dialect::nusmv:
        keywords = std::span<const std::string_view>(nusmv_keywords);
        break;
    case Dialect::spin:
        keywords = std::span<const std::string_view>(spin_keywords);
        break;
    default:
        throw std::logic_error("unknown serialization dialect");
    }

    return (std::find(keywords.begin(), keywords.end(), atom) == keywords.end());
}

std::string_view spelling(LtlOperator op, Dialect dialect) {
    switch (op) {
    case LtlOperator::negation: return "!";
    case LtlOperator::conjunction: return (dialect == Dialect::nusmv) ? "&" : "&&";
    case LtlOperator::disjunction: return (dialect == Dialect::nusmv) ? "|" : "||";
    case LtlOperator::implication: return "->";
    case LtlOperator::next: return "X";
    case LtlOperator::eventually: return (dialect == Dialect::nusmv) ? "F" : "<>";
    case LtlOperator::always: return (dialect == Dialect::nusmv) ? "G" : "[]";
    case LtlOperator::until: return "U";
    case LtlOperator::release: return "V";
    case LtlOperator::atom: break;
    }
    throw std::logic_error("expected an LTL operator");
}

SerializationDiagnostic output_limit() {
    return {SerializationCode::output_limit, "serialized formula exceeds the output byte limit"};
}

// The root owns all visited nodes. Raw pointers here are nonowning traversal keys.
// Measure each unique node once, but count shared children at every occurrence
// in the expanded text. Subtraction avoids overflow even for a SIZE_MAX budget.
std::optional<SerializationDiagnostic> measure(
    const LtlNode* root, Dialect dialect, bool allow_next, std::size_t limit,
    std::unordered_map<const LtlNode*, std::size_t>& sizes) {
    struct Frame {
        const LtlNode* node;
        bool children_visited;
    };
    std::vector<Frame> pending{{root, false}};
    while (!pending.empty()) {
        const auto [node, children_visited] = pending.back();
        pending.pop_back();
        if (sizes.contains(node)) {
            continue;
        }
        if (node->op() == LtlOperator::atom) {
            if (!valid_identifier(node->atom(), dialect)) {
                return SerializationDiagnostic{SerializationCode::invalid_identifier,
                    "invalid or reserved " + std::string((dialect == Dialect::nusmv) ? "NuSMV" : "SPIN")
                    + " identifier: " + node->atom()};
            }
            if (node->atom().size() > limit) {
                return output_limit();
            }
            sizes.emplace(node, node->atom().size());
        } else if (!children_visited) {
            if ((dialect == Dialect::spin) && (node->op() == LtlOperator::next) && !allow_next) {
                return SerializationDiagnostic{SerializationCode::unsupported_operator,
                    "SPIN next operator requires allow_next and a compatible checker configuration"};
            }
            pending.push_back({node, true});
            if (node->right()) {
                pending.push_back({node->right().get(), false});
            }
            pending.push_back({node->left().get(), false});
        } else {
            // Unary: (op child). Binary: (left op right).
            std::size_t size = spelling(node->op(), dialect).size() + (node->right() ? 4 : 3);
            if (size > limit) {
                return output_limit();
            }
            for (const auto* child : {node->left().get(), node->right().get()}) {
                if (child != nullptr) {
                    const auto child_size = sizes.at(child);
                    if (child_size > limit - size) {
                        return output_limit();
                    }
                    size += child_size;
                }
            }
            sizes.emplace(node, size);
        }
    }
    return std::nullopt;
}

std::string emit(const LtlNode* root, Dialect dialect, std::size_t size) {
    struct Frame {
        const LtlNode* node;
        unsigned int phase;
    };
    std::string output;
    output.reserve(size);
    std::vector<Frame> pending{{root, 0}};
    while (!pending.empty()) {
        auto& frame = pending.back();
        const auto* node = frame.node;
        if (node->op() == LtlOperator::atom) {
            output += node->atom();
            pending.pop_back();
        } else if (frame.phase == 0) {
            output += '(';
            if (!node->right()) {
                output += spelling(node->op(), dialect);
                output += ' ';
            }
            frame.phase = 1;
            pending.push_back({node->left().get(), 0});
        } else if ((frame.phase == 1) && node->right()) {
            output += ' ';
            output += spelling(node->op(), dialect);
            output += ' ';
            frame.phase = 2;
            pending.push_back({node->right().get(), 0});
        } else {
            output += ')';
            pending.pop_back();
        }
    }
    return output;
}

SerializationResult serialize(const SharedPtrLtlNode& formula, Dialect dialect,
                              bool allow_next, const SerializationLimits& limits) {
    if (!formula) {
        return SerializationDiagnostic{SerializationCode::null_formula, "formula root is null"};
    }
    std::unordered_map<const LtlNode*, std::size_t> sizes;
    const auto limit = std::min(limits.max_output_bytes, std::string{}.max_size());
    if (auto error = measure(formula.get(), dialect, allow_next, limit, sizes)) {
        return std::move(*error);
    }
    // Emit only after validation and sizing succeed. Iterative traversal keeps
    // both passes independent of the C++ call-stack depth.
    return emit(formula.get(), dialect, sizes.at(formula.get()));
}

} // namespace

SerializationResult serialize_nusmv(const SharedPtrLtlNode& formula, const SerializationLimits& limits) {
    return serialize(formula, Dialect::nusmv, true, limits);
}

SerializationResult serialize_spin(const SharedPtrLtlNode& formula, const SpinSerializationOptions& options,
                                   const SerializationLimits& limits) {
    return serialize(formula, Dialect::spin, options.allow_next, limits);
}

} // namespace evrostos::backends
