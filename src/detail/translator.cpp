// SPDX-License-Identifier: GPL-3.0-or-later
// Translation: Anevlavis et al., DOI 10.1145/3491216, Section 4, Table 2.
#include "translator.hpp"

#include <map>
#include <tuple>
#include <unordered_map>

namespace evrostos::detail {

// Reuse structurally identical LTL nodes within each translation.
// Shared suffixes and operand nodes avoid exponential expansion of the LTL AST.
class LtlBuilder {
public:
    LtlBuilder(std::string_view source, std::size_t limit) : source_(source), limit_(limit) {}

    SharedPtrLtlNode make(LtlOperator op, std::size_t offset, SharedPtrLtlNode left = {},
                      SharedPtrLtlNode right = {}, std::string atom = {}) {
        auto key = std::make_tuple(op, atom, left, right);
        if (const auto found = nodes_.find(key); found != nodes_.end()) {
            return found->second;
        }
        if (nodes_.size() >= limit_) {
            fail(DiagnosticCode::node_limit, "LTL node limit exceeded", source_, offset);
        }
        SharedPtrLtlNode result(new LtlNode(op, std::move(atom), std::move(left), std::move(right)));
        nodes_.emplace(std::move(key), result);
        return result;
    }

private:
    std::string_view source_;
    std::size_t limit_;
    std::map<std::tuple<LtlOperator, std::string, SharedPtrLtlNode, SharedPtrLtlNode>, SharedPtrLtlNode> nodes_;
};

namespace {

using Bits = std::array<SharedPtrLtlNode, 4>;

class Translator {
public:
    Translator(std::string_view source, std::size_t limit) : builder_(source, limit) {}

    Bits visit(const SharedPtrRltlNode& node) {
        if (const auto found = cache_.find(node.get()); found != cache_.end()) {
            return found->second;
        }
        const auto make = [&](LtlOperator op, SharedPtrLtlNode left = {}, SharedPtrLtlNode right = {}) {
            return builder_.make(op, node->offset, std::move(left), std::move(right));
        };
        Bits result;
        if (node->op == RltlOperator::atom) {
            result.fill(builder_.make(LtlOperator::atom, node->offset, {}, {}, node->atom));
        } else {
            const auto a = visit(node->left);
            const auto b = node->right ? visit(node->right) : Bits{};
            switch (node->op) {
            case RltlOperator::negation:
                result.fill(make(LtlOperator::negation, a[0]));
                break;
            case RltlOperator::conjunction:
            case RltlOperator::disjunction:
            case RltlOperator::until: {
                const auto op = node->op == RltlOperator::conjunction ? LtlOperator::conjunction
                              : node->op == RltlOperator::disjunction ? LtlOperator::disjunction
                                                                    : LtlOperator::until;
                for (std::size_t i = 0; i < 4; ++i) {
                    result[i] = make(op, a[i], b[i]);
                }
                break;
            }
            case RltlOperator::implication:
                result[3] = make(LtlOperator::implication, a[3], b[3]);
                for (std::size_t i = 3; i > 0; --i) {
                    result[i - 1] = make(LtlOperator::conjunction,
                                         make(LtlOperator::implication, a[i - 1], b[i - 1]), result[i]);
                }
                break;
            case RltlOperator::next:
            case RltlOperator::eventually: {
                const auto op = node->op == RltlOperator::next
                              ? LtlOperator::next : LtlOperator::eventually;
                for (std::size_t i = 0; i < 4; ++i) {
                    result[i] = make(op, a[i]);
                }
                break;
            }
            case RltlOperator::always:
                result = {make(LtlOperator::always, a[0]),
                          make(LtlOperator::eventually, make(LtlOperator::always, a[1])),
                          make(LtlOperator::always, make(LtlOperator::eventually, a[2])),
                          make(LtlOperator::eventually, a[3])};
                break;
            case RltlOperator::release:
                result = {make(LtlOperator::release, a[0], b[0]),
                          make(LtlOperator::disjunction, make(LtlOperator::eventually, a[1]),
                               make(LtlOperator::eventually, make(LtlOperator::always, b[1]))),
                          make(LtlOperator::disjunction, make(LtlOperator::eventually, a[2]),
                               make(LtlOperator::always, make(LtlOperator::eventually, b[2]))),
                          make(LtlOperator::disjunction, make(LtlOperator::eventually, a[3]),
                               make(LtlOperator::eventually, b[3]))};
                break;
            case RltlOperator::atom: break; // Handled above.
            }
        }
        cache_.emplace(node.get(), result);
        return result;
    }

private:
    LtlBuilder builder_;
    std::unordered_map<const RltlNode*, Bits> cache_;
};

} // namespace

Translation translate(const SharedPtrRltlNode& formula, std::string_view source, const TranslationLimits& limits) {
    return {Translator(source, limits.max_ltl_nodes).visit(formula)};
}

} // namespace evrostos::detail
