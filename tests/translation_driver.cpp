// SPDX-License-Identifier: GPL-3.0-or-later
// Test-only bridge. No stable CLI/protocol and no model-checker integration.
// TODO: Re-evaluate the Python/C++ test split and whether the semantic and
// differential suites should move to GoogleTest, replacing this JSON bridge.
#include "evrostos.hpp"
#include "detail/parser.hpp"
#include "backends/serialization.hpp"

#include <iostream>
#include <map>
#include <stdexcept>
#include <vector>

namespace {

void quoted(std::ostream& out, std::string_view text) {
    out << '"';
    constexpr char hex[] = "0123456789abcdef";
    for (char raw : text) {
        const auto c = static_cast<unsigned char>(raw);
        if (c == '"' || c == '\\') {
            out << '\\' << static_cast<char>(c);
        } else if (c < 0x20) {
            out << "\\u00" << hex[c >> 4] << hex[c & 15];
        } else {
            out << static_cast<char>(c);
        }
    }
    out << '"';
}

std::string_view name(evrostos::LtlOperator op) {
    switch (op) {
    case evrostos::LtlOperator::atom: return "atom";
    case evrostos::LtlOperator::negation: return "!";
    case evrostos::LtlOperator::conjunction: return "&";
    case evrostos::LtlOperator::disjunction: return "|";
    case evrostos::LtlOperator::implication: return "->";
    case evrostos::LtlOperator::next: return "X";
    case evrostos::LtlOperator::eventually: return "F";
    case evrostos::LtlOperator::always: return "G";
    case evrostos::LtlOperator::until: return "U";
    case evrostos::LtlOperator::release: return "R";
    }
    throw std::logic_error("unknown LTL operator");
}

std::string_view name(evrostos::detail::RltlOperator op) {
    switch (op) {
    case evrostos::detail::RltlOperator::atom: return "atom";
    case evrostos::detail::RltlOperator::negation: return "!";
    case evrostos::detail::RltlOperator::conjunction: return "&";
    case evrostos::detail::RltlOperator::disjunction: return "|";
    case evrostos::detail::RltlOperator::implication: return "=>";
    case evrostos::detail::RltlOperator::next: return "rX";
    case evrostos::detail::RltlOperator::eventually: return "rF";
    case evrostos::detail::RltlOperator::always: return "rG";
    case evrostos::detail::RltlOperator::until: return "rU";
    case evrostos::detail::RltlOperator::release: return "rR";
    }
    throw std::logic_error("unknown rLTL operator");
}

std::string_view name(evrostos::DiagnosticCode code) {
    switch (code) {
    case evrostos::DiagnosticCode::syntax_error: return "syntax_error";
    case evrostos::DiagnosticCode::input_limit: return "input_limit";
    case evrostos::DiagnosticCode::depth_limit: return "depth_limit";
    case evrostos::DiagnosticCode::node_limit: return "node_limit";
    }
    throw std::logic_error("unknown diagnostic");
}

void error_json(const evrostos::Diagnostic& error) {
    std::cout << "{\"ok\":false,\"error\":{\"code\":";
    quoted(std::cout, name(error.code));
    std::cout << ",\"message\":";
    quoted(std::cout, error.message);
    std::cout << ",\"offset\":" << error.offset << ",\"line\":" << error.line
              << ",\"column\":" << error.column << "}}\n";
}

void rltl_json(const evrostos::detail::SharedPtrRltlNode& node) {
    if (node->op == evrostos::detail::RltlOperator::atom) {
        quoted(std::cout, node->atom);
        return;
    }
    std::cout << '[';
    quoted(std::cout, name(node->op));
    std::cout << ',';
    rltl_json(node->left);
    if (node->right) {
        std::cout << ',';
        rltl_json(node->right);
    }
    std::cout << ']';
}

class DagJson {
public:
    void write(const evrostos::Translation& result) {
        for (const auto& root : result.bits) {
            visit(root);
        }
        std::cout << "{\"ok\":true,\"nodes\":[";
        bool first = true;
        for (const auto& node : nodes_) {
            if (!first) {
                std::cout << ',';
            }
            first = false;
            std::cout << "{\"op\":";
            quoted(std::cout, name(node->op()));
            if (node->op() == evrostos::LtlOperator::atom) {
                std::cout << ",\"atom\":";
                quoted(std::cout, node->atom());
            }
            if (node->left()) {
                std::cout << ",\"left\":" << ids_.at(node->left().get());
            }
            if (node->right()) {
                std::cout << ",\"right\":" << ids_.at(node->right().get());
            }
            std::cout << '}';
        }
        std::cout << "],\"roots\":[";
        for (std::size_t i = 0; i < 4; ++i) {
            if (i != 0) {
                std::cout << ',';
            }
            std::cout << ids_.at(result.bits[i].get());
        }
        std::cout << "]}\n";
    }

private:
    void visit(const evrostos::SharedPtrLtlNode& node) {
        if (ids_.contains(node.get())) {
            return;
        }
        if (node->left()) {
            visit(node->left());
        }
        if (node->right()) {
            visit(node->right());
        }
        ids_.emplace(node.get(), nodes_.size());
        nodes_.push_back(node);
    }
    std::map<const evrostos::LtlNode*, std::size_t> ids_;
    std::vector<evrostos::SharedPtrLtlNode> nodes_;
};

std::string_view name(evrostos::backends::SerializationCode code) {
    switch (code) {
    case evrostos::backends::SerializationCode::null_formula: return "null_formula";
    case evrostos::backends::SerializationCode::invalid_identifier: return "invalid_identifier";
    case evrostos::backends::SerializationCode::unsupported_operator: return "unsupported_operator";
    case evrostos::backends::SerializationCode::output_limit: return "output_limit";
    }
    throw std::logic_error("unknown serialization diagnostic");
}

void serialization_json(const evrostos::Translation& translation, std::string_view mode) {
    std::array<std::string, 4> formulas;
    for (std::size_t i = 0; i < 4; ++i) {
        auto result = (mode == "serialize-nusmv")
                    ? evrostos::backends::serialize_nusmv(translation.bits[i])
                    : evrostos::backends::serialize_spin(translation.bits[i],
                        {.allow_next = (mode == "serialize-spin-next")});
        if (const auto* error = std::get_if<evrostos::backends::SerializationDiagnostic>(&result)) {
            std::cout << "{\"ok\":false,\"bit\":" << i << ",\"error\":{\"code\":";
            quoted(std::cout, name(error->code));
            std::cout << ",\"message\":";
            quoted(std::cout, error->message);
            std::cout << "}}\n";
            return;
        }
        formulas[i] = std::get<std::string>(std::move(result));
    }
    std::cout << "{\"ok\":true,\"formulas\":[";
    for (std::size_t i = 0; i < 4; ++i) {
        if (i != 0) {
            std::cout << ',';
        }
        quoted(std::cout, formulas[i]);
    }
    std::cout << "]}\n";
}

} // namespace

int main(int argc, char** argv) {
    if (argc != 2) {
        return 2;
    }
    const std::string_view mode = argv[1];
    if ((mode != "parse") && (mode != "translate") && (mode != "serialize-nusmv")
        && (mode != "serialize-spin") && (mode != "serialize-spin-next")) {
        return 2;
    }
    const evrostos::TranslationLimits limits;
    std::string input;
    char c;
    // Bound input in this test harness as well as in the library.
    while (input.size() <= limits.max_input_bytes && std::cin.get(c)) {
        input.push_back(c);
    }
    if (std::cin.bad()) {
        return 2;
    }
    if (mode == "parse") {
        try {
            const auto ast = evrostos::detail::parse(input, limits);
            std::cout << "{\"ok\":true,\"ast\":";
            rltl_json(ast);
            std::cout << "}\n";
        } catch (const evrostos::detail::Failure& failure) {
            error_json(failure.diagnostic);
        }
    } else {
        const auto result = evrostos::Evrostos{}.translate(input);
        if (const auto* error = std::get_if<evrostos::Diagnostic>(&result)) {
            error_json(*error);
        } else if (mode == "translate") {
            DagJson{}.write(std::get<evrostos::Translation>(result));
        } else {
            serialization_json(std::get<evrostos::Translation>(result), mode);
        }
    }
}
