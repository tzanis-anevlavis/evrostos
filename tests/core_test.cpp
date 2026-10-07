// SPDX-License-Identifier: GPL-3.0-or-later
#include "evrostos.hpp"

#include <future>
#include <iostream>
#include <set>
#include <stdexcept>
#include <vector>

namespace {

void check(bool condition, const char* message) {
    if (!condition) {
        throw std::runtime_error(message);
    }
}

evrostos::Translation translated(std::string_view source,
                                const evrostos::TranslationLimits& limits = {}) {
    auto result = evrostos::Evrostos{}.translate(source, limits);
    if (const auto* error = std::get_if<evrostos::Diagnostic>(&result)) {
        throw std::runtime_error(error->message);
    }
    return std::get<evrostos::Translation>(std::move(result));
}

evrostos::Diagnostic rejected(std::string_view source,
                             const evrostos::TranslationLimits& limits = {}) {
    auto result = evrostos::Evrostos{}.translate(source, limits);
    check(std::holds_alternative<evrostos::Diagnostic>(result), "expected diagnostic");
    return std::get<evrostos::Diagnostic>(std::move(result));
}

std::size_t node_count(const evrostos::Translation& translation) {
    std::set<const evrostos::LtlNode*> seen;
    std::vector<evrostos::Ltl> pending(translation.bits.begin(), translation.bits.end());
    while (!pending.empty()) {
        auto node = pending.back();
        pending.pop_back();
        if (!node || !seen.insert(node.get()).second) {
            continue;
        }
        pending.push_back(node->left());
        pending.push_back(node->right());
    }
    return seen.size();
}

void ownership_and_sharing() {
    auto result = [] {
        std::string source = "GFReady2";
        evrostos::Evrostos core;
        auto value = core.translate(source);
        source.assign(source.size(), 'x');
        return std::get<evrostos::Translation>(std::move(value));
    }();
    check(result.bits[0]->atom() == "GFReady2", "result must own identifier storage");
    for (const auto& bit : result.bits) {
        check(bit == result.bits[0], "atom bits should share one node");
        check(!bit->left() && !bit->right(), "atom must have no children");
    }
    check(node_count(result) == 1, "atom should have exactly one node");
    const auto repeated = translated("(rG p) & (rG p)");
    for (const auto& bit : repeated.bits) {
        check(bit->left() == bit->right(), "repeated subexpressions must be shared");
    }

    const auto implication = translated("(rG p) => (rG q)");
    for (std::size_t i = 0; i < 3; ++i) {
        check(implication.bits[i]->right() == implication.bits[i + 1], "implication suffix must be shared");
    }
    check(implication.bits[3]->op() == evrostos::LtlOperator::implication, "weakest implication bit");

    const auto negation = translated("!((rG p) => (rG q))");
    for (const auto& bit : negation.bits) {
        check(bit == negation.bits[0], "negation must replicate the same first-bit negation");
    }
}

void diagnostics_and_limits() {
    using Code = evrostos::DiagnosticCode;
    auto error = rejected("p &\r\n  )");
    check(error.code == Code::syntax_error, "syntax error category");
    check(error.offset == 7 && error.line == 2 && error.column == 3, "CRLF diagnostic location");
    error = rejected("p &\n");
    check(error.offset == 4 && error.line == 2 && error.column == 1, "EOF diagnostic location");
    error = rejected(std::string("p\0", 2));
    check(error.code == Code::syntax_error && error.offset == 1, "embedded NUL must be rejected");
    check(rejected("").code == Code::syntax_error, "empty formula rejected");

    evrostos::TranslationLimits limits;
    limits.max_input_bytes = 1;
    check(translated("p", limits).bits[0]->atom() == "p", "input boundary accepted");
    check(rejected("pp", limits).code == Code::input_limit, "input limit enforced");
    limits = {};
    limits.max_parse_nodes = 2;
    check(rejected("p & q", limits).code == Code::node_limit, "parse node limit enforced");
    limits.max_parse_nodes = 3;
    check(translated("p & q", limits).bits[0]->op() == evrostos::LtlOperator::conjunction,
          "parse node boundary accepted");
    limits = {};
    limits.max_ltl_nodes = 1;
    check(node_count(translated("p", limits)) == 1, "shared nodes count once");
    check(rejected("rG p", limits).code == Code::node_limit, "LTL node limit enforced");
    limits.max_ltl_nodes = 0;
    check(rejected("p", limits).code == Code::node_limit, "zero node budget");

    check(rejected(std::string(256, '!') + "p").code == Code::depth_limit, "unary depth bounded");
    check(rejected(std::string(256, '(') + "p" + std::string(256, ')')).code == Code::depth_limit,
          "parenthesis depth bounded");
    std::string chain = "p";
    for (int i = 0; i < 256; ++i) {
        chain += " & p";
    }
    check(rejected(chain).code == Code::depth_limit, "left-associated tree depth bounded");
    check(translated(std::string(255, '!') + "p").bits[0] != nullptr, "depth boundary accepted");
}

void bounded_growth_and_reentrancy() {
    std::string formula = "rG p";
    for (int i = 0; i < 100; ++i) {
        formula = "(" + formula + ") => (rG q)";
    }
    const auto result = translated(formula);
    check(node_count(result) < 1000, "nested implication must remain a DAG, not expand exponentially");

    const evrostos::Evrostos core;
    std::vector<std::future<bool>> calls;
    for (int i = 0; i < 16; ++i) {
        calls.push_back(std::async(std::launch::async, [&core] {
            const auto bad = core.translate("rG");
            const auto good = core.translate("p rR q");
            return std::holds_alternative<evrostos::Diagnostic>(bad)
                && std::get<evrostos::Translation>(good).bits[0]->op() == evrostos::LtlOperator::release;
        }));
    }
    for (auto& call : calls) {
        check(call.get(), "independent calls on one core object");
    }
}

} // namespace

int main() {
    try {
        ownership_and_sharing();
        diagnostics_and_limits();
        bounded_growth_and_reentrancy();
        std::cout << "Core API, ownership, DAG sharing, limits, and reentrancy passed\n";
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
