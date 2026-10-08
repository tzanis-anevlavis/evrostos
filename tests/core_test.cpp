// SPDX-License-Identifier: GPL-3.0-or-later
#include "evrostos.hpp"

#include <gtest/gtest.h>

#include <future>
#include <set>
#include <stdexcept>
#include <vector>

namespace {

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
    if (!std::holds_alternative<evrostos::Diagnostic>(result)) {
        throw std::runtime_error("Expected a diagnostic for: " + std::string(source));
    }
    return std::get<evrostos::Diagnostic>(std::move(result));
}

std::size_t node_count(const evrostos::Translation& translation) {
    std::set<const evrostos::LtlNode*> seen;
    std::vector<evrostos::SharedPtrLtlNode> pending(translation.bits.begin(), translation.bits.end());
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

TEST(TranslationOwnership, ResultOutlivesInputAndCore) {
    auto result = [] {
        std::string source = "GFReady2";
        evrostos::Evrostos core;
        auto value = core.translate(source);
        source.assign(source.size(), 'x');
        return std::get<evrostos::Translation>(std::move(value));
    }();
    ASSERT_NE(result.bits[0], nullptr);
    EXPECT_EQ(result.bits[0]->atom(), "GFReady2");
}

TEST(TranslationSharing, AtomBitsShareOneNode) {
    const auto result = translated("p");
    for (const auto& bit : result.bits) {
        ASSERT_NE(bit, nullptr);
        EXPECT_EQ(bit, result.bits[0]);
        EXPECT_EQ(bit->left(), nullptr);
        EXPECT_EQ(bit->right(), nullptr);
    }
    EXPECT_EQ(node_count(result), 1u);
}

TEST(TranslationSharing, RepeatedSubexpressionsShareNodes) {
    const auto repeated = translated("(rG p) & (rG p)");
    for (const auto& bit : repeated.bits) {
        ASSERT_NE(bit, nullptr);
        ASSERT_NE(bit->left(), nullptr);
        EXPECT_EQ(bit->left(), bit->right());
    }
}

TEST(TranslationSharing, ImplicationSuffixesShareNodes) {
    const auto implication = translated("(rG p) => (rG q)");
    for (std::size_t i = 0; i < 3; ++i) {
        SCOPED_TRACE(i);
        ASSERT_NE(implication.bits[i], nullptr);
        ASSERT_NE(implication.bits[i + 1], nullptr);
        EXPECT_EQ(implication.bits[i]->right(), implication.bits[i + 1]);
    }
    EXPECT_EQ(implication.bits[3]->op(), evrostos::LtlOperator::implication);
}

TEST(TranslationSharing, NegationBitsShareOneNode) {
    const auto negation = translated("!((rG p) => (rG q))");
    ASSERT_NE(negation.bits[0], nullptr);
    EXPECT_EQ(negation.bits[0]->op(), evrostos::LtlOperator::negation);
    for (const auto& bit : negation.bits) {
        EXPECT_EQ(bit, negation.bits[0]);
    }
}

TEST(TranslationDiagnostics, CrLfCountsAsOneNewline) {
    const auto error = rejected("p &\r\n  )");
    EXPECT_EQ(error.code, evrostos::DiagnosticCode::syntax_error);
    EXPECT_EQ(error.offset, 7u);
    EXPECT_EQ(error.line, 2u);
    EXPECT_EQ(error.column, 3u);
}

TEST(TranslationDiagnostics, MissingOperandReportsEndOfInput) {
    const auto error = rejected("p &\n");
    EXPECT_EQ(error.code, evrostos::DiagnosticCode::syntax_error);
    EXPECT_EQ(error.offset, 4u);
    EXPECT_EQ(error.line, 2u);
    EXPECT_EQ(error.column, 1u);
}

TEST(TranslationDiagnostics, EmbeddedNullIsRejected) {
    const auto error = rejected(std::string("p\0", 2));
    EXPECT_EQ(error.code, evrostos::DiagnosticCode::syntax_error);
    EXPECT_EQ(error.offset, 1u);
}

TEST(TranslationDiagnostics, EmptyFormulaIsRejected) {
    EXPECT_EQ(rejected("").code, evrostos::DiagnosticCode::syntax_error);
}

TEST(TranslationLimits, InputByteBudgetIncludesBoundary) {
    evrostos::TranslationLimits limits;
    limits.max_input_bytes = 1;
    const auto result = translated("p", limits);
    ASSERT_NE(result.bits[0], nullptr);
    EXPECT_EQ(result.bits[0]->atom(), "p");
    EXPECT_EQ(rejected("pp", limits).code, evrostos::DiagnosticCode::input_limit);
}

TEST(TranslationLimits, ParseNodeBudgetIncludesBoundary) {
    evrostos::TranslationLimits limits;
    limits.max_parse_nodes = 2;
    EXPECT_EQ(rejected("p & q", limits).code, evrostos::DiagnosticCode::node_limit);
    limits.max_parse_nodes = 3;
    const auto result = translated("p & q", limits);
    ASSERT_NE(result.bits[0], nullptr);
    EXPECT_EQ(result.bits[0]->op(), evrostos::LtlOperator::conjunction);
}

TEST(TranslationLimits, LtlNodeBudgetCountsSharedNodesOnce) {
    evrostos::TranslationLimits limits;
    limits.max_ltl_nodes = 1;
    EXPECT_EQ(node_count(translated("p", limits)), 1u);
    EXPECT_EQ(rejected("rG p", limits).code, evrostos::DiagnosticCode::node_limit);
}

TEST(TranslationLimits, ZeroLtlNodeBudgetIsRejected) {
    evrostos::TranslationLimits limits;
    limits.max_ltl_nodes = 0;
    EXPECT_EQ(rejected("p", limits).code, evrostos::DiagnosticCode::node_limit);
}

TEST(TranslationLimits, UnaryDepthIncludesBoundary) {
    EXPECT_EQ(rejected(std::string(256, '!') + "p").code, evrostos::DiagnosticCode::depth_limit);
    EXPECT_NE(translated(std::string(255, '!') + "p").bits[0], nullptr);
}

TEST(TranslationLimits, ParenthesisDepthIsBounded) {
    EXPECT_EQ(rejected(std::string(256, '(') + "p" + std::string(256, ')')).code,
              evrostos::DiagnosticCode::depth_limit);
}

TEST(TranslationLimits, LeftAssociatedTreeDepthIsBounded) {
    std::string chain = "p";
    for (int i = 0; i < 256; ++i) {
        chain += " & p";
    }
    EXPECT_EQ(rejected(chain).code, evrostos::DiagnosticCode::depth_limit);
}

TEST(TranslationSharing, NestedImplicationHasBoundedDagGrowth) {
    std::string formula = "rG p";
    for (int i = 0; i < 100; ++i) {
        formula = "(" + formula + ") => (rG q)";
    }
    const auto result = translated(formula);
    EXPECT_LT(node_count(result), 1000u);
}

TEST(TranslationReentrancy, ConcurrentCallsRecoverAfterInvalidInput) {
    const evrostos::Evrostos core;
    std::vector<std::future<bool>> calls;
    for (int i = 0; i < 16; ++i) {
        calls.push_back(std::async(std::launch::async, [&core] {
            const auto bad = core.translate("rG");
            const auto good = core.translate("p rR q");
            const auto* translation = std::get_if<evrostos::Translation>(&good);
            return std::holds_alternative<evrostos::Diagnostic>(bad)
                && translation != nullptr && translation->bits[0] != nullptr
                && translation->bits[0]->op() == evrostos::LtlOperator::release;
        }));
    }
    for (auto& call : calls) {
        EXPECT_TRUE(call.get());
    }
}

} // namespace
