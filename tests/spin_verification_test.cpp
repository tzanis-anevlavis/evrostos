// SPDX-License-Identifier: GPL-3.0-or-later
#include "backends/spin.hpp"
#include "spin_test_support.hpp"

#include <gtest/gtest.h>

#include <array>
#include <future>
#include <set>

namespace {

// Each fixture owns its models and every retained single-property workspace.
class SpinVerification : public testing::Test {
protected:
    TestWorkspace workspace;
    evrostos::backends::SpinOptions options{
        .executable = EVROSTOS_SPIN_EXECUTABLE,
        .c_compiler = EVROSTOS_C_COMPILER,
        .workspace_parent = workspace.path};
};

// Native SPIN realizes all five robust always values. Evidence is indexed by
// bit, and early termination leaves stronger queries without a workspace.
TEST_F(SpinVerification, RecoversAllValuesAndRetainsPerBitEvidence) {
    struct Example {
        const char* model;
        evrostos::RobustValue value;
        std::size_t checks;
    };
    const std::array examples{
        Example{"bool p = false; init { do :: p = false od }", evrostos::RobustValue::b0000, 1},
        Example{"bool p = true; init { do :: p = false od }", evrostos::RobustValue::b0001, 2},
        Example{"bool p = false; init { do :: p = !p od }", evrostos::RobustValue::b0011, 3},
        Example{"bool p = false; init { do :: p = true od }", evrostos::RobustValue::b0111, 4},
        Example{"bool p = true; init { do :: p = true od }", evrostos::RobustValue::b1111, 4}};
    std::set<std::filesystem::path> workspaces;
    const evrostos::backends::SpinBackend backend(options);
    for (const auto& example : examples) {
        SCOPED_TRACE(evrostos::to_string(example.value));
        const auto model = workspace.write("model.pml", example.model);
        const auto checked = backend.verify(model, "rG p");
        ASSERT_TRUE(std::holds_alternative<evrostos::backends::SpinVerification>(checked));
        const auto& result = std::get<evrostos::backends::SpinVerification>(checked);
        EXPECT_EQ(result.summary.exact_value(), example.value);
        for (std::size_t bit = 0; bit < 4; ++bit) {
            ASSERT_EQ(result.checks[bit].has_value(), bit >= (4 - example.checks));
            if (!result.checks[bit]) {
                EXPECT_FALSE(result.summary.outcomes[bit].has_value());
                continue;
            }
            const auto& evidence = *result.checks[bit];
            EXPECT_EQ(result.summary.outcomes[bit], evidence.outcome) << evidence.message;
            EXPECT_FALSE(evidence.commands.empty());
            EXPECT_TRUE(std::filesystem::is_regular_file(evidence.workspace / "model.pml"));
            EXPECT_TRUE(workspaces.insert(evidence.workspace).second);
            if (evidence.outcome == evrostos::backends::CheckOutcome::violated) {
                EXPECT_EQ(evidence.stage, evrostos::backends::SpinStage::replay);
                EXPECT_TRUE(std::filesystem::is_regular_file(evidence.trail));
            }
        }
    }
}

// Implication is translated on traces before aggregation. Its nested negation
// distinguishes full semantics from pointwise implication of aggregated bits.
TEST_F(SpinVerification, PreservesNestedImplicationAndReleaseSemantics) {
    const auto model = workspace.write("implication.pml",
        "bool p = false; bool q = false; init { do :: p = true od }");
    const evrostos::backends::SpinBackend backend(options);
    for (const auto* formula : {"(rG p) => (rG q)", "!((rG p) => (rG q))",
                                "((rG p) => (rG q)) => q"}) {
        SCOPED_TRACE(formula);
        const auto checked = backend.verify(model, formula);
        ASSERT_TRUE(std::holds_alternative<evrostos::backends::SpinVerification>(checked));
        const auto expected = (std::string_view(formula) == "(rG p) => (rG q)")
            ? evrostos::RobustValue::b0000 : evrostos::RobustValue::b1111;
        EXPECT_EQ(std::get<evrostos::backends::SpinVerification>(checked).summary.exact_value(), expected);
    }
    const auto alternating = workspace.write("release.pml",
        "bool p = false; bool q = false; init { do :: q = !q od }");
    const auto released = backend.verify(alternating, "p rR q");
    ASSERT_TRUE(std::holds_alternative<evrostos::backends::SpinVerification>(released));
    EXPECT_EQ(std::get<evrostos::backends::SpinVerification>(released).summary.exact_value(),
              evrostos::RobustValue::b0011);
}

// Parser and translation-budget failures take precedence over model access and
// create no checker workspaces, even when the model path does not exist.
TEST_F(SpinVerification, TranslationDiagnosticsLaunchNoTools) {
    const evrostos::backends::SpinBackend backend(options);
    const auto syntax = backend.verify(workspace.path / "missing.pml", "p &");
    ASSERT_TRUE(std::holds_alternative<evrostos::Diagnostic>(syntax));
    EXPECT_EQ(std::get<evrostos::Diagnostic>(syntax).code, evrostos::DiagnosticCode::syntax_error);
    const auto limited = backend.verify(workspace.path / "missing.pml", "rG p", {.max_input_bytes = 1});
    ASSERT_TRUE(std::holds_alternative<evrostos::Diagnostic>(limited));
    EXPECT_EQ(std::get<evrostos::Diagnostic>(limited).code, evrostos::DiagnosticCode::input_limit);
    EXPECT_TRUE(std::filesystem::is_empty(workspace.path));
}

// Tool failures retain the stage and command diagnostic for each attempted bit.
// With no truth evidence the full interval remains possible.
TEST_F(SpinVerification, ToolFailuresRemainUnresolvedWithDiagnostics) {
    options.executable = workspace.path / "missing";
    const auto model = workspace.write("model.pml", "bool p = true; init { skip }");
    const auto checked = evrostos::backends::SpinBackend(options).verify(model, "rG p");
    ASSERT_TRUE(std::holds_alternative<evrostos::backends::SpinVerification>(checked));
    const auto& result = std::get<evrostos::backends::SpinVerification>(checked);
    EXPECT_EQ(result.summary.lower_bound, evrostos::RobustValue::b0000);
    EXPECT_EQ(result.summary.upper_bound, evrostos::RobustValue::b1111);
    EXPECT_FALSE(result.summary.exact_value().has_value());
    for (std::size_t bit = 0; bit < 4; ++bit) {
        ASSERT_TRUE(result.checks[bit].has_value());
        const auto& evidence = *result.checks[bit];
        EXPECT_EQ(evidence.outcome, evrostos::backends::CheckOutcome::execution_error);
        EXPECT_EQ(result.summary.outcomes[bit], evidence.outcome);
        EXPECT_EQ(evidence.stage, evrostos::backends::SpinStage::version);
        ASSERT_EQ(evidence.commands.size(), 1);
        EXPECT_FALSE(evidence.commands.front().diagnostic.empty());
    }
}

// A per-property capture limit applies to every query, with truncation evidence
// retained for each. Resource exhaustion contributes no false bits.
TEST_F(SpinVerification, OutputLimitsRetainInconclusiveEvidence) {
    options.max_output_bytes = 1;
    const auto model = workspace.write("model.pml", "bool p = true; init { skip }");
    const auto checked = evrostos::backends::SpinBackend(options).verify(model, "rG p");
    ASSERT_TRUE(std::holds_alternative<evrostos::backends::SpinVerification>(checked));
    const auto& result = std::get<evrostos::backends::SpinVerification>(checked);
    EXPECT_EQ(result.summary.lower_bound, evrostos::RobustValue::b0000);
    EXPECT_EQ(result.summary.upper_bound, evrostos::RobustValue::b1111);
    for (std::size_t bit = 0; bit < 4; ++bit) {
        ASSERT_TRUE(result.checks[bit].has_value());
        EXPECT_EQ(result.summary.outcomes[bit], evrostos::backends::CheckOutcome::inconclusive);
        ASSERT_FALSE(result.checks[bit]->commands.empty());
        EXPECT_TRUE(result.checks[bit]->commands.back().output_truncated);
    }
}

// A six-byte serialization budget admits G p and F p but rejects F G p and
// G F p. A later G p proof still establishes 1111; a G p counterexample leaves
// [0001, 0111]. Both results retain the intermediate validation failures.
TEST_F(SpinVerification, StrongerEvidenceRefinesBoundsAfterSerializationFailures) {
    options.serialization_limits.max_output_bytes = 6;
    const evrostos::backends::SpinBackend backend(options);
    for (const bool invariant : {true, false}) {
        SCOPED_TRACE(invariant);
        const auto model = workspace.write("model.pml", invariant
            ? "bool p = true; init { do :: p = true od }"
            : "bool p = false; init { do :: p = !p od }");
        const auto checked = backend.verify(model, "rG p");
        ASSERT_TRUE(std::holds_alternative<evrostos::backends::SpinVerification>(checked));
        const auto& result = std::get<evrostos::backends::SpinVerification>(checked);
        if (invariant) {
            EXPECT_EQ(result.summary.exact_value(), evrostos::RobustValue::b1111);
        } else {
            EXPECT_FALSE(result.summary.exact_value().has_value());
            EXPECT_EQ(result.summary.lower_bound, evrostos::RobustValue::b0001);
            EXPECT_EQ(result.summary.upper_bound, evrostos::RobustValue::b0111);
        }
        EXPECT_EQ(result.summary.outcomes[3], evrostos::backends::CheckOutcome::holds);
        for (const std::size_t bit : {1U, 2U}) {
            ASSERT_TRUE(result.checks[bit].has_value());
            EXPECT_EQ(result.summary.outcomes[bit], evrostos::backends::CheckOutcome::execution_error);
            EXPECT_EQ(result.checks[bit]->stage, evrostos::backends::SpinStage::validation);
            EXPECT_FALSE(result.checks[bit]->message.empty());
            EXPECT_TRUE(result.checks[bit]->commands.empty());
        }
    }
}

// The rLTL entry point inherits default X support and forwards explicit opt-out
// to all four single-property checks without manufacturing a robust value.
TEST_F(SpinVerification, NextUsesBackendOptions) {
    const auto model = workspace.write("model.pml", "bool p = false; init { p = true }");
    const auto checked = evrostos::backends::SpinBackend(options).verify(model, "rX p");
    ASSERT_TRUE(std::holds_alternative<evrostos::backends::SpinVerification>(checked));
    EXPECT_EQ(std::get<evrostos::backends::SpinVerification>(checked).summary.exact_value(),
              evrostos::RobustValue::b1111);
    options.allow_next = false;
    const auto rejected = evrostos::backends::SpinBackend(options).verify(model, "rX p");
    ASSERT_TRUE(std::holds_alternative<evrostos::backends::SpinVerification>(rejected));
    const auto& result = std::get<evrostos::backends::SpinVerification>(rejected);
    EXPECT_FALSE(result.summary.exact_value().has_value());
    for (const auto& evidence : result.checks) {
        ASSERT_TRUE(evidence.has_value());
        EXPECT_EQ(evidence->outcome, evrostos::backends::CheckOutcome::execution_error);
        EXPECT_TRUE(evidence->workspace.empty());
        EXPECT_TRUE(evidence->commands.empty());
    }
}

// Two verification calls on one backend retain disjoint artifacts while sharing
// a read-only model. Their independent schedules both terminate on b4 = false.
TEST_F(SpinVerification, ConcurrentCallsRetainSeparateWorkspaces) {
    const auto model = workspace.write("model.pml", "bool p = false; init { do :: p = false od }");
    const evrostos::backends::SpinBackend backend(options);
    auto first = std::async(std::launch::async, [&] { return backend.verify(model, "rG p"); });
    auto second = std::async(std::launch::async, [&] { return backend.verify(model, "rG p"); });
    const auto a = first.get();
    const auto b = second.get();
    ASSERT_TRUE(std::holds_alternative<evrostos::backends::SpinVerification>(a));
    ASSERT_TRUE(std::holds_alternative<evrostos::backends::SpinVerification>(b));
    const auto& left = std::get<evrostos::backends::SpinVerification>(a);
    const auto& right = std::get<evrostos::backends::SpinVerification>(b);
    EXPECT_EQ(left.summary.exact_value(), evrostos::RobustValue::b0000);
    EXPECT_EQ(right.summary.exact_value(), evrostos::RobustValue::b0000);
    ASSERT_TRUE(left.checks[3].has_value());
    ASSERT_TRUE(right.checks[3].has_value());
    EXPECT_NE(left.checks[3]->workspace, right.checks[3]->workspace);
}

} // namespace
