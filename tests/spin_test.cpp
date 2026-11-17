// SPDX-License-Identifier: GPL-3.0-or-later
#include "backends/spin.hpp"
#include "evrostos.hpp"
#include "spin_test_support.hpp"

#include <gtest/gtest.h>

#include <array>
#include <future>
#include <sstream>

namespace {

// Parse fixture formulas through the public translator so the tests exercise
// the same immutable LTL roots that callers supply to the backend.
evrostos::Translation translate(std::string_view formula) {
    const auto result = evrostos::Evrostos{}.translate(formula);
    if (const auto* error = std::get_if<evrostos::Diagnostic>(&result)) {
        throw std::runtime_error(error->message);
    }
    return std::get<evrostos::Translation>(result);
}

// Attach the full command history to failed assertions, including the stage's
// captured streams, so parser, compiler, and verifier failures remain visible.
std::string diagnostics(const evrostos::backends::SpinResult& result) {
    std::ostringstream text;
    text << result.message << '\n';
    for (const auto& command : result.commands) {
        for (const auto& argument : command.arguments) {
            text << argument << ' ';
        }
        text << "\nexit: " << command.exit_code << '\n' << command.diagnostic << '\n'
             << command.standard_output << command.standard_error << '\n';
    }
    return text.str();
}

// Use the pinned checker and configured C compiler supplied by CMake. Each test
// owns the parent directory of its models and retained backend workspaces.
class Spin : public testing::Test {
protected:
    TestWorkspace workspace;
    evrostos::backends::SpinOptions options{
        .executable = EVROSTOS_SPIN_EXECUTABLE,
        .c_compiler = EVROSTOS_C_COMPILER,
        .workspace_parent = workspace.path};

    evrostos::backends::SpinResult check(const std::string& model,
        std::string_view formula, std::size_t bit = 0) {
        // Each call checks one selected bit; scheduling all four is test logic.
        const auto path = workspace.write("source.pml", model);
        return evrostos::backends::SpinBackend(options).check(path, translate(formula).bits[bit]);
    }
};

// G p holds on a permanently true state and fails when p is initially false.
// The failure keeps a replayed trail, and the two runs use separate workspaces.
TEST_F(Spin, ExhaustiveSafetyHoldAndCounterexample) {
    const auto holds = check("bool p = true; init { do :: p = true od }", "rG p");
    EXPECT_EQ(holds.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(holds);
    const auto violated = check("bool p = false; init { do :: p = !p od }", "rG p");
    EXPECT_EQ(violated.outcome, evrostos::backends::CheckOutcome::violated) << diagnostics(violated);
    EXPECT_TRUE(std::filesystem::is_regular_file(violated.trail));
    EXPECT_NE(holds.workspace, violated.workspace);
    EXPECT_EQ(violated.stage, evrostos::backends::SpinStage::replay);
}

// Alternating p satisfies G F p and F p, but neither G p nor F G p: value 0011.
TEST_F(Spin, ChecksEveryAlwaysBitOnAlternatingTrace) {
    const std::array expected{
        evrostos::backends::CheckOutcome::violated, evrostos::backends::CheckOutcome::violated,
        evrostos::backends::CheckOutcome::holds, evrostos::backends::CheckOutcome::holds};
    for (std::size_t bit = 0; bit < 4; ++bit) {
        const auto result = check("bool p = false; init { do :: p = !p od }", "rG p", bit);
        EXPECT_EQ(result.outcome, expected[bit]) << "bit " << bit << '\n' << diagnostics(result);
    }
}

// F p fails when p stays false indefinitely. The verifier must find a recurring
// accepting state, exercising the cycle-counterexample path.
TEST_F(Spin, LivenessCounterexampleUsesAcceptanceCycle) {
    const auto result = check("bool p = false; init { do :: p = false od }", "rF p");
    EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::violated) << diagnostics(result);
    ASSERT_GE(result.commands.size(), 2);
    EXPECT_NE(result.commands[result.commands.size() - 2].standard_output.find("acceptance cycle"),
              std::string::npos);
}

// Here G p holds, but a model assertion fails. Trail provenance keeps that model
// error separate from a violation of the requested LTL property.
TEST_F(Spin, ModelAssertionIsAnExecutionError) {
    const auto result = check("bool p = true; init { assert(false); do :: p = true od }", "rG p");
    EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::execution_error) << diagnostics(result);
    EXPECT_TRUE(std::filesystem::is_regular_file(result.trail));
    EXPECT_EQ(result.stage, evrostos::backends::SpinStage::replay);
}

// The model's existing claim has the opposite answer. Both cases require the
// adapter to select its appended property by name instead of using the first claim.
TEST_F(Spin, SelectsPropertyByNameWhenModelHasOtherProperties) {
    const auto result = check(
        "bool p = true; init { do :: p = true od } ltl unrelated { [] (!p) }", "rG p");
    EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(result);
    const auto opposite = check(
        "bool p = false; init { do :: p = false od } ltl unrelated { [] (!p) }", "rG p");
    EXPECT_EQ(opposite.outcome, evrostos::backends::CheckOutcome::violated) << diagnostics(opposite);
}

// X is accepted by default and observes individual steps: an irrelevant
// assignment separates X p from X X p.
TEST_F(Spin, NextIsEnabledByDefaultAndCheckedWithoutReduction) {
    const auto result = check("bool p = false; init { p = true }", "rX p");
    EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(result);
    const auto opposite = check("bool p = true; init { p = false }", "rX p");
    EXPECT_EQ(opposite.outcome, evrostos::backends::CheckOutcome::violated) << diagnostics(opposite);
    const std::string steps = "bool p = false; byte unused; init { unused = 1; p = true }";
    const auto first = check(steps, "rX p");
    const auto second = check(steps, "rX rX p");
    EXPECT_EQ(first.outcome, evrostos::backends::CheckOutcome::violated) << diagnostics(first);
    EXPECT_EQ(second.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(second);
}

// Explicitly disabling X rejects direct and nested occurrences before workspace
// creation or tool execution. Properties without X remain checkable.
TEST_F(Spin, NextCanBeDisabledExplicitly) {
    options.allow_next = false;
    for (const auto* formula : {"rX p", "rG (p & rX p)", "!((rX p) => p)"}) {
        SCOPED_TRACE(formula);
        const auto rejected = check("bool p = true; init { p = true }", formula);
        EXPECT_EQ(rejected.outcome, evrostos::backends::CheckOutcome::execution_error);
        EXPECT_EQ(rejected.stage, evrostos::backends::SpinStage::validation);
        EXPECT_TRUE(rejected.workspace.empty());
        EXPECT_TRUE(rejected.commands.empty());
    }
    const auto result = check("bool p = true; init { p = true }", "rG p");
    EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(result);
}

// A relative include defines the formula atom as a macro. Paths with shell
// syntax remain literal, and the original model remains byte-for-byte unchanged.
TEST_F(Spin, PreprocessesMacrosAndRelativeIncludesWithLiteralPaths) {
    const auto directory = workspace.path / "spaces ' ; $()";
    std::filesystem::create_directory(directory);
    TestWorkspace inputs;
    const auto header = inputs.write("state.h", "bool ready = true;\n#define p (ready == true)\n");
    std::filesystem::copy_file(header, directory / "state.h");
    const std::string source = "#include \"state.h\"\ninit { do :: ready = true od }\n";
    const auto original = inputs.write("source.pml", source);
    const auto model = directory / "model ; $(touch injected).pml";
    std::filesystem::copy_file(original, model);
    options.workspace_parent = directory;
    const auto result = evrostos::backends::SpinBackend(options).check(model, translate("rG p").bits[0]);
    EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(result);
    std::ifstream unchanged(model);
    EXPECT_EQ(std::string(std::istreambuf_iterator<char>(unchanged), {}), source);
    EXPECT_FALSE(std::filesystem::exists(directory / "injected"));
}

// Syntax errors, unresolved atoms, and collisions with the reserved property
// name produce execution errors, with no property truth value inferred.
TEST_F(Spin, RejectsMalformedModelsUndefinedAtomsAndPropertyNameCollisions) {
    for (const auto* model : {"bool p = true; init { broken syntax }",
                              "init { skip }",
                              "bool p = true; init { skip } ltl evrostos_property { p }"}) {
        const auto result = check(model, "p");
        EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::execution_error) << diagnostics(result);
    }
}

// Reject unsupported acceptance machinery and embedded C during preprocessing,
// including acceptance labels separated from ':' by less common whitespace.
TEST_F(Spin, RejectsCustomAcceptanceAndEmbeddedC) {
    for (const auto* model : {
        "bool p = true; init { accept_user: do :: p = true od }",
        "bool p = true; init { accept_user\f\v: do :: p = true od }",
        "bool p = true; init { c_code { printf(\"hi\"); }; skip }",
        "bool p = true; init { skip } never { do :: true od }"}) {
        const auto result = check(model, "p");
        EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::execution_error) << diagnostics(result);
        EXPECT_EQ(result.stage, evrostos::backends::SpinStage::preprocessing);
    }
}

// The property depends on the counter, preventing SPIN from discarding it as a
// write-only variable. Shallow search and insufficient memory leave it undecided.
TEST_F(Spin, DepthAndMemoryLimitsAreInconclusive) {
    const std::string model = "#define p (n < 255)\nbyte n = 0; init { do :: n++ od }";
    options.max_depth = 10;
    const auto depth = check(model, "rG p");
    EXPECT_EQ(depth.outcome, evrostos::backends::CheckOutcome::inconclusive) << diagnostics(depth);
    EXPECT_EQ(depth.stage, evrostos::backends::SpinStage::search);
    options.max_depth = 100000;
    options.memory_megabytes = 1;
    const auto memory = check(model, "rG p");
    EXPECT_EQ(memory.outcome, evrostos::backends::CheckOutcome::inconclusive) << diagnostics(memory);
}

// Tiny pipeline and capture budgets stop execution before a result is available.
// The output-limited run also records that its captured stream was truncated.
TEST_F(Spin, PipelineTimeoutAndOutputLimitAreInconclusive) {
    options.timeout = std::chrono::milliseconds(1);
    const auto timed = check("bool p = true; init { skip }", "p");
    EXPECT_EQ(timed.outcome, evrostos::backends::CheckOutcome::inconclusive) << diagnostics(timed);
    options.timeout = std::chrono::seconds(60);
    options.max_output_bytes = 1;
    const auto limited = check("bool p = true; init { skip }", "p");
    EXPECT_EQ(limited.outcome, evrostos::backends::CheckOutcome::inconclusive) << diagnostics(limited);
    ASSERT_FALSE(limited.commands.empty());
    EXPECT_TRUE(limited.commands.back().output_truncated);
}

// Exercise failure before workspace creation, at tool launch, during version
// validation, and in preprocessing; each result identifies the failing stage.
TEST_F(Spin, ReportsValidationAndToolFailures) {
    options.executable = workspace.path / "missing";
    const auto missing = check("bool p = true; init { skip }", "p");
    EXPECT_EQ(missing.outcome, evrostos::backends::CheckOutcome::execution_error);
    EXPECT_EQ(missing.stage, evrostos::backends::SpinStage::version);
    ASSERT_EQ(missing.commands.size(), 1);
    EXPECT_FALSE(missing.commands.front().diagnostic.empty());
    options.executable = "/usr/bin/true";
    const auto version = check("bool p = true; init { skip }", "p");
    EXPECT_EQ(version.outcome, evrostos::backends::CheckOutcome::execution_error);
    EXPECT_NE(version.message.find("unsupported SPIN version"), std::string::npos);
    options.executable = EVROSTOS_SPIN_EXECUTABLE;
    options.c_compiler = "/usr/bin/false";
    const auto compiler = check("bool p = true; init { skip }", "p");
    EXPECT_EQ(compiler.outcome, evrostos::backends::CheckOutcome::execution_error);
    EXPECT_EQ(compiler.stage, evrostos::backends::SpinStage::preprocessing);
    options.max_depth = 0;
    const auto invalid = check("bool p = true; init { skip }", "p");
    EXPECT_EQ(invalid.outcome, evrostos::backends::CheckOutcome::execution_error);
    EXPECT_TRUE(invalid.workspace.empty());
}

// Simultaneous checks share the model and backend options while generated sources,
// executables, and trails remain in distinct per-call workspaces.
TEST_F(Spin, CallsOnSameBackendHaveIndependentWorkspaces) {
    const auto model = workspace.write("shared.pml", "bool p = true; init { do :: p = true od }");
    const evrostos::backends::SpinBackend backend(options);
    const auto translation = translate("rG p");
    auto first = std::async(std::launch::async, [&] { return backend.check(model, translation.bits[0]); });
    auto second = std::async(std::launch::async, [&] { return backend.check(model, translation.bits[3]); });
    const auto a = first.get();
    const auto b = second.get();
    EXPECT_EQ(a.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(a);
    EXPECT_EQ(b.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(b);
    EXPECT_NE(a.workspace, b.workspace);
}

// Run every emitted operator through the native parser and verifier with both
// true and false outcomes on this model, including a tautology and contradiction.
TEST_F(Spin, ExecutesAllSerializedOperators) {
    const std::string model = "bool p = true; bool q = false; init { do :: q = !q od }";
    for (const auto* formula : {"p", "!q", "p & !q", "p | q", "q => p", "rX p",
                                "rF q", "rG p", "p rU q", "q rR p", "p | !p"}) {
        const auto result = check(model, formula);
        EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::holds)
            << formula << '\n' << diagnostics(result);
    }
    for (const auto* formula : {"q", "!p", "p & q", "!p | q", "p => q", "rX !p",
                                "rF !p", "rG q", "q rU !p", "p rR q", "p & !p"}) {
        const auto result = check(model, formula);
        EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::violated)
            << formula << '\n' << diagnostics(result);
    }
}

// Constant, eventually constant, and alternating traces realize all five robust
// always values. Check each of their four translated bits independently.
TEST_F(Spin, RealCheckerDistinguishesAllFiveRobustAlwaysValues) {
    struct Example {
        const char* model;
        const char* bits;
    };
    const std::array cases{
        Example{"bool p = true; init { do :: p = true od }", "1111"},
        Example{"bool p = false; init { do :: p = true od }", "0111"},
        Example{"bool p = false; init { do :: p = !p od }", "0011"},
        Example{"bool p = true; init { do :: p = false od }", "0001"},
        Example{"bool p = false; init { do :: p = false od }", "0000"}};
    for (const auto& example : cases) {
        for (std::size_t bit = 0; bit < 4; ++bit) {
            const auto result = check(example.model, "rG p", bit);
            const auto expected = (example.bits[bit] == '1')
                ? evrostos::backends::CheckOutcome::holds : evrostos::backends::CheckOutcome::violated;
            EXPECT_EQ(result.outcome, expected) << example.bits << " bit " << bit << '\n' << diagnostics(result);
        }
    }
}

// On this trace rG p = 0111 and rG q = 0000, so their implication is 0000.
// Its negation and implication to q are both 1111, testing recursive translation.
TEST_F(Spin, NestedImplicationAndReleasePreserveAllFourBits) {
    const std::string implication_model = "bool p = false; bool q = false; init { do :: p = true od }";
    for (const auto* formula : {"(rG p) => (rG q)", "!((rG p) => (rG q))", "((rG p) => (rG q)) => q"}) {
        const auto expected = (std::string_view(formula) == "(rG p) => (rG q)")
            ? evrostos::backends::CheckOutcome::violated : evrostos::backends::CheckOutcome::holds;
        for (std::size_t bit = 0; bit < 4; ++bit) {
            const auto result = check(implication_model, formula, bit);
            EXPECT_EQ(result.outcome, expected) << formula << " bit " << bit << '\n' << diagnostics(result);
        }
    }
    // With p permanently false, robust release has the value of rG q: 0011
    // when q alternates. This exercises the distinct weaker release queries.
    for (std::size_t bit = 0; bit < 4; ++bit) {
        const auto result = check("bool p = false; bool q = false; init { do :: q = !q od }", "p rR q", bit);
        const auto expected = (bit < 2)
            ? evrostos::backends::CheckOutcome::violated : evrostos::backends::CheckOutcome::holds;
        EXPECT_EQ(result.outcome, expected) << "release bit " << bit << '\n' << diagnostics(result);
    }
}

// A single violating branch falsifies G p. With weak fairness disabled, Spinner
// can also run indefinitely while Setter remains enabled, falsifying F p.
TEST_F(Spin, UniversalCheckingIncludesUnfairExecutions) {
    const auto branching = check("bool p = true; init { if :: p = true :: p = false fi }", "rG p");
    EXPECT_EQ(branching.outcome, evrostos::backends::CheckOutcome::violated) << diagnostics(branching);
    const auto unfair = check("bool p = false; bool tick = false; "
        "active proctype Setter() { p = true } "
        "active proctype Spinner() { do :: tick = !tick od }", "rF p");
    EXPECT_EQ(unfair.outcome, evrostos::backends::CheckOutcome::violated) << diagnostics(unfair);
}

// Repeating the final state makes F p false and G !p true both after normal
// termination and at a blocked statement, matching SPIN's stuttering extension.
TEST_F(Spin, TerminalAndDeadlockedStatesUseSpinStuttering) {
    for (const auto* model : {"bool p = false; init { skip }", "bool p = false; init { false }"}) {
        const auto future = check(model, "rF p");
        const auto invariant = check(model, "rG !p");
        EXPECT_EQ(future.outcome, evrostos::backends::CheckOutcome::violated) << diagnostics(future);
        EXPECT_EQ(invariant.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(invariant);
    }
}

// Reserved-looking text inside comments or strings does not reject a valid model.
// Oversized input, NUL bytes, and a null formula fail before checker execution.
TEST_F(Spin, InputProfileChecksTokensWithoutRejectingCommentsAndStrings) {
    const auto result = check("bool accepted = true;\n#define p accepted\n"
        "/* never { c_code accept_label: } */\n"
        "init { printf(\"never c_code accept_label: evrostos_property\"); do :: accepted = true od }", "rG p");
    EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(result);
    options.max_model_bytes = 1;
    const auto too_large = check("bool p = true; init { skip }", "p");
    EXPECT_EQ(too_large.outcome, evrostos::backends::CheckOutcome::execution_error);
    EXPECT_TRUE(too_large.workspace.empty());
    options.max_model_bytes = 1024;
    const auto nul = check(std::string("x\0y", 3), "p");
    EXPECT_EQ(nul.outcome, evrostos::backends::CheckOutcome::execution_error);
    EXPECT_TRUE(nul.workspace.empty());
    const auto model = workspace.write("null.pml", "init { skip }");
    const auto null = evrostos::backends::SpinBackend(options).check(model, {});
    EXPECT_EQ(null.outcome, evrostos::backends::CheckOutcome::execution_error);
    EXPECT_TRUE(null.commands.empty());
    // Inline LTL accepts this mixed-case atom, including its operator-like prefix.
    const auto uppercase = check("bool GFReady2 = true; init { do :: GFReady2 = true od }", "rG GFReady2");
    EXPECT_EQ(uppercase.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(uppercase);
}

} // namespace
