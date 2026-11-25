// SPDX-License-Identifier: GPL-3.0-or-later
#include "backends/nusmv.hpp"
#include "backend_test_support.hpp"
#ifdef EVROSTOS_SPIN_EXECUTABLE
#include "backends/spin.hpp"
#endif

#include <gtest/gtest.h>

#include <array>
#include <future>
#include <set>
#include <sstream>

namespace {

// Construct actual translation roots and retain all subprocess evidence in
// assertion diagnostics, including native parser and trace-execution failures.
evrostos::SharedPtrLtlNode property(std::string_view formula, std::size_t bit = 0) {
    const auto translated = evrostos::Evrostos{}.translate(formula);
    if (const auto* error = std::get_if<evrostos::Diagnostic>(&translated)) {
        throw std::runtime_error(error->message);
    }
    return std::get<evrostos::Translation>(translated).bits[bit];
}

std::string diagnostics(const evrostos::backends::NuSmvResult& result) {
    std::ostringstream text;
    text << result.message << '\n';
    for (const auto& command : result.commands) {
        text << "exit: " << command.exit_code << '\n' << command.diagnostic << '\n'
             << command.standard_output << command.standard_error << '\n';
    }
    return text.str();
}

class NuSmv : public testing::Test {
protected:
    TestWorkspace workspace;
    evrostos::backends::NuSmvOptions options{
        .executable = EVROSTOS_NUSMV_EXECUTABLE, .workspace_parent = workspace.path};
    const std::string alternating = "MODULE main\nVAR p : boolean; q : boolean;\n"
        "ASSIGN init(p) := FALSE; next(p) := !p; init(q) := TRUE; next(q) := !q;\n";

    evrostos::backends::NuSmvResult check(const std::string& model,
                                         std::string_view formula, std::size_t bit = 0) {
        return evrostos::backends::NuSmvBackend(options).check(
            workspace.write("source.smv", model), property(formula, bit));
    }

    // Controlled executables test adapter classification without relying on
    // machine speed or real resource exhaustion. These are not semantic oracles.
    std::filesystem::path executable(const std::string& body) {
        const auto path = workspace.write("tool", "#!/bin/sh\n" + body);
        std::filesystem::permissions(path, std::filesystem::perms::owner_all);
        return path;
    }
};

// Every serialized operator reaches the native parser with both truth outcomes.
// The alternating trace begins p=false, q=true, and swaps both at every step.
TEST_F(NuSmv, AcceptsEveryOperatorWithTrueAndFalseOutcomes) {
    const std::array pairs{
        std::pair{"q", "p"}, std::pair{"!p", "!q"}, std::pair{"q & !p", "q & p"},
        std::pair{"p | q", "p | !q"}, std::pair{"p => q", "q => p"},
        std::pair{"rX p", "rX q"}, std::pair{"rF p", "rF (p & q)"},
        std::pair{"rG (p | q)", "rG p"}, std::pair{"q rU p", "p rU (p & q)"},
        std::pair{"p rR (p | q)", "q rR p"}};
    for (const auto& [true_formula, false_formula] : pairs) {
        for (const auto& [formula, expected] : {
            std::pair{true_formula, evrostos::backends::CheckOutcome::holds},
            std::pair{false_formula, evrostos::backends::CheckOutcome::violated}}) {
            SCOPED_TRACE(formula);
            const auto result = check(alternating, formula);
            EXPECT_EQ(result.outcome, expected) << diagnostics(result);
            EXPECT_EQ(result.has_admissible_executions, true);
            EXPECT_EQ(result.transition_relation_total, true);
            if (expected == evrostos::backends::CheckOutcome::violated) {
                EXPECT_TRUE(std::filesystem::is_regular_file(result.counterexample));
            }
        }
    }
}

// The query is supplied in main context, independently of the last module or
// existing properties; DEFINE names provide Boolean atomic propositions.
TEST_F(NuSmv, SelectsOnlyRequestedPropertyInMainContext) {
    const auto result = check("MODULE main\nVAR m : child;\nDEFINE p := m.value;\n"
        "LTLSPEC FALSE\nMODULE child\nVAR value : boolean;\n"
        "ASSIGN init(value) := TRUE; next(value) := value;\n", "rG p");
    EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(result);
}

// Universal checking covers all initial states and all nondeterministic
// successors, rather than one sampled execution.
TEST_F(NuSmv, QuantifiesOverInitialStatesAndBranches) {
    for (const auto* assignments : {"next(p) := p;",
                                   "init(p) := TRUE; next(p) := {TRUE, FALSE};"}) {
        const auto result = check(std::string("MODULE main\nVAR p : boolean;\nASSIGN ")
            + assignments, "rG p");
        EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::violated) << diagnostics(result);
    }
}

// Justice and compassion restrict admitted infinite paths. Without fairness,
// the same nondeterministic model has a permanently false execution.
TEST_F(NuSmv, HonorsJusticeAndCompassion) {
    const std::string model = "MODULE main\nVAR p : boolean;\n"
        "ASSIGN init(p) := FALSE; next(p) := {FALSE, TRUE};\n";
    for (const auto* fairness : {"", "JUSTICE p\n", "COMPASSION (TRUE, p)\n"}) {
        const auto result = check(model + fairness, "rG rF p");
        EXPECT_EQ(result.outcome, (std::string_view(fairness).empty()
            ? evrostos::backends::CheckOutcome::violated : evrostos::backends::CheckOutcome::holds))
            << diagnostics(result);
        EXPECT_EQ(result.has_admissible_executions, true);
    }
}

// Strong fairness with explicit or implicit inputs is outside NuSMV's
// supported model language. Comments and quoted names can use the same words.
TEST_F(NuSmv, ValidatesCompassionInputRestrictions) {
    for (const auto* declaration : {"IVAR i : boolean;", "VAR child : process worker;"}) {
        const auto result = check(alternating + declaration + "\nCOMPASSION (TRUE, p)\n"
            "MODULE worker\nVAR x : boolean;\n", "p");
        EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::execution_error);
        EXPECT_TRUE(result.commands.empty());
    }
    const auto commented = check(alternating + "-- IVAR\n/-- process --/\n"
        "DEFINE \"IVAR\" := p;\nCOMPASSION (TRUE, p)\n", "rF p");
    EXPECT_EQ(commented.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(commented);
}

// A property that exceeds the command reader's capacity fails in validation.
TEST_F(NuSmv, RejectsOversizedQueryBeforeLaunchingChecker) {
    const auto result = check(alternating, std::string(65536, 'p'));
    EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::execution_error);
    EXPECT_EQ(result.stage, evrostos::backends::NuSmvStage::validation);
    EXPECT_TRUE(result.commands.empty());
    EXPECT_TRUE(result.workspace.empty());
}

// FALSE holds universally exactly when the admitted execution set is empty.
// Empty initial sets, impossible fairness, and finite deadlocked paths all use
// this convention, including compassion (which CTL emptiness would not cover).
TEST_F(NuSmv, ReportsEmptyExecutionSetsAndDoesNotTotalizeDeadlocks) {
    const std::string model = "MODULE main\nVAR p : boolean;\n"
        "ASSIGN init(p) := FALSE; next(p) := p;\n";
    for (const auto* constraint : {"INIT FALSE\n", "JUSTICE FALSE\n",
                                   "COMPASSION (TRUE, FALSE)\n", "TRANS FALSE\n"}) {
        SCOPED_TRACE(constraint);
        const auto result = check(model + constraint, "p");
        EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(result);
        EXPECT_EQ(result.has_admissible_executions, false);
        EXPECT_NE(result.message.find("vacuously"), std::string::npos);
    }
    // Only the p=true initial state has an infinite execution.
    const auto mixed = check("MODULE main\nVAR p : boolean;\nTRANS p & next(p)\n", "rG p");
    EXPECT_EQ(mixed.outcome, evrostos::backends::CheckOutcome::holds) << diagnostics(mixed);
    EXPECT_EQ(mixed.has_admissible_executions, true);
    EXPECT_EQ(mixed.transition_relation_total, false);
}

// All five robust values exercise bit ordering, early stopping, and retained
// per-bit workspaces through the public rLTL verification API.
TEST_F(NuSmv, RecoversAllRobustValuesAndEvidence) {
    struct Example { const char* initial; const char* next; evrostos::RobustValue value; std::size_t count; };
    const std::array examples{
        Example{"FALSE", "FALSE", evrostos::RobustValue::b0000, 1},
        Example{"TRUE", "FALSE", evrostos::RobustValue::b0001, 2},
        Example{"FALSE", "!p", evrostos::RobustValue::b0011, 3},
        Example{"FALSE", "TRUE", evrostos::RobustValue::b0111, 4},
        Example{"TRUE", "TRUE", evrostos::RobustValue::b1111, 4}};
    std::set<std::filesystem::path> paths;
    for (const auto& example : examples) {
        SCOPED_TRACE(evrostos::to_string(example.value));
        const auto model = workspace.write("value.smv", std::string("MODULE main\nVAR p : boolean;\n")
            + "ASSIGN init(p) := " + example.initial + "; next(p) := " + example.next + ";\n");
        const auto checked = evrostos::backends::NuSmvBackend(options).verify(model, "rG p");
        ASSERT_TRUE(std::holds_alternative<evrostos::backends::NuSmvVerification>(checked));
        const auto& result = std::get<evrostos::backends::NuSmvVerification>(checked);
        EXPECT_EQ(result.summary.exact_value(), example.value);
        for (std::size_t bit = 0; bit < 4; ++bit) {
            ASSERT_EQ(result.checks[bit].has_value(), bit >= (4 - example.count));
            if (result.checks[bit]) {
                const auto& evidence = *result.checks[bit];
                EXPECT_EQ(evidence.outcome, result.summary.outcomes[bit]) << diagnostics(evidence);
                EXPECT_EQ(evidence.has_admissible_executions, true) << diagnostics(evidence);
                EXPECT_TRUE(paths.insert(evidence.workspace).second);
            }
        }
    }
}

// Nested implication must be translated before aggregation. Release exercises
// all four translated bits, not just ordinary first-bit release.
TEST_F(NuSmv, PreservesNestedImplicationAndRelease) {
    const auto model = workspace.write("implication.smv", "MODULE main\nVAR p : boolean;\n"
        "ASSIGN init(p) := FALSE; next(p) := TRUE;\nDEFINE q := FALSE;\n");
    const evrostos::backends::NuSmvBackend backend(options);
    for (const auto* formula : {"(rG p) => (rG q)", "!((rG p) => (rG q))",
                                "((rG p) => (rG q)) => q"}) {
        const auto checked = backend.verify(model, formula);
        ASSERT_TRUE(std::holds_alternative<evrostos::backends::NuSmvVerification>(checked));
        const auto expected = (std::string_view(formula) == "(rG p) => (rG q)")
            ? evrostos::RobustValue::b0000 : evrostos::RobustValue::b1111;
        EXPECT_EQ(std::get<evrostos::backends::NuSmvVerification>(checked).summary.exact_value(), expected);
    }
    const auto released = backend.verify(workspace.write("release.smv", "MODULE main\n"
        "VAR q : boolean;\nASSIGN init(q) := FALSE; next(q) := !q;\nDEFINE p := FALSE;\n"), "p rR q");
    ASSERT_TRUE(std::holds_alternative<evrostos::backends::NuSmvVerification>(released));
    EXPECT_EQ(std::get<evrostos::backends::NuSmvVerification>(released).summary.exact_value(),
              evrostos::RobustValue::b0011);
}

// Syntactic and semantic model errors must not become property counterexamples.
TEST_F(NuSmv, RejectsMalformedModelsAndInvalidAtomBindings) {
    for (const auto* model : {"MODULE main\nVAR p : ;", "MODULE main\nVAR q : boolean;",
                              "MODULE main\nVAR p : 0..5;"}) {
        const auto result = check(model, "rG p");
        EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::execution_error) << diagnostics(result);
        EXPECT_FALSE(result.has_admissible_executions.has_value());
        EXPECT_TRUE(result.counterexample.empty());
    }
}

// Input validation occurs before launching the checker or creating a workspace.
TEST_F(NuSmv, RejectsInvalidPathsLimitsAndInput) {
    const auto model = workspace.write("source.smv", alternating);
    auto invalid = options;
    invalid.executable = "NuSMV";
    EXPECT_EQ(evrostos::backends::NuSmvBackend(invalid).check(model, property("p")).outcome,
              evrostos::backends::CheckOutcome::execution_error);
    for (const auto limit : {std::chrono::milliseconds(0), std::chrono::milliseconds(-1),
                             std::chrono::milliseconds(std::chrono::hours(25))}) {
        invalid = options;
        invalid.timeout = limit;
        EXPECT_TRUE(evrostos::backends::NuSmvBackend(invalid).check(model, property("p")).workspace.empty());
    }
    invalid = options;
    invalid.max_model_bytes = 1;
    EXPECT_TRUE(evrostos::backends::NuSmvBackend(invalid).check(model, property("p")).workspace.empty());
    invalid.max_model_bytes = 0;
    EXPECT_TRUE(evrostos::backends::NuSmvBackend(invalid).check(model, property("p")).workspace.empty());
    EXPECT_TRUE(evrostos::backends::NuSmvBackend(options).check(model, {}).workspace.empty());
    EXPECT_TRUE(evrostos::backends::NuSmvBackend(options).check(model, property("MODULE")).workspace.empty());
    EXPECT_TRUE(evrostos::backends::NuSmvBackend(options).check(workspace.path, property("p")).workspace.empty());
    EXPECT_TRUE(check(std::string("MODULE main\0", 12), "p").workspace.empty());
}

// Missing tools, unexpected versions, nonzero exits, and malformed success
// output remain execution errors, retaining their command evidence.
TEST_F(NuSmv, RejectsToolAndProtocolFailures) {
    const auto model = workspace.write("source.smv", alternating);
    options.executable = workspace.path / "missing";
    auto result = evrostos::backends::NuSmvBackend(options).check(model, property("p"));
    EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::execution_error);
    ASSERT_EQ(result.commands.size(), 1);
    EXPECT_FALSE(result.commands[0].diagnostic.empty());
    for (const auto* body : {"exit 12\n", "printf '*** This is NuSMV 2.6.0\\n'\n",
                            "printf '*** This is NuSMV 2.7.1 (test)\\n'\n"}) {
        options.executable = executable(body);
        result = evrostos::backends::NuSmvBackend(options).check(model, property("p"));
        EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::execution_error);
        EXPECT_FALSE(result.has_admissible_executions);
    }
}

// Bounds terminate subprocesses and preserve uncertainty rather than false bits.
TEST_F(NuSmv, TimeoutAndOutputLimitsAreInconclusive) {
    options.max_output_bytes = 1;
    auto result = check(alternating, "p");
    EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::inconclusive);
    ASSERT_EQ(result.commands.size(), 1);
    EXPECT_TRUE(result.commands[0].output_truncated);
    options.max_output_bytes = 4096;
    options.timeout = std::chrono::milliseconds(150);
    options.executable = executable("printf '*** This is NuSMV 2.7.1 (test)\\n'\n"
        "if [ \"$3\" = check.commands ]; then sleep 5; fi\n");
    result = check(alternating, "p");
    EXPECT_EQ(result.outcome, evrostos::backends::CheckOutcome::inconclusive);
    ASSERT_FALSE(result.commands.empty());
    EXPECT_TRUE(result.commands.back().timed_out);
}

// Translation failures avoid all file/tool work. With operational failures on
// every bit, the scheduler retains the full interval and every failed attempt.
TEST_F(NuSmv, VerificationPreservesTranslationAndExecutionFailures) {
    const auto invalid = evrostos::backends::NuSmvBackend(options).verify(workspace.path / "missing", "p &");
    EXPECT_TRUE(std::holds_alternative<evrostos::Diagnostic>(invalid));
    EXPECT_TRUE(std::filesystem::is_empty(workspace.path));
    options.executable = workspace.path / "missing";
    const auto checked = evrostos::backends::NuSmvBackend(options).verify(
        workspace.write("source.smv", alternating), "rG p");
    ASSERT_TRUE(std::holds_alternative<evrostos::backends::NuSmvVerification>(checked));
    const auto& result = std::get<evrostos::backends::NuSmvVerification>(checked);
    EXPECT_EQ(result.summary.lower_bound, evrostos::RobustValue::b0000);
    EXPECT_EQ(result.summary.upper_bound, evrostos::RobustValue::b1111);
    for (const auto& check_result : result.checks) {
        ASSERT_TRUE(check_result);
        EXPECT_EQ(check_result->outcome, evrostos::backends::CheckOutcome::execution_error);
        EXPECT_FALSE(check_result->commands.empty());
    }
}

// A small serializer budget admits F p and G p but rejects the nested middle
// bits. Native evidence still narrows the result without erasing those failures.
TEST_F(NuSmv, VerificationRefinesBoundsAfterFailedQueries) {
    options.serialization_limits.max_output_bytes = 5;
    const auto checked = evrostos::backends::NuSmvBackend(options).verify(
        workspace.write("source.smv", alternating), "rG p");
    ASSERT_TRUE(std::holds_alternative<evrostos::backends::NuSmvVerification>(checked));
    const auto& result = std::get<evrostos::backends::NuSmvVerification>(checked);
    EXPECT_EQ(result.summary.lower_bound, evrostos::RobustValue::b0001);
    EXPECT_EQ(result.summary.upper_bound, evrostos::RobustValue::b0111);
    EXPECT_EQ(result.summary.outcomes[1], evrostos::backends::CheckOutcome::execution_error);
    EXPECT_EQ(result.summary.outcomes[2], evrostos::backends::CheckOutcome::execution_error);
}

// Caller paths are literal argv/filesystem values; parallel verification calls
// retain disjoint workspaces and leave the source and parent's cwd unchanged.
TEST_F(NuSmv, SupportsLiteralPathsAndConcurrentVerification) {
    const auto parent_cwd = std::filesystem::current_path();
    const auto parent = workspace.path / "spaces ' ; $()";
    std::filesystem::create_directory(parent);
    options.workspace_parent = parent;
    const auto model = workspace.write("model ' ; $().smv", alternating);
    const evrostos::backends::NuSmvBackend backend(options);
    const auto run = [&] { return backend.verify(model, "rG p"); };
    auto first = std::async(std::launch::async, run);
    auto second = std::async(std::launch::async, run);
    const auto a = std::get<evrostos::backends::NuSmvVerification>(first.get());
    const auto b = std::get<evrostos::backends::NuSmvVerification>(second.get());
    EXPECT_EQ(a.summary.exact_value(), evrostos::RobustValue::b0011);
    EXPECT_EQ(b.summary.exact_value(), evrostos::RobustValue::b0011);
    ASSERT_TRUE(a.checks[3]);
    ASSERT_TRUE(b.checks[3]);
    EXPECT_NE(a.checks[3]->workspace, b.checks[3]->workspace);
    EXPECT_EQ(std::filesystem::current_path(), parent_cwd);
    std::ifstream input(model);
    EXPECT_EQ(std::string(std::istreambuf_iterator<char>(input), {}), alternating);
}

#ifdef EVROSTOS_SPIN_EXECUTABLE
// These deterministic models have exactly the same infinite p-label sequence:
// one assignment per SPIN loop step corresponds to one NuSMV transition. No
// fairness constraints, deadlocks, atomic blocks, or extra control steps occur.
TEST_F(NuSmv, AgreesWithSpinOnEquivalentTransitionSystems) {
    const auto smv = workspace.write("paired.smv", "MODULE main\nVAR p : boolean;\n"
        "ASSIGN init(p) := FALSE; next(p) := !p;\n");
    const auto pml = workspace.write("paired.pml", "bool p = false; init { do :: p = !p od }");
    const evrostos::backends::SpinBackend spin({.executable = EVROSTOS_SPIN_EXECUTABLE,
        .c_compiler = EVROSTOS_C_COMPILER, .workspace_parent = workspace.path});
    const evrostos::backends::NuSmvBackend nusmv(options);
    for (const auto* formula : {"rG p", "rX p", "rG (p => rX !p)", "p rR !p"}) {
        SCOPED_TRACE(formula);
        const auto a = std::get<evrostos::backends::NuSmvVerification>(nusmv.verify(smv, formula));
        const auto b = std::get<evrostos::backends::SpinVerification>(spin.verify(pml, formula));
        ASSERT_TRUE(a.summary.exact_value());
        EXPECT_EQ(a.summary.exact_value(), b.summary.exact_value());
    }
}
#endif

} // namespace
