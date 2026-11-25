// SPDX-License-Identifier: GPL-3.0-or-later
#include "backends/detail/nusmv_result.hpp"

#include <gtest/gtest.h>
#include <string>

namespace {

// Fixed output from the pinned script protocol; native integration tests check
// that these section and result spellings agree with the actual checker.
std::string protocol(bool holds, bool empty = false) {
    return std::string("EVROSTOS_MODEL \n"
        "The transition relation is total: No deadlock state exists\n"
        "EVROSTOS_EXECUTIONS \n-- specification FALSE is ") + (empty ? "true" : "false")
        + "\nEVROSTOS_PROPERTY \n-- specification G p is " + (holds ? "true" : "false")
        + (holds ? "\n" : "\nTrace Type: Counterexample \n  -- Loop starts here\n  -> State: 1.1 <-\n")
        + "EVROSTOS_REPLAY \n"
        + (holds ? "" : "Trace 1 execution completed successfully.\n2 steps performed.\n")
        + "EVROSTOS_DONE \n";
}

// Exact version boundaries reject both older releases and prefix collisions.
TEST(NuSmvProtocol, ChecksVersionBoundary) {
    using evrostos::backends::detail::supported_nusmv_version;
    EXPECT_TRUE(supported_nusmv_version("*** This is NuSMV 2.7.1 (compiled today)\n"));
    EXPECT_FALSE(supported_nusmv_version("*** This is NuSMV 2.7.10\n"));
    EXPECT_FALSE(supported_nusmv_version("*** This is NuSMV 2.6.0\n"));
    EXPECT_FALSE(supported_nusmv_version("unrelated output\n*** This is NuSMV 2.7.1\n"));
}

// Truth and execution-set emptiness are independent fields; an empty model
// cannot simultaneously carry a violating execution.
TEST(NuSmvProtocol, SeparatesTruthFromVacuity) {
    for (const bool holds : {true, false}) {
        const auto output = protocol(holds);
        const auto finding = evrostos::backends::detail::inspect_nusmv(output);
        ASSERT_TRUE(finding.has_value());
        EXPECT_EQ(finding->holds, holds);
        EXPECT_TRUE(finding->has_admissible_executions);
        EXPECT_EQ(finding->counterexample.empty(), holds);
    }
    const auto empty = protocol(true, true);
    const auto finding = evrostos::backends::detail::inspect_nusmv(empty);
    ASSERT_TRUE(finding.has_value());
    EXPECT_FALSE(finding->has_admissible_executions);
    EXPECT_FALSE(evrostos::backends::detail::inspect_nusmv(protocol(false, true)).has_value());
}

// A process exit alone cannot compensate for a missing section, verdict, trace,
// loop marker, or replay result. Each deletion invalidates the completed protocol.
TEST(NuSmvProtocol, RejectsMissingEvidence) {
    for (const auto* required : {"EVROSTOS_MODEL \n", "EVROSTOS_EXECUTIONS \n",
        "EVROSTOS_PROPERTY \n", "EVROSTOS_REPLAY \n", "EVROSTOS_DONE \n",
        "The transition relation is total: No deadlock state exists\n",
        "-- specification FALSE is false\n", "-- specification G p is false\n",
        "Trace Type: Counterexample \n", "-- Loop starts here\n", "-> State: 1.1 <-",
        "Trace 1 execution completed successfully.\n"}) {
        SCOPED_TRACE(required);
        auto output = protocol(false);
        output.erase(output.find(required), std::string_view(required).size());
        EXPECT_FALSE(evrostos::backends::detail::inspect_nusmv(output).has_value());
    }
}

// Duplicate and contradictory evidence must not select whichever answer appears
// first. Section ordering is part of the command protocol.
TEST(NuSmvProtocol, RejectsConflictingEvidence) {
    for (const auto* extra : {"-- specification p is true\n", "EVROSTOS_PROPERTY \n"}) {
        auto output = protocol(false);
        output.insert(output.find("EVROSTOS_REPLAY"), extra);
        EXPECT_FALSE(evrostos::backends::detail::inspect_nusmv(output).has_value());
    }
    auto failed = protocol(false);
    failed.insert(failed.find("EVROSTOS_DONE"), "Trace execution failed!\n");
    EXPECT_FALSE(evrostos::backends::detail::inspect_nusmv(failed).has_value());
    auto extra_trace = protocol(true);
    extra_trace.insert(extra_trace.find("EVROSTOS_REPLAY"), "Trace Type: Counterexample \n");
    EXPECT_FALSE(evrostos::backends::detail::inspect_nusmv(extra_trace).has_value());
    auto extra_replay = protocol(true);
    extra_replay.insert(extra_replay.find("EVROSTOS_DONE"), "Trace 1 execution completed successfully.\n");
    EXPECT_FALSE(evrostos::backends::detail::inspect_nusmv(extra_replay).has_value());
}

// Keep both markers unique and exchange their positions to isolate ordering.
TEST(NuSmvProtocol, RejectsOutOfOrderSections) {
    auto output = protocol(true);
    constexpr std::string_view model = "EVROSTOS_MODEL";
    constexpr std::string_view replay = "EVROSTOS_REPLAY";
    const auto first = output.find(model);
    const auto second = output.find(replay);
    output.replace(second, replay.size(), model);
    output.replace(first, model.size(), replay);
    EXPECT_FALSE(evrostos::backends::detail::inspect_nusmv(output).has_value());
}

// Non-total transition systems are represented, not silently repaired.
TEST(NuSmvProtocol, ReportsNonTotalRelations) {
    auto output = protocol(true);
    const std::string total = "The transition relation is total: No deadlock state exists\n";
    output.replace(output.find(total), total.size(),
        "The transition relation is not total. A state without\nsuccessors is:\n");
    const auto finding = evrostos::backends::detail::inspect_nusmv(output);
    ASSERT_TRUE(finding.has_value());
    EXPECT_FALSE(finding->transition_relation_total);
}

} // namespace
