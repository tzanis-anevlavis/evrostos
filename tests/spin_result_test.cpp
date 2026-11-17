// SPDX-License-Identifier: GPL-3.0-or-later
#include "backends/detail/spin_result.hpp"

#include <gtest/gtest.h>

#include <string>

namespace {

// Reduced output fixtures preserve the lines consumed by the pinned protocol.
std::string summary(const std::string& errors = "0") {
    return "Full statespace search for:\n"
        "\tnever claim         \t+ (evrostos_property)\n"
        "\tacceptance   cycles \t+ (fairness disabled)\n"
        "State-vector 28 byte, depth reached 4, errors: " + errors + "\n"
        "pan: elapsed time 0 seconds\n";
}

// Only a complete summary for the expected claim and search mode can establish
// holds. Missing fields, duplicate summaries, and unexpected counts are rejected.
TEST(SpinOutput, RequiresCompleteExhaustiveSelectedPropertySummary) {
    EXPECT_EQ(evrostos::backends::detail::inspect_search(summary(), ""),
              evrostos::backends::detail::SearchFinding::holds);
    for (const auto* text : {"", "errors: 0", "pan: elapsed time 0 seconds\n"}) {
        EXPECT_EQ(evrostos::backends::detail::inspect_search(text, ""),
                  evrostos::backends::detail::SearchFinding::error);
    }
    for (const auto* required : {"Full statespace search for:", "evrostos_property",
                                "fairness disabled", "pan: elapsed time", "errors: 0"}) {
        auto text = summary();
        text.replace(text.find(required), std::string(required).size(), "unexpected");
        EXPECT_EQ(evrostos::backends::detail::inspect_search(text, ""),
                  evrostos::backends::detail::SearchFinding::error) << required;
    }
    EXPECT_EQ(evrostos::backends::detail::inspect_search(summary() + summary(), ""),
              evrostos::backends::detail::SearchFinding::error);
    EXPECT_EQ(evrostos::backends::detail::inspect_search(summary("2"), ""),
              evrostos::backends::detail::SearchFinding::error);
}

// Resource exhaustion invalidates a zero-error proof, including runs without a
// summary. A reported error after a depth warning remains eligible for replay.
TEST(SpinOutput, DoesNotTurnIncompleteSearchesIntoTruth) {
    for (const auto* prefix : {"error: max search depth too small\n",
                              "pan: reached -DMEMLIM bound\n",
                              "out of memory\n", "Warning: Search not completed\n"}) {
        EXPECT_EQ(evrostos::backends::detail::inspect_search(prefix + summary(), ""),
                  evrostos::backends::detail::SearchFinding::incomplete);
    }
    EXPECT_EQ(evrostos::backends::detail::inspect_search("pan: reached -DMEMLIM bound\n", ""),
              evrostos::backends::detail::SearchFinding::incomplete);
    EXPECT_EQ(evrostos::backends::detail::inspect_search(
        "error: max search depth too small\n" + summary("1"), ""),
        evrostos::backends::detail::SearchFinding::counterexample);
}

// Unexpected stderr or verifier diagnostics prevent a successful interpretation;
// SPIN's known single-claim selection warning is accepted.
TEST(SpinOutput, UnknownDiagnosticsAreErrors) {
    EXPECT_EQ(evrostos::backends::detail::inspect_search(summary(), "unexpected\n"),
              evrostos::backends::detail::SearchFinding::error);
    EXPECT_EQ(evrostos::backends::detail::inspect_search("pan: unknown error\n" + summary(), ""),
              evrostos::backends::detail::SearchFinding::error);
    EXPECT_EQ(evrostos::backends::detail::inspect_search(summary(),
        "warning: only one claim defined, -N ignored\n"),
        evrostos::backends::detail::SearchFinding::holds);
}

// A claim assertion requires process zero at the final trail step and a replay
// reaching that step. Model-process assertions and malformed evidence fail validation.
TEST(SpinOutput, AssertionRequiresAReplayedClaimTransition) {
    const std::string search = "pan:1: assertion violated p (at depth 0)\n" + summary("1");
    const std::string trail = "-2:1:-2\n-4:-4:-4\n1:0:9\n";
    const std::string replay = "pan:1: assertion violated p (at depth 1)\n"
        "spin: trail ends after 1 steps\n1: proc 0 (evrostos_property)\n";
    EXPECT_TRUE(evrostos::backends::detail::property_counterexample(search, trail, replay));
    for (const auto* invalid : {"", "garbage\n", "-2:1:-2\n1:1:9\n", "1:0:9\n",
                                "-2:1:-2\n1:0:9extra\n", "-2:1:-2\n1:0:9\n1:0:9\n"}) {
        EXPECT_FALSE(evrostos::backends::detail::property_counterexample(search, invalid, replay));
    }
    for (const auto* invalid : {"", "pan: Error\n", "pan: error\n", "Recovered at state 3\n"}) {
        const auto text = std::string(invalid).empty() ? "" : invalid + replay;
        EXPECT_FALSE(evrostos::backends::detail::property_counterexample(search, trail, text));
    }
    EXPECT_FALSE(evrostos::backends::detail::property_counterexample(search,
        "-2:1:-2\n1:0:9\n2:0:1\n", replay));
}

// A reported acceptance cycle needs matching trail and replay cycle markers.
// Other verifier errors do not become property counterexamples through replay alone.
TEST(SpinOutput, AcceptanceCyclesRequireTrailAndReplayMarkers) {
    const std::string search = "pan:1: acceptance cycle (at depth 0)\n" + summary("1");
    const std::string trail = "-2:1:-2\n-4:-4:-4\n-1:-1:-1\n1:0:9\n2:1:1\n";
    const std::string replay = "<<<<<START OF CYCLE>>>>>\n"
        "spin: trail ends after 2 steps\n2: proc 0 (evrostos_property)\n";
    EXPECT_TRUE(evrostos::backends::detail::property_counterexample(search, trail, replay));
    EXPECT_FALSE(evrostos::backends::detail::property_counterexample(search,
        "-2:1:-2\n1:0:9\n2:1:1\n", replay));
    EXPECT_FALSE(evrostos::backends::detail::property_counterexample(search, trail,
        "spin: trail ends after 2 steps\n2: proc 0 (evrostos_property)\n"));
    EXPECT_FALSE(evrostos::backends::detail::property_counterexample(
        "pan:1: invalid end state (at depth 0)\n", trail, replay));
}

} // namespace
