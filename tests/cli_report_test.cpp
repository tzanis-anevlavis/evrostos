// SPDX-License-Identifier: GPL-3.0-or-later
#include "cli/report.hpp"
#include <gtest/gtest.h>
#include <nlohmann/json.hpp>

namespace {

// The b3 witness survives the unresolved b4 query. Unattempted bits have no record.
TEST(CliReport, TimeoutAndViolationPreserveBothKindsOfEvidence) {
    evrostos::Verification summary;
    summary.upper_bound = evrostos::RobustValue::b0001;
    const std::vector<evrostos::cli::BitCheck> checks{
        {"b4", evrostos::backends::CheckOutcome::inconclusive, "search", "deadline exceeded", {}, std::nullopt},
        {"b3", evrostos::backends::CheckOutcome::violated, "replay", "validated", {}, "step 1\nstep 2"}};
    const auto report = evrostos::cli::verification_report({"spin", "model.pml", "rG p"}, summary, checks);
    const auto fields = nlohmann::json::parse(report.json);
    EXPECT_EQ(report.exit_code, 1);
    EXPECT_EQ(fields.at("possible_values"), (nlohmann::json::array({"0000", "0001"})));
    EXPECT_FALSE(fields.contains("value"));
    EXPECT_TRUE(report.console.starts_with("Inconclusive: {0000, 0001}\n"));
    EXPECT_NE(report.console.find("b4 (search): deadline exceeded"), std::string::npos);
    EXPECT_EQ(report.console.find("step 1"), std::string::npos);
    const auto& witness = fields.at("counterexample");
    EXPECT_EQ(witness.at("bit"), "b3");
    EXPECT_EQ(witness.at("trace"), "step 1\nstep 2");
    ASSERT_TRUE(report.counterexamples.has_value());
    EXPECT_EQ(*report.counterexamples, "Formula: rG p\nViolated bit: b3\n\nstep 1\nstep 2");
    EXPECT_EQ(fields.at("checks").size(), 2U);
}

// A proof for b1 resolves the value after an earlier failed check.
TEST(CliReport, ExactResultRetainsEarlierDiagnostic) {
    evrostos::Verification summary;
    summary.lower_bound = evrostos::RobustValue::b1111;
    const std::vector<evrostos::cli::BitCheck> checks{
        {"b4", evrostos::backends::CheckOutcome::execution_error, "checking", "command failed", {}, std::nullopt},
        {"b3", evrostos::backends::CheckOutcome::holds, "checking", "holds", {}, std::nullopt},
        {"b2", evrostos::backends::CheckOutcome::holds, "checking", "holds", {}, std::nullopt},
        {"b1", evrostos::backends::CheckOutcome::holds, "checking", "holds", {}, std::nullopt}};
    const auto report = evrostos::cli::verification_report({"nusmv", "model.smv", "rG p"}, summary, checks);
    const auto fields = nlohmann::json::parse(report.json);
    EXPECT_EQ(report.exit_code, 0);
    EXPECT_EQ(fields.at("value"), "1111");
    EXPECT_FALSE(fields.contains("counterexample"));
    EXPECT_FALSE(report.counterexamples.has_value());
    EXPECT_EQ(fields.at("checks").at(0).at("outcome"), "execution_error");
}

// Check native bytes after serialization, including JSON escapes and a truncated UTF-8 sequence.
TEST(CliReport, EncodesNativeTextInDiagnosticsAndTraces) {
    const evrostos::cli::ReportInput input{"spin", "model-\xff.pml", "rG p"};
    const std::string bytes = std::string("α\n\"\\") + '\0' + "\xe2\x82";
    const std::string expected = std::string("α\n\"\\") + '\0' + "�";
    const evrostos::cli::CommandDiagnostic command{
        "search", 1, 0, false, false, bytes, bytes, bytes};
    const std::vector<evrostos::cli::BitCheck> checks{
        {"b4", evrostos::backends::CheckOutcome::execution_error, "search", bytes, command, std::nullopt},
        {"b3", evrostos::backends::CheckOutcome::violated, "replay", "validated", {}, bytes}};
    evrostos::Verification summary;
    summary.upper_bound = evrostos::RobustValue::b0001;
    const auto fields = nlohmann::json::parse(evrostos::cli::verification_report(input, summary, checks).json);
    EXPECT_EQ(fields.at("model"), "model-�.pml");
    EXPECT_EQ(fields.at("counterexample").at("trace"), expected);
    const auto& failed = fields.at("checks").at(0);
    EXPECT_EQ(failed.at("message"), expected);
    for (const auto* key : {"stdout", "stderr", "diagnostic"}) {
        EXPECT_EQ(failed.at("command").at(key), expected);
    }
}
} // namespace
