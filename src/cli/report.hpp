// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include "evrostos.hpp"
#include <chrono>
#include <filesystem>
#include <vector>

namespace evrostos::cli {

struct ReportInput {
    std::string backend;
    std::filesystem::path model;
    std::string formula;
    std::optional<std::size_t> index = std::nullopt; // One-based position in a batch.
};

struct CommandDiagnostic {
    std::string stage;
    int exit_code;
    int signal;
    bool timed_out;
    bool output_truncated;
    std::string standard_output;
    std::string standard_error;
    std::string diagnostic;
};

// Only attempted checks have an entry. A violated bit carries its validated replay.
struct BitCheck {
    std::string bit;
    backends::CheckOutcome outcome;
    std::string stage;
    std::string message;
    std::optional<CommandDiagnostic> command;
    std::optional<std::string> trace;
};

struct Report {
    std::string json;
    std::string console;
    int exit_code;
    std::optional<std::string> counterexamples;
};

Report verification_report(const ReportInput& input, const Verification& summary,
                           const std::vector<BitCheck>& checks);
Report error_report(const ReportInput& input, const std::string& message);
Report error_report(const ReportInput& input, const Diagnostic& diagnostic);
Report verify(const ReportInput& input, const std::filesystem::path& workspace, std::chrono::seconds timeout);
Report verify_list(const std::string& backend, const std::filesystem::path& model,
                   const std::filesystem::path& list, const std::filesystem::path& workspace,
                   std::chrono::seconds timeout);

} // namespace evrostos::cli
