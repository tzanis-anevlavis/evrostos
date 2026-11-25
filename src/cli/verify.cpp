// SPDX-License-Identifier: GPL-3.0-or-later
#include "report.hpp"
#ifdef EVROSTOS_NUSMV_EXECUTABLE
#include "backends/nusmv.hpp"
#endif
#ifdef EVROSTOS_SPIN_EXECUTABLE
#include "backends/spin.hpp"
#endif
#include <fstream>
#include <stdexcept>

namespace evrostos::cli {
namespace {

#ifdef EVROSTOS_NUSMV_EXECUTABLE
const char* stage_name(backends::NuSmvStage stage) {
    switch (stage) {
    case backends::NuSmvStage::validation: return "validation";
    case backends::NuSmvStage::version: return "version";
    case backends::NuSmvStage::checking: return "checking";
    }
    throw std::logic_error("unknown NuSMV stage");
}

// Read the BDD-validated trace before the CLI releases the run workspace.
std::string validated_trace(const backends::NuSmvResult& check) {
    std::string trace(std::filesystem::file_size(check.counterexample), '\0');
    std::ifstream input(check.counterexample, std::ios::binary);
    input.exceptions(std::ios::badbit | std::ios::failbit);
    input.read(trace.data(), static_cast<std::streamsize>(trace.size()));
    return trace;
}
#endif

#ifdef EVROSTOS_SPIN_EXECUTABLE
const char* stage_name(backends::SpinStage stage) {
    switch (stage) {
    case backends::SpinStage::validation: return "validation";
    case backends::SpinStage::version: return "version";
    case backends::SpinStage::preprocessing: return "preprocessing";
    case backends::SpinStage::generation: return "generation";
    case backends::SpinStage::compilation: return "compilation";
    case backends::SpinStage::search: return "search";
    case backends::SpinStage::replay: return "replay";
    }
    throw std::logic_error("unknown SPIN stage");
}

std::string validated_trace(const backends::SpinResult& check) {
    // The successful replay contains the readable steps of the validated trail.
    return check.commands.back().standard_output;
}
#endif

template<typename BackendVerification>
Report make_report(const ReportInput& input, const std::variant<BackendVerification, Diagnostic>& result) {
    if (const auto* error = std::get_if<Diagnostic>(&result)) {
        return error_report(input, *error);
    }
    const auto& verification = std::get<BackendVerification>(result);
    std::vector<BitCheck> checks;
    for (auto remaining = verification.checks.size(); remaining > 0; --remaining) {
        const auto& attempted = verification.checks[remaining - 1];
        if (!attempted.has_value()) {
            continue;
        }
        const auto& evidence = *attempted;
        BitCheck check{.bit = "b" + std::to_string(remaining), .outcome = evidence.outcome,
                       .stage = stage_name(evidence.stage), .message = evidence.message,
                       .command = std::nullopt, .trace = std::nullopt};
        if (evidence.outcome == backends::CheckOutcome::violated) {
            check.trace = validated_trace(evidence);
        } else if (!evidence.commands.empty() && (evidence.outcome != backends::CheckOutcome::holds)) {
            // Keep the failing tool's explanation after its working files are removed.
            const auto& command = evidence.commands.back();
            check.command = CommandDiagnostic{
                .stage = stage_name(command.stage), .exit_code = command.exit_code, .signal = command.signal,
                .timed_out = command.timed_out, .output_truncated = command.output_truncated,
                .standard_output = command.standard_output, .standard_error = command.standard_error,
                .diagnostic = command.diagnostic};
        }
        checks.push_back(std::move(check));
    }
    return verification_report(input, verification.summary, checks);
}

} // namespace

Report verify(const ReportInput& input, const std::filesystem::path& workspace, std::chrono::seconds timeout) {
    if (!std::filesystem::is_regular_file(input.model)) {
        return error_report(input, "model is not a regular file: " + input.model.string());
    }
#ifdef EVROSTOS_NUSMV_EXECUTABLE
    if (input.backend == "nusmv") {
        const backends::NuSmvBackend checker({.executable = EVROSTOS_NUSMV_EXECUTABLE,
            .workspace_parent = workspace, .timeout = timeout});
        return make_report(input, checker.verify(input.model, input.formula));
    }
#endif
#ifdef EVROSTOS_SPIN_EXECUTABLE
    if (input.backend == "spin") {
        const backends::SpinBackend checker({.executable = EVROSTOS_SPIN_EXECUTABLE,
            .c_compiler = EVROSTOS_C_COMPILER, .workspace_parent = workspace, .timeout = timeout});
        return make_report(input, checker.verify(input.model, input.formula));
    }
#endif
    return error_report(input, "backend unavailable in this build: " + input.backend);
}

} // namespace evrostos::cli
