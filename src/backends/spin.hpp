// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include "serialization.hpp"

#include <chrono>
#include <filesystem>
#include <string>
#include <vector>

namespace evrostos::backends {

// holds requires a complete search; violated requires a replayed counterexample.
// Incomplete evidence and execution failures remain separate from truth values.
enum class CheckOutcome { holds, violated, inconclusive, execution_error };
enum class SpinStage { validation, version, preprocessing, generation, compilation, search, replay };

struct SpinOptions {
    // Absolute paths. The workspace parent must already exist. Use the pinned
    // evrostos_spin_tool executable and a GCC-compatible C compiler.
    std::filesystem::path executable;
    std::filesystem::path c_compiler;
    std::filesystem::path workspace_parent;
    // One deadline shared by all subprocess stages, including trail replay.
    std::chrono::milliseconds timeout{60000};
    // Verifier transitions, including claim steps; not just Promela statements.
    unsigned int max_depth = 100000;
    // Bounds SPIN's verifier allocator, not compiler or total process memory.
    unsigned int memory_megabytes = 256;
    // Combined stdout/stderr per command; also bounds the trail read for replay.
    std::size_t max_output_bytes = 4 * 1024 * 1024;
    // Original model bytes, before appending the property or expanding includes.
    std::size_t max_model_bytes = 4 * 1024 * 1024;
    SerializationLimits serialization_limits;
    // X is supported by the pinned NXT build and reduction settings used by check().
    // Set false to reject properties containing X during validation.
    bool allow_next = true;
};

// Evidence from one command attempt, including partial output after cancellation.
struct SpinCommand {
    SpinStage stage;
    std::vector<std::string> arguments;
    int exit_code = -1; // -1 when no normal exit status was obtained.
    int signal = 0;
    bool timed_out = false;
    bool output_truncated = false;
    std::string standard_output;
    std::string standard_error;
    std::string diagnostic;
};

struct SpinResult {
    CheckOutcome outcome = CheckOutcome::execution_error;
    // Last attempted stage, including the stage that produced an error.
    SpinStage stage = SpinStage::validation;
    std::string message;
    // Each call retains a unique workspace, including failed runs. The caller
    // owns its lifetime. Empty if validation failed before workspace creation.
    std::filesystem::path workspace;
    // Expected trail path after a reported verifier error. Artifact validation
    // can fail if the file is missing or unreadable.
    std::filesystem::path trail;
    // Command attempts in pipeline order, including failed or stopped commands.
    std::vector<SpinCommand> commands;
};

// Check one positive LTL property over a trusted Promela model. Calls are
// independent and may run concurrently. No shell interpolation of caller paths.
// Subprocess, search, and trail limits return inconclusive; invalid inputs and
// tool failures return execution_error. Allocation failures propagate.
class SpinBackend {
public:
    explicit SpinBackend(SpinOptions options);
    [[nodiscard]] SpinResult check(const std::filesystem::path& model,
                                   const SharedPtrLtlNode& property) const;
private:
    SpinOptions options_;
};

} // namespace evrostos::backends
