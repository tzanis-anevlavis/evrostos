// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include "serialization.hpp"
#include "evrostos.hpp"

#include <array>
#include <chrono>
#include <filesystem>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace evrostos::backends {

enum class NuSmvStage { validation, version, checking };

struct NuSmvOptions {
    // Absolute paths to the pinned checker and an existing workspace parent.
    std::filesystem::path executable;
    std::filesystem::path workspace_parent;
    // One deadline for version detection, model construction, checking, and replay.
    std::chrono::milliseconds timeout{60000};
    std::size_t max_output_bytes = 4 * 1024 * 1024;
    std::size_t max_model_bytes = 4 * 1024 * 1024;
    SerializationLimits serialization_limits;
};

// Each process attempt retains its arguments and bounded diagnostic streams.
struct NuSmvCommand {
    NuSmvStage stage;
    std::vector<std::string> arguments;
    int exit_code = -1;
    int signal = 0;
    bool timed_out = false;
    bool output_truncated = false;
    std::string standard_output;
    std::string standard_error;
    std::string diagnostic;
};

struct NuSmvResult {
    CheckOutcome outcome = CheckOutcome::execution_error;
    NuSmvStage stage = NuSmvStage::validation;
    std::string message;
    // A separate LTL FALSE query determines whether any fair infinite execution
    // starts in an initial state. Empty until a completed protocol is validated.
    std::optional<bool> has_admissible_executions;
    // Totality covers all invariant states, including unreachable states.
    std::optional<bool> transition_relation_total;
    // Calls retain private workspaces; their lifetime belongs to the caller.
    std::filesystem::path workspace;
    // Text of the selected property's counterexample, after native BDD replay.
    std::filesystem::path counterexample;
    std::vector<NuSmvCommand> commands;
};

struct NuSmvVerification {
    Verification summary;
    // b1..b4 order; unattempted queries have no evidence entry.
    std::array<std::optional<NuSmvResult>, 4> checks{};
};

using NuSmvVerificationResult = std::variant<NuSmvVerification, Diagnostic>;

// Universal LTL checking over the model's fair infinite executions. Deadlocks
// are not extended by stuttering. Models and executables must be trusted.
class NuSmvBackend {
public:
    explicit NuSmvBackend(NuSmvOptions options);
    [[nodiscard]] NuSmvResult check(const std::filesystem::path& model,
                                    const SharedPtrLtlNode& property) const;
    // Translation and bit scheduling use the core. Limits apply per property;
    // the model must remain unchanged throughout this call. Calls are independent
    // and may run concurrently. Allocation failures propagate.
    [[nodiscard]] NuSmvVerificationResult verify(
        const std::filesystem::path& model, std::string_view formula,
        const TranslationLimits& limits = {}) const;
private:
    NuSmvOptions options_;
};

} // namespace evrostos::backends
