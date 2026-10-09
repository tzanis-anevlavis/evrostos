// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include "ltl.hpp"

#include <cstddef>
#include <string>
#include <variant>

namespace evrostos::backends {

enum class SerializationCode {
    null_formula,
    invalid_identifier,
    unsupported_operator,
    output_limit
};

struct SerializationDiagnostic {
    SerializationCode code;
    std::string message;
};

struct SerializationLimits {
    // Includes parentheses and spaces in one formula; excludes a terminating NUL.
    std::size_t max_output_bytes = 1024 * 1024;
};

struct SpinSerializationOptions {
    // X requires a SPIN build with NXT and verification without partial-order
    // reduction. The caller is responsible for that execution configuration.
    bool allow_next = false;
};

using SerializationResult = std::variant<std::string, SerializationDiagnostic>;

// Serialize one root, preserving its operators, atoms, and polarity. Each call
// returns complete text or a diagnostic. Allocation failures propagate.
// Atoms must be nonreserved identifiers in the core's [A-Za-z][A-Za-z0-9]*
// alphabet. Their declarations and Boolean interpretation belong to the model.
[[nodiscard]] SerializationResult serialize_nusmv(
    const SharedPtrLtlNode& formula, const SerializationLimits& limits = {});

// Produces the formula body for an inline `ltl name { ... }` property.
[[nodiscard]] SerializationResult serialize_spin(
    const SharedPtrLtlNode& formula, const SpinSerializationOptions& options = {},
    const SerializationLimits& limits = {});

} // namespace evrostos::backends
