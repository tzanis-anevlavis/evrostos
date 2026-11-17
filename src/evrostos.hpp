// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include "ltl.hpp"
#include "verification.hpp"

#include <array>
#include <cstddef>
#include <string>
#include <string_view>
#include <variant>

namespace evrostos {

enum class DiagnosticCode { syntax_error, input_limit, depth_limit, node_limit };

struct Diagnostic {
    DiagnosticCode code;
    std::string message;
    std::size_t offset; // Zero-based byte offset; EOF may equal input length.
    std::size_t line;   // One-based. CRLF is one newline; CR and LF also work.
    std::size_t column; // One-based byte column; a tab counts as one byte.
};

struct TranslationLimits {
    std::size_t max_input_bytes = 1024 * 1024;
    std::size_t max_parse_nodes = 100'000;
    std::size_t max_ltl_nodes = 1'000'000;
};

struct Translation {
    std::array<SharedPtrLtlNode, 4> bits; // Strongest (b1) through weakest (b4), never query order.
};

using TranslationResult = std::variant<Translation, Diagnostic>;
using VerificationResult = std::variant<Verification, Diagnostic>;

// The core entry point. Translation has no I/O or model-checker dependency;
// verification delegates checking to a caller-supplied callback. No per-call
// state is retained. Translation results own their nodes independently.
class Evrostos final {
public:
    [[nodiscard]] TranslationResult translate(
        std::string_view formula, const TranslationLimits& limits = {}) const;

    // Check b4 through b1, stopping after a proven false bit. Errors and
    // incomplete checks remain recorded even when other evidence fixes the value.
    // An empty checker is rejected before translation. Translation failures
    // invoke no checker. Callback exceptions propagate.
    [[nodiscard]] VerificationResult verify(
        std::string_view formula, const LtlChecker& checker,
        const TranslationLimits& limits = {}) const;
};

} // namespace evrostos
