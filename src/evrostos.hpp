// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include "ltl.hpp"

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
    std::array<Ltl, 4> bits; // Strongest (b1) through weakest (b4), never query order.
};

using TranslationResult = std::variant<Translation, Diagnostic>;

// The core entry point. No I/O, model-checker dependency, or per-call state is
// retained. Results own their nodes and outlive both the input and this object.
class Evrostos final {
public:
    [[nodiscard]] TranslationResult translate(
        std::string_view formula, const TranslationLimits& limits = {}) const;
};

} // namespace evrostos
