// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include "backends/check.hpp"
#include "ltl.hpp"

#include <array>
#include <cstddef>
#include <functional>
#include <optional>
#include <string_view>

namespace evrostos {

// Ordered from false to true. The numeric values encode b1 through b4.
enum class RobustValue : unsigned char {
    b0000 = 0, b0001 = 1, b0011 = 3, b0111 = 7, b1111 = 15
};

[[nodiscard]] std::string_view to_string(RobustValue value);

struct Verification {
    // Inclusive bounds over the five legal robust values.
    RobustValue lower_bound = RobustValue::b0000;
    RobustValue upper_bound = RobustValue::b1111;
    // Actual checker outcomes in b1..b4 order. Empty entries were not checked;
    // inferred bits are represented by the bounds, not fabricated outcomes.
    std::array<std::optional<backends::CheckOutcome>, 4> outcomes{};

    [[nodiscard]] std::optional<RobustValue> exact_value() const noexcept;
};

// The index is zero-based in b1..b4 order. The callback checks a positive LTL
// property over one fixed model and execution convention for the entire call.
// Backend-specific evidence can be retained by the callback under this index.
using LtlChecker = std::function<backends::CheckOutcome(std::size_t, const SharedPtrLtlNode&)>;

} // namespace evrostos
