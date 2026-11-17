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

// Elements of the robust truth-value algebra. Their order is defined by
// less_equal; the enum representation carries no semantic meaning.
enum class RobustValue {
    b0000, b0001, b0011, b0111, b1111
};

[[nodiscard]] std::string_view to_string(RobustValue value);

// The chain is 0000 < 0001 < 0011 < 0111 < 1111. Meet is its greatest lower
// bound; join is its least upper bound. Invalid enum values are rejected.
[[nodiscard]] bool less_equal(RobustValue left, RobustValue right);
[[nodiscard]] RobustValue meet(RobustValue left, RobustValue right);
[[nodiscard]] RobustValue join(RobustValue left, RobustValue right);

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
