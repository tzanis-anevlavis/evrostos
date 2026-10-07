// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include "rltl.hpp"

namespace evrostos::detail {

// Both syntactic nesting and AST height are bounded, including left-associated
// chains which do not cause parser recursion but would deepen later traversals.
inline constexpr std::size_t max_formula_depth = 256;

[[nodiscard]] Rltl parse(std::string_view source, const TranslationLimits& limits);

} // namespace evrostos::detail
