// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include "rltl.hpp"

namespace evrostos::detail {

[[nodiscard]] Translation translate(const SharedPtrRltlNode& formula, std::string_view source,
                                    const TranslationLimits& limits);

} // namespace evrostos::detail
