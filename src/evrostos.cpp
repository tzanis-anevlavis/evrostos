// SPDX-License-Identifier: GPL-3.0-or-later
#include "evrostos.hpp"
#include "detail/parser.hpp"
#include "detail/translator.hpp"

namespace evrostos {

TranslationResult Evrostos::translate(std::string_view formula, const TranslationLimits& limits) const {
    try {
        const auto parsed = detail::parse(formula, limits);
        return detail::translate(parsed, formula, limits);
    } catch (const detail::Failure& failure) {
        return failure.diagnostic;
    }
}

} // namespace evrostos
