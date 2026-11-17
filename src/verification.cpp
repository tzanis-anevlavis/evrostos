// SPDX-License-Identifier: GPL-3.0-or-later
#include "evrostos.hpp"

#include <algorithm>
#include <stdexcept>

namespace evrostos {

std::string_view to_string(RobustValue value) {
    switch (value) {
        case RobustValue::b0000: return "0000";
        case RobustValue::b0001: return "0001";
        case RobustValue::b0011: return "0011";
        case RobustValue::b0111: return "0111";
        case RobustValue::b1111: return "1111";
    }
    throw std::invalid_argument("invalid robust value");
}

std::optional<RobustValue> Verification::exact_value() const noexcept {
    if (lower_bound == upper_bound) {
        return lower_bound;
    }
    return std::nullopt;
}

VerificationResult Evrostos::verify(std::string_view formula, const LtlChecker& checker,
                                   const TranslationLimits& limits) const {
    // Translate once, before invoking the checker. Recursive implication and
    // every other operator are resolved at trace level by the translator.
    const auto translated = translate(formula, limits);
    if (const auto* error = std::get_if<Diagnostic>(&translated)) {
        return *error;
    }
    if (!checker) {
        throw std::invalid_argument("verification requires an LTL checker");
    }
    const auto& bits = std::get<Translation>(translated).bits;
    Verification result;
    constexpr std::array true_bounds{
        RobustValue::b1111, RobustValue::b0111, RobustValue::b0011, RobustValue::b0001};
    constexpr std::array false_bounds{
        RobustValue::b0111, RobustValue::b0011, RobustValue::b0001, RobustValue::b0000};

    // b1 <= b2 <= b3 <= b4 on every trace and after universal aggregation.
    // A true bit proves all weaker bits; a false bit refutes all stronger bits.
    for (std::size_t remaining = bits.size(); remaining > 0; --remaining) {
        const auto bit = remaining - 1;
        const auto outcome = checker(bit, bits[bit]);
        result.outcomes[bit] = outcome;
        switch (outcome) {
            case backends::CheckOutcome::holds:
                result.lower_bound = std::max(result.lower_bound, true_bounds[bit]);
                break;
            case backends::CheckOutcome::violated:
                result.upper_bound = std::min(result.upper_bound, false_bounds[bit]);
                // No remaining stronger query can refine these bounds. Weaker
                // unresolved bits stay unresolved; they are not retried here.
                return result;
            case backends::CheckOutcome::inconclusive:
            case backends::CheckOutcome::execution_error:
                // A stronger proof may resolve this bit, so continue checking.
                break;
            default:
                throw std::invalid_argument("invalid LTL checker outcome");
        }
    }
    return result;
}

} // namespace evrostos
