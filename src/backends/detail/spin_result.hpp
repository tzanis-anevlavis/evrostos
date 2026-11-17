// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include <string_view>

namespace evrostos::backends::detail {

// counterexample is a candidate reported by the search. Trail validation and
// replay still need to distinguish an LTL failure from a model error.
enum class SearchFinding { holds, counterexample, incomplete, error };

// Parse the output contract of the pinned exhaustive, single-core verifier.
SearchFinding inspect_search(std::string_view output, std::string_view error);
// Validate property-error provenance across the search, trail, and replay.
// The caller has already checked command completion and the model profile.
bool property_counterexample(std::string_view search, std::string_view trail,
                             std::string_view replay);

} // namespace evrostos::backends::detail
