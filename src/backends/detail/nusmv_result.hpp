// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include <optional>
#include <string_view>

namespace evrostos::backends::detail {

// Evidence extracted from one normally completed, successful script process.
struct NuSmvFinding {
    bool holds;
    bool has_admissible_executions;
    bool transition_relation_total;
    std::string_view counterexample;
};

// Section markers separate model diagnostics, the emptiness query, the selected
// property, and replay. Missing or inconsistent evidence returns no finding.
std::optional<NuSmvFinding> inspect_nusmv(std::string_view output);
bool supported_nusmv_version(std::string_view output);

} // namespace evrostos::backends::detail
