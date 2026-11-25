// SPDX-License-Identifier: GPL-3.0-or-later
#include "nusmv_result.hpp"

#include <array>

namespace evrostos::backends::detail {
namespace {

// Require exactly one complete verdict line in a query section. NuSMV prints
// its normalized formula, so comparison with the serializer's spelling would
// incorrectly reject equivalent pretty-printing.
std::optional<bool> verdict(std::string_view section) {
    std::optional<bool> result;
    while (!section.empty()) {
        const auto end = section.find('\n');
        const auto line = section.substr(0, end);
        if (line.starts_with("-- specification ")) {
            if (result) {
                return std::nullopt;
            }
            if (line.ends_with(" is true")) {
                result = true;
            } else if (line.ends_with(" is false")) {
                result = false;
            } else {
                return std::nullopt;
            }
        }
        if (end == std::string_view::npos) {
            break;
        }
        section.remove_prefix(end + 1);
    }
    return result;
}

} // namespace

bool supported_nusmv_version(std::string_view output) {
    return output.starts_with("*** This is NuSMV 2.7.1 ")
        || output.starts_with("*** This is NuSMV 2.7.1\n");
}

std::optional<NuSmvFinding> inspect_nusmv(std::string_view output) {
    constexpr std::array markers{
        "EVROSTOS_MODEL \n", "EVROSTOS_EXECUTIONS \n", "EVROSTOS_PROPERTY \n",
        "EVROSTOS_REPLAY \n", "EVROSTOS_DONE \n"};
    std::array<std::size_t, markers.size()> positions{};
    // Markers must occur once, on their own lines, and in pipeline order.
    for (std::size_t i = 0; i < markers.size(); ++i) {
        const std::string_view marker = markers[i];
        const auto position = output.find(marker);
        if ((position == std::string_view::npos)
            || ((position != 0) && (output[position - 1] != '\n'))
            || (output.find(marker, position + marker.size()) != std::string_view::npos)
            || ((i != 0) && (position <= positions[i - 1]))) {
            return std::nullopt;
        }
        positions[i] = position;
    }
    const auto section = [&](std::size_t i) {
        const auto start = positions[i] + std::string_view(markers[i]).size();
        return output.substr(start, positions[i + 1] - start);
    };
    const auto model = section(0);
    const bool total = model.find("The transition relation is total: No deadlock state exists\n")
        != std::string_view::npos;
    const bool partial = model.find("The transition relation is not total. A state without\n")
        != std::string_view::npos;
    const auto empty = verdict(section(1));
    const auto holds = verdict(section(2));
    if ((total == partial) || !empty || !holds || (*empty && !*holds)) {
        return std::nullopt;
    }
    const auto property = section(2);
    const auto replay = section(3);
    const bool trace = property.find("Trace Type: Counterexample \n") != std::string_view::npos;
    const bool replayed = replay.find("Trace 1 execution completed successfully.\n")
        != std::string_view::npos;
    if (*holds) {
        if (trace || replayed) {
            return std::nullopt;
        }
    } else {
        // A false result must carry the selected property's infinite trace and
        // a successful BDD replay, including NuSMV's loopback consistency check.
        if (!trace || !replayed
            || (property.find("-- Loop starts here\n") == std::string_view::npos)
            || (property.find("-> State: 1.1 <-") == std::string_view::npos)
            || (replay.find("Trace execution failed") != std::string_view::npos)) {
            return std::nullopt;
        }
    }
    return NuSmvFinding{*holds, !*empty, total, *holds ? std::string_view{} : property};
}

} // namespace evrostos::backends::detail
