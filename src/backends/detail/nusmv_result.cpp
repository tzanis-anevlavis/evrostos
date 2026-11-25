// SPDX-License-Identifier: GPL-3.0-or-later
#include "nusmv_result.hpp"

#include <array>

namespace evrostos::backends::detail {
namespace {

constexpr std::string_view counterexample_header = "Trace Type: Counterexample \n";
constexpr std::string_view replay_completed = "Trace 1 execution completed successfully.\n";

bool contains(std::string_view text, std::string_view part) {
    return text.find(part) != std::string_view::npos;
}

// NuSMV prints a lasso starting at state 1.1. Its BDD executor then checks
// the transitions and loopback of that trace against the model.
bool has_replayed_counterexample(std::string_view property, std::string_view replay) {
    return contains(property, counterexample_header)
        && contains(property, "-> State: 1.1 <-")
        && contains(property, "-- Loop starts here\n")
        && contains(replay, replay_completed)
        && !contains(replay, "Trace execution failed");
}

std::optional<bool> transition_relation_total(std::string_view model) {
    const bool total = contains(model, "The transition relation is total: No deadlock state exists\n");
    const bool partial = contains(model, "The transition relation is not total. A state without\n");
    if (total == partial) {
        return std::nullopt;
    }
    return total;
}

// Require exactly one complete verdict line in a query section. NuSMV prints
// its normalized formula, so comparison with the serializer's spelling would
// incorrectly reject equivalent pretty-printing.
std::optional<bool> verdict(std::string_view section) {
    std::optional<bool> result;
    while (!section.empty()) {
        const auto end = section.find('\n');
        const auto line = section.substr(0, end);
        if (line.starts_with("-- specification ")) {
            if (result.has_value()) {
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

struct TranscriptSections {
    std::string_view model;
    std::string_view executions;
    std::string_view property;
    std::string_view replay;
};

std::optional<TranscriptSections> split_transcript(std::string_view output) {
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
    return TranscriptSections{.model = section(0), .executions = section(1),
                              .property = section(2), .replay = section(3)};
}

} // namespace

bool supported_nusmv_version(std::string_view output) {
    return output.starts_with("*** This is NuSMV 2.7.1 ")
        || output.starts_with("*** This is NuSMV 2.7.1\n");
}

std::optional<NuSmvFinding> inspect_nusmv(std::string_view output) {
    const auto sections = split_transcript(output);
    if (!sections.has_value()) {
        return std::nullopt;
    }
    const auto total = transition_relation_total(sections->model);
    // The probe checks FALSE over all admissible executions. A true verdict
    // means the execution set is empty.
    const auto execution_set_empty = verdict(sections->executions);
    const auto holds = verdict(sections->property);
    if (!total.has_value() || !execution_set_empty.has_value() || !holds.has_value()) {
        return std::nullopt;
    }
    if (*holds && (contains(sections->property, counterexample_header)
                   || contains(sections->replay, replay_completed))) {
        return std::nullopt;
    }
    if (!*holds && (*execution_set_empty
                    || !has_replayed_counterexample(sections->property, sections->replay))) {
        return std::nullopt;
    }
    return NuSmvFinding{.holds = *holds, .has_admissible_executions = !*execution_set_empty,
                        .transition_relation_total = *total,
                        .counterexample = *holds ? std::string_view{} : sections->property};
}

} // namespace evrostos::backends::detail
