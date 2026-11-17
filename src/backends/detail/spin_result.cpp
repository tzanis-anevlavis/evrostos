// SPDX-License-Identifier: GPL-3.0-or-later
#include "spin_result.hpp"

#include <regex>
#include <sstream>
#include <string>

namespace evrostos::backends::detail {
namespace {

bool contains(std::string_view text, std::string_view part) {
    return text.find(part) != std::string_view::npos;
}

} // namespace

SearchFinding inspect_search(std::string_view output, std::string_view error) {
    // The single-claim warning is expected when selecting a property by name.
    if (!error.empty() && (error != "warning: only one claim defined, -N ignored\n")) {
        return SearchFinding::error;
    }
    if (contains(output, "max search depth too small")
        || contains(output, "reached -DMEMLIM bound")
        || contains(output, "out of memory")) {
        // A completed counterexample may still be available after a depth limit.
        if (!contains(output, "errors: 1\n")) {
            return SearchFinding::incomplete;
        }
    }
    // Match the search configuration together with its error count. A zero count
    // is meaningful only for the selected claim under the expected search mode.
    const std::string text(output);
    static const std::regex summary(R"((^|\n)State-vector [0-9]+ byte, depth reached [0-9]+, errors: ([0-9]+)\n)");
    static const std::regex claim(R"(never claim[ \t]+\+ \(evrostos_property\))");
    static const std::regex cycles(R"(acceptance[ \t]+cycles[ \t]+\+ \(fairness disabled\))");
    std::smatch match;
    if (!contains(output, "Full statespace search for:\n")
        || !std::regex_search(text, claim) || !std::regex_search(text, cycles)
        || !contains(output, "pan: elapsed time ")
        || !std::regex_search(text, match, summary)) {
        return SearchFinding::error;
    }
    const auto count = match[2].str();
    const auto suffix = match.suffix().str();
    // One run supplies one summary; concatenated summaries are ambiguous evidence.
    if (std::regex_search(suffix, summary)) {
        return SearchFinding::error;
    }
    // The verifier stops at its first error. Return a candidate for trail/replay
    // validation before treating the accompanying incomplete-search warning.
    if (count == "1") {
        return SearchFinding::counterexample;
    }
    if (count != "0") {
        return SearchFinding::error;
    }
    if (contains(output, "Warning: Search not completed")) {
        return SearchFinding::incomplete;
    }
    // Unknown verifier diagnostics cannot establish completion. A successful
    // run may report timing and the explicitly selected property name.
    std::istringstream lines(text);
    std::string line;
    while (std::getline(lines, line)) {
        if (line.starts_with("pan:") && !line.starts_with("pan: elapsed time ")
            && !line.starts_with("pan: rate ") && (line != "pan: ltl formula evrostos_property")) {
            return SearchFinding::error;
        }
        if (line.starts_with("error:") || line.starts_with("warning:")
            || line.starts_with("Warning:")) {
            return SearchFinding::error;
        }
    }
    return SearchFinding::holds;
}

bool property_counterexample(std::string_view search, std::string_view trail,
                             std::string_view replay) {
    // With an inline claim, process zero is the claim. The final transition's
    // process distinguishes a generated claim assertion from a model assertion.
    std::istringstream lines{std::string(trail)};
    std::string line;
    long long last_depth = 0;
    long long last_process = -1;
    bool cycle = false;
    bool claim = false;
    while (std::getline(lines, line)) {
        // Each trail record has depth:process:transition fields. Positive depths
        // identify ordered execution steps; negative depths carry metadata.
        std::istringstream fields(line);
        long long depth = 0, process = 0, transition = 0;
        char first = 0, second = 0;
        if (!(fields >> depth >> first >> process >> second >> transition)
            || (first != ':') || (second != ':')) {
            return false;
        }
        fields >> std::ws;
        if (!fields.eof()) {
            return false;
        }
        // Negative record types: -1 starts a cycle, -2 identifies the claim,
        // and -4 carries statement-merging metadata.
        if (depth > 0) {
            if ((depth <= last_depth) || (process < 0) || (transition < 0)) {
                return false;
            }
            last_depth = depth;
            last_process = process;
        } else if (depth == -1) {
            cycle = true;
        } else if (depth == -2) {
            claim = true;
        } else if (depth != -4) {
            return false;
        }
    }
    // Replay must reach the recorded final step with the selected claim and no
    // transition errors or state-recovery messages.
    if (!claim || (last_depth == 0)
        || !contains(replay, "spin: trail ends after " + std::to_string(last_depth) + " steps\n")
        || contains(replay, "pan: Error") || contains(replay, "pan: error")
        || contains(replay, "Recovered at state")
        || !contains(replay, "proc 0 (evrostos_property)")) {
        return false;
    }
    if (contains(search, "pan:1: assertion violated ")) {
        return (last_process == 0) && contains(replay, "pan:1: assertion violated ");
    }
    // User-defined acceptance labels and raw never claims are excluded by the
    // model profile, so an acceptance cycle belongs to the selected LTL claim.
    return contains(search, "pan:1: acceptance cycle ") && cycle
        && contains(replay, "<<<<<START OF CYCLE>>>>>");
}

} // namespace evrostos::backends::detail
