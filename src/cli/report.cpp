// SPDX-License-Identifier: GPL-3.0-or-later
#include "report.hpp"
#include <nlohmann/json.hpp>
#include <stdexcept>

namespace evrostos::cli {
namespace {

const char* outcome_name(backends::CheckOutcome outcome) {
    switch (outcome) {
    case backends::CheckOutcome::holds: return "holds";
    case backends::CheckOutcome::violated: return "violated";
    case backends::CheckOutcome::inconclusive: return "inconclusive";
    case backends::CheckOutcome::execution_error: return "execution_error";
    }
    throw std::logic_error("unknown checker outcome");
}

// Apply the same UTF-8 replacement policy to paths, diagnostics and native traces.
std::string serialize_report(nlohmann::ordered_json& fields, const ReportInput& input) {
    fields["backend"] = input.backend;
    fields["model"] = input.model.string();
    fields["formula"] = input.formula;
    if (input.index.has_value()) {
        fields["index"] = *input.index;
    }
    return fields.dump(-1, ' ', false, nlohmann::ordered_json::error_handler_t::replace);
}

} // namespace

Report error_report(const ReportInput& input, const std::string& message) {
    nlohmann::ordered_json fields{{"status", "error"}, {"diagnostic", {{"message", message}}}};
    return {serialize_report(fields, input), "Error: " + message, 2, std::nullopt};
}

Report error_report(const ReportInput& input, const Diagnostic& diagnostic) {
    nlohmann::ordered_json fields{{"status", "error"}, {"diagnostic", {
        {"message", diagnostic.message}, {"line", diagnostic.line},
        {"column", diagnostic.column}, {"offset", diagnostic.offset}}}};
    return {serialize_report(fields, input), "Error: " + diagnostic.message, 2, std::nullopt};
}

Report verification_report(const ReportInput& input, const Verification& summary,
                           const std::vector<BitCheck>& checks) {
    Report report{};
    nlohmann::ordered_json fields;
    const auto exact = summary.exact_value();
    if (exact.has_value()) {
        const std::string value(to_string(*exact));
        fields = {{"status", "exact"}, {"value", value}};
        report.console = "Exact value: " + value;
        report.exit_code = 0;
    } else {
        auto possible = nlohmann::ordered_json::array();
        report.console = "Inconclusive: {";
        for (const auto value : robust_values) {
            if (less_equal(summary.lower_bound, value) && less_equal(value, summary.upper_bound)) {
                if (!possible.empty()) {
                    report.console += ", ";
                }
                const std::string name(to_string(value));
                possible.emplace_back(name);
                report.console += name;
            }
        }
        report.console += '}';
        fields = {{"status", "inconclusive"}, {"possible_values", std::move(possible)}};
        report.exit_code = 1;
    }

    auto records = nlohmann::ordered_json::array();
    for (const auto& check : checks) {
        nlohmann::ordered_json record{{"bit", check.bit}, {"outcome", outcome_name(check.outcome)},
                                      {"stage", check.stage}, {"message", check.message}};
        if (check.command.has_value()) {
            const auto& command = *check.command;
            record["command"] = {{"stage", command.stage}, {"exit_code", command.exit_code},
                {"signal", command.signal}, {"timed_out", command.timed_out},
                {"output_truncated", command.output_truncated}, {"stdout", command.standard_output},
                {"stderr", command.standard_error}, {"diagnostic", command.diagnostic}};
        }
        records.emplace_back(std::move(record));
        if (!exact.has_value() && ((check.outcome == backends::CheckOutcome::inconclusive)
                               || (check.outcome == backends::CheckOutcome::execution_error))) {
            report.console += "\n" + check.bit + " (" + check.stage + "): " + check.message;
        }
        // A validated violation supplies a witness even with unresolved weaker bits.
        if (check.trace.has_value()) {
            fields["counterexample"] = {{"bit", check.bit}, {"trace", *check.trace}};
            const auto label = input.index.has_value() ? "Formula " + std::to_string(*input.index) : "Formula";
            report.counterexamples = label + ": " + input.formula + "\nViolated bit: " + check.bit
                + "\n\n" + *check.trace;
            report.console += "\nViolated bit: " + check.bit;
        }
    }
    fields["checks"] = std::move(records);
    report.json = serialize_report(fields, input);
    return report;
}

} // namespace evrostos::cli
