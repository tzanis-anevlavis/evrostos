// SPDX-License-Identifier: GPL-3.0-or-later
#include "report.hpp"

#include <algorithm>
#include <fstream>
#include <nlohmann/json.hpp>
#include <stdexcept>

namespace evrostos::cli {
namespace fs = std::filesystem;
namespace {

// Validate the list structure before starting any checker. Individual formula
// syntax is checked during verification so later entries can still run.
std::vector<std::string> read_formulas(const fs::path& list) {
    if (!fs::is_regular_file(list)) {
        throw std::runtime_error("formula list is not a regular file: " + list.string());
    }
    std::ifstream source(list, std::ios::binary);
    if (!source.is_open()) {
        throw std::runtime_error("cannot read formula list: " + list.string());
    }
    source.exceptions(std::ios::badbit);
    std::vector<std::string> formulas;
    if (list.extension() == ".txt") {
        std::string line;
        while (std::getline(source, line)) {
            if (line.ends_with('\r')) {
                line.pop_back();
            }
            if (line.find_first_not_of(" \t") != std::string::npos) {
                formulas.push_back(std::move(line));
            }
        }
    } else if (list.extension() == ".json") {
        // The JSON lexer treats a literal NUL as end of input. Read up to
        // that byte and require physical EOF before parsing the document.
        std::string document;
        std::getline(source, document, '\0');
        if (!source.eof()) {
            throw std::runtime_error("literal NUL in formula list JSON at byte " + std::to_string(document.size() + 1));
        }
        nlohmann::json array;
        try {
            array = nlohmann::json::parse(document);
        } catch (const nlohmann::json::parse_error& error) {
            throw std::runtime_error("invalid formula list JSON at byte " + std::to_string(error.byte));
        } catch (const nlohmann::json::out_of_range&) {
            throw std::runtime_error("expected a JSON array of formula strings");
        }
        if (!array.is_array()) {
            throw std::runtime_error("expected a JSON array of formula strings");
        }
        for (const auto& item : array) {
            if (!item.is_string()) {
                throw std::runtime_error("formula " + std::to_string(formulas.size() + 1) + " must be a JSON string");
            }
            formulas.push_back(item.get<std::string>());
        }
    } else {
        throw std::runtime_error("formula list filename must end in .txt or .json");
    }
    if (formulas.empty()) {
        throw std::runtime_error("formula list is empty");
    }
    return formulas;
}

} // namespace

Report verify_list(const std::string& backend, const fs::path& model, const fs::path& list,
                   const fs::path& workspace, std::chrono::seconds timeout) {
    nlohmann::ordered_json fields{{"backend", backend}, {"model", model.string()}, {"formula_list", list.string()}};
    auto results = nlohmann::ordered_json::array();
    Report batch{};
    try {
        const auto formulas = read_formulas(list);
        if (!fs::is_regular_file(model)) {
            throw std::runtime_error("model is not a regular file: " + model.string());
        }
        // A SPIN run can generate a verifier for each bit. Release its files
        // after capturing the evidence for the current formula.
        const auto item_workspace = workspace / "formula";
        for (std::size_t i = 0; i < formulas.size(); ++i) {
            const ReportInput input{backend, model, formulas[i], i + 1};
            Report item;
            try {
                fs::create_directory(item_workspace);
                item = verify(input, item_workspace, timeout);
            } catch (const std::runtime_error& error) {
                item = error_report(input, error.what());
            }
            // Store shared metadata once. Checks, diagnostic locations and
            // witnesses keep the same structure as in a single-formula report.
            auto record = nlohmann::ordered_json::parse(item.json);
            record.erase("backend");
            record.erase("model");
            results.push_back(std::move(record));
            if (i > 0) {
                batch.console += '\n';
            }
            batch.console += "Formula " + std::to_string(i + 1) + ": " + input.formula + "\n" + item.console;
            batch.exit_code = std::max(batch.exit_code, item.exit_code);
            if (item.counterexamples.has_value()) {
                if (batch.counterexamples.has_value()) {
                    *batch.counterexamples += "\n\n";
                } else {
                    batch.counterexamples.emplace();
                }
                *batch.counterexamples += *item.counterexamples;
            }
            fs::remove_all(item_workspace);
        }
    } catch (const std::runtime_error& error) {
        fields["diagnostic"] = {{"message", error.what()}};
        if (!batch.console.empty()) {
            batch.console += '\n';
        }
        batch.console += std::string("Error: ") + error.what();
        batch.exit_code = 2;
    }
    if (batch.exit_code == 2) {
        fields["status"] = "error";
    } else if (batch.exit_code == 1) {
        fields["status"] = "inconclusive";
    } else {
        fields["status"] = "exact";
    }
    fields["results"] = std::move(results);
    batch.json = fields.dump(-1, ' ', false, nlohmann::ordered_json::error_handler_t::replace);
    return batch;
}

} // namespace evrostos::cli
