// SPDX-License-Identifier: GPL-3.0-or-later
#include "output.hpp"
#include <cerrno>
#include <charconv>
#include <cstdlib>
#include <iostream>
#include <map>
#include <stdexcept>
#include <system_error>
#include <unistd.h>

namespace {
namespace fs = std::filesystem;

constexpr std::string_view usage =
    "Usage: evrostos verify --backend nusmv|spin --model FILE\n"
    "                      (--formula FORMULA | --formula-list FILE.txt|FILE.json)\n"
    "                      [--output FILE] [--timeout-s N]\n"
    "Default report: report.json. Validated traces: counterexamples.txt.\n"
    "Default timeout: 300 seconds per bit.\n";

struct Options {
    std::string backend;
    fs::path model;
    std::variant<std::string, fs::path> formulas;
    fs::path output;
    std::chrono::seconds timeout;
};

Options parse_options(int argc, char** argv) {
    if ((argc < 2) || (std::string_view(argv[1]) != "verify")) {
        throw std::invalid_argument("expected the verify command");
    }
    std::map<std::string, std::string> values;
    for (int i = 2; i < argc; i += 2) {
        const std::string key(argv[i]);
        if ((key != "--backend") && (key != "--model") && (key != "--formula")
            && (key != "--formula-list") && (key != "--output") && (key != "--timeout-s")) {
            throw std::invalid_argument("unknown option: " + key);
        }
        if ((i + 1) == argc) {
            throw std::invalid_argument("missing value for " + key);
        }
        if (!values.emplace(key, argv[i + 1]).second) {
            throw std::invalid_argument("duplicate option: " + key);
        }
    }
    for (const auto* key : {"--backend", "--model"}) {
        if (!values.contains(key)) {
            throw std::invalid_argument(std::string("missing required option: ") + key);
        }
    }
    if (values.contains("--formula") == values.contains("--formula-list")) {
        throw std::invalid_argument("supply exactly one of --formula and --formula-list");
    }
    const auto& backend = values.at("--backend");
    if ((backend != "nusmv") && (backend != "spin")) {
        throw std::invalid_argument("expected --backend nusmv or spin");
    }
    int seconds = 300;
    if (values.contains("--timeout-s")) {
        const auto& value = values.at("--timeout-s");
        const auto end = value.data() + value.size();
        const auto parsed = std::from_chars(value.data(), end, seconds);
        if ((parsed.ec != std::errc{}) || (parsed.ptr != end)
            || (seconds <= 0) || (seconds > 86400)) {
            throw std::invalid_argument("--timeout-s must be an integer between 1 and 86400");
        }
    }
    std::variant<std::string, fs::path> formulas;
    if (values.contains("--formula-list")) {
        formulas = fs::path(values.at("--formula-list"));
    } else {
        formulas = values.at("--formula");
    }
    return {backend, values.at("--model"), std::move(formulas),
            values.contains("--output") ? values.at("--output") : "report.json",
            std::chrono::seconds(seconds)};
}

// Keep generated checker files private. Staging the JSON in the same directory
// tree as the destination lets rename publish a complete report atomically.
class RunDirectory {
public:
    explicit RunDirectory(const fs::path& parent) {
        auto pattern = (parent / ".evrostos-XXXXXX").string();
        if (::mkdtemp(pattern.data()) == nullptr) {
            throw std::system_error(errno, std::generic_category(), "cannot create report workspace");
        }
        path = std::move(pattern);
    }
    RunDirectory(const RunDirectory&) = delete;
    RunDirectory& operator=(const RunDirectory&) = delete;
    ~RunDirectory() {
        std::error_code error;
        fs::remove_all(path, error);
        if (error) {
            std::cerr << "Could not remove " << path << ": " << error.message() << '\n';
        }
    }
    fs::path path;
};

int run(const Options& options) {
    const auto model = fs::absolute(options.model);
    const auto output = fs::absolute(options.output);
    evrostos::cli::validate_output(output, model);
    const RunDirectory workspace(output.parent_path());
    evrostos::cli::Report report;
    const auto* list = std::get_if<fs::path>(&options.formulas);
    if (list != nullptr) {
        const auto list_path = fs::absolute(*list);
        evrostos::cli::validate_output(output, list_path);
        report = evrostos::cli::verify_list(options.backend, model, list_path, workspace.path, options.timeout);
    } else {
        const evrostos::cli::ReportInput input{options.backend, model, std::get<std::string>(options.formulas)};
        try {
            report = evrostos::cli::verify(input, workspace.path, options.timeout);
        } catch (const std::runtime_error& error) {
            report = evrostos::cli::error_report(input, error.what());
        }
    }
    const auto companion = evrostos::cli::publish_report(report, options.output, workspace.path, model, list);
    std::cout << report.console << '\n';
    if (report.counterexamples.has_value()) {
        std::cout << "Counterexamples: " << companion.string() << '\n';
    }
    std::cout << "Report: " << options.output.string() << '\n';
    return report.exit_code;
}
} // namespace

int main(int argc, char** argv) {
    if (((argc == 2) && (std::string_view(argv[1]) == "--help"))
        || ((argc == 3) && (std::string_view(argv[1]) == "verify")
            && (std::string_view(argv[2]) == "--help"))) {
        std::cout << usage;
        return 0;
    }
    try {
        return run(parse_options(argc, argv));
    } catch (const std::invalid_argument& error) {
        std::cerr << error.what() << '\n' << usage;
    } catch (const std::exception& error) {
        std::cerr << "Error: " << error.what() << '\n';
    }
    return 2;
}
