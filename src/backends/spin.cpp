// SPDX-License-Identifier: GPL-3.0-or-later
#include "spin.hpp"
#include "detail/process.hpp"
#include "detail/spin_result.hpp"

#include <array>
#include <cerrno>
#include <cstring>
#include <fstream>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <utility>

#include <stdlib.h>
#include <unistd.h>

namespace evrostos::backends {
namespace {

// The fixed name connects the appended property, verifier selection, and replay
// checks. The version string identifies the output format parsed by this adapter.
constexpr std::string_view property_name = "evrostos_property";
constexpr std::string_view spin_version = "Spin Version 6.5.2 -- 6 December 2019\n";

// Expected input and artifact failures become SpinResult diagnostics. Allocation
// failures remain exceptions and are not handled as invalid input.
class InputError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

// Check bytes as they are read, so the limit also applies if a file grows after
// inspection. Models and trails are text; embedded NUL bytes are rejected.
std::string read_file(const std::filesystem::path& path, std::size_t limit) {
    if (!std::filesystem::is_regular_file(path)) {
        throw InputError("not a regular file: " + path.string());
    }
    std::ifstream input(path, std::ios::binary);
    if (!input) {
        throw InputError("cannot read: " + path.string());
    }
    std::string text;
    std::array<char, 8192> buffer;
    while (input) {
        input.read(buffer.data(), static_cast<std::streamsize>(buffer.size()));
        const auto count = static_cast<std::size_t>(input.gcount());
        if (count > (limit - text.size())) {
            throw InputError("file exceeds byte limit: " + path.string());
        }
        text.append(buffer.data(), count);
    }
    if (!input.eof()) {
        throw InputError("failed reading: " + path.string());
    }
    if (text.find('\0') != std::string::npos) {
        throw InputError("file contains NUL: " + path.string());
    }
    return text;
}

void write_file(const std::filesystem::path& path, const std::string& text) {
    std::ofstream output(path, std::ios::binary);
    output.write(text.data(), static_cast<std::streamsize>(text.size()));
    // Closing explicitly exposes flush failures before the next tool consumes it.
    output.close();
    if (!output) {
        throw InputError("cannot write: " + path.string());
    }
}

// Promela tokens include underscores even though core formula atoms do not.
bool identifier_start(char value) {
    return ((value >= 'a') && (value <= 'z'))
        || ((value >= 'A') && (value <= 'Z')) || (value == '_');
}

bool identifier_part(char value) {
    return identifier_start(value) || ((value >= '0') && (value <= '9'));
}

// Scan preprocessed Promela tokens, ignoring strings and comments. Excluding
// embedded C and custom acceptance machinery keeps counterexample provenance
// tied to the selected property. This is a supported-input check, not a parser.
void validate_model(std::string_view text) {
    unsigned int property_occurrences = 0;
    for (std::size_t i = 0; i < text.size();) {
        if ((text[i] == '"') || (text[i] == '\'')) {
            // Escaped quotes stay inside the literal, whose contents do not
            // participate in keyword or reserved-name checks.
            const char quote = text[i++];
            while (i < text.size()) {
                if (text[i++] == quote) {
                    break;
                }
                if ((text[i - 1] == '\\') && (i < text.size())) {
                    ++i;
                }
            }
        } else if ((text[i] == '/') && ((i + 1) < text.size())
                   && (text[i + 1] == '/')) {
            while ((i < text.size()) && (text[i] != '\n')) {
                ++i;
            }
        } else if ((text[i] == '/') && ((i + 1) < text.size())
                   && (text[i + 1] == '*')) {
            const auto end = text.find("*/", i + 2);
            i = (end == std::string_view::npos) ? text.size() : end + 2;
        } else if (identifier_start(text[i])) {
            const auto start = i++;
            while ((i < text.size()) && identifier_part(text[i])) {
                ++i;
            }
            const auto token = text.substr(start, i - start);
            if (token == property_name) {
                ++property_occurrences;
            }
            // Only a label declaration makes an accept-prefixed name special;
            // ordinary identifiers such as accepted remain valid.
            auto next = i;
            while ((next < text.size()) && ((text[next] == ' ') || (text[next] == '\t')
                || (text[next] == '\r') || (text[next] == '\n')
                || (text[next] == '\f') || (text[next] == '\v'))) {
                ++next;
            }
            const bool acceptance_label = token.starts_with("accept")
                && (next < text.size()) && (text[next] == ':');
            if ((token == "never") || (token == "trace") || (token == "notrace")
                || (token == "c_code") || (token == "c_decl") || (token == "c_expr")
                || (token == "c_state") || (token == "c_track")
                || acceptance_label) {
                throw InputError("unsupported Promela token: " + std::string(token));
            }
        } else {
            ++i;
        }
    }
    // The appended declaration contributes one occurrence. Extra occurrences
    // collide with its name; none means preprocessing removed or renamed it.
    if (property_occurrences != 1) {
        throw InputError("model must reserve the name evrostos_property for the adapter");
    }
}

bool valid_path(const std::filesystem::path& path) {
    // Child cwd differs from the caller's, and POSIX paths terminate at NUL.
    return path.is_absolute() && (path.string().find('\0') == std::string::npos);
}

void validate_options(const SpinOptions& options, const std::filesystem::path& model) {
    if (!valid_path(options.executable) || !valid_path(options.c_compiler)
        || !valid_path(options.workspace_parent) || !valid_path(model)) {
        throw InputError("tool, workspace parent, and model paths must be absolute and contain no NUL");
    }
    // Bound deadline arithmetic, generated numeric flags, and captured inputs
    // before creating artifacts or launching tools.
    if ((options.timeout.count() <= 0) || (options.timeout > std::chrono::hours(24))
        || (options.max_depth < 2) || (options.max_depth > 10000000)
        || (options.memory_megabytes == 0) || (options.memory_megabytes > 65536)
        || (options.max_output_bytes == 0) || (options.max_model_bytes == 0)
        || (options.max_model_bytes > (256 * 1024 * 1024))
        || (options.max_output_bytes > (256 * 1024 * 1024))) {
        throw InputError("invalid SPIN resource limits");
    }
    if (!std::filesystem::is_directory(options.workspace_parent)) {
        throw InputError("workspace parent must be an existing directory");
    }
}

} // namespace

SpinBackend::SpinBackend(SpinOptions options) : options_(std::move(options)) {}

SpinResult SpinBackend::check(const std::filesystem::path& model,
                              const SharedPtrLtlNode& property) const {
    SpinResult result;
    try {
        // Reject invalid paths, limits, and formulas before creating a workspace.
        validate_options(options_, model);
        const auto deadline = std::chrono::steady_clock::now() + options_.timeout;
        const auto serialized = serialize_spin(property, {.allow_next = options_.allow_next},
                                                options_.serialization_limits);
        if (const auto* error = std::get_if<SerializationDiagnostic>(&serialized)) {
            result.message = error->message;
            return result;
        }
        // Append a positive property to a copy of the model. SPIN's inline-LTL
        // translation supplies the negation needed for counterexample search.
        auto source = read_file(model, options_.max_model_bytes);
        source += "\nltl " + std::string(property_name) + " { " + std::get<std::string>(serialized) + " }\n";
        // Unique per-call paths keep fixed SPIN filenames separate across runs.
        auto workspace = (options_.workspace_parent / "evrostos-spin-XXXXXX").string();
        if (::mkdtemp(workspace.data()) == nullptr) {
            throw InputError("cannot create workspace: " + std::string(std::strerror(errno)));
        }
        result.workspace = workspace;
        write_file(result.workspace / "input.pml", source);

        // Record each attempt before deciding whether the pipeline can continue.
        // Returning no text stops the caller while preserving captured evidence.
        const auto execute = [&](SpinStage stage, std::vector<std::string> arguments)
            -> std::optional<std::string> {
            result.stage = stage;
            auto execution = detail::run_process(arguments, result.workspace, deadline,
                                                  options_.max_output_bytes);
            const auto status = execution.status;
            const auto exit_code = execution.exit_code;
            result.commands.push_back({stage, std::move(arguments), exit_code, execution.signal,
                status == detail::ProcessStatus::timed_out,
                status == detail::ProcessStatus::output_limit,
                std::move(execution.out), std::move(execution.err), std::move(execution.diagnostic)});
            if ((status == detail::ProcessStatus::timed_out)
                || (status == detail::ProcessStatus::output_limit)) {
                result.outcome = CheckOutcome::inconclusive;
                result.message = (status == detail::ProcessStatus::timed_out)
                    ? "SPIN pipeline deadline exceeded" : "command output limit exceeded";
                return std::nullopt;
            }
            // Search output can identify a resource limit even with a nonzero
            // exit status; the search classifier handles that case below.
            if ((status != detail::ProcessStatus::exited)
                || ((exit_code != 0) && (stage != SpinStage::search))) {
                result.message = "SPIN pipeline command failed; see command diagnostics";
                return std::nullopt;
            }
            return result.commands.back().standard_output;
        };

        const auto version = execute(SpinStage::version, {options_.executable.string(), "-V"});
        if (!version) {
            return result;
        }
        if (*version != spin_version) {
            result.message = "unsupported SPIN version; use the pinned evrostos_spin_tool build";
            return result;
        }
        // Preprocess using argv, so include-directory paths never enter SPIN's
        // internal shell command. /bin/cat then passes fixed model.pml unchanged.
        const auto preprocessed = execute(SpinStage::preprocessing,
            {options_.c_compiler.string(), "-E", "-P", "-x", "c", "-iquote",
             model.parent_path().string(), "input.pml"});
        if (!preprocessed) {
            return result;
        }
        // Apply the model profile after macros and includes have been expanded.
        validate_model(*preprocessed);
        write_file(result.workspace / "model.pml", *preprocessed);
        // Statement merging changes the step count observed by X.
        if (!execute(SpinStage::generation,
                {options_.executable.string(), "-P/bin/cat", "-o3", "-a", "model.pml"})) {
            return result;
        }
        // Preserve full-state verification and support step-sensitive formulas.
        // MEMLIM constrains the generated verifier's own allocations.
        if (!execute(SpinStage::compilation,
                {options_.c_compiler.string(), "-O0", "-DNOREDUCE",
                 "-DMEMLIM=" + std::to_string(options_.memory_megabytes), "-o", "pan", "pan.c"})) {
            return result;
        }
        const auto verifier = (result.workspace / "pan").string();
        // Select the appended property explicitly. Acceptance cycles are enabled;
        // omitting -f leaves weak fairness disabled, and -w18 sizes the hash table.
        const auto search = execute(SpinStage::search,
            {verifier, "-a", "-N", std::string(property_name),
             "-m" + std::to_string(options_.max_depth), "-w18"});
        if (!search) {
            return result;
        }
        // A zero exit code alone carries no truth value: the summary must also
        // identify the selected property, search mode, and completion status.
        const auto finding = detail::inspect_search(*search, result.commands.back().standard_error);
        if ((finding != detail::SearchFinding::incomplete)
            && (result.commands.back().exit_code != 0)) {
            result.message = "verifier exited unsuccessfully";
        } else if (finding == detail::SearchFinding::holds) {
            result.outcome = CheckOutcome::holds;
            result.message = "exhaustive search completed without a property counterexample";
        } else if (finding == detail::SearchFinding::incomplete) {
            result.outcome = CheckOutcome::inconclusive;
            result.message = "verifier search incomplete";
        } else if (finding == detail::SearchFinding::counterexample) {
            // A reported error can come from a model assertion. The trail and
            // replay determine whether it belongs to the selected LTL property.
            result.trail = result.workspace / "model.pml.trail";
            if (std::filesystem::file_size(result.trail) > options_.max_output_bytes) {
                result.outcome = CheckOutcome::inconclusive;
                result.message = "counterexample trail exceeds byte limit";
                return result;
            }
            const auto trail = read_file(result.trail, options_.max_output_bytes);
            const auto replay = execute(SpinStage::replay,
                {verifier, "-r", "-v", "-N", std::string(property_name)});
            if (!replay) {
                return result;
            }
            if (detail::property_counterexample(*search, trail, *replay)) {
                result.outcome = CheckOutcome::violated;
                result.message = "selected property has a replayed counterexample";
            } else {
                result.message = "verifier error is not a validated property counterexample; see trail and diagnostics";
            }
        } else {
            result.message = "unrecognized or inconsistent verifier output";
        }
    } catch (const InputError& error) {
        result.message = error.what();
    } catch (const std::filesystem::filesystem_error& error) {
        result.message = error.what();
    }
    return result;
}

} // namespace evrostos::backends
