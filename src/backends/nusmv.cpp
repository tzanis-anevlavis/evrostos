// SPDX-License-Identifier: GPL-3.0-or-later
#include "nusmv.hpp"
#include "detail/nusmv_result.hpp"
#include "detail/process.hpp"

#include <algorithm>
#include <array>
#include <cerrno>
#include <cstring>
#include <fstream>
#include <stdexcept>
#include <utility>

#include <stdlib.h>
#include <unistd.h>

namespace evrostos::backends {
namespace {

// Expected input/artifact errors become result diagnostics; allocation failures
// remain exceptions. File reads are bounded even if the source grows mid-read.
class InputError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

std::string read_model(const std::filesystem::path& path, std::size_t limit) {
    if (!std::filesystem::is_regular_file(path)) {
        throw InputError("model is not a regular file");
    }
    std::ifstream input(path, std::ios::binary);
    if (!input) {
        throw InputError("cannot read model");
    }
    std::string text;
    std::array<char, 8192> buffer;
    while (input) {
        input.read(buffer.data(), static_cast<std::streamsize>(buffer.size()));
        const auto count = static_cast<std::size_t>(input.gcount());
        if (count > (limit - text.size())) {
            throw InputError("model exceeds byte limit");
        }
        text.append(buffer.data(), count);
    }
    if (!input.eof() || (text.find('\0') != std::string::npos)) {
        throw InputError("model read failed or contains NUL");
    }
    return text;
}

void write_file(const std::filesystem::path& path, std::string_view text) {
    std::ofstream output(path, std::ios::binary);
    output.write(text.data(), static_cast<std::streamsize>(text.size()));
    output.close();
    if (!output) {
        throw InputError("cannot write: " + path.string());
    }
}

bool valid_path(const std::filesystem::path& path) {
    return path.is_absolute() && (path.string().find('\0') == std::string::npos);
}

// NuSMV 2.7.1 requires compassion models to have no input variables. A
// process instance introduces an implicit scheduler input. Match declaration
// tokens throughout the source, skipping quoted names and both comment forms.
void validate_compassion(std::string_view source) {
    if (source.find("COMPASSION") == std::string_view::npos) {
        return;
    }
    constexpr std::string_view letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_";
    constexpr std::string_view identifier = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789$#-";
    bool compassion = false;
    bool inputs = false;
    while (!source.empty()) {
        if (source.starts_with("--")) {
            source.remove_prefix(std::min(source.find('\n'), source.size()));
        } else if (source.starts_with("/--")) {
            const auto end = source.find("--/", 3);
            source.remove_prefix((end == source.npos) ? source.size() : end + 3);
        } else if (source.front() == '"') {
            const auto end = source.find('"', 1);
            source.remove_prefix((end == source.npos) ? source.size() : end + 1);
        } else if (letters.find(source.front()) != letters.npos) {
            const auto length = std::min(source.find_first_not_of(identifier), source.size());
            const auto token = source.substr(0, length);
            compassion = compassion || (token == "COMPASSION");
            inputs = inputs || (token == "IVAR") || (token == "process");
            if (compassion && inputs) {
                throw InputError("NuSMV compassion checking requires a model without input variables or processes");
            }
            source.remove_prefix(length);
        } else {
            source.remove_prefix(1);
        }
    }
}

void validate_options(const NuSmvOptions& options, const std::filesystem::path& model) {
    if (!valid_path(options.executable) || !valid_path(options.workspace_parent)
        || !valid_path(model)) {
        throw InputError("tool, workspace parent, and model paths must be absolute and contain no NUL");
    }
    if ((options.timeout.count() <= 0) || (options.timeout > std::chrono::hours(24))
        || (options.max_model_bytes == 0) || (options.max_model_bytes > (256 * 1024 * 1024))
        || (options.max_output_bytes == 0) || (options.max_output_bytes > (256 * 1024 * 1024))) {
        throw InputError("invalid NuSMV resource limits");
    }
    if (!std::filesystem::is_directory(options.workspace_parent)) {
        throw InputError("workspace parent must be an existing directory");
    }
}

constexpr std::string_view query_prefix = "check_ltlspec -p \"";
constexpr std::string_view query_suffix = "\"\n";
// The script reader's 65,536-byte buffer includes the newline and final NUL.
constexpr auto max_query_formula_bytes = 65536 - 1 - query_prefix.size() - query_suffix.size();

// The serializer's alphabet fits in a quoted command argument. Fixed filenames
// address the model copy and scripts in this call's private workspace.
std::string checking_script(const std::string& formula) {
    return "set on_failure_script_quits\n"
        "read_model -i model.smv\n"
        "go\n"
        "echo EVROSTOS_MODEL\n"
        "check_fsm\n"
        "echo EVROSTOS_EXECUTIONS\n"
        "set counter_examples 0\n"
        "check_ltlspec -p \"FALSE\"\n"
        "echo EVROSTOS_PROPERTY\n"
        "set counter_examples 1\n"
        + std::string(query_prefix) + formula + std::string(query_suffix) +
        "echo EVROSTOS_REPLAY\n"
        "execute_traces -e bdd -a\n"
        "echo EVROSTOS_DONE\n"
        "quit\n";
}

} // namespace

NuSmvBackend::NuSmvBackend(NuSmvOptions options) : options_(std::move(options)) {}

NuSmvVerificationResult NuSmvBackend::verify(const std::filesystem::path& model,
                                             std::string_view formula,
                                             const TranslationLimits& limits) const {
    NuSmvVerification result;
    // The core owns ordering and bounds; the adapter retains the actual evidence
    // without manufacturing results for bits resolved by inference.
    const auto verified = Evrostos{}.verify(formula,
        [&](std::size_t bit, const SharedPtrLtlNode& property) {
            result.checks[bit] = check(model, property);
            return result.checks[bit]->outcome;
        }, limits);
    if (const auto* error = std::get_if<Diagnostic>(&verified)) {
        return *error;
    }
    result.summary = std::get<Verification>(verified);
    return result;
}

NuSmvResult NuSmvBackend::check(const std::filesystem::path& model,
                                const SharedPtrLtlNode& property) const {
    NuSmvResult result;
    try {
        validate_options(options_, model);
        const auto deadline = std::chrono::steady_clock::now() + options_.timeout;
        const SerializationLimits limits{
            std::min(options_.serialization_limits.max_output_bytes, max_query_formula_bytes)};
        const auto serialized = serialize_nusmv(property, limits);
        if (const auto* error = std::get_if<SerializationDiagnostic>(&serialized)) {
            result.message = error->message;
            return result;
        }
        const auto source = read_model(model, options_.max_model_bytes);
        validate_compassion(source);
        auto workspace = (options_.workspace_parent / "evrostos-nusmv-XXXXXX").string();
        if (::mkdtemp(workspace.data()) == nullptr) {
            throw InputError("cannot create workspace: " + std::string(std::strerror(errno)));
        }
        result.workspace = workspace;
        write_file(result.workspace / "model.smv", source);
        write_file(result.workspace / "version.commands", "quit\n");
        write_file(result.workspace / "check.commands", checking_script(std::get<std::string>(serialized)));

        // Every command uses the same deadline. Cancellation retains diagnostic
        // prefixes but cannot establish a truth value from incomplete output.
        const auto execute = [&](NuSmvStage stage, const std::string& script) {
            result.stage = stage;
            std::vector<std::string> arguments{options_.executable.string(), "-s", "-source", script};
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
                    ? "NuSMV pipeline deadline exceeded" : "command output limit exceeded";
                return false;
            }
            if ((status != detail::ProcessStatus::exited) || (exit_code != 0)) {
                result.message = "NuSMV command failed; see command diagnostics";
                return false;
            }
            return true;
        };
        if (!execute(NuSmvStage::version, "version.commands")) {
            return result;
        }
        if (!detail::supported_nusmv_version(result.commands.back().standard_output)) {
            result.message = "unsupported NuSMV version; use the pinned evrostos_nusmv_tool build";
            return result;
        }
        if (!execute(NuSmvStage::checking, "check.commands")) {
            return result;
        }
        const auto finding = detail::inspect_nusmv(result.commands.back().standard_output);
        if (!finding) {
            result.message = "unrecognized or inconsistent NuSMV evidence";
            return result;
        }
        result.has_admissible_executions = finding->has_admissible_executions;
        result.transition_relation_total = finding->transition_relation_total;
        if (!finding->holds) {
            // Preserve the bounded trace text only after successful native replay.
            const auto trace_path = result.workspace / "counterexample.txt";
            write_file(trace_path, finding->counterexample);
            result.counterexample = trace_path;
            result.outcome = CheckOutcome::violated;
            result.message = "selected property has a BDD-replayed counterexample";
        } else {
            result.outcome = CheckOutcome::holds;
            result.message = finding->has_admissible_executions
                ? "BDD LTL checking completed: property holds"
                : "property holds vacuously: no fair infinite execution starts in an initial state";
        }
    } catch (const InputError& error) {
        result.message = error.what();
    } catch (const std::filesystem::filesystem_error& error) {
        result.message = error.what();
    }
    return result;
}

} // namespace evrostos::backends
