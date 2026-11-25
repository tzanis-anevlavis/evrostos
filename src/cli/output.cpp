// SPDX-License-Identifier: GPL-3.0-or-later
#include "output.hpp"

#include <cerrno>
#include <fcntl.h>
#include <fstream>
#include <stdexcept>
#include <sys/file.h>
#include <system_error>
#include <unistd.h>

namespace evrostos::cli {
namespace fs = std::filesystem;
namespace {

constexpr std::string_view heading = "Evrostos counterexamples\n";

// Serialize publication by concurrent CLI runs targeting the same directory.
// Checking the model and staging the files take place outside the lock.
class DirectoryLock {
public:
    explicit DirectoryLock(const fs::path& directory) : fd_(::open(directory.c_str(), O_RDONLY | O_DIRECTORY)) {
        if (fd_ < 0) {
            throw std::system_error(errno, std::generic_category(), "cannot open output directory");
        }
        while (::flock(fd_, LOCK_EX) != 0) {
            if (errno != EINTR) {
                const int error = errno;
                ::close(fd_);
                throw std::system_error(error, std::generic_category(), "cannot lock output directory");
            }
        }
    }
    ~DirectoryLock() { ::close(fd_); }
    DirectoryLock(const DirectoryLock&) = delete;
    DirectoryLock& operator=(const DirectoryLock&) = delete;
private:
    int fd_;
};

void write_text(const fs::path& path, std::string_view first, std::string_view second) {
    std::ofstream file;
    file.exceptions(std::ios::badbit | std::ios::failbit);
    file.open(path, std::ios::binary);
    file << first << second;
    file.close();
}

bool is_generated_companion(const fs::path& file) {
    std::ifstream input(file, std::ios::binary);
    char prefix[heading.size()];
    input.read(prefix, sizeof(prefix));
    return (input.gcount() == sizeof(prefix)) && (std::string_view(prefix, sizeof(prefix)) == heading);
}

fs::path counterexamples_path(const fs::path& output, const fs::path& staged_json) {
    // Look up the requested spelling beside the staged report.json. Both
    // directories use the output filesystem, even on the first publication.
    const auto spelling = staged_json.parent_path() / output.filename();
    if (fs::exists(spelling) && fs::equivalent(spelling, staged_json)) {
        return output.parent_path() / "counterexamples.txt";
    }
    return output.string() + ".counterexamples.txt";
}

} // namespace

void validate_output(const fs::path& path, const fs::path& input) {
    const auto target = fs::symlink_status(path);
    if (fs::exists(target) && !fs::is_regular_file(target)) {
        throw std::runtime_error("output destination must be a regular file: " + path.string());
    }
    if ((fs::weakly_canonical(input) == fs::weakly_canonical(path))
        || (fs::exists(target) && fs::exists(input) && fs::equivalent(input, path))) {
        throw std::runtime_error("input and output paths must differ: " + path.string());
    }
}

fs::path publish_report(const Report& report, const fs::path& output, const fs::path& workspace,
                        const fs::path& model, const fs::path* formula_list) {
    const auto staged_json = workspace / "report.json";
    const auto staged_text = workspace / "counterexamples.txt";
    write_text(staged_json, report.json, "\n");
    const auto companion = counterexamples_path(output, staged_json);
    validate_output(companion, model);
    if (formula_list != nullptr) {
        validate_output(companion, *formula_list);
    }
    if (report.counterexamples.has_value()) {
        write_text(staged_text, heading, *report.counterexamples);
    }

    const DirectoryLock lock(workspace.parent_path());
    if (fs::is_regular_file(fs::symlink_status(output)) && is_generated_companion(output)) {
        throw std::runtime_error("report destination contains a counterexample trace: " + output.string());
    }
    const auto target = fs::symlink_status(companion);
    const bool previous = fs::is_regular_file(target) && is_generated_companion(companion);
    if (fs::exists(target) && !previous && report.counterexamples.has_value()) {
        throw std::runtime_error("counterexample file already exists: " + companion.string());
    }
    // Keep the preceding witness until both outputs are ready. A run with
    // no witness retires only a companion produced by Evrostos.
    const auto backup = workspace / "previous.txt";
    if (previous) {
        fs::rename(companion, backup);
    }
    bool published_text = false;
    try {
        if (report.counterexamples.has_value()) {
            fs::rename(staged_text, companion);
            published_text = true;
        }
        fs::rename(staged_json, output);
    } catch (const fs::filesystem_error&) {
        if (previous) {
            fs::rename(backup, companion);
        } else if (published_text) {
            fs::remove(companion);
        }
        throw;
    }
    return companion;
}

} // namespace evrostos::cli
