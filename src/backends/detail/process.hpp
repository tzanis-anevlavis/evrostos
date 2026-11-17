// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include <chrono>
#include <cstddef>
#include <filesystem>
#include <string>
#include <vector>

namespace evrostos::backends::detail {

// Process-level outcomes only; the backend interprets tool output separately.
// A normal exit may have a nonzero exit_code, including 127.
enum class ProcessStatus { exited, signaled, timed_out, output_limit, launch_error };

struct ProcessResult {
    ProcessStatus status = ProcessStatus::launch_error;
    // Populated for an uncancelled normal exit or signal termination, respectively.
    int exit_code = -1;
    int signal = 0;
    // Captured prefixes are retained when a deadline or output limit stops a run.
    std::string out;
    std::string err;
    // Launch or I/O diagnostics from the runner; separate from the child's stderr.
    std::string diagnostic;
};

// argv[0] must be absolute. The child gets its own process group and working
// directory. A deadline or combined stdout/stderr limit kills the whole group.
// The runner owns child reaping. It fixes the child environment and supplies EOF
// on stdin without changing the parent's cwd or environment.
ProcessResult run_process(const std::vector<std::string>& argv,
                          const std::filesystem::path& directory,
                          std::chrono::steady_clock::time_point deadline,
                          std::size_t output_limit);

} // namespace evrostos::backends::detail
