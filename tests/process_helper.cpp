// SPDX-License-Identifier: GPL-3.0-or-later
// Subprocess fixture: each mode supplies a specific exit, stream, or process-tree
// condition for the GoogleTest suite. Built only with the backend tests.
#include "backends/detail/process.hpp"
#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <string_view>
#include <thread>
#include <csignal>
#include <unistd.h>

int main(int argc, char** argv) {
    if (argc < 2) {
        return 64;
    }
    const std::string_view mode(argv[1]);
    // Change descriptors or SIGCHLD handling in this fixture process, leaving
    // the test runner intact. The nested run reports its check through exit status.
    if ((mode == "closed_stdio") || (mode == "ignored_sigchld")) {
        if (mode == "closed_stdio") {
            ::close(STDIN_FILENO);
            ::close(STDOUT_FILENO);
            ::close(STDERR_FILENO);
        } else {
            std::signal(SIGCHLD, SIG_IGN);
        }
        const auto result = evrostos::backends::detail::run_process(
            {argv[0], "arguments", "restored"}, std::filesystem::current_path(),
            std::chrono::steady_clock::now() + std::chrono::seconds(5), 8192);
        const bool correct = (mode == "closed_stdio")
            ? ((result.status == evrostos::backends::detail::ProcessStatus::exited)
               && (result.exit_code == 0) && (result.out == "restored\n"))
            : (result.status == evrostos::backends::detail::ProcessStatus::launch_error);
        return correct ? 0 : 1;
    }
    if (mode == "arguments") {
        // Newline-separated arguments preserve empty arguments and literal text.
        for (int i = 2; i < argc; ++i) {
            std::cout << argv[i] << '\n';
        }
        std::cerr << "stderr\n";
        return 0;
    }
    if (mode == "environment") {
        // Expose child-local cwd and locale; an EOF read confirms stdin setup.
        std::cout << std::filesystem::current_path().string() << '\n';
        const auto* locale = std::getenv("LC_ALL");
        std::cout << ((locale != nullptr) ? locale : "missing") << '\n';
        char input = 0;
        return (::read(STDIN_FILENO, &input, 1) == 0) ? 0 : 1;
    }
    if (mode == "exit") {
        // This program did execute, even though 127 also denotes setup failure
        // in the runner's pre-exec child.
        return 127;
    }
    if (mode == "signal") {
        // Signal termination must be distinguishable from a numeric exit code.
        std::raise(SIGTERM);
        return 1;
    }
    if (mode == "flood") {
        // Continuous output exercises bounded capture and process cancellation.
        const std::string block(8192, 'x');
        for (;;) {
            std::cout << block << std::flush;
            std::cerr << block << std::flush;
        }
    }
    if (mode == "sleep") {
        // Stay silent beyond the test deadline so timeout does not depend on I/O.
        std::this_thread::sleep_for(std::chrono::seconds(10));
        return 0;
    }
    if ((mode == "descendant") && (argc == 3)) {
        const auto child = ::fork();
        if (child < 0) {
            return 1;
        }
        if (child == 0) {
            // The marker appears only if group cancellation fails to stop this
            // descendant before its delayed write.
            std::this_thread::sleep_for(std::chrono::milliseconds(500));
            std::ofstream marker(argv[2]);
            marker << "orphan survived";
            return 0;
        }
        // Exit with a child still holding the output pipes open.
        return 0;
    }
    return 64;
}
