// SPDX-License-Identifier: GPL-3.0-or-later
#include "backends/detail/process.hpp"
#include "spin_test_support.hpp"

#include <gtest/gtest.h>

#include <future>
#include <thread>
#include <csignal>

namespace {

// Each test owns a workspace and invokes the fixture executable by absolute path.
class Process : public testing::Test {
protected:
    TestWorkspace workspace;
    evrostos::backends::detail::ProcessResult run(std::vector<std::string> arguments,
        std::chrono::milliseconds timeout = std::chrono::seconds(5), std::size_t limit = 8192) {
        arguments.insert(arguments.begin(), EVROSTOS_PROCESS_HELPER);
        return evrostos::backends::detail::run_process(arguments, workspace.path,
            std::chrono::steady_clock::now() + timeout, limit);
    }
};

// Spaces, empty arguments, and shell syntax reach the child literally, including
// when the executable path itself contains shell metacharacters.
TEST_F(Process, PassesArgumentsLiterallyAndCapturesBothStreams) {
    const auto result = run({"arguments", "two words", "'\";$(touch surprise)&*", ""});
    EXPECT_EQ(result.status, evrostos::backends::detail::ProcessStatus::exited);
    EXPECT_EQ(result.exit_code, 0);
    EXPECT_EQ(result.out, "two words\n'\";$(touch surprise)&*\n\n");
    EXPECT_EQ(result.err, "stderr\n");
    EXPECT_FALSE(std::filesystem::exists(workspace.path / "surprise"));
    const auto executable = workspace.path / "tool with spaces ; '$()";
    std::filesystem::create_symlink(EVROSTOS_PROCESS_HELPER, executable);
    const auto literal = evrostos::backends::detail::run_process(
        {executable.string(), "arguments", "literal"}, workspace.path,
        std::chrono::steady_clock::now() + std::chrono::seconds(5), 8192);
    EXPECT_EQ(literal.exit_code, 0);
    EXPECT_EQ(literal.out, "literal\n");
}

// The child observes its assigned cwd, C locale, and EOF on stdin. The parent's
// working directory remains unchanged after the call.
TEST_F(Process, SetsChildDirectoryLocaleAndClosedInputWithoutChangingParent) {
    const auto parent = std::filesystem::current_path();
    const auto result = run({"environment"});
    EXPECT_EQ(result.exit_code, 0);
    EXPECT_EQ(result.out, workspace.path.string() + "\nC\n");
    EXPECT_EQ(std::filesystem::current_path(), parent);
}

// A missing executable, an executed program returning 127, and SIGTERM each
// produce a distinct process result with the corresponding diagnostic or status.
TEST_F(Process, DistinguishesLaunchFailureExit127AndSignal) {
    const auto missing = evrostos::backends::detail::run_process(
        {(workspace.path / "missing").string()}, workspace.path,
        std::chrono::steady_clock::now() + std::chrono::seconds(5), 8192);
    EXPECT_EQ(missing.status, evrostos::backends::detail::ProcessStatus::launch_error);
    EXPECT_FALSE(missing.diagnostic.empty());
    const auto exited = run({"exit"});
    EXPECT_EQ(exited.status, evrostos::backends::detail::ProcessStatus::exited);
    EXPECT_EQ(exited.exit_code, 127);
    const auto signaled = run({"signal"});
    EXPECT_EQ(signaled.status, evrostos::backends::detail::ProcessStatus::signaled);
    EXPECT_EQ(signaled.signal, SIGTERM);
}

// Reject paths requiring lookup and arguments that POSIX would truncate at NUL.
TEST_F(Process, RejectsRelativeExecutablesAndNulArguments) {
    const auto relative = evrostos::backends::detail::run_process({"relative"}, workspace.path,
        std::chrono::steady_clock::now() + std::chrono::seconds(5), 8192);
    EXPECT_EQ(relative.status, evrostos::backends::detail::ProcessStatus::launch_error);
    EXPECT_EQ(run({"arguments", std::string("x\0y", 3)}).status,
              evrostos::backends::detail::ProcessStatus::launch_error);
}

// Both streams share one byte budget. Exact-size output fits; overflow and a
// zero budget stop the child while retaining no more than the configured bytes.
TEST_F(Process, BoundsCombinedOutputAndStopsNoisyProcesses) {
    const auto result = run({"flood"}, std::chrono::seconds(5), 10000);
    EXPECT_EQ(result.status, evrostos::backends::detail::ProcessStatus::output_limit);
    EXPECT_EQ(result.out.size() + result.err.size(), 10000);
    const auto exact = run({"arguments", "x"}, std::chrono::seconds(5), 9);
    EXPECT_EQ(exact.status, evrostos::backends::detail::ProcessStatus::exited);
    EXPECT_EQ(exact.out.size() + exact.err.size(), 9);
    EXPECT_EQ(run({"arguments", "x"}, std::chrono::seconds(5), 0).status,
              evrostos::backends::detail::ProcessStatus::output_limit);
}

// A failed chdir is a launch error; an already-expired deadline prevents launch
// and reports timeout independently of the executable's behavior.
TEST_F(Process, ReportsInvalidDirectoryAndExpiredDeadline) {
    const auto missing = evrostos::backends::detail::run_process(
        {EVROSTOS_PROCESS_HELPER, "environment"}, workspace.path / "missing",
        std::chrono::steady_clock::now() + std::chrono::seconds(5), 8192);
    EXPECT_EQ(missing.status, evrostos::backends::detail::ProcessStatus::launch_error);
    const auto expired = evrostos::backends::detail::run_process(
        {EVROSTOS_PROCESS_HELPER, "environment"}, workspace.path,
        std::chrono::steady_clock::now(), 8192);
    EXPECT_EQ(expired.status, evrostos::backends::detail::ProcessStatus::timed_out);
}

// Pipe allocation remains correct when the caller starts with fds 0, 1, and 2
// closed. The fixture verifies this through a nested process invocation.
TEST_F(Process, RestoresClosedStandardDescriptorsInTheChild) {
    const auto result = run({"closed_stdio"});
    EXPECT_EQ(result.exit_code, 0) << result.diagnostic << result.err;
}

// Ignoring SIGCHLD lets the OS reap the child before the runner can collect it.
// The nested fixture checks that loss of child ownership becomes a launch error.
TEST_F(Process, ReportsUnsupportedExternalReaping) {
    const auto result = run({"ignored_sigchld"});
    EXPECT_EQ(result.exit_code, 0) << result.diagnostic << result.err;
}

// Timeout stops a silent child and a descendant whose leader has already exited.
// Waiting past the descendant's planned write checks that it left no marker.
TEST_F(Process, TimesOutAndKillsDescendantsAfterLeaderExit) {
    EXPECT_EQ(run({"sleep"}, std::chrono::milliseconds(100)).status,
              evrostos::backends::detail::ProcessStatus::timed_out);
    const auto marker = workspace.path / "survivor";
    EXPECT_EQ(run({"descendant", marker.string()}, std::chrono::milliseconds(100)).status,
              evrostos::backends::detail::ProcessStatus::timed_out);
    std::this_thread::sleep_for(std::chrono::milliseconds(600));
    EXPECT_FALSE(std::filesystem::exists(marker));
}

// Concurrent launches retain their own argument values, streams, and exit status.
TEST_F(Process, HandlesConcurrentCalls) {
    std::vector<std::future<evrostos::backends::detail::ProcessResult>> tasks;
    for (int i = 0; i < 8; ++i) {
        tasks.push_back(std::async(std::launch::async, [&, i] {
            return run({"arguments", std::to_string(i)});
        }));
    }
    for (std::size_t i = 0; i < tasks.size(); ++i) {
        const auto result = tasks[i].get();
        EXPECT_EQ(result.exit_code, 0) << result.diagnostic;
        EXPECT_EQ(result.out, std::to_string(i) + "\n");
    }
}

// Repeated short runs exercise process-group setup and cleanup under rapid exit,
// a regression for competing parent and child setpgid calls on macOS.
TEST_F(Process, RepeatedLaunchesEstablishTheirProcessGroups) {
    for (int i = 0; i < 100; ++i) {
        const auto result = run({"arguments", "ready"});
        ASSERT_EQ(result.status, evrostos::backends::detail::ProcessStatus::exited)
            << "launch " << i << ": " << result.diagnostic;
        ASSERT_EQ(result.exit_code, 0) << "launch " << i;
        EXPECT_EQ(result.out, "ready\n");
    }
}

} // namespace
