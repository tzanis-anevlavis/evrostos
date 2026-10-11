// SPDX-License-Identifier: GPL-3.0-or-later
#include "process.hpp"

#include <algorithm>
#include <array>
#include <cerrno>
#include <cstring>
#include <fcntl.h>
#include <mutex>
#include <poll.h>
#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>

namespace evrostos::backends::detail {
namespace {

// Own each pipe end separately so early returns and exceptions close open fds.
class Descriptor {
public:
    explicit Descriptor(int value = -1) : value_(value) {}
    ~Descriptor() { close(); }
    Descriptor(const Descriptor&) = delete;
    Descriptor& operator=(const Descriptor&) = delete;
    int get() const { return value_; }
    void close() {
        if (value_ >= 0) {
            ::close(value_);
            value_ = -1;
        }
    }
private:
    int value_;
};

// Serialize descriptor creation and fork within this runner. All descriptors
// are close-on-exec; the parent never changes cwd or the process environment.
std::mutex launch_mutex;

enum class LaunchOperation { process_group, directory, input, output, error, input_flags, executable };

// A fixed-size record carries setup failures across fork without allocating or
// formatting text in the child. The parent adds the operation name and message.
struct LaunchFailure {
    LaunchOperation operation;
    int error;
};

const char* operation_name(LaunchOperation operation) {
    switch (operation) {
        case LaunchOperation::process_group: return "setpgid";
        case LaunchOperation::directory: return "chdir";
        case LaunchOperation::input: return "dup2(stdin)";
        case LaunchOperation::output: return "dup2(stdout)";
        case LaunchOperation::error: return "dup2(stderr)";
        case LaunchOperation::input_flags: return "fcntl(stdin)";
        case LaunchOperation::executable: return "execve";
    }
    return "unknown launch operation";
}

[[noreturn]] void child_failure(int descriptor, LaunchOperation operation) {
    // Preserve errno before reporting it; _exit avoids inherited C++ destructors
    // and stdio flushing in a child that never reached exec.
    const LaunchFailure failure{operation, errno};
    while ((::write(descriptor, &failure, sizeof(failure)) < 0) && (errno == EINTR)) {
    }
    ::_exit(127);
}

bool make_pipe(std::array<int, 2>& descriptors) {
    if (::pipe(descriptors.data()) != 0) {
        return false;
    }
    // Keep private descriptors above stdio even when the caller closed stdio.
    for (auto& descriptor : descriptors) {
        if (descriptor < 3) {
            const int replacement = ::fcntl(descriptor, F_DUPFD, 3);
            if (replacement < 0) {
                ::close(descriptors[0]);
                ::close(descriptors[1]);
                return false;
            }
            ::close(descriptor);
            descriptor = replacement;
        }
    }
    // These pipe descriptors close on exec; their standard-stream duplicates
    // remain available to the tool. Unused write ends cannot keep a pipe open.
    if ((::fcntl(descriptors[0], F_SETFD, FD_CLOEXEC) < 0)
        || (::fcntl(descriptors[1], F_SETFD, FD_CLOEXEC) < 0)) {
        ::close(descriptors[0]);
        ::close(descriptors[1]);
        return false;
    }
    return true;
}

} // namespace

ProcessResult run_process(const std::vector<std::string>& argv,
                          const std::filesystem::path& directory,
                          std::chrono::steady_clock::time_point deadline,
                          std::size_t output_limit) {
    ProcessResult result;
    if (argv.empty() || !std::filesystem::path(argv[0]).is_absolute()) {
        result.diagnostic = "executable path must be absolute";
        return result;
    }
    if (std::chrono::steady_clock::now() >= deadline) {
        result.status = ProcessStatus::timed_out;
        return result;
    }
    // Prepare the null-terminated argv array before fork. execve accepts mutable
    // pointers but does not modify these strings; their storage remains owned here.
    std::vector<char*> arguments;
    for (const auto& argument : argv) {
        if (argument.find('\0') != std::string::npos) {
            result.diagnostic = "process argument contains NUL";
            return result;
        }
        arguments.push_back(const_cast<char*>(argument.c_str()));
    }
    arguments.push_back(nullptr);
    // Fix locale and tool search paths for reproducible checker diagnostics.
    auto path = std::to_array("PATH=/usr/bin:/bin");
    auto locale = std::to_array("LC_ALL=C");
    const auto cwd = directory.string();
    if (!directory.is_absolute() || (cwd.find('\0') != std::string::npos)) {
        result.diagnostic = "working directory must be absolute and contain no NUL";
        return result;
    }
    // Compiler scratch files belong to the same per-call workspace.
    std::string temporary_directory = "TMPDIR=" + cwd;
    std::array<char*, 4> environment{
        path.data(), locale.data(), temporary_directory.data(), nullptr};
    std::unique_lock lock(launch_mutex);
    std::array<int, 2> output_pipe;
    if (!make_pipe(output_pipe)) {
        result.diagnostic = std::strerror(errno);
        return result;
    }
    Descriptor output_read(output_pipe[0]), output_write(output_pipe[1]);
    std::array<int, 2> error_pipe;
    if (!make_pipe(error_pipe)) {
        result.diagnostic = std::strerror(errno);
        return result;
    }
    Descriptor error_read(error_pipe[0]), error_write(error_pipe[1]);
    // A close-on-exec side channel distinguishes setup/exec failure from a
    // successfully launched program that chooses to exit with status 127.
    std::array<int, 2> launch_pipe;
    if (!make_pipe(launch_pipe)) {
        result.diagnostic = std::strerror(errno);
        return result;
    }
    Descriptor launch_read(launch_pipe[0]), launch_write(launch_pipe[1]);
    Descriptor input(::open("/dev/null", O_RDONLY | O_CLOEXEC));
    // Nonblocking reads let the parent drain both streams and check the deadline
    // even when a child writes to just one stream or holds a pipe open silently.
    if ((input.get() < 0)
        || (::fcntl(output_read.get(), F_SETFL, O_NONBLOCK) < 0)
        || (::fcntl(error_read.get(), F_SETFL, O_NONBLOCK) < 0)
        || (::fcntl(launch_read.get(), F_SETFL, O_NONBLOCK) < 0)) {
        result.diagnostic = std::strerror(errno);
        return result;
    }
    const pid_t child = ::fork();
    if (child < 0) {
        result.diagnostic = std::strerror(errno);
        return result;
    }
    if (child == 0) {
        // Only async-signal-safe operations between fork and exec.
        if (::setpgid(0, 0) != 0) {
            child_failure(launch_write.get(), LaunchOperation::process_group);
        }
        if (::chdir(cwd.c_str()) != 0) {
            child_failure(launch_write.get(), LaunchOperation::directory);
        }
        if (::dup2(input.get(), STDIN_FILENO) < 0) {
            child_failure(launch_write.get(), LaunchOperation::input);
        }
        if (::dup2(output_write.get(), STDOUT_FILENO) < 0) {
            child_failure(launch_write.get(), LaunchOperation::output);
        }
        if (::dup2(error_write.get(), STDERR_FILENO) < 0) {
            child_failure(launch_write.get(), LaunchOperation::error);
        }
        // dup2 leaves flags unchanged when /dev/null already occupies fd 0.
        // Clear close-on-exec explicitly so the child retains its EOF input.
        if (::fcntl(STDIN_FILENO, F_SETFD, 0) < 0) {
            child_failure(launch_write.get(), LaunchOperation::input_flags);
        }
        ::execve(arguments[0], arguments.data(), environment.data());
        child_failure(launch_write.get(), LaunchOperation::executable);
    }
    // The child establishes its group before exec. A second setpgid in the
    // parent can race with that operation on macOS.
    output_write.close();
    error_write.close();
    launch_write.close();
    input.close();
    // Only launch setup is serialized; output capture and waiting run concurrently.
    lock.unlock();

    // Exited means waitid observed termination; the leader remains unreaped until
    // group cleanup. child_owned becomes false if another reaper released its PID.
    bool exited = false;
    bool child_owned = true;
    bool stopped = false;
    int status = 0;
    result.status = ProcessStatus::exited;
    const auto stop = [&](ProcessStatus reason) {
        result.status = reason;
        stopped = true;
        // Kill the leader first: it may not have established its group yet.
        // Once the leader is stopped it cannot create further descendants.
        if (child_owned) {
            (void)::kill(child, SIGKILL);
            (void)::kill(-child, SIGKILL);
        }
    };
    const auto drain = [&](Descriptor& descriptor, std::string& text) {
        std::array<char, 8192> buffer;
        // Bound work per iteration so continuously noisy children cannot delay
        // the deadline check indefinitely.
        for (int i = 0; (i < 16) && (descriptor.get() >= 0); ++i) {
            const auto count = ::read(descriptor.get(), buffer.data(), buffer.size());
            if (count > 0) {
                const auto size = static_cast<std::size_t>(count);
                // One budget covers both streams, including the retained prefix
                // of the read that first exceeds the limit.
                const auto remaining = output_limit - result.out.size() - result.err.size();
                text.append(buffer.data(), std::min(size, remaining));
                if (size > remaining) {
                    stop(ProcessStatus::output_limit);
                    return;
                }
            } else if (count == 0) {
                descriptor.close();
            } else if (errno != EINTR) {
                // EAGAIN means this pipe is drained for now; EOF closes it above.
                if ((errno != EAGAIN) && (errno != EWOULDBLOCK)) {
                    result.diagnostic = std::strerror(errno);
                    stop(ProcessStatus::launch_error);
                }
                return;
            }
        }
    };
    // Reap and kill on exceptions as well (e.g. capture allocation failure).
    try {
        // Leader termination and output EOF are separate: descendants may still
        // hold the write ends after the leader exits.
        while (!exited || (output_read.get() >= 0) || (error_read.get() >= 0)) {
            if (stopped) {
                break;
            }
            if (std::chrono::steady_clock::now() >= deadline) {
                stop(ProcessStatus::timed_out);
            }
            if (stopped) {
                break;
            }
            drain(output_read, result.out);
            if (!stopped) {
                drain(error_read, result.err);
            }
            if (!exited) {
                // Keep the leader waitable until group cleanup, preventing its
                // PID from being reused while descendants still hold pipes.
                siginfo_t information{};
                const auto waited = ::waitid(P_PID, static_cast<id_t>(child), &information,
                                              WEXITED | WNOHANG | WNOWAIT);
                if ((waited == 0) && (information.si_pid == child)) {
                    exited = true;
                } else if ((waited < 0) && (errno != EINTR)) {
                    if (errno == ECHILD) {
                        // An external reaper or SIGCHLD=SIG_IGN has released the
                        // PID; it is no longer safe to signal that identifier.
                        child_owned = false;
                    }
                    result.diagnostic = std::strerror(errno);
                    stop(ProcessStatus::launch_error);
                }
            }
            // A short poll bounds the delay before checking process state and
            // deadline again. Closed descriptors use -1 and are ignored by poll.
            std::array<pollfd, 2> fds{{{output_read.get(), POLLIN, 0}, {error_read.get(), POLLIN, 0}}};
            (void)::poll(fds.data(), fds.size(), 10);
        }
    } catch (...) {
        if (child_owned) {
            (void)::kill(child, SIGKILL);
            (void)::kill(-child, SIGKILL);
            while ((::waitpid(child, &status, 0) < 0) && (errno == EINTR)) {
            }
        }
        throw;
    }
    // A completed leader must not leave background descendants behind.
    if (child_owned) {
        (void)::kill(-child, SIGKILL);
        while ((::waitpid(child, &status, 0) < 0) && (errno == EINTR)) {
        }
    }
    // Successful exec closed this pipe without a record. A record identifies a
    // setup failure; otherwise cancellation takes precedence over the exit status.
    LaunchFailure failure{};
    ssize_t failure_bytes;
    do {
        failure_bytes = ::read(launch_read.get(), &failure, sizeof(failure));
    } while ((failure_bytes < 0) && (errno == EINTR));
    if (failure_bytes == static_cast<ssize_t>(sizeof(failure))) {
        result.status = ProcessStatus::launch_error;
        result.diagnostic = std::string(operation_name(failure.operation)) + ": " + std::strerror(failure.error);
    } else if (!stopped) {
        if (WIFEXITED(status)) {
            result.exit_code = WEXITSTATUS(status);
        } else if (WIFSIGNALED(status)) {
            result.status = ProcessStatus::signaled;
            result.signal = WTERMSIG(status);
        }
    }
    return result;
}

} // namespace evrostos::backends::detail
