// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include <filesystem>
#include <fstream>
#include <stdexcept>
#include <string>
#include <stdlib.h>
#include <unistd.h>

// Own a freshly created directory beneath the test's build directory. Backend
// workspaces are retained inside it until fixture teardown removes the test tree.
class TestWorkspace {
public:
    TestWorkspace() {
        auto pattern = (std::filesystem::current_path() / "spin-test-XXXXXX").string();
        if (::mkdtemp(pattern.data()) == nullptr) {
            throw std::runtime_error("cannot create test workspace");
        }
        path = pattern;
    }
    ~TestWorkspace() {
        // The error-code overload keeps filesystem cleanup errors from replacing
        // the test's diagnostic. Removal stays within this fixture's directory.
        std::error_code error;
        std::filesystem::remove_all(path, error);
    }
    TestWorkspace(const TestWorkspace&) = delete;
    TestWorkspace& operator=(const TestWorkspace&) = delete;
    // Materialize fixture text before launching a tool and return its absolute path.
    std::filesystem::path write(const std::string& name, const std::string& text) const {
        const auto file = path / name;
        std::ofstream output(file, std::ios::binary);
        output << text;
        output.close();
        if (!output) {
            throw std::runtime_error("cannot write test fixture");
        }
        return file;
    }
    std::filesystem::path path;
};
