// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include "evrostos.hpp"

#include <algorithm>
#include <utility>

namespace evrostos::detail {

enum class RltlOperator {
    atom, negation, conjunction, disjunction, implication,
    next, eventually, always, until, release
};

struct RltlNode;
using Rltl = std::shared_ptr<const RltlNode>;

struct RltlNode {
    const RltlOperator op;
    const std::string atom;
    const Rltl left;
    const Rltl right;
    const std::size_t offset;
    const std::size_t height;
};

// Internal control flow; expected input failures become a public Diagnostic.
struct Failure { Diagnostic diagnostic; };

[[noreturn]] inline void fail(DiagnosticCode code, std::string message,
                              std::string_view source, std::size_t offset) {
    offset = std::min(offset, source.size());
    std::size_t line = 1;
    std::size_t column = 1;
    for (std::size_t i = 0; i < offset; ++i) {
        if (source[i] == '\r') {
            ++line;
            column = 1;
        } else if (source[i] == '\n') {
            if (i == 0 || source[i - 1] != '\r') {
                ++line;
            }
            column = 1;
        } else {
            ++column;
        }
    }
    throw Failure{{code, std::move(message), offset, line, column}};
}

} // namespace evrostos::detail
