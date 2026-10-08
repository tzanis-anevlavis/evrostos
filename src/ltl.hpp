// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include <memory>
#include <string>
#include <utility>

namespace evrostos {

namespace detail { class LtlBuilder; }

enum class LtlOperator {
    atom, negation, conjunction, disjunction, implication,
    next, eventually, always, until, release
};

class LtlNode;
using SharedPtrLtlNode = std::shared_ptr<const LtlNode>;

// Immutable, shared nodes. Atom nodes have no children, unary nodes have only
// a left child, and binary nodes have both. Only atoms have a nonempty name.
class LtlNode final {
public:
    [[nodiscard]] LtlOperator op() const noexcept { return op_; }
    [[nodiscard]] const std::string& atom() const noexcept { return atom_; }
    [[nodiscard]] const SharedPtrLtlNode& left() const noexcept { return left_; }
    [[nodiscard]] const SharedPtrLtlNode& right() const noexcept { return right_; }

private:
    friend class detail::LtlBuilder;
    LtlNode(LtlOperator op, std::string atom, SharedPtrLtlNode left, SharedPtrLtlNode right)
        : op_(op), atom_(std::move(atom)), left_(std::move(left)), right_(std::move(right)) {}

    const LtlOperator op_;
    const std::string atom_;
    const SharedPtrLtlNode left_;
    const SharedPtrLtlNode right_;
};

} // namespace evrostos
