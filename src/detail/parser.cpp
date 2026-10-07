// SPDX-License-Identifier: GPL-3.0-or-later
#include "parser.hpp"

#include <stdexcept>

namespace evrostos::detail {
namespace {

enum class TokenKind {
    end, atom, left_paren, right_paren, negation, conjunction, disjunction,
    implication, next, eventually, always, until, release
};

struct Token {
    TokenKind kind;
    std::string_view text;
    std::size_t offset;
};

bool letter(char c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); }
bool digit(char c) { return c >= '0' && c <= '9'; }
bool space(char c) { return c == ' ' || c == '\t' || c == '\r' || c == '\n'; }

// Preserve the JavaCC grammar: implication binds MORE tightly than conjunction.
int precedence(TokenKind kind) {
    switch (kind) {
    case TokenKind::disjunction: return 1;
    case TokenKind::conjunction: return 2;
    case TokenKind::implication: return 3;
    case TokenKind::until: case TokenKind::release: return 4;
    default: return 0;
    }
}

RltlOperator operation(TokenKind kind) {
    switch (kind) {
    case TokenKind::negation: return RltlOperator::negation;
    case TokenKind::conjunction: return RltlOperator::conjunction;
    case TokenKind::disjunction: return RltlOperator::disjunction;
    case TokenKind::implication: return RltlOperator::implication;
    case TokenKind::next: return RltlOperator::next;
    case TokenKind::eventually: return RltlOperator::eventually;
    case TokenKind::always: return RltlOperator::always;
    case TokenKind::until: return RltlOperator::until;
    case TokenKind::release: return RltlOperator::release;
    default: throw std::logic_error("token is not an rLTL operator");
    }
}

class Parser {
public:
    Parser(std::string_view source, const TranslationLimits& limits)
        : source_(source), limits_(limits) { advance(); }

    Rltl run() {
        auto result = expression(1, 0);
        if (token_.kind != TokenKind::end) {
            syntax("expected end of formula", token_.offset);
        }
        return result;
    }

private:
    [[noreturn]] void syntax(std::string message, std::size_t offset) const {
        fail(DiagnosticCode::syntax_error, std::move(message), source_, offset);
    }

    void advance() {
        while (cursor_ < source_.size() && space(source_[cursor_])) {
            ++cursor_;
        }
        const auto start = cursor_;
        if (start == source_.size()) {
            token_ = {TokenKind::end, {}, start};
            return;
        }
        const char c = source_[cursor_++];
        TokenKind kind;
        if (letter(c)) {
            while (cursor_ < source_.size() && (letter(source_[cursor_]) || digit(source_[cursor_]))) {
                ++cursor_;
            }
            const auto word = source_.substr(start, cursor_ - start);
            kind = TokenKind::atom;
            if (word == "rX") {
                kind = TokenKind::next;
            } else if (word == "rF") {
                kind = TokenKind::eventually;
            } else if (word == "rG") {
                kind = TokenKind::always;
            } else if (word == "rU") {
                kind = TokenKind::until;
            } else if (word == "rR") {
                kind = TokenKind::release;
            }
        } else {
            switch (c) {
            case '(': kind = TokenKind::left_paren; break;
            case ')': kind = TokenKind::right_paren; break;
            case '!': kind = TokenKind::negation; break;
            case '&': kind = TokenKind::conjunction; break;
            case '|': kind = TokenKind::disjunction; break;
            case '=':
                if (cursor_ == source_.size() || source_[cursor_] != '>') {
                    syntax("expected '=>'", start);
                }
                ++cursor_;
                kind = TokenKind::implication;
                break;
            default: syntax("unexpected character in formula", start);
            }
        }
        token_ = {kind, source_.substr(start, cursor_ - start), start};
    }

    Rltl node(RltlOperator op, std::size_t offset, Rltl left = {}, Rltl right = {}, std::string atom = {}) {
        const auto height = 1 + std::max(left ? left->height : 0, right ? right->height : 0);
        if (height > max_formula_depth) {
            fail(DiagnosticCode::depth_limit, "formula AST depth exceeds 256", source_, offset);
        }
        if (nodes_ >= limits_.max_parse_nodes) {
            fail(DiagnosticCode::node_limit, "rLTL node limit exceeded", source_, offset);
        }
        ++nodes_;
        return std::make_shared<const RltlNode>(RltlNode{
            op, std::move(atom), std::move(left), std::move(right), offset, height});
    }

    Rltl expression(int minimum, std::size_t nesting) {
        auto left = unary(nesting);
        while (precedence(token_.kind) >= minimum) {
            const auto op = token_;
            advance();
            // Higher minimum on the right makes every binary operator left-associative.
            auto right = expression(precedence(op.kind) + 1, nesting);
            left = node(operation(op.kind), op.offset, std::move(left), std::move(right));
        }
        return left;
    }

    Rltl unary(std::size_t nesting) {
        if (nesting >= max_formula_depth) {
            fail(DiagnosticCode::depth_limit, "formula nesting exceeds 256", source_, token_.offset);
        }
        const auto token = token_;
        switch (token.kind) {
        case TokenKind::atom:
            advance();
            return node(RltlOperator::atom, token.offset, {}, {}, std::string(token.text));
        case TokenKind::negation: case TokenKind::next:
        case TokenKind::eventually: case TokenKind::always: {
            advance();
            auto operand = unary(nesting + 1);
            return node(operation(token.kind), token.offset, std::move(operand));
        }
        case TokenKind::left_paren: {
            advance();
            auto result = expression(1, nesting + 1);
            if (token_.kind != TokenKind::right_paren) {
                syntax("expected ')'", token_.offset);
            }
            advance();
            return result;
        }
        default: syntax("expected an atom, unary operator, or '('", token.offset);
        }
    }

    std::string_view source_;
    const TranslationLimits& limits_;
    std::size_t cursor_ = 0;
    std::size_t nodes_ = 0;
    Token token_{};
};

} // namespace

Rltl parse(std::string_view source, const TranslationLimits& limits) {
    if (source.size() > limits.max_input_bytes) {
        fail(DiagnosticCode::input_limit, "formula input byte limit exceeded", source, limits.max_input_bytes);
    }
    return Parser(source, limits).run();
}

} // namespace evrostos::detail
