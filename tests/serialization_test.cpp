// SPDX-License-Identifier: GPL-3.0-or-later
#include "backends/serialization.hpp"
#include "evrostos.hpp"

#include <gtest/gtest.h>

#include <array>
#include <future>
#include <limits>
#include <stdexcept>
#include <string_view>
#include <vector>

namespace {

evrostos::Translation translated(std::string_view source) {
    auto result = evrostos::Evrostos{}.translate(source);
    if (const auto* error = std::get_if<evrostos::Diagnostic>(&result)) {
        throw std::runtime_error(error->message);
    }
    return std::get<evrostos::Translation>(std::move(result));
}

std::string text(evrostos::backends::SerializationResult result) {
    if (const auto* error = std::get_if<evrostos::backends::SerializationDiagnostic>(&result)) {
        throw std::runtime_error(error->message);
    }
    return std::get<std::string>(std::move(result));
}

void expect_error(const evrostos::backends::SerializationResult& result,
                  evrostos::backends::SerializationCode code) {
    const auto* error = std::get_if<evrostos::backends::SerializationDiagnostic>(&result);
    ASSERT_NE(error, nullptr);
    EXPECT_EQ(error->code, code);
    EXPECT_FALSE(error->message.empty());
}

struct OperatorCase {
    const char* name;
    const char* source;
    std::size_t bit;
    const char* nusmv;
    const char* spin;
};

class SerializationOperators : public testing::TestWithParam<OperatorCase> {};

// Each case isolates an LTL operator and checks its token and operand order in
// both dialects. Implication uses bit 4 to isolate a single implication node.
TEST_P(SerializationOperators, PreservesOperatorAndOperands) {
    const auto& example = GetParam();
    const auto formula = translated(example.source).bits[example.bit];
    EXPECT_EQ(text(evrostos::backends::serialize_nusmv(formula)), example.nusmv);
    EXPECT_EQ(text(evrostos::backends::serialize_spin(formula, {.allow_next = true})), example.spin);
}

INSTANTIATE_TEST_SUITE_P(AllOperators, SerializationOperators, testing::Values(
    OperatorCase{"Atom", "p", 0, "p", "p"},
    OperatorCase{"Negation", "!p", 0, "(! p)", "(! p)"},
    OperatorCase{"Conjunction", "p & q", 0, "(p & q)", "(p && q)"},
    OperatorCase{"Disjunction", "p | q", 0, "(p | q)", "(p || q)"},
    OperatorCase{"Implication", "p => q", 3, "(p -> q)", "(p -> q)"},
    OperatorCase{"Next", "rX p", 0, "(X p)", "(X p)"},
    OperatorCase{"Eventually", "rF p", 0, "(F p)", "(<> p)"},
    OperatorCase{"Always", "rG p", 0, "(G p)", "([] p)"},
    OperatorCase{"Until", "p rU q", 0, "(p U q)", "(p U q)"},
    OperatorCase{"Release", "p rR q", 0, "(p V q)", "(p V q)"}
), [](const testing::TestParamInfo<OperatorCase>& info) { return info.param.name; });

// Robust always yields G p, F G p, G F p, and F p in strongest-to-weakest order.
TEST(SerializationStructure, AlwaysPreservesFourBitOrder) {
    const auto translation = translated("rG p");
    const std::array nusmv{"(G p)", "(F (G p))", "(G (F p))", "(F p)"};
    const std::array spin{"([] p)", "(<> ([] p))", "([] (<> p))", "(<> p)"};
    for (std::size_t i = 0; i < 4; ++i) {
        SCOPED_TRACE(i);
        EXPECT_EQ(text(evrostos::backends::serialize_nusmv(translation.bits[i])), nusmv[i]);
        EXPECT_EQ(text(evrostos::backends::serialize_spin(translation.bits[i])), spin[i]);
    }
}

// Release keeps V in bit 1; the weaker bits combine F p with weakened G q.
// Check the grouping and bit order of these compound translations.
TEST(SerializationStructure, ReleasePreservesFourBitOrder) {
    const auto translation = translated("p rR q");
    const std::array nusmv{"(p V q)", "((F p) | (F (G q)))", "((F p) | (G (F q)))", "((F p) | (F q))"};
    const std::array spin{"(p V q)", "((<> p) || (<> ([] q)))", "((<> p) || ([] (<> q)))", "((<> p) || (<> q))"};
    for (std::size_t i = 0; i < 4; ++i) {
        SCOPED_TRACE(i);
        EXPECT_EQ(text(evrostos::backends::serialize_nusmv(translation.bits[i])), nusmv[i]);
        EXPECT_EQ(text(evrostos::backends::serialize_spin(translation.bits[i])), spin[i]);
    }
}

// Stronger implication bits conjoin suffixes shared in the DAG. Serialization
// expands those suffixes and preserves repeated terms even when they are equal.
TEST(SerializationStructure, ImplicationPreservesSharedSuffixesInText) {
    const auto translation = translated("p => q");
    const std::array nusmv{
        "((p -> q) & ((p -> q) & ((p -> q) & (p -> q))))",
        "((p -> q) & ((p -> q) & (p -> q)))", "((p -> q) & (p -> q))", "(p -> q)"};
    const std::array spin{
        "((p -> q) && ((p -> q) && ((p -> q) && (p -> q))))",
        "((p -> q) && ((p -> q) && (p -> q)))", "((p -> q) && (p -> q))", "(p -> q)"};
    for (std::size_t i = 0; i < 4; ++i) {
        SCOPED_TRACE(i);
        EXPECT_EQ(text(evrostos::backends::serialize_nusmv(translation.bits[i])), nusmv[i]);
        EXPECT_EQ(text(evrostos::backends::serialize_spin(translation.bits[i])), spin[i]);
    }
}

// Mixed unary and binary operators retain their AST grouping in either dialect,
// independently of the backend's precedence rules.
TEST(SerializationStructure, ParenthesesPreserveGrouping) {
    const auto formula = translated("!((p | q) & (rF p))").bits[0];
    EXPECT_EQ(text(evrostos::backends::serialize_nusmv(formula)), "(! ((p | q) & (F p)))");
    EXPECT_EQ(text(evrostos::backends::serialize_spin(formula)), "(! ((p || q) && (<> p)))");
}

// Operator-like substrings belong to the atom name; only operator nodes change
// spelling when the dialect changes.
TEST(SerializationIdentifiers, OperatorLettersWithinNamesArePreserved) {
    const auto formula = translated("rG (GFReady2 & rFuture)").bits[0];
    EXPECT_EQ(text(evrostos::backends::serialize_nusmv(formula)), "(G (GFReady2 & rFuture))");
    EXPECT_EQ(text(evrostos::backends::serialize_spin(formula)), "([] (GFReady2 && rFuture))");
}

// The core accepts these names as atoms, but NuSMV would interpret them as
// keywords or constants. Serialization reports invalid_identifier for them.
TEST(SerializationIdentifiers, NuSmvReservedNamesAreRejected) {
    for (const auto* name : {"TRUE", "FALSE", "MODULE", "G", "F", "X", "V", "U",
                             "next", "init", "self", "Word", "typeof", "CONSTARRAY", "READ"}) {
        SCOPED_TRACE(name);
        const auto result = evrostos::backends::serialize_nusmv(translated(name).bits[0]);
        expect_error(result, evrostos::backends::SerializationCode::invalid_identifier);
    }
}

// SPIN's reserved set includes both inline-LTL operators and Promela keywords.
TEST(SerializationIdentifiers, SpinReservedNamesAreRejected) {
    for (const auto* name : {"true", "false", "skip", "timeout", "U", "V", "W", "X", "ltl",
                             "always", "eventually", "release", "next", "empty", "nfull", "proctype"}) {
        SCOPED_TRACE(name);
        expect_error(evrostos::backends::serialize_spin(translated(name).bits[0]),
                     evrostos::backends::SerializationCode::invalid_identifier);
    }
}

// Case changes and suffixes can make a keyword into an identifier. A name
// reserved by one backend can also remain a valid atom in the other.
TEST(SerializationIdentifiers, ValidationIsCaseSensitiveAndDialectSpecific) {
    for (const auto* name : {"True", "TRUE2", "Always", "Module", "G1", "release2"}) {
        const auto formula = translated(name).bits[0];
        EXPECT_EQ(text(evrostos::backends::serialize_nusmv(formula)), name);
        EXPECT_EQ(text(evrostos::backends::serialize_spin(formula)), name);
    }
    EXPECT_EQ(text(evrostos::backends::serialize_nusmv(translated("true").bits[0])), "true");
    EXPECT_EQ(text(evrostos::backends::serialize_spin(translated("TRUE").bits[0])), "TRUE");
    EXPECT_EQ(text(evrostos::backends::serialize_spin(translated("G").bits[0])), "G");
}

// Both backends diagnose a null root before traversal.
TEST(SerializationDiagnostics, NullRootsAreRejected) {
    expect_error(evrostos::backends::serialize_nusmv({}), evrostos::backends::SerializationCode::null_formula);
    expect_error(evrostos::backends::serialize_spin({}), evrostos::backends::SerializationCode::null_formula);
}

// Next requires opt-in even when nested inside another operator or introduced
// into several output bits. Enabling it permits serialization of each bit.
TEST(SerializationDiagnostics, SpinNextRequiresExplicitPermissionAtAnyDepth) {
    for (const auto* source : {"rX p", "rG (p & rX q)", "!((rX p) => q)"}) {
        const auto translation = translated(source);
        for (const auto& bit : translation.bits) {
            expect_error(evrostos::backends::serialize_spin(bit),
                         evrostos::backends::SerializationCode::unsupported_operator);
            EXPECT_FALSE(text(evrostos::backends::serialize_spin(bit, {.allow_next = true})).empty());
        }
    }
}

// For each bit and dialect, the exact emitted length fits the budget, while
// one byte less fails. Parentheses, spaces, and multi-character tokens all count.
TEST(SerializationLimits, ExactByteBoundaryIsAcceptedForEveryOperator) {
    for (const auto* source : {"p", "!p", "p & q", "p | q", "p => q", "rX p", "rF p",
                               "rG p", "p rU q", "p rR q", "(rG p) => (rG q)"}) {
        SCOPED_TRACE(source);
        for (const auto& bit : translated(source).bits) {
            const auto nusmv = text(evrostos::backends::serialize_nusmv(bit));
            const auto spin = text(evrostos::backends::serialize_spin(bit, {.allow_next = true}));
            EXPECT_EQ(text(evrostos::backends::serialize_nusmv(bit, {nusmv.size()})), nusmv);
            EXPECT_EQ(text(evrostos::backends::serialize_spin(bit, {.allow_next = true}, {spin.size()})), spin);
            expect_error(evrostos::backends::serialize_nusmv(bit, {nusmv.size() - 1}),
                         evrostos::backends::SerializationCode::output_limit);
            expect_error(evrostos::backends::serialize_spin(bit, {.allow_next = true}, {spin.size() - 1}),
                         evrostos::backends::SerializationCode::output_limit);
        }
    }
}

// Both occurrences of the shared child contribute to the 15-byte output length.
TEST(SerializationLimits, SharedNodesCountAtEveryTextOccurrence) {
    const auto formula = translated("(!p) & (!p)").bits[0];
    ASSERT_EQ(formula->left(), formula->right());
    EXPECT_EQ(text(evrostos::backends::serialize_nusmv(formula, {15})), "((! p) & (! p))");
    expect_error(evrostos::backends::serialize_nusmv(formula, {14}),
                 evrostos::backends::SerializationCode::output_limit);
}

// A one-character atom remains serializable with a SIZE_MAX budget, exercising
// allocation based on the measured output size.
TEST(SerializationLimits, LargeBudgetDoesNotAllocateUnusedCapacity) {
    const auto formula = translated("p").bits[0];
    const evrostos::backends::SerializationLimits limits{std::numeric_limits<std::size_t>::max()};
    EXPECT_EQ(text(evrostos::backends::serialize_nusmv(formula, limits)), "p");
    EXPECT_EQ(text(evrostos::backends::serialize_spin(formula, {}, limits)), "p");
}

// A compact DAG can expand beyond the configured budget or string capacity.
// Repeated negated implications exercise checked sizing without emitting that text.
TEST(SerializationLimits, ExponentialExpansionIsRejectedBeforeEmissionOrOverflow) {
    std::string source = "p";
    // Negation copies bit 1 into all four bits. The next implication repeats
    // that operand four times in its strongest-bit translation.
    for (int i = 0; i < 40; ++i) {
        source = "!(" + source + " => q)";
    }
    const auto formula = translated(source).bits[0];
    for (const auto budget : {std::size_t{0}, std::size_t{1024 * 1024},
                              std::numeric_limits<std::size_t>::max()}) {
        expect_error(evrostos::backends::serialize_nusmv(formula, {budget}),
                     evrostos::backends::SerializationCode::output_limit);
        expect_error(evrostos::backends::serialize_spin(formula, {}, {budget}),
                     evrostos::backends::SerializationCode::output_limit);
    }
}

// Exercise iterative emission at the parser's depth limit, checking every
// nested negation and its closing parenthesis.
TEST(SerializationStructure, MaximumParserDepthSerializes) {
    const auto formula = translated(std::string(255, '!') + "p").bits[0];
    const std::string expected = [&] {
        std::string result;
        for (int i = 0; i < 255; ++i) {
            result += "(! ";
        }
        return result + "p" + std::string(255, ')');
    }();
    EXPECT_EQ(text(evrostos::backends::serialize_nusmv(formula)), expected);
    EXPECT_EQ(text(evrostos::backends::serialize_spin(formula)), expected);
}

// Concurrent calls recover from a per-call budget failure and serialize both
// dialects. The root's owner count is unchanged after they complete.
TEST(SerializationOwnership, CallsAreIndependentAndDoNotRetainNodes) {
    const auto formula = translated("rG p").bits[0];
    const auto owners = formula.use_count();
    std::vector<std::future<bool>> calls;
    for (int i = 0; i < 16; ++i) {
        calls.push_back(std::async(std::launch::async, [&formula] {
            const auto failed = evrostos::backends::serialize_nusmv(formula, {0});
            return std::holds_alternative<evrostos::backends::SerializationDiagnostic>(failed)
                && (text(evrostos::backends::serialize_nusmv(formula)) == "(G p)")
                && (text(evrostos::backends::serialize_spin(formula)) == "([] p)");
        }));
    }
    for (auto& call : calls) {
        EXPECT_TRUE(call.get());
    }
    EXPECT_EQ(formula.use_count(), owners);
}

} // namespace
