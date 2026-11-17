// SPDX-License-Identifier: GPL-3.0-or-later
#include "evrostos.hpp"

#include <gtest/gtest.h>

#include <algorithm>
#include <array>
#include <future>
#include <stdexcept>
#include <vector>

namespace {

constexpr std::array values{
    evrostos::RobustValue::b0000, evrostos::RobustValue::b0001,
    evrostos::RobustValue::b0011, evrostos::RobustValue::b0111,
    evrostos::RobustValue::b1111};
constexpr std::array<std::string_view, 5> bit_strings{"0000", "0001", "0011", "0111", "1111"};

// The public value representation has exactly the five ordered spellings used
// by the semantics. An invalid enum cast must not print a legal truth value.
TEST(Verification, FormatsRobustValues) {
    for (std::size_t i = 0; i < values.size(); ++i) {
        EXPECT_EQ(evrostos::to_string(values[i]), bit_strings[i]);
    }
    EXPECT_THROW((void)evrostos::to_string(static_cast<evrostos::RobustValue>(-1)), std::invalid_argument);
}

// Check every pair against the declared chain, including reflexivity and both
// operand orders. Meet and join must select the corresponding chain elements.
TEST(Verification, UsesTheExplicitTruthValueOrder) {
    for (std::size_t left = 0; left < values.size(); ++left) {
        for (std::size_t right = 0; right < values.size(); ++right) {
            SCOPED_TRACE(std::string(bit_strings[left]) + ", " + std::string(bit_strings[right]));
            EXPECT_EQ(evrostos::less_equal(values[left], values[right]), left <= right);
            EXPECT_EQ(evrostos::meet(values[left], values[right]), values[std::min(left, right)]);
            EXPECT_EQ(evrostos::join(values[left], values[right]), values[std::max(left, right)]);
        }
    }
}

// Exhaust the finite carrier to check the bounded distributive lattice laws:
// identities, idempotence, commutativity, absorption, associativity, and distribution.
TEST(Verification, MeetAndJoinSatisfyLatticeLaws) {
    for (const auto a : values) {
        EXPECT_EQ(evrostos::meet(a, evrostos::RobustValue::b1111), a);
        EXPECT_EQ(evrostos::join(a, evrostos::RobustValue::b0000), a);
        EXPECT_EQ(evrostos::meet(a, evrostos::RobustValue::b0000), evrostos::RobustValue::b0000);
        EXPECT_EQ(evrostos::join(a, evrostos::RobustValue::b1111), evrostos::RobustValue::b1111);
        EXPECT_EQ(evrostos::meet(a, a), a);
        EXPECT_EQ(evrostos::join(a, a), a);
        for (const auto b : values) {
            EXPECT_EQ(evrostos::meet(a, b), evrostos::meet(b, a));
            EXPECT_EQ(evrostos::join(a, b), evrostos::join(b, a));
            EXPECT_EQ(evrostos::meet(a, evrostos::join(a, b)), a);
            EXPECT_EQ(evrostos::join(a, evrostos::meet(a, b)), a);
            for (const auto c : values) {
                EXPECT_EQ(evrostos::meet(a, evrostos::meet(b, c)),
                          evrostos::meet(evrostos::meet(a, b), c));
                EXPECT_EQ(evrostos::join(a, evrostos::join(b, c)),
                          evrostos::join(evrostos::join(a, b), c));
                EXPECT_EQ(evrostos::meet(a, evrostos::join(b, c)),
                          evrostos::join(evrostos::meet(a, b), evrostos::meet(a, c)));
                EXPECT_EQ(evrostos::join(a, evrostos::meet(b, c)),
                          evrostos::meet(evrostos::join(a, b), evrostos::join(a, c)));
            }
        }
    }
}

// Invalid casts are outside the algebra and must be rejected in either operand,
// including comparisons where the two invalid operands would otherwise be equal.
TEST(Verification, AlgebraRejectsInvalidElements) {
    const auto invalid = static_cast<evrostos::RobustValue>(-1);
    for (const auto value : values) {
        EXPECT_THROW((void)evrostos::less_equal(invalid, value), std::invalid_argument);
        EXPECT_THROW((void)evrostos::less_equal(value, invalid), std::invalid_argument);
        EXPECT_THROW((void)evrostos::meet(invalid, value), std::invalid_argument);
        EXPECT_THROW((void)evrostos::meet(value, invalid), std::invalid_argument);
        EXPECT_THROW((void)evrostos::join(invalid, value), std::invalid_argument);
        EXPECT_THROW((void)evrostos::join(value, invalid), std::invalid_argument);
    }
    EXPECT_THROW((void)evrostos::less_equal(invalid, invalid), std::invalid_argument);
    EXPECT_THROW((void)evrostos::meet(invalid, invalid), std::invalid_argument);
    EXPECT_THROW((void)evrostos::join(invalid, invalid), std::invalid_argument);
}

// Each legal value is recovered in reverse bit order. The first false bit stops
// checking; unqueried stronger bits remain absent from the recorded outcomes.
TEST(Verification, RecoversEveryValueWithEarlyTermination) {
    constexpr std::array<std::size_t, 5> counts{1, 2, 3, 4, 4};
    for (std::size_t value = 0; value < values.size(); ++value) {
        SCOPED_TRACE(bit_strings[value]);
        std::vector<std::size_t> order;
        const auto checked = evrostos::Evrostos{}.verify("rG p",
            [&](std::size_t bit, const evrostos::SharedPtrLtlNode& property) {
                EXPECT_NE(property, nullptr);
                order.push_back(bit);
                return (bit_strings[value][bit] == '1')
                    ? evrostos::backends::CheckOutcome::holds
                    : evrostos::backends::CheckOutcome::violated;
            });
        ASSERT_TRUE(std::holds_alternative<evrostos::Verification>(checked));
        const auto& result = std::get<evrostos::Verification>(checked);
        EXPECT_EQ(result.exact_value(), values[value]);
        ASSERT_EQ(order.size(), counts[value]);
        for (std::size_t i = 0; i < order.size(); ++i) {
            EXPECT_EQ(order[i], 3 - i);
        }
        for (std::size_t bit = 0; bit < 4; ++bit) {
            EXPECT_EQ(result.outcomes[bit].has_value(), bit >= (4 - counts[value]));
        }
    }
}

// Enumerate all 4^4 scripted outcome sequences. Independently filter the five
// legal values by the evidence actually requested, then compare both bounds.
// This covers every position and combination of incomplete checks and errors.
TEST(Verification, ExhaustiveOutcomeSequencesMatchEvidenceBounds) {
    constexpr std::array outcomes{
        evrostos::backends::CheckOutcome::holds, evrostos::backends::CheckOutcome::violated,
        evrostos::backends::CheckOutcome::inconclusive, evrostos::backends::CheckOutcome::execution_error};
    for (unsigned int code = 0; code < 256; ++code) {
        SCOPED_TRACE(code);
        auto digits = code;
        std::array<evrostos::backends::CheckOutcome, 4> script;
        for (auto& outcome : script) {
            outcome = outcomes[digits % 4];
            digits /= 4;
        }
        std::vector<std::size_t> requested;
        const auto checked = evrostos::Evrostos{}.verify("rG p",
            [&](std::size_t bit, const evrostos::SharedPtrLtlNode&) {
                requested.push_back(bit);
                return script[bit];
            });
        ASSERT_TRUE(std::holds_alternative<evrostos::Verification>(checked));
        const auto& result = std::get<evrostos::Verification>(checked);
        std::vector<evrostos::RobustValue> possible;
        for (std::size_t candidate = 0; candidate < values.size(); ++candidate) {
            bool matches = true;
            for (const auto bit : requested) {
                if (script[bit] == evrostos::backends::CheckOutcome::holds) {
                    matches = matches && (bit_strings[candidate][bit] == '1');
                } else if (script[bit] == evrostos::backends::CheckOutcome::violated) {
                    matches = matches && (bit_strings[candidate][bit] == '0');
                }
            }
            if (matches) {
                possible.push_back(values[candidate]);
            }
        }
        ASSERT_FALSE(possible.empty());
        EXPECT_EQ(result.lower_bound, possible.front());
        EXPECT_EQ(result.upper_bound, possible.back());
        EXPECT_EQ(result.exact_value().has_value(), possible.size() == 1);
        std::vector<std::size_t> expected_order;
        for (std::size_t remaining = 4; remaining > 0; --remaining) {
            expected_order.push_back(remaining - 1);
            if (script[remaining - 1] == evrostos::backends::CheckOutcome::violated) {
                break;
            }
        }
        EXPECT_EQ(requested, expected_order);
        for (std::size_t bit = 0; bit < 4; ++bit) {
            if (std::find(requested.begin(), requested.end(), bit) != requested.end()) {
                EXPECT_EQ(result.outcomes[bit], script[bit]);
            } else {
                EXPECT_FALSE(result.outcomes[bit].has_value());
            }
        }
    }
}

// A proof of b3 resolves an earlier error on b4. A counterexample to b2 then
// fixes 0011 without checking b1; the original error remains visible.
TEST(Verification, LaterEvidenceCanDetermineAnExactValueAfterFailure) {
    const auto checked = evrostos::Evrostos{}.verify("rG p",
        [](std::size_t bit, const evrostos::SharedPtrLtlNode&) {
            if (bit == 3) {
                return evrostos::backends::CheckOutcome::execution_error;
            }
            return (bit == 2) ? evrostos::backends::CheckOutcome::holds
                              : evrostos::backends::CheckOutcome::violated;
        });
    const auto& result = std::get<evrostos::Verification>(checked);
    EXPECT_EQ(result.exact_value(), evrostos::RobustValue::b0011);
    EXPECT_EQ(result.outcomes[3], evrostos::backends::CheckOutcome::execution_error);
    EXPECT_FALSE(result.outcomes[0].has_value());
}

// A failed weak query followed by a false stronger query narrows the value but
// cannot distinguish 0000 from 0001. No false bit is inferred from the timeout.
TEST(Verification, UnresolvedWeakerBitPreventsAnExactValue) {
    const auto checked = evrostos::Evrostos{}.verify("rG p",
        [](std::size_t bit, const evrostos::SharedPtrLtlNode&) {
            return (bit == 3) ? evrostos::backends::CheckOutcome::inconclusive
                              : evrostos::backends::CheckOutcome::violated;
        });
    const auto& result = std::get<evrostos::Verification>(checked);
    EXPECT_EQ(result.lower_bound, evrostos::RobustValue::b0000);
    EXPECT_EQ(result.upper_bound, evrostos::RobustValue::b0001);
    EXPECT_FALSE(result.exact_value().has_value());
}

// All four roots for an atom share ownership within one translation. Retaining
// them also checks that callback arguments stay valid after verify returns.
TEST(Verification, TranslatesOnceAndPassesPositiveRoots) {
    std::vector<evrostos::SharedPtrLtlNode> roots;
    const auto checked = evrostos::Evrostos{}.verify("p",
        [&](std::size_t, const evrostos::SharedPtrLtlNode& property) {
            roots.push_back(property);
            return evrostos::backends::CheckOutcome::holds;
        });
    EXPECT_EQ(std::get<evrostos::Verification>(checked).exact_value(), evrostos::RobustValue::b1111);
    ASSERT_EQ(roots.size(), 4);
    for (const auto& root : roots) {
        EXPECT_EQ(root, roots.front());
        EXPECT_EQ(root->op(), evrostos::LtlOperator::atom);
        EXPECT_EQ(root->atom(), "p");
    }
}

// The callback index identifies the corresponding translated root, independently
// of reverse query order. Robust always yields G p, F G p, G F p, and F p.
TEST(Verification, PassesTheRootForEachBitIndex) {
    constexpr std::array outer{
        evrostos::LtlOperator::always, evrostos::LtlOperator::eventually,
        evrostos::LtlOperator::always, evrostos::LtlOperator::eventually};
    constexpr std::array inner{
        evrostos::LtlOperator::atom, evrostos::LtlOperator::always,
        evrostos::LtlOperator::eventually, evrostos::LtlOperator::atom};
    const auto checked = evrostos::Evrostos{}.verify("rG p",
        [&](std::size_t bit, const evrostos::SharedPtrLtlNode& property) {
            EXPECT_EQ(property->op(), outer[bit]);
            EXPECT_EQ(property->left()->op(), inner[bit]);
            return evrostos::backends::CheckOutcome::holds;
        });
    EXPECT_EQ(std::get<evrostos::Verification>(checked).exact_value(), evrostos::RobustValue::b1111);
}

// Syntax and translation-budget diagnostics are returned before any callback,
// preserving the translator's source locations and diagnostic codes.
TEST(Verification, TranslationFailuresInvokeNoChecker) {
    std::size_t calls = 0;
    const evrostos::LtlChecker checker = [&](std::size_t, const evrostos::SharedPtrLtlNode&) {
        ++calls;
        return evrostos::backends::CheckOutcome::holds;
    };
    const auto syntax = evrostos::Evrostos{}.verify("p &", checker);
    ASSERT_TRUE(std::holds_alternative<evrostos::Diagnostic>(syntax));
    EXPECT_EQ(std::get<evrostos::Diagnostic>(syntax).code, evrostos::DiagnosticCode::syntax_error);
    EXPECT_EQ(std::get<evrostos::Diagnostic>(syntax).offset, 3);
    const auto limited = evrostos::Evrostos{}.verify("rG p", checker, {.max_input_bytes = 1});
    ASSERT_TRUE(std::holds_alternative<evrostos::Diagnostic>(limited));
    EXPECT_EQ(std::get<evrostos::Diagnostic>(limited).code, evrostos::DiagnosticCode::input_limit);
    EXPECT_EQ(calls, 0);
}

// An empty checker is an invalid API argument, even when the formula also has
// a syntax error or exceeds a translation budget.
TEST(Verification, RejectsEmptyCheckerBeforeTranslation) {
    EXPECT_THROW((void)evrostos::Evrostos{}.verify("p", {}), std::invalid_argument);
    EXPECT_THROW((void)evrostos::Evrostos{}.verify("p &", {}), std::invalid_argument);
    EXPECT_THROW((void)evrostos::Evrostos{}.verify("rG p", {}, {.max_input_bytes = 1}),
                 std::invalid_argument);
}

// Invalid callback outcomes and callback exceptions stay programming or
// allocation failures rather than becoming evidence about a formula.
TEST(Verification, RejectsInvalidCallbacksAndPropagatesExceptions) {
    EXPECT_THROW((void)evrostos::Evrostos{}.verify("p",
        [](std::size_t, const evrostos::SharedPtrLtlNode&) {
            return static_cast<evrostos::backends::CheckOutcome>(99);
        }), std::invalid_argument);
    EXPECT_THROW((void)evrostos::Evrostos{}.verify("p",
        [](std::size_t, const evrostos::SharedPtrLtlNode&) -> evrostos::backends::CheckOutcome {
            throw std::runtime_error("checker exception");
        }), std::runtime_error);
}

// Scheduling state belongs to each invocation, even when one core instance is
// shared by concurrent callers using different checker evidence.
TEST(Verification, ConcurrentCallsHaveIndependentEvidence) {
    const evrostos::Evrostos core;
    std::vector<std::future<evrostos::VerificationResult>> tasks;
    for (std::size_t value = 0; value < values.size(); ++value) {
        tasks.push_back(std::async(std::launch::async, [&, value] {
            return core.verify("rG p", [value](std::size_t bit, const evrostos::SharedPtrLtlNode&) {
                return (bit_strings[value][bit] == '1') ? evrostos::backends::CheckOutcome::holds
                                                       : evrostos::backends::CheckOutcome::violated;
            });
        }));
    }
    for (std::size_t value = 0; value < values.size(); ++value) {
        EXPECT_EQ(std::get<evrostos::Verification>(tasks[value].get()).exact_value(), values[value]);
    }
}

} // namespace
