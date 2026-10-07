"""Validate differential-test selection and trace generation without Java."""

import itertools
import unittest

from java_differential import (canonical_compatible, generated, gr_free, operators,
                               outer_compatible, traces, vectors)
from reference import evaluate_rltl


class DifferentialSupportTests(unittest.TestCase):
    def test_fragment_guards_are_recursive(self):
        safe = ("=>", ("rF", "p"), ("rG", "q"))
        outer = ("=>", ("rG", "p"), ("rG", "q"))
        self.assertTrue(canonical_compatible(safe))
        self.assertTrue(canonical_compatible(("rR", safe, "q")))
        self.assertFalse(canonical_compatible(outer))
        self.assertTrue(outer_compatible(outer))
        self.assertFalse(outer_compatible(("!", outer)))
        self.assertFalse(outer_compatible(("=>", outer, "q")))
        self.assertFalse(canonical_compatible(("=>", "p", outer)))
        self.assertFalse(canonical_compatible(("=>", ("rR", "p", "q"), "p")))
        self.assertTrue(gr_free(("rU", "p", "q")))

    def test_generators_obey_profiles_and_are_repeatable(self):
        for profile in ("general", "boolean", "compatible", "no_implication", "optimized"):
            formulas = generated(123, 50, profile)
            self.assertEqual(formulas, generated(123, 50, profile))
            self.assertEqual(len(formulas), len(set(formulas)))
            for ast in formulas:
                with self.subTest(profile=profile, ast=ast):
                    if profile in ("boolean", "compatible", "no_implication"):
                        self.assertTrue(canonical_compatible(ast))
                    if profile == "boolean":
                        self.assertTrue(gr_free(ast))
                    if profile == "no_implication":
                        self.assertNotIn("=>", operators(ast))
                    if profile == "optimized":
                        self.assertFalse(operators(ast).intersection(("rU", "rR")))

    def test_exhaustive_trace_shapes_and_truth_values(self):
        corpus = list(traces(("p", "q")))
        self.assertEqual(len(corpus), 4 + 2 * 16 + 3 * 64 + 24)
        shapes = {(len(prefix), len(cycle)) for prefix, cycle in corpus[:228]}
        self.assertEqual(shapes, {(0, 1), (0, 2), (1, 1), (0, 3), (1, 2), (2, 1)})
        pairs = {(evaluate_rltl(("rG", "p"), prefix, cycle)[0],
                  evaluate_rltl(("rG", "q"), prefix, cycle)[0]) for prefix, cycle in corpus}
        self.assertEqual(pairs, set(itertools.product(range(5), repeat=2)))

    def test_all_positions_and_bit_order_are_observable(self):
        self.assertEqual(vectors(("p", "q", "p", "q"), ("p", "q"), [["p"]], [["q"]]),
                         ((True, False, True, False), (False, True, False, True)))


if __name__ == "__main__":
    unittest.main()
