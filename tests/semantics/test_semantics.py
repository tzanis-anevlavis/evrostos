"""Executable contract tests, not Java/C++ or model-checker integration tests."""

import itertools
import json
from pathlib import Path
import random
import re
import unittest

from reference import Lasso, VALUES, bits, evaluate_ltl, evaluate_rltl, implies, rank_of, robust


HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent


def fixture(name):
    return json.loads((HERE / name).read_text(encoding="utf-8"))


RULES = {rule["operator"]: rule for rule in fixture("translation_rules.json")["rules"]}


def compose_templates(ast):
    """Test-only substitution of the JSON rules; no parser or simplifier."""
    if isinstance(ast, str):
        return [ast] * 4
    operator, *children = ast
    operands = [compose_templates(child) for child in children]
    bindings = {f"{name}{i + 1}": bit_ast
                for name, operand in zip("ab", operands)
                for i, bit_ast in enumerate(operand)}

    def substitute(node):
        if isinstance(node, str):
            return bindings[node]
        return [node[0], *(substitute(child) for child in node[1:])]

    return [substitute(template) for template in RULES[operator]["ltl"]]


def staged_result(queries):
    """Legacy weak-to-strong scheduling for complete Boolean query answers."""
    proved = 0
    for answer in reversed(queries):
        if not answer:
            break
        proved += 1
    return proved


class AlgebraTests(unittest.TestCase):
    def test_implication_table_and_residuation(self):
        expected = ((4, 4, 4, 4, 4), (0, 4, 4, 4, 4), (0, 1, 4, 4, 4),
                    (0, 1, 2, 4, 4), (0, 1, 2, 3, 4))
        for a, b in itertools.product(range(5), repeat=2):
            self.assertEqual(implies(a, b), expected[a][b])
            raw = tuple(not x or y for x, y in zip(bits(a), bits(b)))
            self.assertEqual(rank_of(all(raw[i:]) for i in range(4)), expected[a][b])
            violation = any(x and not y for x, y in zip(bits(a), bits(b)))
            self.assertEqual(rank_of(not violation or y for y in bits(b)), expected[a][b])
            if a in (0, 4):
                self.assertEqual(rank_of(raw), expected[a][b])
            for c in range(5):
                self.assertEqual(min(a, c) <= b, c <= implies(a, b))

    def test_negation_is_not_complement_or_involution(self):
        lasso = Lasso(5, 0)
        values = tuple(range(5))
        negated = robust("!", values, None, lasso)
        self.assertEqual(negated, (4, 4, 4, 4, 0))
        self.assertEqual(robust("!", negated, None, lasso), (0, 0, 0, 0, 4))
        with self.assertRaises(ValueError):
            rank_of((True, False, False, False))

    def test_staged_outer_implication_on_two_trace_models(self):
        pairs = tuple(itertools.product(range(5), repeat=2))
        for model in itertools.product(pairs, repeat=2):
            queries = [all(not bits(a)[i] or bits(b)[i] for a, b in model)
                       for i in range(4)]
            self.assertEqual(staged_result(queries), min(implies(a, b) for a, b in model))

    def test_system_aggregation_does_not_commute_with_implication(self):
        model = ((0, 0), (4, 0))
        self.assertEqual(min(implies(a, b) for a, b in model), 0)
        self.assertEqual(implies(min(a for a, _ in model), min(b for _, b in model)), 4)

    def test_nested_legacy_implication_counterexample(self):
        case = next(case for case in fixture("trace_cases.json")["cases"]
                    if case["id"] == "nested-implication-regression")
        a = evaluate_rltl(["rG", "p"], case["prefix"], case["cycle"])[0]
        b = evaluate_rltl(["rG", "q"], case["prefix"], case["cycle"])[0]
        raw = tuple(not x or y for x, y in zip(bits(a), bits(b)))
        self.assertEqual(raw, (True, False, False, False))
        self.assertEqual(staged_result(raw), 0)
        self.assertEqual(tuple(not raw[0] for _ in range(4)), bits(0))
        self.assertEqual(evaluate_rltl(case["ast"], case["prefix"], case["cycle"])[0], 4)

    def test_release_is_not_robust_until_dual(self):
        prefix, cycle = [], [["q"], []]
        release = evaluate_rltl(["rR", "p", "q"], prefix, cycle)[0]
        dual = evaluate_rltl(["!", ["rU", ["!", "p"], ["!", "q"]]], prefix, cycle)[0]
        self.assertEqual(release, 2)
        self.assertEqual(dual, 0)


class TranslationRuleTests(unittest.TestCase):
    def compare_rule(self, rule, left, right, lasso):
        environment = {}
        for name, operand in (("a", left), ("b", right)):
            if operand is not None:
                for i in range(4):
                    environment[f"{name}{i + 1}"] = tuple(bits(value)[i] for value in operand)
        translated = [evaluate_ltl(ast, environment, lasso) for ast in rule["ltl"]]
        actual = tuple(rank_of(v) for v in zip(*translated))
        self.assertEqual(actual, robust(rule["operator"], left, right, lasso))

    def test_complete_operator_inventory_and_atom(self):
        self.assertEqual(set(RULES), {"atom", "!", "&", "|", "=>", "rX", "rF", "rG", "rU", "rR"})
        for rule in RULES.values():
            self.assertEqual(len(rule["ltl"]), 4)
        for loop in (0, 1):
            for p in itertools.product((False, True), repeat=2):
                result = [evaluate_ltl(ast, {"p": p}, Lasso(2, loop))
                          for ast in RULES["atom"]["ltl"]]
                self.assertEqual(tuple(rank_of(v) for v in zip(*result)), tuple(4 if v else 0 for v in p))

    def test_all_two_position_operand_valuations(self):
        # All 25 unary and 625 binary operand valuations, on both graph shapes.
        operands = tuple(itertools.product(range(5), repeat=2))
        for rule in RULES.values():
            if rule["arity"] == 0:
                continue
            for loop in (0, 1):
                for left in operands:
                    for right in operands if rule["arity"] == 2 else (None,):
                        with self.subTest(operator=rule["operator"], loop=loop, left=left, right=right):
                            self.compare_rule(rule, left, right, Lasso(2, loop))

    def test_longer_seeded_operand_lassos(self):
        random_source = random.Random(20261006)
        for rule in RULES.values():
            if rule["arity"] == 0:
                continue
            for _ in range(100):
                size = random_source.randrange(1, 9)
                lasso = Lasso(size, random_source.randrange(size))
                left = tuple(random_source.randrange(5) for _ in range(size))
                right = tuple(random_source.randrange(5) for _ in range(size)) if rule["arity"] == 2 else None
                with self.subTest(operator=rule["operator"], lasso=lasso, left=left, right=right):
                    self.compare_rule(rule, left, right, lasso)

    def test_named_trace_expectations_and_nested_templates(self):
        cases = fixture("trace_cases.json")["cases"]
        self.assertEqual(len({case["id"] for case in cases}), len(cases))
        for case in cases:
            with self.subTest(case=case["id"]):
                prefix, cycle = case["prefix"], case["cycle"]
                direct = evaluate_rltl(case["ast"], prefix, cycle)
                self.assertEqual(VALUES[direct[0]], case["expected"])
                states = prefix + cycle
                environment = {atom: tuple(atom in state for state in states) for atom in ("p", "q")}
                outputs = [evaluate_ltl(ast, environment, Lasso(len(states), len(prefix)))
                           for ast in compose_templates(case["ast"])]
                self.assertEqual(tuple(rank_of(v) for v in zip(*outputs)), direct)

    def test_empty_cycle_rejected(self):
        with self.assertRaises(ValueError):
            evaluate_rltl("p", [["p"]], [])


class HistoricalCorpusTests(unittest.TestCase):
    def test_report_values_and_formula_order_are_transcribed(self):
        cases = fixture("historical_cases.json")["cases"]
        self.assertEqual(sum(len(case["expected"]) for case in cases), 37)
        for case in cases:
            with self.subTest(case=case["id"]):
                for key in ("input", "report", "manifest_model", "replay_model"):
                    self.assertTrue((ROOT / case[key]).is_file(), case[key])
                report = (ROOT / case["report"]).read_text(encoding="utf-8")
                self.assertEqual(re.findall(r"truth value ([01]{4})", report), case["expected"])
                lines = (ROOT / case["input"]).read_text(encoding="utf-8").splitlines()
                section = next(i for i, line in enumerate(lines) if line.strip().lower() == "rltlspecs:")
                count = int(lines[section + 1])
                self.assertEqual(count, len(case["expected"]))
                formulas = lines[section + 2:section + 2 + count]
                reported = re.findall(r"Original rLTL Formula No\s*\d+:\s*\n([^\n]+)", report)
                normalize = lambda formula: re.sub(r"\s+", "", formula)
                self.assertEqual(list(map(normalize, formulas)), list(map(normalize, reported)))
                model_line = next(i for i, line in enumerate(lines) if line.strip().lower() == "model name:")
                self.assertEqual(Path(lines[model_line + 1]).as_posix(), case["manifest_model"])


if __name__ == "__main__":
    unittest.main()
