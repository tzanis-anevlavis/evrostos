"""Exercise the compiled internal core via a private, test-only DAG bridge."""

import itertools
import json
from pathlib import Path
import random
import subprocess
import sys
import unittest

from reference import Lasso, VALUES, evaluate_ltl, evaluate_rltl, freeze, rank_of
from test_semantics import RULES, ROOT, compose_templates, fixture


DRIVER = None


def run_core(source, mode="translate"):
    completed = subprocess.run([str(DRIVER), mode], input=source, text=True,
                               encoding="utf-8", capture_output=True, check=True, timeout=10)
    if completed.stderr:
        raise AssertionError(completed.stderr)
    return json.loads(completed.stdout)


def source_of(ast):
    if isinstance(ast, str):
        return ast
    if len(ast) == 2:
        return f"({ast[0]} {source_of(ast[1])})"
    return f"({source_of(ast[1])} {ast[0]} {source_of(ast[2])})"


class CompiledCoreTests(unittest.TestCase):
    def translated(self, source):
        result = run_core(source)
        self.assertTrue(result["ok"], result)
        nodes = []
        for index, node in enumerate(result["nodes"]):
            op = node["op"]
            if op == "atom":
                self.assertEqual(set(node), {"op", "atom"})
                self.assertIsInstance(node["atom"], str)
                nodes.append(node["atom"])
                continue
            self.assertIn(op, ("!", "X", "F", "G", "&", "|", "->", "U", "R"))
            fields = ("left",) if op in ("!", "X", "F", "G") else ("left", "right")
            self.assertEqual(set(node), {"op", *fields})
            for field in fields:
                self.assertGreaterEqual(node[field], 0)
                self.assertLess(node[field], index, "DAG must have children before parents")
            nodes.append((op, *(nodes[node[field]] for field in fields)))
        self.assertEqual(len(result["roots"]), 4)
        for root in result["roots"]:
            self.assertGreaterEqual(root, 0)
            self.assertLess(root, len(nodes))
        return tuple(nodes[root] for root in result["roots"])

    def compare_trace(self, rltl, translated, prefix, cycle):
        direct = evaluate_rltl(rltl, prefix, cycle)
        states = prefix + cycle
        environment = {p: tuple(p in state for state in states) for p in ("p", "q", "r")}
        values = [evaluate_ltl(ast, environment, Lasso(len(states), len(prefix))) for ast in translated]
        self.assertEqual(tuple(rank_of(row) for row in zip(*values)), direct)

    def test_legacy_parser_acceptance_and_grouping(self):
        for case in fixture("parser_cases.json")["accept"]:
            with self.subTest(source=case["source"]):
                result = run_core(case["source"], "parse")
                self.assertTrue(result["ok"], result)
                self.assertEqual(result["ast"], case["ast"])

    def test_invalid_input_returns_structured_diagnostics(self):
        rejected = fixture("parser_cases.json")["reject"] + ["p\x00", "p\v", "π", "rU p", "rR", "rF"]
        for source in rejected:
            for mode in ("parse", "translate"):
                with self.subTest(source=source, mode=mode):
                    result = run_core(source, mode)
                    self.assertFalse(result["ok"])
                    error = result["error"]
                    self.assertEqual(error["code"], "syntax_error")
                    self.assertTrue(error["message"])
                    self.assertGreaterEqual(error["offset"], 0)
                    self.assertLessEqual(error["offset"], len(source.encode("utf-8")))
                    self.assertGreaterEqual(error["line"], 1)
                    self.assertGreaterEqual(error["column"], 1)
        self.assertEqual(run_core("p &\r\n  )")["error"]["offset"], 7)
        self.assertEqual(run_core("p &\r\n  )")["error"]["line"], 2)
        self.assertEqual(run_core("p &\r\n  )")["error"]["column"], 3)

    def test_all_published_rules_match_fixtures(self):
        for rule in RULES.values():
            if rule["arity"] == 0:
                ast = "p"
            elif rule["arity"] == 1:
                ast = [rule["operator"], ["rG", "p"]]
            else:
                ast = [rule["operator"], ["rG", "p"], ["rG", "q"]]
            with self.subTest(operator=rule["operator"]):
                self.assertEqual(self.translated(source_of(ast)), tuple(freeze(t) for t in compose_templates(ast)))

    def test_named_trace_cases(self):
        for case in fixture("trace_cases.json")["cases"]:
            with self.subTest(case=case["id"]):
                parsed = run_core(case["source"], "parse")
                self.assertTrue(parsed["ok"], parsed)
                self.assertEqual(parsed["ast"], case["ast"])
                translated = self.translated(case["source"])
                self.compare_trace(case["ast"], translated, case["prefix"], case["cycle"])
                direct = evaluate_rltl(case["ast"], case["prefix"], case["cycle"])
                self.assertEqual(VALUES[direct[0]], case["expected"])

    def test_generated_formulas_on_infinite_lassos(self):
        rng = random.Random(20261006)
        unary = ("!", "rX", "rF", "rG")
        binary = ("&", "|", "=>", "rU", "rR")

        def formula(depth):
            if depth == 0 or rng.random() < 0.2:
                return rng.choice(("p", "q"))
            if rng.random() < 0.45:
                return [rng.choice(unary), formula(depth - 1)]
            return [rng.choice(binary), formula(depth - 1), formula(depth - 1)]

        alphabet = ([], ["p"], ["q"], ["p", "q"])
        for _ in range(80):
            ast = formula(4)
            source = source_of(ast)
            with self.subTest(source=source):
                parsed = run_core(source, "parse")
                self.assertTrue(parsed["ok"], parsed)
                self.assertEqual(parsed["ast"], ast)
                translated = self.translated(source)
                # Exhaustive two-position traces, with and without a prefix.
                for states in itertools.product(alphabet, repeat=2):
                    self.compare_trace(ast, translated, [], list(states))
                    self.compare_trace(ast, translated, [states[0]], [states[1]])
                for _ in range(8):
                    prefix = [rng.choice(alphabet) for _ in range(rng.randrange(4))]
                    cycle = [rng.choice(alphabet) for _ in range(rng.randrange(1, 5))]
                    self.compare_trace(ast, translated, prefix, cycle)

    def test_historical_and_instructional_formulas_translate(self):
        inputs = {case["input"] for case in fixture("historical_cases.json")["cases"]}
        inputs.update(("case_studies/instructional_example/inputfileNuSMV.txt",
                       "case_studies/instructional_example/inputfileSPIN.txt"))
        for path in sorted(inputs):
            lines = (ROOT / path).read_text(encoding="utf-8").splitlines()
            start = next(i for i, line in enumerate(lines) if line.strip().lower() == "rltlspecs:")
            count = int(lines[start + 1])
            for source in lines[start + 2:start + 2 + count]:
                with self.subTest(path=path, source=source):
                    # Parse/translate only: this does not reverify model-checking results.
                    self.translated(source)

    def test_resource_limits_and_shared_dag(self):
        for source, code in (("p" * (1024 * 1024 + 1), "input_limit"),
                             ("!" * 256 + "p", "depth_limit"),
                             ("(" * 256 + "p" + ")" * 256, "depth_limit"),
                             (" & ".join(["p"] * 257), "depth_limit")):
            with self.subTest(code=code, length=len(source)):
                result = run_core(source)
                self.assertFalse(result["ok"])
                self.assertEqual(result["error"]["code"], code)
        source = "rG p"
        for _ in range(100):
            source = f"({source}) => (rG q)"
        result = run_core(source)
        self.assertTrue(result["ok"], result)
        self.assertLess(len(result["nodes"]), 1000)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        raise SystemExit("Pass the CMake-built translation_test_driver path")
    DRIVER = Path(sys.argv[1]).resolve(strict=True)
    unittest.main(argv=[sys.argv[0]], verbosity=2)
