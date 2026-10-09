"""Parse emitted formula text independently and compare it with the input LTL DAG."""

from pathlib import Path
import random
import re
import sys
import unittest

import cpp_integration
from cpp_integration import run_core, source_of
from test_semantics import ROOT, fixture


def parse_formula(text, dialect):
    """Recognize the printers' parenthesized subset, consuming every character."""
    tokens = re.findall(r"[A-Za-z][A-Za-z0-9]*|->|&&|\|\||\[\]|<>|[()!&|]", text)
    if "".join(tokens) != re.sub(r"\s", "", text):
        raise ValueError("unrecognized formula character")
    unary = {"!": "!", "X": "X"}
    unary.update({"F": "F", "G": "G"} if dialect == "nusmv" else {"<>": "F", "[]": "G"})
    binary = {"->": "->", "U": "U", "V": "R"}
    binary.update({"&": "&", "|": "|"} if dialect == "nusmv" else {"&&": "&", "||": "|"})
    position = 0

    def take():
        nonlocal position
        if position == len(tokens):
            raise ValueError("unexpected end of formula")
        token = tokens[position]
        position += 1
        return token

    def expression():
        token = take()
        if token != "(":
            if not re.fullmatch(r"[A-Za-z][A-Za-z0-9]*", token) or token in unary or token in binary:
                raise ValueError("expected atom")
            return token
        if position < len(tokens) and tokens[position] in unary:
            operator = unary[take()]
            result = (operator, expression())
        else:
            left = expression()
            operator = take()
            if operator not in binary:
                raise ValueError("expected binary operator")
            result = (binary[operator], left, expression())
        if take() != ")":
            raise ValueError("expected closing parenthesis")
        return result

    result = expression()
    if position != len(tokens):
        raise ValueError("trailing formula tokens")
    return result


def dag_roots(result):
    nodes = []
    for node in result["nodes"]:
        if node["op"] == "atom":
            nodes.append(node["atom"])
        elif "right" in node:
            nodes.append((node["op"], nodes[node["left"]], nodes[node["right"]]))
        else:
            nodes.append((node["op"], nodes[node["left"]]))
    return tuple(nodes[index] for index in result["roots"])


class SerializationIntegrationTests(unittest.TestCase):
    def assert_round_trip(self, source):
        result = run_core(source)
        self.assertTrue(result["ok"], result)
        expected = dag_roots(result)
        for dialect, mode in (("nusmv", "serialize-nusmv"), ("spin", "serialize-spin-next")):
            with self.subTest(source=source, dialect=dialect):
                serialized = run_core(source, mode)
                self.assertTrue(serialized["ok"], serialized)
                self.assertEqual(len(serialized["formulas"]), 4)
                actual = tuple(parse_formula(text, dialect) for text in serialized["formulas"])
                self.assertEqual(actual, expected)

    def test_named_and_historical_formulas(self):
        sources = [case["source"] for case in fixture("trace_cases.json")["cases"]]
        inputs = {case["input"] for case in fixture("historical_cases.json")["cases"]}
        inputs.update(("case_studies/instructional_example/inputfileNuSMV.txt",
                       "case_studies/instructional_example/inputfileSPIN.txt"))
        for path in sorted(inputs):
            lines = (ROOT / path).read_text(encoding="utf-8").splitlines()
            start = next(i for i, line in enumerate(lines) if line.strip().lower() == "rltlspecs:")
            sources.extend(lines[start + 2:start + 2 + int(lines[start + 1])])
        for source in sources:
            self.assert_round_trip(source)

    def test_generated_nested_formulas(self):
        rng = random.Random(20261008)
        unary = ("!", "rX", "rF", "rG")
        binary = ("&", "|", "=>", "rU", "rR")

        def formula(depth):
            if depth == 0 or rng.random() < 0.2:
                return rng.choice(("p", "q", "GFReady2"))
            if rng.random() < 0.45:
                return (rng.choice(unary), formula(depth - 1))
            return (rng.choice(binary), formula(depth - 1), formula(depth - 1))

        # Every ordered pair of binary operators, on both sides of a parent.
        for outer in binary:
            for inner in binary:
                self.assert_round_trip(f"(p {inner} q) {outer} GFReady2")
                self.assert_round_trip(f"p {outer} (q {inner} GFReady2)")
        for _ in range(100):
            self.assert_round_trip(source_of(formula(4)))

    def test_nusmv_lexer_keywords_are_rejected(self):
        parser = ROOT / "modules/NuSMV-2.6.0/NuSMV/code/nusmv/core/parser"
        names = set()
        for path in parser.glob("input.l.*"):
            names.update(re.findall(r'^"([A-Za-z][A-Za-z0-9]*)"', path.read_text(), re.MULTILINE))
        self.assertGreater(len(names), 90)
        for name in sorted(names):
            with self.subTest(name=name):
                result = run_core(name, "serialize-nusmv")
                self.assertFalse(result["ok"], result)
                self.assertEqual(result["error"]["code"], "invalid_identifier")

    def test_spin_lexer_keywords_are_rejected(self):
        lexer = (ROOT / "modules/Spin/Src/spinlex.c").read_text()
        tables = lexer[lexer.index("} LTL_syms[]"):lexer.index("static int\ncheck_name")]
        names = set(re.findall(r'\{\s*"([A-Za-z][A-Za-z0-9]*)"', tables))
        self.assertGreater(len(names), 60)
        for name in sorted(names):
            with self.subTest(name=name):
                result = run_core(name, "serialize-spin")
                self.assertFalse(result["ok"], result)
                self.assertEqual(result["error"]["code"], "invalid_identifier")

    def test_text_parser_rejects_corrupt_output(self):
        for dialect in ("nusmv", "spin"):
            for text in ("", "(p U)", "p q", "(! p", "(p -> q))", "(p R q)", "p;", "p_1"):
                with self.subTest(dialect=dialect, text=text):
                    with self.assertRaises(ValueError):
                        parse_formula(text, dialect)
        for text, dialect in (("(G p)", "spin"), ("(p & q)", "spin"),
                               ("([] p)", "nusmv"), ("(p && q)", "nusmv")):
            with self.assertRaises(ValueError):
                parse_formula(text, dialect)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        raise SystemExit("Pass the CMake-built translation_test_driver path")
    cpp_integration.DRIVER = Path(sys.argv[1]).resolve(strict=True)
    unittest.main(argv=[sys.argv[0]], verbosity=2)
