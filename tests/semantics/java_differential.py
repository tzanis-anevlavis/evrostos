"""Execute Java/C++ parser and translation comparisons, without model checkers."""

import argparse
import base64
from collections import Counter
from functools import cache
import hashlib
import itertools
import json
import os
from pathlib import Path
import random
import re
import subprocess
import sys
import tempfile
import unittest

import cpp_integration as compiled
from reference import Lasso, VALUES, evaluate_ltl, evaluate_rltl, freeze, rank_of
from test_semantics import ROOT, compose_templates, fixture, staged_result


UNARY = ("!", "rX", "rF", "rG")
BINARY = ("&", "|", "=>", "rU", "rR")
OPTIONS = None
COUNTS = Counter()


def atoms(ast):
    if isinstance(ast, str):
        return {ast}
    return set().union(*(atoms(child) for child in ast[1:]))


def operators(ast):
    if isinstance(ast, str):
        return set()
    return {ast[0]}.union(*(operators(child) for child in ast[1:]))


def gr_free(ast):
    return not operators(ast).intersection(("rG", "rR"))


def canonical_compatible(ast):
    """Sufficient structural guard, applied recursively to every implication."""
    if isinstance(ast, str):
        return True
    return (all(canonical_compatible(child) for child in ast[1:])
            and (ast[0] != "=>" or gr_free(ast[1])))


def outer_compatible(ast):
    return (not isinstance(ast, str) and ast[0] == "=>"
            and all(canonical_compatible(child) for child in ast[1:]))


def generated(seed, count, profile, depth=4):
    rng = random.Random(seed)

    def formula(level, kind):
        if level == 0 or rng.random() < 0.18:
            return rng.choice(("p", "q"))
        unary = ("!", "rX", "rF") if kind == "boolean" else UNARY
        binary = ("&", "|", "=>") if kind in ("boolean", "optimized") else BINARY
        if kind == "no_implication":
            binary = ("&", "|", "rU", "rR")
        if rng.random() < 0.4:
            return (rng.choice(unary), formula(level - 1, kind))
        op = rng.choice(binary)
        left_kind = "boolean" if kind == "compatible" and op == "=>" else kind
        return (op, formula(level - 1, left_kind), formula(level - 1, kind))

    # Deduplicate so coverage counts describe distinct formulas, not draws.
    results = set()
    while len(results) < count:
        results.add(formula(depth, profile))
    return sorted(results, key=repr)


def historical_sources():
    paths = {case["input"] for case in fixture("historical_cases.json")["cases"]}
    paths.update(("case_studies/instructional_example/inputfileNuSMV.txt",
                  "case_studies/instructional_example/inputfileSPIN.txt"))
    result = []
    for path in sorted(paths):
        lines = (ROOT / path).read_text(encoding="utf-8").splitlines()
        start = next(i for i, line in enumerate(lines) if line.strip().lower() == "rltlspecs:")
        count = int(lines[start + 1])
        result.extend((path, i + 1, source) for i, source in enumerate(lines[start + 2:start + 2 + count]))
    return result


def traces(names, seed=3491216):
    """All lassos through three positions for <=2 atoms, plus longer traces."""
    names = sorted(names)
    rng = random.Random(seed)
    if len(names) <= 2:
        alphabet = [tuple(name for i, name in enumerate(names) if mask & (1 << i))
                    for mask in range(1 << len(names))]
        for size in range(1, 4):
            for states in itertools.product(alphabet, repeat=size):
                for loop in range(size):
                    yield list(states[:loop]), list(states[loop:])
    else:
        # Large historical alphabets: targeted constants, singleton/toggle traces.
        for state in ([], names, *([name] for name in names)):
            yield [], [state]
            yield [state], [names if not state else []]
        yield [], [[], names]
    for _ in range(24):
        size = rng.randrange(4, 13)
        loop = rng.randrange(size)
        states = [[name for name in names if rng.getrandbits(1)] for _ in range(size)]
        yield states[:loop], states[loop:]


def vectors(roots, names, prefix, cycle):
    states = prefix + cycle
    environment = {name: tuple(name in state for state in states) for name in names}
    columns = [evaluate_ltl(root, environment, Lasso(len(states), len(prefix))) for root in roots]
    return tuple(zip(*columns))


@cache
def cpp(source, mode="translate"):
    return compiled.run_core(source, mode)


@cache
def cpp_roots(source):
    # Reuse the DAG schema/arity/topological-order checks from the core suite.
    return compiled.CompiledCoreTests().translated(source)


def java_batch(sources, mode="translate"):
    payload = "".join(mode + "\t" + base64.b64encode(source.encode("utf-8")).decode("ascii") + "\n"
                      for source in sources)
    result = subprocess.run(
        [OPTIONS.java, "-Xmx512m", "-Xss8m", "-cp",
         os.pathsep.join((str(OPTIONS.bridge), str(OPTIONS.reference))), "LegacyBridge"],
        input=payload, text=True, encoding="utf-8", capture_output=True, timeout=120)
    if result.returncode:
        raise AssertionError(f"Java bridge exited {result.returncode}:\n{result.stderr}\n{result.stdout[-2000:]}")
    if result.stderr:
        raise AssertionError(result.stderr)
    responses = [json.loads(line) for line in result.stdout.splitlines()]
    if len(responses) != len(sources):
        raise AssertionError(f"Expected {len(sources)} Java responses, got {len(responses)}")
    COUNTS["java_" + mode + "_requests"] += len(sources)
    return responses


def classical_text(ast):
    """The legacy fully parenthesized printer syntax, used only for CLI checks."""
    if isinstance(ast, str):
        return ast
    if len(ast) == 2:
        space = "" if ast[0] == "!" else " "
        return f"({ast[0]}{space}{classical_text(ast[1])})"
    return f"({classical_text(ast[1])} {ast[0]} {classical_text(ast[2])})"


def java_cli():
    command = [OPTIONS.java, "-Xmx512m"]
    if OPTIONS.reference.is_file():
        return command + ["-jar", str(OPTIONS.reference)]
    return command + ["-cp", str(OPTIONS.reference), "org.mpi_sws.rltl.main.RLTL2LTL"]


class JavaDifferentialTests(unittest.TestCase):
    def check_translation(self, source, java, expected_ast=None, require_default=False):
        self.assertTrue(java["ok"], (source, java))
        ast = freeze(java["ast"])
        if expected_ast is not None:
            self.assertEqual(ast, freeze(expected_ast), source)
        parsed = cpp(source, "parse")
        self.assertTrue(parsed["ok"], (source, parsed))
        self.assertEqual(freeze(parsed["ast"]), ast, source)
        actual = cpp_roots(source)
        default = freeze(java["default"])
        self.assertEqual(len(default), 4)
        supports_optimized = not operators(ast).intersection(("rU", "rR"))
        if supports_optimized:
            self.assertIn("optimized", java, source)
            optimized = freeze(java["optimized"])
            self.assertEqual(len(optimized), 4)
        else:
            self.assertEqual(java.get("optimized_error"), "unsupported_operator", source)
        compatible = canonical_compatible(ast)
        if require_default:
            self.assertTrue(compatible, source)
        names = atoms(ast)
        aggregate_default, aggregate_expected = [True] * 4, 4
        for prefix, cycle in traces(names):
            with self.subTest(source=source, prefix=prefix, cycle=cycle):
                direct = evaluate_rltl(ast, prefix, cycle)
                cpp_values = vectors(actual, names, prefix, cycle)
                self.assertEqual(tuple(rank_of(row) for row in cpp_values), direct)
                java_values = vectors(default, names, prefix, cycle)
                if compatible:
                    self.assertEqual(java_values, cpp_values)
                    COUNTS["default_bitwise_trace_comparisons"] += 1
                elif outer_compatible(ast):
                    self.assertEqual(tuple(staged_result(row) for row in java_values), direct)
                    aggregate_default = [a and b for a, b in zip(aggregate_default, java_values[0])]
                    aggregate_expected = min(aggregate_expected, direct[0])
                    self.assertEqual(staged_result(aggregate_default), aggregate_expected)
                    COUNTS["outer_implication_trace_comparisons"] += 1
                if supports_optimized:
                    self.assertEqual(vectors(optimized, names, prefix, cycle), cpp_values)
                    COUNTS["optimized_trace_comparisons"] += 1
                COUNTS["cpp_oracle_trace_comparisons"] += 1
        return ast

    def check_corpus(self, formulas, require_default=False):
        sources = [compiled.source_of(ast) for ast in formulas]
        for ast, source, java in zip(formulas, sources, java_batch(sources)):
            self.check_translation(source, java, ast, require_default)
        COUNTS["semantic_formula_cases"] += len(formulas)

    def test_bridge_operator_mapping(self):
        formulas = ["p", *((op, "p") for op in UNARY), *((op, "p", "q") for op in BINARY)]
        self.check_corpus(formulas, require_default=True)

    def test_parser_acceptance_grouping_and_mutations(self):
        cases = fixture("parser_cases.json")
        sources = [case["source"] for case in cases["accept"]] + cases["reject"]
        sources += [f"p {a} q {b} r" for a, b in itertools.product(BINARY, repeat=2)]
        sources += [f"{a} {b} p" for a, b in itertools.product(UNARY, repeat=2)]
        sources += [f"{a} p {b} q" for a, b in itertools.product(UNARY, BINARY)]
        for word in ("rX", "rF", "rG", "rU", "rR", "X", "F", "G", "U", "R", "true", "false"):
            sources += [word, word + "p", word + "2", "p" + word, word + " p"]
        for code in range(128):
            sources += [chr(code), "p" + chr(code), "p" + chr(code) + "&q"]
        for character in ("π", "é", "中", "😀", "\u00a0", "\u2003", "\u2028", "\ufeff"):
            sources += [character, "p" + character, character + "p", "p " + character + " q"]
        formulas = generated(1, 120, "general")
        rng = random.Random(2)
        for ast in formulas:
            source = compiled.source_of(ast)
            sources += [source, "\r\n\t" + source + "\r", "((" + source + "))"]
            tokens = re.findall(r"=>|[A-Za-z][A-Za-z0-9]*|[^\s]", source)
            sources.append("\t\r\n".join(tokens))
            position = rng.randrange(len(source))
            sources += [source[:position] + source[position + 1:], source + " p",
                        source[:position] + "$" + source[position:]]
        tokens = ("p", "q", "rG", "rU", "!", "&", "|", "=>", "(", ")")
        sources += [" ".join(rng.choices(tokens, k=rng.randrange(1, 15))) for _ in range(150)]
        sources = list(dict.fromkeys(sources))
        accepted = rejected = 0
        for source, java in zip(sources, java_batch(sources, "parse")):
            with self.subTest(source=source):
                actual = cpp(source, "parse")
                self.assertEqual(actual["ok"], java["ok"], (actual, java))
                if java["ok"]:
                    self.assertEqual(actual["ast"], java["ast"])
                    accepted += 1
                else:
                    self.assertEqual(actual["error"]["code"], "syntax_error")
                    rejected += 1
        self.assertGreater(accepted, 300)
        self.assertGreater(rejected, 300)
        COUNTS.update(parser_acceptances=accepted, parser_rejections=rejected)

    def test_explicit_depth_limit_difference(self):
        sources = []
        for depth in (32, 128, 254, 255, 256, 257):
            sources += ["!" * depth + "p", "(" * depth + "p" + ")" * depth,
                        " & ".join(["p"] * (depth + 1))]
        for source, java in zip(sources, java_batch(sources, "parse")):
            with self.subTest(source=source):
                self.assertTrue(java["ok"])
                result = cpp(source, "parse")
                depth = max(source.count("!"), source.count("("), source.count("&"))
                if depth < 256:
                    self.assertTrue(result["ok"])
                    self.assertEqual(result["ast"], java["ast"])
                else:
                    self.assertFalse(result["ok"])
                    self.assertEqual(result["error"]["code"], "depth_limit")

    def test_default_compatible_formulas(self):
        formulas = generated(3, 100, "no_implication") + generated(4, 100, "compatible")
        formulas += generated(5, 40, "boolean", depth=6)
        # Exercise each operator as both parent and child, in both binary slots.
        children = [(op, "p") for op in UNARY] + [(op, "p", "q") for op in BINARY if op != "=>"]
        for child in children:
            formulas.extend((op, child) for op in UNARY)
            for op in ("&", "|", "rU", "rR"):
                formulas.extend(((op, child, "q"), (op, "q", child)))
        self.check_corpus(formulas, require_default=True)

    def test_unrestricted_and_optimized_formulas(self):
        formulas = generated(6, 100, "general") + generated(7, 100, "optimized")
        self.check_corpus(formulas)

    def test_outer_implication_scheduling(self):
        operands = generated(8, 40, "compatible", depth=3)
        formulas = [("=>", left, right) for left, right in zip(operands, reversed(operands))]
        formulas.append(("=>", ("rG", "p"), ("rG", "q")))
        self.check_corpus(formulas)

    def test_historical_and_instructional_formulas(self):
        records = historical_sources()
        for (path, number, source), java in zip(records, java_batch([record[2] for record in records])):
            with self.subTest(path=path, formula_number=number):
                ast = self.check_translation(source, java)
                self.assertTrue(canonical_compatible(ast) or outer_compatible(ast), source)
        COUNTS["historical_formula_cases"] += len(records)

    def test_expected_implication_divergences(self):
        # Witness: p is false once then always true; q is always false.
        prefix, cycle = [[]], [["p"]]
        sources = ["(rG p) => (rG q)", "!((rG p) => (rG q))",
                   "((rG p) => (rG q)) => q"]
        expected_java = ("1000", "0000", "0111")
        expected_correct = ("0000", "1111", "1111")
        for source, java, old, correct in zip(sources, java_batch(sources), expected_java, expected_correct):
            with self.subTest(source=source):
                self.assertTrue(java["ok"])
                ast = freeze(java["ast"])
                self.assertEqual(VALUES[evaluate_rltl(ast, prefix, cycle)[0]], correct)
                for roots, expected in ((java["default"], old), (java["optimized"], correct),
                                        (cpp_roots(source), correct)):
                    row = vectors(roots, ("p", "q"), prefix, cycle)[0]
                    self.assertEqual("".join(str(int(bit)) for bit in row), expected)
                self.assertNotEqual(old, correct)

    def test_all_five_valued_implication_pairs(self):
        source = "(rG p) => (rG q)"
        java = java_batch([source])[0]
        self.assertTrue(java["ok"])
        roots = cpp_roots(source)
        witnesses = {}
        for prefix, cycle in traces(("p", "q")):
            left = evaluate_rltl(("rG", "p"), prefix, cycle)[0]
            right = evaluate_rltl(("rG", "q"), prefix, cycle)[0]
            witnesses.setdefault((left, right), (prefix, cycle))
        self.assertEqual(set(witnesses), set(itertools.product(range(5), repeat=2)))
        raw_values = {}
        for (left, right), (prefix, cycle) in witnesses.items():
            with self.subTest(left=VALUES[left], right=VALUES[right]):
                expected = 4 if left <= right else right
                actual = vectors(roots, ("p", "q"), prefix, cycle)[0]
                self.assertEqual(rank_of(actual), expected)
                self.assertEqual(vectors(java["optimized"], ("p", "q"), prefix, cycle)[0], actual)
                raw = vectors(java["default"], ("p", "q"), prefix, cycle)[0]
                raw_values[left, right] = raw
                self.assertEqual(staged_result(raw), expected)
        # All two-execution models over the 25 operand pairs, using actual Java output.
        for first, second in itertools.product(witnesses, repeat=2):
            queries = [a and b for a, b in zip(raw_values[first], raw_values[second])]
            expected = min(4 if a <= b else b for a, b in (first, second))
            self.assertEqual(staged_result(queries), expected, (first, second))
        COUNTS["implication_operand_pairs"] += len(witnesses)
        COUNTS["two_execution_models"] += len(witnesses) ** 2

    def test_semantic_comparison_detects_faults(self):
        # Deliberately wrong outputs calibrate the oracle and trace corpus.
        # No production source or reference implementation is modified.
        cases = [
            (("rG", "p"), lambda roots: tuple(reversed(roots))),
            (("rG", "p"), lambda roots: (roots[0], roots[2], roots[1], roots[3])),
            (("!", ("rG", "p")),
             lambda roots: tuple(("!", freeze(bit)) for bit in compose_templates(["rG", "p"]))),
            (("=>", ("rG", "p"), ("rG", "q")),
             lambda roots: tuple(("->", freeze(a), freeze(b)) for a, b in
                                 zip(compose_templates(["rG", "p"]), compose_templates(["rG", "q"])))),
            (("rX", "p"), lambda roots: ("p",) * 4),
            (("rF", "p"), lambda roots: (("G", "p"),) * 4),
            (("rU", "p", "q"), lambda roots: (("U", "q", "p"),) * 4),
            (("rR", "p", "q"), lambda roots: (("R", "p", "q"),) * 4),
        ]
        for ast, mutate in cases:
            with self.subTest(ast=ast):
                correct = cpp_roots(compiled.source_of(ast))
                wrong = mutate(correct)
                detected = False
                for prefix, cycle in traces(atoms(ast)):
                    expected = evaluate_rltl(ast, prefix, cycle)
                    self.assertEqual(tuple(rank_of(row) for row in vectors(correct, atoms(ast), prefix, cycle)),
                                     expected)
                    expected_bits = tuple(tuple(bit == "1" for bit in VALUES[value]) for value in expected)
                    if vectors(wrong, atoms(ast), prefix, cycle) != expected_bits:
                        detected = True
                        break
                self.assertTrue(detected, "Trace corpus failed to detect a deliberate translation fault")
                COUNTS["detected_semantic_mutants"] += 1

    def test_real_java_command_line(self):
        sources = list(dict.fromkeys([record[2] for record in historical_sources()] +
                                    ["p", "GFReady2", "p rU q", "p rR q",
                                     "!((rG p) => (rG q))", "p & q => r"]))
        for source, java in zip(sources, java_batch(sources)):
            for optimized in (False, True):
                command = java_cli() + ["-i"]
                if optimized:
                    command.append("-O")
                result = subprocess.run(command, input=source, text=True, encoding="utf-8",
                                        capture_output=True, timeout=15)
                with self.subTest(source=source, optimized=optimized):
                    self.assertFalse(result.stderr, result.stderr)
                    if optimized and "optimized_error" in java:
                        self.assertNotEqual(result.returncode, 0)
                        self.assertIn("not implemented", result.stdout)
                    else:
                        self.assertEqual(result.returncode, 0, result.stdout)
                        roots = java["optimized" if optimized else "default"]
                        self.assertEqual(result.stdout.splitlines(), [classical_text(root) for root in roots])
                COUNTS["java_cli_invocations"] += 1
        for source in ("", "rG", "p &", "p_q", "p q", "0001", "p\x00"):
            result = subprocess.run(java_cli() + ["-i"], input=source,
                                    text=True, encoding="utf-8", capture_output=True, timeout=15)
            self.assertNotEqual(result.returncode, 0, source)
            self.assertIn("Error:", result.stdout)
            COUNTS["java_cli_invocations"] += 1

    def test_java_file_input_and_output(self):
        source = "!((rG p) => (rG q))"
        java = java_batch([source])[0]
        self.assertTrue(java["ok"])
        # All generated files live in the build tree, not beside legacy sources.
        with tempfile.TemporaryDirectory(prefix="cli paths with spaces ", dir=OPTIONS.bridge.parent) as directory:
            input_path = Path(directory) / "input formula.txt"
            output_path = Path(directory) / "output formulas.txt"
            input_path.write_text(source, encoding="utf-8")
            for optimized in (False, True):
                command = java_cli() + [str(input_path), "-o", str(output_path)]
                if optimized:
                    command.append("-O")
                result = subprocess.run(command, text=True, encoding="utf-8", capture_output=True, timeout=15)
                self.assertEqual(result.returncode, 0, (result.stdout, result.stderr))
                self.assertFalse(result.stdout)
                self.assertFalse(result.stderr)
                roots = java["optimized" if optimized else "default"]
                self.assertEqual(output_path.read_text(encoding="utf-8").splitlines(),
                                 [classical_text(root) for root in roots])
                self.assertEqual(input_path.read_text(encoding="utf-8"), source)
                COUNTS["java_cli_invocations"] += 1


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--driver", required=True, type=Path)
    parser.add_argument("--java", required=True)
    parser.add_argument("--bridge", required=True, type=Path)
    parser.add_argument("--reference", required=True, type=Path)
    OPTIONS = parser.parse_args()
    compiled.DRIVER = OPTIONS.driver.resolve(strict=True)
    OPTIONS.bridge = OPTIONS.bridge.resolve(strict=True)
    OPTIONS.reference = OPTIONS.reference.resolve(strict=True)
    version = subprocess.run([OPTIONS.java, "-version"], capture_output=True, text=True, check=True, timeout=15)
    print(version.stderr.strip(), flush=True)
    print(f"Java reference: {OPTIONS.reference}", flush=True)
    if OPTIONS.reference.is_file():
        print(f"Reference SHA-256: {hashlib.sha256(OPTIONS.reference.read_bytes()).hexdigest()}", flush=True)
    else:
        digest = hashlib.sha256()
        for path in sorted(OPTIONS.reference.rglob("*.class")):
            digest.update(path.relative_to(OPTIONS.reference).as_posix().encode("utf-8") + b"\0")
            digest.update(path.read_bytes())
        print(f"Reference class-tree SHA-256: {digest.hexdigest()}", flush=True)
    result = unittest.main(argv=[sys.argv[0]], verbosity=2, exit=False).result
    print("Coverage counters: " + json.dumps(dict(sorted(COUNTS.items()))), flush=True)
    sys.exit(0 if result.wasSuccessful() else 1)
