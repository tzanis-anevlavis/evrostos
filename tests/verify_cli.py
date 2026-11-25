# SPDX-License-Identifier: GPL-3.0-or-later
"""Exercise CLI reports through the selected build's pinned checker."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from concurrent.futures import ThreadPoolExecutor

CLI, BACKEND = sys.argv[1:]
CLI = str(Path(CLI).resolve())
VALUES = ["0000", "0001", "0011", "0111", "1111"]


def model_source(initial, successor):
    if BACKEND == "nusmv":
        return (f"MODULE main\nVAR p : boolean;\nASSIGN init(p) := {initial};\n"
                f"next(p) := {successor};\n")
    return f"bool p = {initial.lower()}; init {{ do :: p = {successor.lower()} od }}\n"


class VerifyCli(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.root = Path(directory.name)
        self.model = self.root / ('state "α".smv' if BACKEND == "nusmv" else 'state "α".pml')
        self.report = self.root / "report.json"
        self.companion = self.root / "counterexamples.txt"

    def run_cli(self, formula="rG p", *extra):
        return subprocess.run([CLI, "verify", "--backend", BACKEND, "--model", self.model.name,
                               "--formula", formula, *extra], cwd=self.root,
                              text=True, capture_output=True, timeout=60)

    def read_report(self, path=None):
        return json.loads((path or self.report).read_text(encoding="utf-8"))

    def run_list(self, path, *extra):
        return subprocess.run([CLI, "verify", "--backend", BACKEND, "--model", str(self.model),
                               "--formula-list", str(path), *extra], cwd=self.root,
                              text=True, capture_output=True, timeout=90)

    def test_ordered_batch_and_numbered_witnesses(self):
        self.model.write_text(model_source("FALSE", "FALSE"))
        formulas = ["p", "p &", "!p", "p"]
        for name in ("requirements.txt", "requirements.json"):
            with self.subTest(name=name):
                path = self.root / name
                if path.suffix == ".txt":
                    path.write_bytes(("\r\n \t\r\n".join(formulas) + "\r\n").encode())
                else:
                    path.write_text(json.dumps(formulas))
                run = self.run_list(path)
                self.assertEqual(run.returncode, 2, run.stderr + run.stdout)
                report = self.read_report()
                self.assertEqual(report["status"], "error")
                self.assertEqual(Path(report["formula_list"]).resolve(), path.resolve())
                results = report["results"]
                self.assertEqual([r["index"] for r in results], [1, 2, 3, 4])
                self.assertEqual([r["formula"] for r in results], formulas)
                self.assertEqual([r.get("value") for r in results], ["0000", None, "1111", "0000"])
                self.assertIn("line", results[1]["diagnostic"])
                self.assertNotIn("counterexample", results[2])
                traces = self.companion.read_bytes()
                for index in (1, 4):
                    witness = results[index - 1]["counterexample"]
                    section = f"Formula {index}: p\nViolated bit: b4\n\n".encode()
                    self.assertIn(section + witness["trace"].encode(), traces)
                    self.assertIn(f"Formula {index}: p", run.stdout)
                self.assertEqual(traces.count(b"Violated bit:"), 2)
                self.assertLess(traces.index(b"Formula 1:"), traces.index(b"Formula 4:"))

    def test_batch_status_and_input_errors(self):
        path = self.root / "requirements.json"
        self.model.write_text(model_source("TRUE", "TRUE"))
        path.write_text(json.dumps(["p", "rG\np"]))
        self.assertEqual(self.run_list(path).returncode, 0)
        self.assertEqual(self.read_report()["status"], "exact")
        self.assertFalse(self.companion.exists())
        self.model.write_text("broken model\n")
        self.assertEqual(self.run_list(path).returncode, 1)
        report = self.read_report()
        self.assertEqual(report["status"], "inconclusive")
        self.assertTrue(all(r["possible_values"] == VALUES for r in report["results"]))
        for contents in ('["p",', '["p", 2]', '[1e9999]', '"p"', "[]"):
            path.write_text(contents)
            self.assertEqual(self.run_list(path).returncode, 2)
            report = self.read_report()
            self.assertTrue(report["diagnostic"]["message"])
            self.assertEqual(report["results"], [])
        path.write_text('["p"]')
        self.assertEqual(self.run_list(path, "--output", str(path)).returncode, 2)
        self.assertEqual(path.read_text(), '["p"]')
        self.assertIn("exactly one", self.run_cli("p", "--formula-list", str(path)).stderr)
        for name, contents in (("empty.txt", "\r\n \t\n"), ("list.csv", "p"), ("missing.txt", None)):
            other = self.root / name
            if contents is not None:
                other.write_text(contents)
            result = self.run_list(other)
            self.assertEqual(result.returncode, 2)
            self.assertEqual(self.read_report()["results"], [])

    def test_json_list_rejects_literal_nul_before_checking_any_formula(self):
        path = self.root / "requirements.json"
        self.model.write_text(model_source("TRUE", "TRUE"))
        for contents in (b'["p"]\0["q"]', b'["p"]\0', b'["p\0q"]', b'\0["p"]'):
            with self.subTest(contents=contents):
                path.write_bytes(contents)
                self.assertEqual(self.run_list(path).returncode, 2)
                report = self.read_report()
                self.assertEqual(report["status"], "error")
                self.assertEqual(report["results"], [])
                self.assertIn("NUL", report["diagnostic"]["message"])

        # An escaped NUL is valid JSON. Formula validation handles the decoded string.
        path.write_text(json.dumps(["p\0q", "p"]))
        self.run_list(path)
        self.assertEqual([r["formula"] for r in self.read_report()["results"]], ["p\0q", "p"])

    def test_report_name_case_follows_the_output_filesystem(self):
        self.model.write_text(model_source("FALSE", "FALSE"))
        alias = self.root / "REPORT.JSON"
        self.assertEqual(self.run_cli("p", "--output", alias.name).returncode, 0)
        named_companion = self.root / "REPORT.JSON.counterexamples.txt"
        if self.report.exists():
            # The first run used the alternate spelling before either output existed.
            self.assertTrue(alias.samefile(self.report))
            self.assertTrue(self.companion.exists())
            self.assertFalse(named_companion.exists())
            self.assertEqual(self.run_cli("!p").returncode, 0)
            self.assertFalse(self.companion.exists())
            self.assertEqual(self.run_cli("p").returncode, 0)
            self.assertEqual(self.run_cli("!p", "--output", alias.name).returncode, 0)
            self.assertFalse(self.companion.exists())
        else:
            # Distinct report files keep independent witnesses.
            self.assertTrue(named_companion.exists())
            self.assertEqual(self.run_cli("!p").returncode, 0)
            self.assertTrue(named_companion.exists())
            self.assertFalse(self.companion.exists())

    def test_concurrent_runs_publish_matching_outputs(self):
        self.model.write_text(model_source("FALSE", "FALSE"))
        # One process produces a trace while the other has an exact 1111.
        with ThreadPoolExecutor(max_workers=2) as pool:
            runs = list(pool.map(self.run_cli, ["p", "!p"]))
        self.assertTrue(all(run.returncode == 0 for run in runs))
        last = self.read_report()
        if last["formula"] == "p":
            self.assertIn(last["counterexample"]["trace"].encode(), self.companion.read_bytes())
        else:
            self.assertEqual(last["formula"], "!p")
            self.assertFalse(self.companion.exists())

    def test_exact_values_and_witnesses(self):
        # These five infinite traces distinguish every robust always value.
        for value, initial, successor, witness in zip(
                VALUES, ["FALSE", "TRUE", "FALSE", "FALSE", "TRUE"],
                ["FALSE", "FALSE", "!p", "TRUE", "TRUE"], ["b4", "b3", "b2", "b1", None]):
            with self.subTest(value=value):
                source = model_source(initial, successor)
                self.model.write_text(source)
                result = self.run_cli()
                self.assertEqual(result.returncode, 0, result.stderr + result.stdout)
                report = self.read_report()
                self.assertEqual((report["backend"], report["formula"]), (BACKEND, "rG p"))
                self.assertEqual(Path(report["model"]).resolve(), self.model.resolve())
                self.assertEqual((report["status"], report["value"]), ("exact", value))
                self.assertNotIn("possible_values", report)
                self.assertIn("Exact value: " + value, result.stdout)
                self.assertIn("Report: report.json", result.stdout)
                expected = ["b4", "b3", "b2", "b1"]
                if witness:
                    expected = expected[:expected.index(witness) + 1]
                    self.assertEqual(report["counterexample"]["bit"], witness)
                    self.assertEqual(report["checks"][-1]["outcome"], "violated")
                    marker = "-> State: 1.1 <-" if BACKEND == "nusmv" else "spin: trail ends after"
                    self.assertIn(marker, report["counterexample"]["trace"])
                    self.assertNotIn(marker, result.stdout)
                    self.assertIn("Counterexamples: counterexamples.txt", result.stdout)
                    heading, trace = self.companion.read_bytes().split(b"\n\n", 1)
                    self.assertIn(f"Formula: rG p\nViolated bit: {witness}".encode(), heading)
                    self.assertEqual(trace, report["counterexample"]["trace"].encode())
                else:
                    self.assertNotIn("counterexample", report)
                self.assertEqual([check["bit"] for check in report["checks"]], expected)
                self.assertEqual(self.model.read_text(), source)
                files = {self.model, self.report}
                if witness:
                    files.add(self.companion)
                self.assertEqual(set(self.root.iterdir()), files)

    def test_named_reports_keep_their_own_traces(self):
        self.model.write_text(model_source("FALSE", "FALSE"))
        self.assertEqual(self.run_cli().returncode, 0)
        original = self.companion.read_bytes()
        custom = self.root / "other.json.counterexamples.txt"
        self.assertEqual(self.run_cli("p", "--output", "other.json").returncode, 0)
        self.assertIn("Formula: p\n", custom.read_text())
        self.assertEqual(self.run_cli("p &", "--output", "other.json").returncode, 2)
        self.assertFalse(custom.exists())
        self.assertEqual(self.companion.read_bytes(), original)

    def test_companion_collision_preserves_model_and_existing_files(self):
        self.model.write_text(model_source("FALSE", "FALSE"))
        self.report.write_text("previous report\n")
        self.companion.write_text("user notes\n")
        result = self.run_cli()
        self.assertEqual(result.returncode, 2)
        self.assertEqual(self.companion.read_text(), "user notes\n")
        self.assertEqual(self.report.read_text(), "previous report\n")
        self.companion.unlink()
        self.companion.hardlink_to(self.model)
        self.assertEqual(self.run_cli().returncode, 2)
        self.assertEqual(self.model.read_text(), model_source("FALSE", "FALSE"))

    def test_unresolved_model_errors(self):
        self.model.write_text("broken model\n")
        result = self.run_cli()
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn("Inconclusive: {" + ", ".join(VALUES) + "}", result.stdout)
        report = self.read_report()
        self.assertEqual(report["status"], "inconclusive")
        self.assertEqual(report["possible_values"], VALUES)
        self.assertNotIn("value", report)
        self.assertNotIn("counterexample", report)
        self.assertEqual([c["bit"] for c in report["checks"]], ["b4", "b3", "b2", "b1"])
        for check in report["checks"]:
            self.assertEqual(check["outcome"], "execution_error")
            self.assertTrue(check["message"])
            command = check["command"]
            self.assertTrue(command["stdout"] or command["stderr"] or command["diagnostic"])

    def test_input_and_report_failures(self):
        self.assertEqual(self.run_cli().returncode, 2)
        self.assertEqual(self.read_report()["status"], "error")
        self.model.write_text(model_source("TRUE", "TRUE"))
        alternate = self.root / "custom report.json"
        formula = 'p &\n"'
        result = self.run_cli(formula, "--output", str(alternate))
        self.assertEqual(result.returncode, 2)
        report = self.read_report(alternate)
        self.assertEqual(report["formula"], formula)
        self.assertEqual(report["diagnostic"]["line"], 2)
        self.assertNotIn("value", report)
        self.assertIn(str(alternate), result.stdout)
        # The model must survive aliased output paths, including hard links.
        alias = self.root / "other"
        alias.hardlink_to(self.model)
        for path in (self.model, alias, self.root / "missing" / "report.json"):
            result = self.run_cli("p", "--output", str(path))
            self.assertEqual(result.returncode, 2)
            self.assertNotIn("Report:", result.stdout)
        self.assertEqual(self.model.read_text(), model_source("TRUE", "TRUE"))

    @unittest.skipUnless(BACKEND == "spin", "uses the Promela preprocessor")
    def test_deadline_records_the_reason(self):
        # A header with no writer blocks preprocessing until each bit's deadline.
        os.mkfifo(self.root / "pending.h")
        self.model.write_text('#include "pending.h"\n' + model_source("FALSE", "!p"))
        result = self.run_cli("rG p", "--timeout-s", "1")
        self.assertEqual(result.returncode, 1, result.stderr + result.stdout)
        report = self.read_report()
        self.assertEqual(report["status"], "inconclusive")
        timed = [c for c in report["checks"] if c["outcome"] == "inconclusive"]
        self.assertEqual([check["bit"] for check in timed], ["b4", "b3", "b2", "b1"])
        for check in timed:
            self.assertTrue(check["command"]["timed_out"])
            self.assertIn("deadline", check["message"])
            self.assertIn(check["bit"], result.stdout)

    def test_timeout_seconds(self):
        self.model.write_text(model_source("FALSE", "FALSE"))
        for seconds in ("1", "86400"):
            with self.subTest(seconds=seconds):
                # A syntax diagnostic distinguishes an accepted option from a usage error.
                result = self.run_cli("p &", "--timeout-s", seconds)
                self.assertEqual(result.returncode, 2)
                self.assertIn("Report:", result.stdout)
                self.assertIn("line", self.read_report()["diagnostic"])
        for seconds in ("-1", "0", "1.5", "86401"):
            with self.subTest(seconds=seconds):
                result = self.run_cli("p", "--timeout-s", seconds)
                self.assertEqual(result.returncode, 2)
                self.assertIn("--timeout-s must be an integer", result.stderr)

    def test_usage(self):
        for extra in (("--backend", BACKEND), ("--formula", "q"), ("--output",), ("--timeout-ms", "1"), ("--unknown", "x")):
            self.assertEqual(self.run_cli("p", *extra).returncode, 2)
        self.assertFalse(self.report.exists())


if __name__ == "__main__":
    unittest.main(argv=[sys.argv[0]])
