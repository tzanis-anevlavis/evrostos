# Verify rLTL formulas on a model

Build with at least one native backend enabled. The command uses the pinned
checker from that build. CMake fetches the header-only nlohmann/json 3.12.0
release for the CLI and verifies its SHA-256 checksum. For an offline build,
set `FETCHCONTENT_SOURCE_DIR_NLOHMANN_JSON` to the extracted release directory.
The [NuSMV](nusmv.md#build) and [SPIN](spin.md#build) build instructions list the
checker dependencies.

```sh
cmake -S . -B build/verify -DEVROSTOS_ENABLE_NUSMV=ON -DEVROSTOS_ENABLE_SPIN=ON \
  -DFLEX_EXECUTABLE="$(brew --prefix flex)/bin/flex"
cmake --build build/verify --target evrostos_cli --parallel
build/verify/bin/evrostos verify --backend nusmv --model model.smv --formula 'rG p'
build/verify/bin/evrostos verify --backend spin --model model.pml --formula 'rG p' --output spin.json
build/verify/bin/evrostos verify --backend nusmv --model model.smv --formula-list formulas.txt
```

Use `--formula` for one rLTL expression or `--formula-list` to check several
independently on the same model. Relative paths resolve from
the current directory. `--timeout-s N` gives each attempted LTL bit check its
own deadline, including replay. It accepts whole seconds from 1 to 86400 and
defaults to 300 seconds (5 minutes) per bit. Backend-only builds can set
`EVROSTOS_BUILD_CLI=OFF`.

## Formula lists

A `.txt` list contains one formula per line. Blank lines are ignored.
For example, the following list checks three properties:

```text
rG p
rF p
!p
```

The equivalent `.json` input is `["rG p", "rF p", "!p"]`. A list must
contain at least one entry. The CLI validates the file structure before
running the checker. It then checks each entry in order, including duplicates.
A syntax error in one formula produces a diagnostic for that entry, and
verification continues with the next one.

## Console and exit status

| Exit | Result | Example first line |
| --- | --- | --- |
| 0 | Exact robust value | `Exact value: 0011` |
| 1 | Several robust values remain possible | `Inconclusive: {0000, 0001}` |
| 2 | Input, invocation, or report error | `Error: model is not a regular file: model.smv` |

Every exact value, including `0000`, exits with 0. For an inconclusive run,
the console identifies each unresolved check and its reason. When a witness
exists, the console names its bit and the plain-text trace file. The final line gives the saved report path.
Invalid command syntax and report write failures go to standard error.

For a batch, exit 2 takes precedence if any entry has status `error`.
Otherwise the CLI exits with 1 if any entry is inconclusive, or 0 if all
values are exact.

## JSON report

The default output is `report.json` in the current directory. `--output FILE`
selects another path in an existing directory. A completed write atomically
replaces an existing regular report file. The report includes the backend
and absolute model path. A single-formula report includes the original formula.
A batch report includes the absolute `formula_list` path and an ordered
`results` array. Each entry contains its one-based `index`, original `formula`,
and the same result fields as a single-formula report. A malformed list produces
a top-level diagnostic and an empty `results` array.

| `status` | Result fields |
| --- | --- |
| `exact` | `value` is one of `"0000"`, `"0001"`, `"0011"`, `"0111"`, or `"1111"`. |
| `inconclusive` | `possible_values` lists each remaining value in ascending algebraic order. |
| `error` | `diagnostic.message` explains the problem. Formula diagnostics include the byte offset (zero-based), line and byte column (one-based). |

For a verification result, `checks` lists attempted bits in execution order,
starting with `b4`. Each entry records the bit, backend stage, outcome
(`holds`, `violated`, `inconclusive`, or `execution_error`), and message.
An incomplete or failed check with a command record includes the last command's
stage, exit code, signal, timeout and truncation flags, bounded output streams,
and diagnostic. A later proof can establish an exact value even after a failed
check. The report retains both checks.

A validated violation adds `counterexample.bit` and `counterexample.trace`.
For exact values `0000`, `0001`, `0011`, and `0111`, the witness belongs to
`b4`, `b3`, `b2`, and `b1`, respectively. For example, if `b4` times out and
`b3` is violated, the report contains `["0000", "0001"]`, the `b4` diagnostic,
and the `b3` witness. NuSMV contributes its BDD-validated trace, and SPIN
contributes its readable trail replay. The report embeds this text before the
CLI releases its temporary checker files.

## Plain-text counterexamples

A run with a validated violation writes `counterexamples.txt` beside
`report.json`, including when the result is inconclusive. The file identifies
the rLTL formula and violated bit, followed by the checker's trace with its
original line breaks. Batch traces appear in input order, labeled with the
same formula numbers as the JSON results. On a case-insensitive filesystem,
`REPORT.JSON` and `report.json` both use `counterexamples.txt`. For a custom
report such as `spin.json`, the companion is `spin.json.counterexamples.txt`.
Rerunning the same report replaces its trace
or removes the previous generated trace when the run has no validated violation.

The JSON report uses UTF-8. Invalid byte sequences in filenames and checker output
are displayed as the Unicode replacement character.
