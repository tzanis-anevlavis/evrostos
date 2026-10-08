# Legacy semantic audit

Source and report audit: 2026-10-06, tracked tree
`44aa61f4fdbca7f8dbeaccb7e227575ecb0aef79`. Reference tests and
[Java/C++ comparisons](../tests/semantics/README.md#java-differential-tests)
check translation. NuSMV/SPIN runs and historical results remain unverified.
The executable validation record below also identifies the untracked JAR tested.

## Operator audit

[`evrostosSource.cpp`](../modules/evrostosSource.cpp) uses the default
[`RLTL2LTLVisitor`](../modules/rltl2ltl/src/org/mpi_sws/rltl/visitors/RLTL2LTLVisitor.java),
not the [`OptimizedRLTL2LTLVisitor`](../modules/rltl2ltl/src/org/mpi_sws/rltl/visitors/OptimizedRLTL2LTLVisitor.java)
selected by `-O`. Here, GR-free means containing neither `rG` nor `rR`.

| Operator | Default visitor | `-O` visitor | C++ behavior |
| --- | --- | --- | --- |
| Atom | Replicated four times | Same | Preserve |
| `&`, `\|` | Pointwise | Same | Preserve |
| `!` | Replicates negation of first bit | Negates conjunction of bits unless GR-free | Preserve robust negation; equivalent on valid inputs |
| `=>` | Pointwise; suffix-conjunction code commented out | Compares operand values unless both are GR-free | Suffix conjunctions for all formulas |
| `rX`, `rF` | Pointwise | Same | Preserve |
| `rG` | `G`, `FG`, `GF`, `F` | Same; marks operand non-GR-free | Preserve |
| `rU` | Pointwise ordinary until | Throws `UnsupportedOperationException` | Preserve |
| `rR` | Four-bit release rule | Throws `UnsupportedOperationException` | Preserve; not a robust-negation dual of until |

Except for implication, the default rules match [rLTL semantics](semantics.md)
when given correct operand bits. Incorrect implication bits can therefore spoil
an otherwise correct enclosing rule.

The optimized visitor uses `D -> Bi`, with `D = OR_k(Ak & !Bk)`. For legal
ordered vectors, `D` means `A > B`: the result is `1111` if false and `B` if
true, implementing robust implication. The `-O` visitor rejects until and release,
including uses in the case studies; its supported operator set is smaller than
the default visitor's.

## Implication: unenforced preconditions

Pointwise implication gives semantic bits in the core efficient fragment. The
extended fragment also allows an unrestricted outermost implication: its raw
queries recover the correct result through weakest-to-strongest checking, but
need not represent semantic bits. See [the conditions and derivation](semantics.md#implication-optimizations).
Neither parser nor wrapper enforces these restrictions, so accepted syntax
exceeds the fragment on which correctness is guaranteed.

The restrictions are sufficient, not necessary. For example,
`!((rG p) => (rG p))` lies outside the fragment yet translates correctly on every
trace: each inner query `Ai -> Ai` is true, and negation returns `0000`.

For a counterexample, let a trace start with neither `p` nor `q`, then have `p`
forever and never `q`. Thus `rG p = 0111` and `rG q = 0000`. Section 4, Remark 1
of the [paper](https://doi.org/10.1145/3491216), pages 8:18-8:19, uses this trace
to distinguish robust implication from first-bit implication. Nesting it exposes
the translation error:

| Formula | rLTL value | Default Java output on this trace |
| --- | --- | --- |
| `(rG p) => (rG q)` | `0000` | Raw query vector `1000` |
| `!((rG p) => (rG q))` | `1111` | `0000` |
| `((rG p) => (rG q)) => q` | `1111` | `0111` |

For a model with this single execution, wrapper scheduling recovers `0000` in
the first row by stopping at the false fourth query. It cannot repair nesting:
negation reads the wrong first bit of `1000`, yielding `0000`; the third formula
applies pointwise implication to `1000` and `0000`, yielding `0111`.

Differential tests assert these values using both Java visitors, C++, and direct
rLTL evaluation. The optimized visitor and C++ agree with rLTL in all three
cases. Wrapper scheduling is simulated; the wrapper is not executed.

## Parser and I/O boundaries

The [JavaCC grammar](../modules/rltl2ltl/src/org/mpi_sws/rltl/parser/LTLParser.jj)
accepts uppercase letters but no numeric truth constants, contrary to parts of
the README. Implication binds more tightly than conjunction, and chains associate
left. Tests compare accepted ASTs and rejected inputs between Java and C++.
C++ returns `depth_limit` above its nesting/AST-height bound; Java accepts the
tested over-bound inputs. Exact diagnostic text need not match.

[`startupRoutine`](../modules/routines.cpp) reads the manifest filename from
stdin, not `argv[1]`. Model paths are relative to the working directory.

The wrapper uses shell commands and fixed files (`rLTLinput.txt`, `LTLoutput.txt`,
`tempmodel.*`, `bitvalue.txt`, `pan`) without validating translator exit status
or four-line output. `checkbit` treats missing/malformed responses as false,
conflating failures with counterexamples. Global character replacement for SPIN
can corrupt identifiers containing `G`, `F`, or `R`. NuSMV queries start at
property index zero, assuming no properties already exist in the model.

Checker communication depends on patches in
[`ltl.c`](../modules/NuSMV-2.6.0/NuSMV/code/nusmv/core/ltl/ltl.c) and
[`pangen1.h`](../modules/Spin/Src/pangen1.h), not a supported upstream protocol.
The SPIN patch exports a character derived from the error count. Search
completion requires separate validation. Adapters need tests for errors,
incomplete searches, property polarity, and resource limits. `modules/ltl_mc`
is an older harness; the application uses `modules/evrostosSource.cpp`.

## Historical corpus

[`historical_cases.json`](../tests/semantics/historical_cases.json) records 37
reported values across AAC original (12), AAC abstract (12), telephone (9),
motivating example (1), and WBS (3). Tests check their transcription, formula
order, and file references without running model checkers.

- AAC abstract reuses the AAC formulas with an explicit abstract-model override.
- The telephone manifest incorrectly names `aac_original.smv`. The candidate
  replacement `telephone.smv` is inferred from the artifacts and needs confirmation.
  The manifest is unchanged.
- Older motivating-example reports contain suffix-conjoined implication strings;
  WBS uses pointwise queries. Use reported truth values for regression checks;
  translation strings, parentheses, formatting, dialect, timings, and generic
  `model.smv` labels vary between reports.
- No expected model-checking results are recorded for the instructional NuSMV
  and SPIN models. RERS is outside the audited corpus.

## Executable validation record

On 2026-10-07, all 22 CTest entries (18 GoogleTest cases and four Python suites)
passed in Debug, Release, and ASan/UBSan builds on macOS ARM64 with AppleClang
21.0.0, Python 3.14.6, GoogleTest 1.18.0, and Corretto JDK 11.0.32.1. The 20
default entries passed without Java using a local GoogleTest source tree.
The downloaded dependency configuration also passed; the library-only build
required no GoogleTest, Java, or Python discovery or downloads.

References were tested separately: classes compiled from unchanged repository
sources, and the untracked `modules/rltl2ltl/rltl2ltl.jar`, SHA-256
`532511d48e2545ecec56b575351a3cb57e58e4cc350b191c66cad899e4d5be19`.
Both runs reported:

- 1,520 parser cases: 787 accepted with identical ASTs and 733 rejected by both
  parsers; 18 additional depth-boundary cases.
- 587 generated/systematic semantic formula cases and 28 historical/instructional
  occurrences: 125,223 formula/trace comparisons with direct rLTL evaluation.
  Formula counts include repeated occurrences across cases.
- 114,656 default-Java four-bit comparisons, 56,406 optimized-Java comparisons,
  and 8,551 outer-implication scheduling comparisons. These overlap the oracle
  comparisons and are non-additive.
- All 25 implication operand pairs and all 625 two-execution combinations,
  three implication-divergence witnesses, and eight detected translation mutations.
- 77 Java command-line invocations, including default/optimized translation,
  malformed-input handling, unsupported operators, stdin/stdout, and file paths
  containing spaces; archive runs used the manifest entry point.

No C++ discrepancy or sanitizer finding was observed. Validation covers the
listed corpus, platform, and translator versions. Equivalence over all infinite
traces, backend parity, other platforms, and other translator versions remain
unverified. See
[reproduction commands and comparison guards](../tests/semantics/README.md#java-differential-tests).

## Migration boundaries

The [C++ core](core.md) supplies translation, immutable ASTs, diagnostics, and
tests through a library API. Integration requires:

1. AST-based NuSMV/SPIN printers and wrapper integration, with application I/O
   separate from the translation API.
2. Adapters for unmodified checkers: discovered/pinned executable versions,
   argument arrays, isolated workspaces, diagnostics, timeouts, and typed outcomes.
   Upgrade and reproduce the corpus one backend at a time.
3. End-to-end tests before replacing the legacy build.

CMake builds the core and tests, not the legacy application or checkers. It
downloads no checkers. The wrapper and vendored backends are unchanged.
