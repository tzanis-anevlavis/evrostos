# Semantic and compiled-core tests

From the repository root, run reference and harness tests with Python 3.10+
(standard library only):

```sh
python3 -B -m unittest discover -s tests/semantics -p 'test_*.py' -v
```

Include C++ API and integration tests with C++20 and CMake 3.20+:

```sh
cmake -S . -B build/core-debug -DCMAKE_BUILD_TYPE=Debug
cmake --build build/core-debug --parallel
ctest --test-dir build/core-debug --output-on-failure
```

Neither command sequence uses Java, model checkers, or the legacy wrapper.
CMake downloads GoogleTest as described below; the Python-only command has no
third-party dependencies. `-B` suppresses Python bytecode; build output stays
under ignored `build/`. `BUILD_TESTING=OFF` builds only the library, without
GoogleTest, Python, Java, or downloads; see
[build options](../../docs/core.md#build-and-test-locally).

## C++ unit tests

`core_test.cpp` uses GoogleTest 1.18.0. CMake fetches its source archive with a
pinned SHA-256 and builds `gtest_main`; GoogleMock and installation are disabled.
The dependency is confined to test builds and is not linked into `evrostos_core`.
CTest discovers each unit test separately, with the `core.api.` prefix and `unit`
label. Assertions report the failing source line and actual/expected values.

```sh
ctest --test-dir build/core-debug -L unit --output-on-failure
ctest --test-dir build/core-debug -R 'core.api.TranslationSharing' --output-on-failure
```

For an offline build, supply an already extracted GoogleTest 1.18.0 source tree:

```sh
cmake -S . -B build/core-offline -DCMAKE_BUILD_TYPE=Debug \
  -DFETCHCONTENT_SOURCE_DIR_GOOGLETEST=/absolute/path/to/googletest-1.18.0
cmake --build build/core-offline --parallel
ctest --test-dir build/core-offline --output-on-failure
```

The local-source override bypasses downloading and archive checksum validation.
`translation_test_driver` retains its own `main()` and JSON protocol for the
Python semantic and Java differential suites; it is not a GoogleTest executable.

## What is checked

| Layer | Coverage |
| --- | --- |
| Algebra | All 25 implications, 125 residuation triples, negation, ordered bits |
| Temporal rules | Exhaustive two-position operand sequences and seeded longer lassos; C++ output matches all ten rule templates |
| Nested formulas | 32 trace fixtures; 80 seeded formulas evaluated on 3,200 lassos |
| Early termination | All 625 two-trace outer-implication value combinations |
| Historical reports | 37 transcribed values; historical/instructional formulas parse and translate |
| Parser | Acceptance, rejection, grouping, diagnostics, byte/nesting/node limits |
| C++ API | Ownership, immutable shared nodes, bounded DAG growth, reentrancy |
| Serialization | NuSMV/SPIN operator spellings, identifiers, output bounds, text-to-AST round trips |
| Java (optional) | Parser, both visitors, CLI, semantic comparisons, expected differences |

`reference.py` evaluates rLTL from five-valued algebra and temporal limits,
independently of the translation rules. Its LTL evaluator uses Boolean
least/greatest fixed points. Both operate on lassos: finite prefixes followed
by nonempty, infinitely repeating cycles. These distinguish `FG` from `GF`
and test cycle wraparound.

`translation_rules.json` encodes Section 4, Table 2 (page 8:19) of the
[paper](https://doi.org/10.1145/3491216) as AST templates. Metadata identifies the
verified PDF; running tests does not require it. Tests compare template outputs
with direct rLTL evaluation, substituting templates for nested formulas.

`cpp_integration.py` evaluates compiled C++ output with the same oracle.
Its `translation_test_driver` exports a shared DAG through a private test
interface, preserving node sharing.

`serialization_integration.py` parses emitted NuSMV and SPIN formula text and
compares all four trees with the C++ DAG. It covers named/historical formulas,
binary-operator nesting, and 100 seeded formulas. Lexer keyword checks use the
vendored sources. GoogleTest covers exact spellings, diagnostics, output limits,
and concurrency; see [serialization](../../docs/serialization.md).

## Fixtures

JSON uses `schema_version: 1`. rLTL ASTs encode atoms as strings and operators as
`[operator, operand, ...]`. C++ tests parse `source` and compare the AST.
LTL templates use `!`, `&`, `|`, `->`, `X`, `F`, `G`, `U`, `R`, with
`a1` through `a4` and `b1` through `b4` as operand-bit placeholders.
Trace states list true atoms; all others are false. `expected` is the value at
position zero, while differential checks cover every position. Historical
results are unverified replay targets; the telephone manifest's model mismatch
is recorded in the [audit](../../docs/legacy-audit.md#historical-corpus).

## Java differential tests

Enable Java comparisons with JDK 11+ (`java` and `javac`):

```sh
cmake -S . -B build/core-java -DCMAKE_BUILD_TYPE=Debug \
  -DEVROSTOS_ENABLE_JAVA_DIFFERENTIAL_TESTS=ON
cmake --build build/core-java --parallel
ctest --test-dir build/core-java --output-on-failure
```

This compiles unchanged sources in `modules/rltl2ltl/src`, including the generated
parser, into the build tree. Java comparisons do not run Ant/JavaCC, download
additional dependencies, or write into `modules/`. The option defaults to `OFF`;
library-only builds ignore it. When enabled, missing tools fail configuration
and Java process errors fail the build or tests.

To override JDK discovery, pass
`-DJava_JAVA_EXECUTABLE=/path/to/jdk/bin/java` and
`-DJava_JAVAC_EXECUTABLE=/path/to/jdk/bin/javac` to CMake.

`-DEVROSTOS_JAVA_REFERENCE_JAR=/absolute/path/to/rltl2ltl.jar` adds a separate
run against an archive alongside the source run, each with its own classpath.
The local JAR is untracked and optional. `ctest --test-dir build/core-java -V -L java`
shows the JVM version, reference path, SHA-256 (archive or class tree), and counters.

`java_differential.py` uses `tests/java/LegacyBridge.java` to serialize existing
Java ASTs returned by the parser and translation visitors. Checks include:

- Parser acceptance and AST grouping: fixtures, every binary-operator pair,
  unary combinations, keyword/identifier edges, ASCII characters in three
  contexts, selected non-ASCII characters, seeded mutations, and random tokens.
  The bridge resets the static JavaCC parser after each request, including errors.
- Default visitor: compare semantic bits where every implication antecedent is
  `rG`/`rR`-free, including implication-free formulas. Check the guard recursively.
- Outermost implication: with guarded operands, compare weakest-to-strongest
  scheduling of Java queries with C++ values. Cover all 25 operand-value pairs
  and 625 two-execution combinations. A test helper simulates scheduling.
- Optimized visitor: compare unrestricted implications without `rU`/`rR`;
  require unsupported-operator errors when either occurs.
- Expected differences: assert Java and rLTL values for outer, negated, and
  antecedent-nested implication. Check Java acceptance versus C++ `depth_limit`
  at the bound; diagnostic text and resource policies need not match.
- Historical/instructional inputs: compare ASTs and translations on synthetic
  traces. The 37 reported model-checking results serve as replay targets.
- CLI: run default and `-O` translation in separate JVMs; compare four output
  lines with visitor ASTs. Check error exit statuses, stdin/stdout, and file I/O
  with spaces in paths. Archive runs use `java -jar`, testing the manifest entry point.
- Harness: check fragment classification, repeatable generation, and detection
  of eight mutations: bit reversal, `FG`/`GF` swap, pointwise negation/implication,
  dropped `X`, and incorrect eventually/until/release rules.

For formulas with at most two atoms, test every valuation and loop entry on
one-to-three-position lassos, plus 24 seeded lassos of four to twelve positions:
252 cases for two atoms. Larger historical alphabets use constant, singleton,
alternating, and seeded traces. Compare all four bits at every position.
Every semantic case checks C++ against direct rLTL evaluation. Default-Java
bitwise parity assertions apply to formulas satisfying the recursive antecedent
guard; outer implications use the scheduling comparison.

## Limits

Coverage is limited to the test corpus. Equivalence over all formulas and
infinite traces remains unverified. Syntax extensions, unbounded inputs,
checker acceptance of serialized text, wrapper integration/file protocols, and
model-checker results are outside the automated test scope.
Backend aggregation, scheduling, unknown/error outcomes, evidence retention,
property selection/polarity, resource limits, and concurrency need adapter tests;
the [audit](../../docs/legacy-audit.md) records source findings only for those paths.
See [semantics](../../docs/semantics.md) for the mathematical definitions and
[the validation record](../../docs/legacy-audit.md#executable-validation-record)
for measured coverage and platform limits.
