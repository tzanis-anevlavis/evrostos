# Internal C++ core

The C++20 library in `src/` translates rLTL formulas into four LTL abstract
syntax trees (ASTs). The separate application in `modules/` uses Java and the
vendored model checkers.

## Structure

```text
src/
  evrostos.hpp / evrostos.cpp     Public Evrostos entry point and result types
  ltl.hpp                       Read-only LTL nodes returned by translation
  detail/
    rltl.hpp                    Internal parsed representation and diagnostics
    parser.hpp / parser.cpp     Legacy-compatible grammar
    translator.hpp / .cpp       Section 4, Table 2 translation
tests/
  core_test.cpp                 API, ownership, sharing, limits, reentrancy
  translation_driver.cpp        Private bridge to the Python reference tests
  java/LegacyBridge.java        Private bridge to the Java parser and visitors
  semantics/                    Fixtures and independent semantic evaluation
```

`evrostos::Evrostos` is the public API; the parser and translator are internal.
Link the static CMake target `evrostos_core` or its alias `evrostos::core`.
There is no separate translator package, install target, or public CLI.

## Translation API

```cpp
#include "evrostos.hpp"

evrostos::Evrostos core;
auto result = core.translate("!((rG p) => (rG q))");

if (const auto* error = std::get_if<evrostos::Diagnostic>(&result)) {
    // Handle error->code, message, offset, line, and column.
} else {
    const auto& translation = std::get<evrostos::Translation>(result);
    const auto& strongest = translation.bits[0]; // b1
    const auto& weakest = translation.bits[3];   // b4
    // Inspect operators and children through the immutable LTL nodes.
}
```

The roots represent bits `b1` through `b4`, strongest to weakest. Translation
supports all [operators](semantics.md#translation-rules), including nested
implications, without fragment-specific optimizations. The
[grammar](semantics.md#language-compatibility-profile) defines identifiers,
precedence, and associativity; numeric truth constants are unsupported.

Each LTL node has an operator, an atom name, and up to two children. Atoms have
names and no children; unary operators use the left child, binary operators both.
Nodes are immutable and reference-counted. Results own their nodes and outlive
the input string and `Evrostos` instance. Repeated subexpressions and implication
suffixes share nodes within each translation, forming a directed acyclic graph
(DAG). No global cache or mutable instance state is used; calls on the same
instance can run concurrently.

## Diagnostics and bounds

Syntax and resource-limit failures return `Diagnostic`:

| Code | Meaning |
| --- | --- |
| `syntax_error` | Invalid token, missing operand/parenthesis, or trailing input |
| `input_limit` | Source byte length exceeds the configured limit |
| `depth_limit` | Syntactic nesting or parsed AST height exceeds the fixed bound |
| `node_limit` | Parsed-node or unique-LTL-node budget exceeded |

Offsets are zero-based byte positions; EOF may equal the input length. Lines
and byte columns are one-based. CRLF, CR, and LF each count as one newline;
a tab counts as one byte.

`TranslationLimits` sets budgets for input bytes (default 1 MiB), parsed nodes
(100,000), and unique LTL nodes (1,000,000). Applications can lower these budgets.
Syntactic nesting and AST height are capped at 256, protecting recursive parsing,
traversal, and destruction, including left-associated chains. Allocation failures
such as `std::bad_alloc` propagate as exceptions. A call returns either a complete
`Translation` or a `Diagnostic`.

The core performs no filesystem I/O, process execution, serialization, or model
checking. Expanding the DAG into text can be exponential; serialization and
output-size limits are outside the core's scope.

## Build and test locally

Requires C++20 and CMake 3.20+. Tests use GoogleTest 1.18.0 and Python 3.10+
(standard library only). CMake downloads the checksum-pinned GoogleTest source
into the build directory at first configuration; an
[offline source override](../tests/semantics/README.md#cpp-unit-tests) is available.
Only the optional [Java comparisons](../tests/semantics/README.md#java-differential-tests)
require JDK 11+; no tests require model checkers.

```sh
cmake -S . -B build/core-debug -DCMAKE_BUILD_TYPE=Debug
cmake --build build/core-debug --parallel
ctest --test-dir build/core-debug --output-on-failure
```

Build only the internal library, without GoogleTest, Python, Java, or downloads:

```sh
cmake -S . -B build/core-only -DBUILD_TESTING=OFF
cmake --build build/core-only --parallel
```

Address/undefined-behavior sanitizer checks with Clang or GCC:

```sh
cmake -S . -B build/core-sanitize -DCMAKE_BUILD_TYPE=Debug -DEVROSTOS_ENABLE_SANITIZERS=ON
cmake --build build/core-sanitize --parallel
ctest --test-dir build/core-sanitize --output-on-failure
```

With `BUILD_TESTING=ON`, a driver exports the LTL DAG to Python through a private
test protocol, bypassing backend serialization. Tests cover
the parser, published rules, named and generated formulas, and historical inputs
using independent rLTL evaluation. `EVROSTOS_ENABLE_JAVA_DIFFERENTIAL_TESTS=ON`
also exercises the Java parser, both visitors, and CLI, including implication
and resource-limit differences. See [test coverage and limits](../tests/semantics/README.md).
