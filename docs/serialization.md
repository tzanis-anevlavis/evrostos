# LTL serialization

`backends/serialization.hpp` exposes two functions in `evrostos::backends`:
`serialize_nusmv` and `serialize_spin`. Both are part of `evrostos::core`.
Each accepts one immutable LTL root and returns `SerializationResult`: complete
formula text or a `SerializationDiagnostic`.

```cpp
#include "evrostos.hpp"
#include "backends/serialization.hpp"

auto result = evrostos::Evrostos{}.translate("rG p");
if (const auto* translation = std::get_if<evrostos::Translation>(&result)) {
    for (const auto& bit : translation->bits) {
        auto serialized = evrostos::backends::serialize_nusmv(bit);
        if (const auto* error =
                std::get_if<evrostos::backends::SerializationDiagnostic>(&serialized)) {
            // Handle error->code and error->message.
        } else {
            const auto& formula = std::get<std::string>(serialized);
            // Pass formula to the model-checker adapter.
        }
    }
} else {
    // Handle the translation diagnostic.
}
```

Serialization preserves AST structure, operand order, atom spelling, and polarity.
Iterating `Translation::bits` yields `b1` through `b4`, strongest to weakest.
Each call has its own byte budget. Calls share no mutable state and retain no nodes.

## Output syntax

The NuSMV printer targets the expression after `LTLSPEC`. The SPIN printer targets
the body of an inline `ltl name { ... }` property. The returned string contains
only the expression, with no declaration, property name, semicolon, or newline.

| LTL operator | NuSMV | SPIN inline LTL |
| --- | --- | --- |
| Negation | `!` | `!` |
| Conjunction | `&` | `&&` |
| Disjunction | `\|` | `\|\|` |
| Implication | `->` | `->` |
| Next | `X` | `X`, explicitly enabled |
| Eventually | `F` | `<>` |
| Always | `G` | `[]` |
| Until | `U` | `U` |
| Release | `V` | `V` |

Atoms print as identifiers, unary nodes as `(operator operand)`, and binary nodes
as `(left operator right)`. For example, `rG p` produces:

| Bit | NuSMV | SPIN |
| --- | --- | --- |
| b1 | `(G p)` | `([] p)` |
| b2 | `(F (G p))` | `(<> ([] p))` |
| b3 | `(G (F p))` | `([] (<> p))` |
| b4 | `(F p)` | `(<> p)` |

The syntax follows the [NuSMV 2.6 manual, §2.4.3](https://nusmv.fbk.eu/userman/v26/nusmv.pdf)
and [SPIN inline-LTL reference](https://spinroot.com/spin/Man/ltl.html).
SPIN's inline mechanism handles counterexample polarity when consuming a positive
property. The printer performs no negation for the checker.

## Identifiers and SPIN next

Atoms use the core's ASCII alphabet `[A-Za-z][A-Za-z0-9]*`. Validation is
case-sensitive and rejects the selected backend's reserved words, including
Boolean constants. Names such as `GFReady2` remain unchanged.
NuSMV's reserved set combines its manual's list with its lexer keywords.
SPIN's set covers Promela and inline-LTL keywords. The checked-in
[NuSMV lexer](../modules/NuSMV-2.6.0/NuSMV/code/nusmv/core/parser/input.l.2.50)
and [SPIN lexer](../modules/Spin/Src/spinlex.c) provide regression coverage.

An atom must resolve to a Boolean-valued symbol or predicate in the supplied
model. Serialization does not inspect declarations, expand macros, map names,
or accept backend expressions as atom text. SPIN output targets inline LTL;
its identifier rules differ from the older `spin -f` interface.

The SPIN printer returns `unsupported_operator` for `X` by default. Enable its
serialization with:

```cpp
auto serialized = evrostos::backends::serialize_spin(
    bit, {.allow_next = true});
```

This option permits the spelling `X`; it does not configure a checker. The
execution configuration must use SPIN built with `NXT` and disable partial-order
reduction for properties using next. See the
[SPIN reference notes](https://spinroot.com/spin/Man/ltl.html).

## Diagnostics and bounds

| Code | Meaning |
| --- | --- |
| `null_formula` | The supplied root is null |
| `invalid_identifier` | An atom violates the identifier profile or is reserved |
| `unsupported_operator` | SPIN next was encountered without `allow_next` |
| `output_limit` | Expanded formula text exceeds the byte budget or string capacity |

Diagnostics contain a code and message. ASTs carry no source locations, so these
diagnostics have no input offsets. A failed call returns no partial text.
Allocation failures propagate as exceptions.

`SerializationLimits::max_output_bytes` defaults to 1 MiB per formula. It counts
ASCII characters, including parentheses and spaces, but excludes a terminating
NUL. The exact boundary is accepted; a zero budget rejects every formula.

```cpp
evrostos::backends::SerializationLimits limits{64 * 1024};
auto nusmv = evrostos::backends::serialize_nusmv(bit, limits);
auto spin = evrostos::backends::serialize_spin(bit, {}, limits);
```

A sizing pass validates and measures each unique node once. Shared children
contribute their full text length at every occurrence. Checked additions reject
excessive expansion before allocating the output string. A second pass writes
the expression using an explicit traversal stack. For a DAG with N reachable
nodes and B output bytes, expected time is O(N + B), with O(N + B) memory.

The budget limits serialization output. Checker-specific parser and search
limits are separate. Model assembly, property selection, execution, and result
interpretation belong to backend adapters.

## Tests

```sh
cmake --build build/core-debug --parallel
ctest --test-dir build/core-debug -L serialization --output-on-failure
```

GoogleTest covers every operator, four-bit order, grouping, identifiers,
SPIN-next gating, null roots, exact byte boundaries, shared-node expansion,
overflow-sized formulas, depth, and concurrent calls.

The Python integration suite independently parses both output dialects back to
LTL trees and compares every bit with the original DAG. Cases include named and
historical formulas, all ordered binary-operator pairs in both operand positions,
and 100 seeded formulas. Additional checks cover lexer keywords and malformed
text rejected by the test parser. The parser recognizes the emitted subset;
these tests do not execute NuSMV or SPIN or establish model-checking results.

On 2026-10-08, all 49 CTest entries passed in Debug, Release, and ASan/UBSan
builds on macOS ARM64 with AppleClang 21.0.0. This includes 26 serialization
unit cases, the five-method serialization integration suite, and both Java
differential suites. Offline default tests and a library-only build also passed.
