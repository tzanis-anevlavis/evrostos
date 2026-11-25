# NuSMV backend

`evrostos::backends::NuSmvBackend` checks positive LTL properties and rLTL
formulas against an SMV model. Include `backends/nusmv.hpp` and link
`evrostos::nusmv`.

## Build

Enable `EVROSTOS_ENABLE_NUSMV` on macOS or Linux. The build requires CMake
3.20+, C++20, a C compiler, Meson 1.5+, Ninja, Python 3, Flex, Bison, patch,
pkg-config, and development files for libedit, GMP, and libxml2. On macOS use
upstream Flex. Apple's bundled Flex generates incompatible scanner declarations.

```sh
cmake -S . -B build/nusmv -DEVROSTOS_ENABLE_NUSMV=ON
cmake --build build/nusmv --parallel
ctest --test-dir build/nusmv --output-on-failure
```

With Homebrew Flex, add `-DFLEX_EXECUTABLE="$(brew --prefix flex)/bin/flex"`
to the configure command. CMake builds the official NuSMV 2.7.1 sources with
Meson in the build tree. The resulting executable is `build/nusmv/nusmv-build/NuSMV`.
The build uses the release's bundled CUDD and enables the shell, GMP, libxml2
and COMPASS. The verification engine uses BDDs. MiniSat is disabled.

CMake verifies the archive SHA-256
`f1e11931f71d98aa9b84181eed67db584d7111100c2e967c904a31c15f823f60`.
The distribution includes NuSMV's LGPL-2.1 license and CUDD's license.
See the [upstream source and checksum](https://nusmv.fbk.eu/downloads.html).
For offline builds, point `FETCHCONTENT_SOURCE_DIR_NUSMV_SOURCE` at a writable
extracted copy of this release. Meson unpacks its bundled subproject there.
With tests enabled, also supply GoogleTest 1.18.0 through
`FETCHCONTENT_SOURCE_DIR_GOOGLETEST`. Source overrides bypass archive verification.

## Checking a model

```cpp
#include "backends/nusmv.hpp"

evrostos::backends::NuSmvBackend backend({
    .executable = "/absolute/path/to/nusmv-build/NuSMV",
    .workspace_parent = "/absolute/path/to/existing/runs"
});
auto result = backend.verify("/absolute/path/to/model.smv", "rG p");
if (const auto* run = std::get_if<evrostos::backends::NuSmvVerification>(&result)) {
    auto value = run->summary.exact_value();
    // If unresolved, inspect lower_bound, upper_bound and checks in b1..b4 order.
}
```

`verify` translates once and delegates bit ordering and early termination to
the [core scheduler](verification.md). Translation errors return a `Diagnostic`
before model access. `check(model, property)` accepts a single LTL root.
Keep the model fixed across a verification call. Each attempted bit receives
its own limits and private workspace.

The adapter copies a self-contained SMV source file into the workspace and
runs NuSMV with initialization files disabled. NuSMV builds the model, checks
transition totality, then evaluates the supplied formula in the context of
`main`. Formula atoms name Boolean model symbols or Boolean `DEFINE`s.
Existing model properties are parsed but only explicit queries are checked.
Model preprocessing and external includes are outside this profile.

## Execution semantics and evidence

LTL quantifies over infinite executions from any initial state that satisfy
the model's justice and compassion constraints. A deadlock ends a path.
A separate LTL query checks `FALSE` under the same fairness conditions. When
that query holds, the execution set is empty. Every well-typed property then
holds vacuously, and the model's robust value is `1111`.

`has_admissible_executions` records the emptiness result.
`transition_relation_total` describes the transition relation across invariant
states, including unreachable states. A non-total relation can still admit
infinite executions. Both fields are optional when checking fails.
NuSMV requires models with compassion constraints to have no input variables.
The adapter rejects SMV source combining `COMPASSION` with `IVAR` or `process`
declarations, including declarations in unused modules. A process introduces
an implicit scheduler input. See the [NuSMV manual](https://nusmv.fbk.eu/userman/v27/nusmv.pdf).

| Outcome | Evidence |
| --- | --- |
| `holds` | Completed BDD check reporting true for the selected formula |
| `violated` | False verdict, a counterexample with a loop, and successful BDD replay |
| `inconclusive` | Subprocess deadline or output limit reached |
| `execution_error` | Invalid input, tool failure, or missing or inconsistent checker evidence |

Each run retains its model copy, command scripts, and a readable counterexample
after successful replay. The caller owns workspace cleanup. `commands` records
argument vectors, exit status, signal, bounded stdout/stderr and diagnostics.
Model paths, executable paths and workspace parents must be absolute. The
adapter expects trusted models and executables.

The default deadline is 60 seconds per property. Model input and combined
stdout/stderr per command default to 4 MiB each. Accepted limits are up to 24
hours and 256 MiB respectively, with positive minima. Formula text also has to
fit the checker's 65,536-byte command-line buffer with its command, quotes,
newline and terminating NUL. Memory and workspace disk usage have no separate quota.

The native tests exercise all LTL operators, all five robust values, empty
execution sets, fairness, deadlocks and replay. Enabling both backends adds a
comparison on SMV and Promela models with the same infinite label sequence.
