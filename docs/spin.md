# SPIN backend

`backends/spin.hpp` provides `evrostos::backends::SpinBackend`. It checks one
positive LTL AST against a Promela model and returns a `SpinResult`. Link
`evrostos::spin`.

The backend does not schedule the four rLTL queries or aggregate their answers.
Translation and serialization remain in `evrostos::core`.

## Build

Requires macOS or Linux, CMake 3.20+, C++20, a GCC-compatible C compiler, and
Bison. The optional build downloads SPIN into the build directory:

```sh
cmake -S . -B build/spin-debug -DCMAKE_BUILD_TYPE=Debug -DEVROSTOS_ENABLE_SPIN=ON
cmake --build build/spin-debug --parallel
ctest --test-dir build/spin-debug -L spin --output-on-failure
```

`EVROSTOS_ENABLE_SPIN` defaults to `OFF`. Core-only builds require neither SPIN
nor a C compiler or Bison. `BUILD_TESTING=OFF` also removes the GoogleTest and
Python requirements.

The checker target `evrostos_spin_tool` produces `spin` in the build directory.
It compiles unchanged upstream sources with `NXT` enabled. The source pin is
SPIN tag `version-6.5.2`, commit
[`4883fbb1b1ef1f75fa9dda78efe1ad8910baf819`](https://github.com/nimble-code/Spin/tree/4883fbb1b1ef1f75fa9dda78efe1ad8910baf819),
with archive SHA-256
`6f4963a4d6ca38f1af9ceaa76a815fbbd92e7ed7f2c424f1af88f67ec3f289f6`.
This revision reports `Spin Version 6.5.2 -- 6 December 2019`.
Upstream's [BSD 3-Clause license](https://github.com/nimble-code/Spin/blob/4883fbb1b1ef1f75fa9dda78efe1ad8910baf819/Src/LICENSE)
is retained in the downloaded source.

For an offline build, set `FETCHCONTENT_SOURCE_DIR_SPIN_SOURCE` to an extracted
copy of that revision, and `FETCHCONTENT_SOURCE_DIR_GOOGLETEST` to GoogleTest
1.18.0 when building tests. Source overrides bypass archive checksum validation.
The vendored checker under `modules/Spin` and the Makefile application are unchanged.

## API

```cpp
#include "evrostos.hpp"
#include "backends/spin.hpp"

evrostos::backends::SpinOptions options;
options.executable = "/absolute/path/to/build/spin-debug/spin";
options.c_compiler = "/usr/bin/cc";
options.workspace_parent = "/absolute/path/to/existing/runs";
evrostos::backends::SpinBackend backend(options);

auto translated = evrostos::Evrostos{}.translate("rG p");
if (const auto* translation = std::get_if<evrostos::Translation>(&translated)) {
    auto result = backend.check("/absolute/path/to/model.pml", translation->bits[0]);
    // Inspect result.outcome, message, commands, workspace, and trail.
} else {
    // Handle the translation diagnostic.
}
```

All paths must be absolute; the workspace parent must exist. Executable paths
are explicit, with no PATH-based discovery. The version check rejects a checker
with a different version string; it does not authenticate the binary. The pinned
build supplies the required `NXT` support.

Each call creates a private `evrostos-spin-XXXXXX` directory, preserving the
original model. Calls on the same backend can run concurrently. Workspaces are
retained on success and failure; their lifetime belongs to the caller. They
contain the assembled and preprocessed models, generated verifier sources,
verifier executable, and any trail. `SpinResult::commands` records each argument
array, bounded stdout/stderr, exit status, signal, timeout/truncation flags, and
launch diagnostics. Validation failures can occur before a workspace exists.
Allocation failures propagate as exceptions.

## Input and execution profile

Models, included files, and executables are trusted. Process separation and
private workspaces are not a security sandbox. The supported model profile is
Promela without embedded C (`c_code`, `c_decl`, `c_expr`, `c_state`, `c_track`),
raw `never` claims, `trace`/`notrace` declarations, or user acceptance labels
(labels beginning with `accept`). These constructs return `execution_error`.
Other inline LTL properties are allowed. The name `evrostos_property` is reserved.

Atoms resolve through model declarations or preprocessor macros. The backend
appends `ltl evrostos_property { formula }` to a copy of the model and preprocesses
both together using the configured C compiler. Quoted includes resolve relative
to the original model directory; nested includes follow the compiler's rules.
Preprocessing uses the copied filename, so `__FILE__` reflects the workspace.
Diagnostics refer to the preprocessed model. Extra include paths, macro-definition
options, and arbitrary checker/compiler flags are not exposed.

The execution stages are:

1. Check the SPIN version and preprocess the model/property.
2. Generate `pan.c` with `spin -P/bin/cat -o3 -a model.pml`.
3. Compile with `-O0 -DNOREDUCE -DMEMLIM=<limit>`.
4. Run `pan -a -N evrostos_property -m<depth> -w18`.
5. Replay a reported error with `pan -r -v -N evrostos_property`.

Caller paths are passed as process arguments. SPIN's internal preprocessing
command contains only the fixed `/bin/cat` command and fixed filenames. Children
use `LC_ALL=C`, `PATH=/usr/bin:/bin`, workspace-local `TMPDIR`, and EOF on stdin.
The parent process's environment and working directory remain unchanged.
The backend owns child reaping; ignoring `SIGCHLD` or reaping its children from
an external handler is unsupported.

Search is exhaustive, single-core, with acceptance-cycle detection enabled and
partial-order reduction and weak fairness disabled. SPIN negates the positive
inline property internally. Its standard stuttering extension applies to terminal
and deadlocked states; invalid-end-state checking is disabled by the LTL claim.
Model assertions encountered during this property search are execution errors.
This search is not an independent check of every model assertion.

Properties containing `X` are accepted by default. The pinned build enables
`NXT`, and the backend disables partial-order reduction and statement merging
for all runs to preserve the steps observed by next. Setting `allow_next=false`
rejects these properties during validation. Temporal operators refer to SPIN's
Promela execution semantics, including its handling of `atomic` and `d_step`.
See the [SPIN options](https://spinroot.com/spin/Man/Spin.html),
[verifier options](https://spinroot.com/spin/Man/Pan.html), and
[inline LTL reference](https://spinroot.com/spin/Man/ltl.html).

## Outcomes and limits

| Outcome | Evidence |
| --- | --- |
| `holds` | Normal verifier exit and a complete, zero-error exhaustive-search summary for the selected property |
| `violated` | A property counterexample with a readable trail and successful replay |
| `inconclusive` | Deadline, output/trail bound, search depth, verifier memory limit, or reported incomplete search |
| `execution_error` | Invalid input, unsupported model/tool profile, launch/compiler/parser failure, model assertion, or unrecognized/inconsistent evidence |

SPIN uses assertion violations for both model assertions and some generated
LTL claims. The final trail transition identifies which process failed: process
zero is the selected claim. Acceptance-cycle counterexamples require cycle
markers in the trail and replay. A counterexample can establish `violated` after
a depth warning; a zero-error incomplete search cannot establish `holds`.

Defaults are a shared 60-second subprocess deadline, depth 100,000, verifier
memory limit 256 MiB, combined stdout/stderr limit 4 MiB per command, model-input
limit 4 MiB, trail-read limit 4 MiB, and the serializer's 1 MiB formula limit.
The trail-read limit uses `max_output_bytes`. Depth counts verifier transitions,
including claim steps. `MEMLIM` bounds SPIN's verifier allocator, not the compiler
or total pipeline memory. A deadline or output limit terminates the subprocess
group. Serialization and local file I/O are synchronous; generated-file disk usage
has no separate quota.

Accepted option ranges are: timeout `(0, 24 hours]`, depth `[2, 10,000,000]`,
memory `[1, 65,536]` MiB, and model/output byte limits `(0, 256 MiB]`.
The serialization limit follows the [serializer contract](serialization.md#diagnostics-and-bounds).

## Tests

GoogleTest runs the pinned native checker, generated C compiler pipeline, search,
and replay. Cases cover every serialized operator with true and false outcomes,
all five robust always values, four-bit release and nested implication, named
property selection, next-step preservation, unfair executions, terminal/deadlocked
states, macros/includes, literal paths, concurrency, input/tool failures, and
resource limits. Process tests cover stream bounds, signals, launch errors,
deadlines, descendant cleanup, and argument handling. Output-protocol tests
reject malformed summaries and missing or inconsistent counterexample evidence.

The real-checker suite establishes SPIN parser acceptance for these fixtures.
NuSMV execution remains outside this backend and its tests.
`EVROSTOS_ENABLE_SANITIZERS` instruments the Evrostos C++ targets, not upstream
SPIN or the generated C verifiers.
