# rLTL verification

`Evrostos::verify` translates an rLTL formula once, checks its LTL bits through
a caller-supplied checker, and returns bounds on the model-level robust value.
It belongs to `evrostos::core` and has no dependency on SPIN or filesystem types.
`SpinBackend::verify` connects this scheduler to the [SPIN backend](spin.md).

## Scheduling and evidence

The model-level value is the infimum over admitted executions, ordered as
`0000 < 0001 < 0011 < 0111 < 1111`. Bits are indexed `0..3` for `b1..b4`,
strongest to weakest. The scheduler checks `b4`, `b3`, `b2`, then `b1`:

- `holds` proves that bit and every weaker bit true.
- `violated` establishes that bit and every stronger bit false, ending the run.
- `inconclusive` and `execution_error` leave truth unresolved. Checking continues
  because a proof of a stronger bit can resolve a failed weaker query.

Each bit is attempted at most once. Identical roots are not deduplicated, and
there are no retries. Once a false bit is found, remaining stronger queries
cannot refine the result. Implication is translated before checking; operand
values are never aggregated separately.

`Verification::lower_bound` and `upper_bound` delimit the legal robust values
consistent with completed evidence. `exact_value()` returns a `RobustValue` only
when the bounds coincide. `to_string` provides its four-bit spelling.

`RobustValue` names the five algebra elements without a numeric interpretation.
`less_equal` defines their order through the explicit chain above. `meet` returns
the greatest lower bound and `join` the least upper bound. Verification refines
its lower bound with `join` and its upper bound with `meet`.

`outcomes` stores actual checker outcomes in `b1..b4` order. An empty entry was
not checked; it is distinct from a failed query. Inference narrows the bounds
without replacing recorded failures or adding synthetic checker outcomes.

For example, an error on `b4`, a proof of `b3`, and a counterexample to `b2`
determine `0011`; `b1` is not checked and the `b4` error remains recorded.
An incomplete `b4` followed by a counterexample to `b3` instead leaves the
interval `[0000, 0001]`.

## Core interface

```cpp
#include "evrostos.hpp"

evrostos::LtlChecker checker =
    [&](std::size_t bit, const evrostos::SharedPtrLtlNode& property) {
        // Check property on the fixed model, retaining any evidence under bit.
        return check_one_property(bit, property);
    };
auto result = evrostos::Evrostos{}.verify("rG p", checker);
```

The callback returns `backends::CheckOutcome` for the positive LTL AST. It must
use sound universal checking over the same model, atom interpretation, fairness,
and terminal-state conventions for every query. These conditions give the bit
ordering used for inference. The scheduler does not independently validate a
checker's evidence or detect model changes.

`VerificationResult` contains either `Verification` or a translation `Diagnostic`.
An empty callback throws `std::invalid_argument` before translation. With a valid
callback, syntax and translation-limit failures invoke no checker. The optional
third argument supplies `TranslationLimits`. An invalid outcome enum also throws
`std::invalid_argument`; callback exceptions and allocation failures propagate.
Operational checker failures are returned as outcomes to retain completed
evidence. The callback is invoked synchronously and is not retained.

## SPIN interface

```cpp
#include "backends/spin.hpp"

evrostos::backends::SpinOptions options;
options.executable = "/absolute/path/to/build/spin-debug/spin";
options.c_compiler = "/usr/bin/cc";
options.workspace_parent = "/absolute/path/to/existing/runs";
evrostos::backends::SpinBackend backend(options);

auto result = backend.verify("/absolute/path/to/model.pml", "rG p");
if (const auto* error = std::get_if<evrostos::Diagnostic>(&result)) {
    // Handle the translation diagnostic.
} else {
    const auto& verification = std::get<evrostos::backends::SpinVerification>(result);
    const auto value = verification.summary.exact_value();
    // Inspect value or bounds, and verification.checks for per-bit evidence.
}
```

`SpinVerification::summary` is the backend-independent result. `checks` contains
an optional `SpinResult` for each bit in `b1..b4` order, retaining diagnostics,
commands, workspaces, and counterexample trails. An exact value may coexist with
a failed query whose bit was resolved by other evidence.

The model and included files must remain unchanged throughout the call. Each
attempt uses the existing single-property pipeline and its own workspace.
`SpinOptions` limits apply separately to each property: up to four subprocess
deadlines and four verifier generations per call. There is no additional overall
deadline or shared artifact quota. Workspace cleanup remains the caller's
responsibility. Translation diagnostics occur before model or tool validation.

The SPIN execution profile, including default `X` support, disabled weak fairness,
and terminal-state stuttering, is unchanged. The legacy application, NuSMV
execution, and a public CLI are outside this interface.

## Tests

Core tests enumerate all 256 four-query outcome sequences and compare the
computed bounds with an independent filter over the five legal values. They
also check scheduling, root identity and bit mapping, translation diagnostics,
callback errors, and concurrent calls without a model checker.
Algebra tests cover all 25 ordered pairs and the bounded distributive lattice
laws over all five elements, including every triple for associativity and
distributivity.

Native SPIN tests cover all five robust values, nested implication, release,
default and disabled `X`, early termination, retained per-bit artifacts, tool
and output-limit failures, translation diagnostics, and concurrent verification.
Mixed serialization failures and native checker evidence exercise both exact
values and partial bounds while retaining the failed queries.
