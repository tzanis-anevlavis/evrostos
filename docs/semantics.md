# rLTL translation semantics

The C++ core implements these rules for the grammar below, including nested
implications outside the efficient fragment. See the [legacy audit](legacy-audit.md)
for differences from Java.

## Values and bit order

Truth values form the chain `0000 < 0001 < 0011 < 0111 < 1111`.
Bits `(b1,b2,b3,b4)` run strongest to weakest, with `b1 <= b2 <= b3 <= b4`.
Arrays and serialized translations keep this order regardless of checking order.

At each position of an infinite trace, an atom is `1111` when true and `0000`
otherwise. Conjunction and disjunction take the minimum and maximum on the chain.
Robust negation maps `1111` to `0000` and every other value to `1111`;
it is neither bitwise complement nor an involution (`!!a` need not equal `a`).
Robust implication `a => b` is `1111` when `a <= b`, and `b` otherwise.

For `rG p`, the five values distinguish: never true;
true a positive finite number of times; true and false infinitely often;
false a positive finite number of times; always true. This interpretation is
specific to the formula `rG p`.

## Translation rules

Let `Ai = Ti(phi)` and `Bi = Ti(psi)`. Outputs use ordinary LTL (`->` is Boolean
implication). At each trace position, `Ti` is true exactly when bit `i` of the
rLTL value is 1.

| rLTL input | T1 | T2 | T3 | T4 |
| --- | --- | --- | --- | --- |
| `p` | `p` | `p` | `p` | `p` |
| `phi & psi` | `A1 & B1` | `A2 & B2` | `A3 & B3` | `A4 & B4` |
| `phi \| psi` | `A1 \| B1` | `A2 \| B2` | `A3 \| B3` | `A4 \| B4` |
| `!phi` | `!A1` | `!A1` | `!A1` | `!A1` |
| `phi => psi` | `(A1 -> B1) & C2` | `(A2 -> B2) & C3` | `(A3 -> B3) & C4` | `A4 -> B4` |
| `rX phi` | `X A1` | `X A2` | `X A3` | `X A4` |
| `rF phi` | `F A1` | `F A2` | `F A3` | `F A4` |
| `rG phi` | `G A1` | `F G A2` | `G F A3` | `F A4` |
| `phi rU psi` | `A1 U B1` | `A2 U B2` | `A3 U B3` | `A4 U B4` |
| `phi rR psi` | `A1 R B1` | `F A2 \| F G B2` | `F A3 \| G F B3` | `F A4 \| F B4` |

For implication, `C4 = A4 -> B4` and `Ci = (Ai -> Bi) & C(i+1)` for
`i = 3,2,1`: each bit conjoins the implications from `i` through 4.
Robust release `phi rR psi` and `!(!phi rU !psi)` are semantically distinct.

The rules follow Section 4, Table 2 of Anevlavis, Philippe, Neider,
and Tabuada, *Being Correct Is Not Enough: Efficient Verification Using Robust
Linear Temporal Logic*, ACM Transactions on Computational Logic 23(2), Article 8,
2022, [DOI: 10.1145/3491216](https://doi.org/10.1145/3491216).
Section 4 is on pages 8:18-8:19, Table 2 on 8:19. Equations (29)-(31) define
the translations, (32) gives their ordering, and Lemma 4.1 states the translation
result. Section 3 defines the truth algebra; Section 6 gives optimizations for
the efficient fragment.

The open-access [arXiv version](https://arxiv.org/abs/2102.11991v2) has the same
table on page 17. [`translation_rules.json`](../tests/semantics/translation_rules.json)
encodes the rules as printer-independent ASTs and records PDF verification metadata.

## System-level result and early termination

The model-level value is the infimum of the formula's values over admissible
infinite executions. Universal model checking of each `Ti` yields its bits.
The C++ core returns these formulas without executing a model checker.

Checking `T4`, `T3`, `T2`, then `T1` allows early termination: a false bit makes
all stronger bits false.

A proof establishes a true bit; a counterexample establishes a false bit.
Failed or incomplete checks leave a bit unresolved unless a counterexample
has been found. An exact rLTL result requires enough evidence to determine
all four bits. The [legacy wrapper](legacy-audit.md#parser-and-io-boundaries)
does not distinguish all checker failures from false results.

Implication is translated before aggregation, preserving the relationship
between its operands on each execution. Aggregating operand values separately
loses this relationship. Trace, fairness, and deadlock conventions determine
the admissible executions; equivalent conventions are needed for cross-backend
comparisons. The infimum of an empty execution set is `1111`.

## Implication optimizations

Pointwise `Ai -> Bi` is valid when the antecedent is Boolean-valued. Absence of
`rG` and `rR` suffices: the antecedent is then either `0000` (all implications
are true) or `1111` (the result is `B`). The core efficient fragment restricts
every implication's antecedent this way. The extended fragment also permits
one unrestricted outermost implication with operands in the core fragment.

For that outermost implication, the wrapper checks queries `Qi = Ai -> Bi`
weakest to strongest. When it reaches `Qi`, all weaker queries have been proved
true, so checking `Qi` equals checking the suffix conjunction defining `Ti`.
The raw query vector can be an invalid truth value, making this shortcut
unsuitable for a subformula whose enclosing operator needs semantic bits.
Its correctness depends on the fragment restrictions above.
See Definition 9, Algorithm 1, and Section 6 of
[Anevlavis et al.](https://arxiv.org/abs/2102.11991v2).

The C++ core always uses the translation table, without fragment optimizations.
It shares immutable AST nodes; [text expansion can be exponential](core.md#diagnostics-and-bounds).

## Language compatibility profile

The grammar preserves JavaCC precedence and associativity, including implication
binding more tightly than conjunction:

```text
expression  := disjunction EOF
disjunction := conjunction ("|" conjunction)*
conjunction := implication ("&" implication)*
implication := temporal ("=>" temporal)*
temporal    := unary (("rU" | "rR") unary)*
unary       := ("!" | "rF" | "rG" | "rX") unary | atom
atom        := IDENTIFIER | "(" disjunction ")"
IDENTIFIER  := [A-Za-z][A-Za-z0-9]*
```

Binary operators associate left. Whitespace is space, tab, CR, or LF. Longest-match
lexing makes `rGp` an identifier and `rG p` an operator application. Operator
keywords cannot be identifiers. Uppercase letters are allowed; underscores, dots,
indexing, numeric literals, and model predicates are not. Translation preserves
identifier spelling. Extensions to identifiers, constants, or precedence change
the input language and its compatibility profile.

[`parser_cases.json`](../tests/semantics/parser_cases.json) tests acceptance and
grouping in C++ without Java; the optional differential suite runs both parsers.
