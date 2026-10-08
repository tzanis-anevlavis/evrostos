"""Independent, test-only rLTL and LTL semantics on infinite lassos.

The rLTL side evaluates the five-valued algebra and temporal infima/suprema
directly. The LTL side uses Boolean fixed points. Neither parses source text,
calls Java, nor invokes a model checker. See ../../docs/semantics.md.
"""

from dataclasses import dataclass
from functools import cache


VALUES = ("0000", "0001", "0011", "0111", "1111")


def bits(rank):
    return tuple(bit == "1" for bit in VALUES[rank])


def rank_of(bit_values):
    # Reject non-monotone vectors instead of silently rounding them.
    return VALUES.index("".join("1" if bit else "0" for bit in bit_values))


def implies(left, right):
    return 4 if left <= right else right


def freeze(ast):
    return tuple(freeze(item) for item in ast) if isinstance(ast, list) else ast


@dataclass(frozen=True)
class Lasso:
    """Positions [0, size); the last position jumps to loop_start."""

    size: int
    loop_start: int

    def __post_init__(self):
        if not 0 <= self.loop_start < self.size:
            raise ValueError("A lasso must have a nonempty cycle")

    def successor(self, position):
        return position + 1 if position + 1 < self.size else self.loop_start

    def orbit(self, start):
        seen, order = {}, []
        while start not in seen:
            seen[start] = len(order)
            order.append(start)
            start = self.successor(start)
        return order, seen[start]


def limit_value(sequence, loop_start):
    """inf b1, liminf b2, limsup b3, sup b4 of a five-valued sequence."""
    vectors = [bits(value) for value in sequence]
    cycle = vectors[loop_start:]
    return rank_of((all(v[0] for v in vectors),
                    all(v[1] for v in cycle),
                    any(v[2] for v in cycle),
                    any(v[3] for v in vectors)))


def robust(operator, left, right, lasso):
    """Evaluate one rLTL operator on operand values at every lasso position."""
    if len(left) != lasso.size or any(value not in range(5) for value in left):
        raise ValueError("Invalid left operand values")
    if right is not None and (len(right) != lasso.size
                              or any(value not in range(5) for value in right)):
        raise ValueError("Invalid right operand values")
    if operator == "!":
        return tuple(0 if value == 4 else 4 for value in left)
    if operator in ("&", "|", "=>"):
        operation = {"&": min, "|": max, "=>": implies}[operator]
        return tuple(operation(a, b) for a, b in zip(left, right))
    if operator == "rX":
        return tuple(left[lasso.successor(i)] for i in range(lasso.size))

    result = []
    for start in range(lasso.size):
        order, loop = lasso.orbit(start)
        if operator == "rF":
            value = max(left[i] for i in order)
        elif operator == "rG":
            value = limit_value([left[i] for i in order], loop)
        elif operator == "rU":
            # sup_j min(B(j), inf_{i<j} A(i)); inf of the empty prefix is top.
            # Repeated laps cannot improve a witness: its prefix minimum falls.
            value, prefix_min = 0, 4
            for i in order:
                value = max(value, min(prefix_min, right[i]))
                prefix_min = min(prefix_min, left[i])
        elif operator == "rR":
            # q(j) = max(B(j), sup_{i<j} A(i)), then take the four limits.
            # Track memory as part of the cycle: A on a late cycle position
            # changes q on the next lap. A single unrolling would be wrong.
            seen, sequence = {}, []
            position, prefix_max = start, 0
            while (position, prefix_max) not in seen:
                seen[position, prefix_max] = len(sequence)
                sequence.append(max(right[position], prefix_max))
                prefix_max = max(prefix_max, left[position])
                position = lasso.successor(position)
            value = limit_value(sequence, seen[position, prefix_max])
        else:
            raise ValueError(f"Unknown rLTL operator: {operator}")
        result.append(value)
    return tuple(result)


def evaluate_rltl(ast, prefix, cycle):
    """Return values at every position; each trace position lists true atoms."""
    if not cycle:
        raise ValueError("A lasso must have a nonempty cycle")
    states = prefix + cycle
    lasso = Lasso(len(states), len(prefix))

    @cache
    def evaluate(node):
        if isinstance(node, str):
            return tuple(4 if node in state else 0 for state in states)
        operator, *children = node
        if len(children) != (2 if operator in ("&", "|", "=>", "rU", "rR") else 1):
            raise ValueError("Invalid rLTL AST arity")
        left = evaluate(children[0])
        right = evaluate(children[1]) if len(children) == 2 else None
        return robust(operator, left, right, lasso)

    return evaluate(freeze(ast))


def evaluate_ltl(ast, environment, lasso):
    """Classical LTL on a finite graph with an infinite loop, via fixed points."""
    @cache
    def evaluate(node):
        if isinstance(node, str):
            return tuple(environment[node])
        operator, *children = node
        left = evaluate(children[0])
        right = evaluate(children[1]) if len(children) == 2 else None
        if operator == "!":
            return tuple(not value for value in left)
        if operator == "X":
            return tuple(left[lasso.successor(i)] for i in range(lasso.size))
        if operator in ("&", "|", "->"):
            operation = {"&": lambda a, b: a and b,
                         "|": lambda a, b: a or b,
                         "->": lambda a, b: not a or b}[operator]
            return tuple(operation(a, b) for a, b in zip(left, right))
        if operator not in ("F", "G", "U", "R"):
            raise ValueError(f"Unknown LTL operator: {operator}")
        current = (operator in ("G", "R"),) * lasso.size
        while True:
            following = tuple(current[lasso.successor(i)] for i in range(lasso.size))
            if operator == "F":
                updated = tuple(a or f for a, f in zip(left, following))
            elif operator == "G":
                updated = tuple(a and f for a, f in zip(left, following))
            elif operator == "U":
                updated = tuple(b or (a and f) for a, b, f in zip(left, right, following))
            else:
                updated = tuple(b and (a or f) for a, b, f in zip(left, right, following))
            if updated == current:
                return updated
            current = updated

    return evaluate(freeze(ast))
