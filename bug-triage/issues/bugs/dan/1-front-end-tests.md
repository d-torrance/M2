Mike's list of tests is met. Dan's two exceptions are not, and this issue covers both.

### The predicates Mike wanted now exist

The June 2006 conditions for a Hilbert-aided Gröbner basis are `canUseHilbertHint` (`gb.m2:260-271`),
item for item: `degreeLength R === 1`, polynomial ring or quotient of one, over a field, commutative or
skew, every variable's first degree positive, matrix homogeneous. Landed in `6e72d40d14` (2025-07-29),
renamed in `ef88222289` — nineteen years on.

### Exception (a): no Hilbert hint for `res`

Still literally as Dan describes. `options gb` has `Hilbert`; `options res` does not. `comp-res.hpp` and
`comp-res.cpp` contain no `hilbert` at all, and `set_hilbert_function` exists only on GB classes.

Demonstrated by installing a deliberately **wrong** hint, `poincare J = 1`:

- `gens gb J` becomes the zero map — so `gb` used it,
- `betti res J` is unchanged from the correct resolution — so `res` ignored it.

`res` is now a synonym for Complexes' `freeResolution`, and the front-end store exists
(`poincare M = p`, `hilbert.m2:110`) with no resolution consumer.

### Exception (b): no way to ask about one degree component

`isHomogeneous` has 12 methods and none takes more than one argument, so "homogeneous with respect to
just the first component of the degree" is not expressible. `Complexes/FreeResolution.m2:215-380`
substitutes `heft R =!= null`, which is weaker, plus full multigraded `isHomogeneous`, and does not
enforce Mike's "algorithm 3 additionally singly graded".

### Related

**#4487** is adjacent to (b); **#3937** is why `canUseHilbertHint` was hardened.
