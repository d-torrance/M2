An empty block in a monomial ordering is dropped inside the engine, so `selectInSubring` counts
blocks differently from the way they were written and silently returns the wrong columns.

```m2
i1 : R = QQ[x,y,z, MonomialOrder => {Eliminate 0, Eliminate 1}]

o1 = R

o1 : PolynomialRing

i2 : selectInSubring(1,vars R)

o2 = | y z |

             1      2
o2 : Matrix R  <-- R
```

`Eliminate 0` eliminates nothing, so the first block imposes no condition and `selectInSubring(1, ...)`
should return all three variables. Instead it returns the answer belonging to the *second* block —
identical to what a ring with only `{Eliminate 1}` gives.

### Where it goes wrong

The top-level monoid keeps the empty block:

```m2
i1 : R = QQ[x,y,z, MonomialOrder => {Eliminate 0, Eliminate 1}];

i2 : (monoid R).Options.MonomialOrder

o2 = {MonomialSize => 32  }
     {Weights => {}       }
     {Weights => {1}      }
     {GRevLex => {1, 1, 1}}
     {Position => Up      }
```

So this is not visible from the monoid. But `selectInSubring` does not consult it —
[`matrix2.m2:700-702`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/matrix2.m2#L700-L702)
hands the question straight to the engine as `rawEliminateVariables(i, m.RawMatrix)`, and the engine
filters blocks through `is_good`, which rejects any block covering no variables:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/e/interface/monomial-ordering.cpp#L66-L90

`is_good` gates both the block count and the block iteration (lines 219 and 230, and again at 658 and
669), so the empty `Weights => {}` block is absent from the engine's numbering and every later block
shifts down by one.

### Tests

These come from the bug file, written by Dan. The first fails today; the second and third pass and are
useful as controls, since they differ only in the first block being non-empty:

```m2
R = QQ[x,y,z, MonomialOrder => {Eliminate 0, Eliminate 1}]
assert( (selectInSubring(1,vars R)) === map(R^1,R^{{-1},{-1},{-1}},{{x, y, z}}) )
R = QQ[x,y,z, MonomialOrder => {Eliminate 1, Eliminate 1}]
assert( (selectInSubring(1,vars R)) === map(R^1,R^{{-1},{-1}},{{y, z}}) )
R = QQ[x,y,z, MonomialOrder => {Eliminate 2, Eliminate 1}]
assert( (selectInSubring(1,vars R)) === map(R^1,R^{{-1}},{{z}}) )
```

### A fix that was already tried

The bug file records an attempt at the obvious change — commenting out the `nvars > 0` test so that
empty blocks survive — with the note that it *"broke many tests"*, and the reasoning:

> it is a bad idea to elide blocks in the ordering, because that changes the number of them, and
> functions such as `selectInSubring` depend on the numbering

So the elision is load-bearing somewhere else, and preserving empty blocks wholesale is not the fix.
Reconciling the two numberings, or rejecting an ordering whose blocks cannot be represented, may be
closer.

Not the same as [#883](https://github.com/Macaulay2/M2/issues/883), which is about `selectInSubring`
being under-documented — the term "block" not being defined — and a proposal to replace it with
something clearer. That issue would not be closed by fixing this one.

Adjacent but distinct from [#4593](https://github.com/Macaulay2/M2/issues/4593), which is also about
how `MonomialOrder` blocks are validated: there an over-long weight vector is rejected outside
`MonomialOrder` and silently accepted inside it. Empty blocks appear in that report only as
scaffolding in the examples, and it concerns neither block numbering nor `selectInSubring`. The two
are worth reading together if someone revisits how blocks are checked and stored.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36).
`bugs/mike/0-empty-monomial-ordering-blocks` is Dan's write-up, and it opens with the diagnosis this
issue confirms: *"The answer should be `| x y z |`, but the Eliminate 0 block is removed during
joining."*
