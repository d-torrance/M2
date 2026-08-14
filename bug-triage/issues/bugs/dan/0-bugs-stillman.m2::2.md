`Macaulay2Doc/functions/monomials-doc.m2` describes a one-row output and says each monomial appears once.
Neither holds once the argument is a matrix with more than one row, which is the case the request is about.

### The Outputs are wrong for a matrix

The node's `Outputs` reads

> a one row matrix in the same ring with all of the monomials that appear in `f`

and the prose immediately after adds *"Each monomial only appears once"*. Both are true of a `RingElement`
and false of a `Matrix` with more than one row:

```m2
i1 : R = ZZ[a..d,x,y];

i2 : m = matrix{{a*x^2, b*x*y},{c*y^2, a*x^2}};

i3 : monomials m

o3 = | 0     0     ax2 bxy |
     | ax2   cy2   0   0   |

             2       4
o3 : Matrix R  <-- R
```

Two rows, not one, and `a*x^2` appears twice.

The code is not what is wrong. `monomials(Matrix)` is one line —
[`m2/matrix2.m2:649-652`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/matrix2.m2#L649-L652):

```m2
monomials(Matrix) := o -> (f) -> (
     vrs := listOfVars(ring f,o.Variables);
     map(target f,,rawMonomials(vrs, raw f))
     )
```

The result is built over `target f`, so it has as many rows as the argument by construction, and each entry's
monomials sit in that entry's row. That is the factorization `coefficients` needs, and it holds:
`monomials m * last coefficients m == m` is true for the matrix above. `monomials(RingElement)` goes through
`matrix{{f}}` at `:648`, which is where the one-row description comes from — it is a description of one of
the two methods in the node's `Key`.

The node's only example is a one-row matrix, so nothing in it exercises the case.

### The rest of the node

It is `--- status: DRAFT`'s worse neighbour — `--- status: TODO` with an empty `--- author(s):` line, unchanged
since it was stubbed. Beyond the `Outputs`:

- **Its own prose contains an unanswered question.** Between the `Outputs` and the example:
  *"Each monomial only appears once, and the monomials are sorted in what order?"* That has been published as
  part of the manual for years.
- **No `Caveat`, and nothing distinguishes the two methods.** `(monomials, RingElement)` and
  `(monomials, Matrix)` share one `Usage`, one `Inputs` and one `Outputs`, and the difference between them is
  exactly what this issue is about.
- **`Consequences` on the `[monomials, Variables]` node describes an input, not a consequence** — *"Each
  variable not in the set of variables x is considered a coefficient"* is a statement about how the option is
  interpreted.

### What I did not establish

**The sort order.** I tried to answer the node's own question and could not, which is itself a reason the node
should answer it. `monomials` returns its columns in an order consistent with `leadTerm` picking the first, and
on every pair I tried the order was the same in `ZZ[a,b,c]` and in `ZZ[a,b,c, MonomialOrder => Lex]` —
including `{a*c, b^2}` and `{a*c^2, b^3}`, where I had expected the two orders to disagree. So I have no
example distinguishing "the ring's monomial order" from anything else, and I am not asserting the answer. It
should come from whoever knows `rawMonomials`.

**Whether the multi-row convention is written down elsewhere.** I checked this node and `coefficients`. I did
not read the overviews.

This is distinct from [#4647](https://github.com/Macaulay2/M2/issues/4647), which collects pre-`Headline`
nodes under `Macaulay2Doc/operators/`; this one is a function node whose content is wrong rather than whose
style is old.

### Provenance

The request is one line of a 2005 scratchpad, and the reply beneath it is the whole reason it is still here:

> `-- 'monomials' applied to a matrix with more than one row is coming out messed up`
> `-- see the test in functions/monomials-doc.m2`
>
> &nbsp;&nbsp;&nbsp;&nbsp;`what's the problem? [drg]`

The problem is the documentation, and the file it points at is the file that has it.
