The documentation for `GroupRevLex` ends with

```m2
     Caveat => { "This feature has not been implemented yet."}
```

at [`ov_monomial_orderings.m2:616`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/Macaulay2Doc/ov_monomial_orderings.m2#L608-L617),
but the feature works. Running that node's own example verbatim:

```m2
i1 : R = QQ[a..d, MonomialOrder=>GroupRevLex=>2, Global=>false];

i2 : (monoid R).Options.MonomialOrder
o2 = {MonomialSize => 32, GroupRevLex => 2, GRevLex => {1, 1}, Position => Up}

i3 : a^-1
      -1
o3 = a

i4 : a^-1 * a
o4 = 1

i5 : a^-2 * a^3
o5 = a

i6 : b^-1
      -1
o6 = b

i7 : try c^(-1) else "failed"
o7 = failed
```

That is exactly what the node's prose promises — "inverts the first `n` variables in the polynomial ring. In the
following example, `a^-1` is in the ring, but `c^-1` is not." The inverted variables behave as units, and the boundary
falls where documented.

### Why this is worth a moment rather than a one-line deletion

A caveat saying a feature is unimplemented is self-fulfilling: a reader deciding whether `GroupRevLex` is usable reads it
and stops. So the order may well have gone unused for years while working.

But I would not simply delete the line, because these caveats are not boilerplate. The sibling `GroupLex` node, twelve
lines below, carries a substantive one:

> The element `a/b` is in the fraction ring, while `a*b^(-1)` belongs to `R`.

— a real distinction a user needs. Whoever knows how complete `GroupRevLex` is should decide what belongs in its place.

**One genuine limitation I did find:** Gröbner bases fail over such a ring.

```m2
i8 : gens gb ideal(a*c-b)
     -- error
```

so a caveat scoped to Gröbner bases may be warranted where the blanket one is now. I have not probed further than ring
construction, arithmetic and `gb`, so there may be other gaps; that is precisely why this is filed as a question about
the documentation rather than as a patch.

### Provenance

One request from `bugs/dan/0-bugs-iswanson.m2`, which reads:

```text
GroupRevLex does not seem to be installed
Then uncomment the lines in monomorderings.m2 in the manual
for GroupRevLex.
```

The situation has inverted since. The manual lines are no longer commented out — `monomorderings.m2` is now
`ov_monomial_orderings.m2` and documents `GroupRevLex` at `:608`, with cross-references at `:93`, `:114`, `:168` and
`:684` — so that half of the request is done. What remains is the opposite of what was reported: the feature is
installed and the documentation says it isn't.

For the record, my own first attempt reproduced the 2005 report, because I wrote `MonomialOrder=>{GroupRevLex=>4}` over
all four variables without `Global => false` and read the resulting error as the feature being absent. The documented
form works.

The other seven requests in that file were triaged in the same pass and are recorded in the catalogue: six are met —
including the exponent limits, which now cap at exactly the `2^15-1` and `2^7-1` the file asked for, erroring instead of
wrapping silently — and one is a duplicate of [#3449](https://github.com/Macaulay2/M2/issues/3449).

Searched titles for `GroupRevLex`, `GroupLex`, `Laurent`, `invert`, `MonomialSize` and "inverse of a variable", bodies for
`GroupRevLex`, and comments for `GroupRevLex`, `GroupLex` and "not been implemented yet". Nothing tracks this; the
`Laurent` and `invert` hits are unrelated (#598 Laurent *series*, #4357 denominators, #2718 matrices, #3973 tower rings
via `toField`).
