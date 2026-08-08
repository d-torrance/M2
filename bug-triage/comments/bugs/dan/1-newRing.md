Two things from the pre-GitHub `bugs/` tree, further to this issue.

**A cause for the second and third lines of the report.** The two errors have the
same origin, and it is dispatch rather than a missing case. `Ring ** Ring` is
`newring.m2:45`, and `tensor(Ring, Ring)` (`:48-59`) opens with

```m2
if R === (try coefficientRing S) then return S;
```

But `QuotientRing ** PolynomialRing` is installed at `:62`, and it is more
specific, so for any `QuotientRing` coefficient ring the call goes to
`tensor(QuotientRing, PolynomialRing)` at `:68` instead, whose first line is
`k := coefficientRing R` — and `coefficientRing (ZZ/101)` is the
`no coefficient ring present` error at `rings.m2:56`.

**A worse symptom of it.** The identity case fails too, for every `QuotientRing`
coefficient field, although the line quoted above is written to return the ring
unchanged:

```m2
k = ZZ/5;       R = k[x,y];   coefficientRing R === k   -- true
k ** R                        -- error: no coefficient ring present
```

Same for `ZZ/32003`, `QQ[a]/(a^2-2)` and `ZZ[b]/(b^2-2)`. `QQ`, `ZZ` and `GF 4`
all work, because `class QQ` is `Ring` and `class (GF 4)` is `GaloisField`, so
neither is caught by the `:62` method. Tested on 1.26.06-40-gd8e86d689d.

**The other spelling, and a piece of the ask that is not here.**
`bugs/dan/1-newRing` is two lines by Dan Grayson:

> add a coefficient ring option to `newRing`
> which accepts a ring or a ring map

`newRing` is the other natural place to ask for base change, and it has no such
option: `options newRing` is 18 monoid options, because `newring.m2:16` builds the
table from `applyValues(monoidDefaults, ...)` and the coefficient ring is not one
of them.

The *ring map* half is not covered by `**` in any form, since `**` takes a ring and
has no way to be told how the old coefficients map into the new one.
`(QQ[t]) ** (QQ[x,y]/(x^2-y^2))` returns `QQ[t,x,y]/(x^2-y^2)` with
`coefficientRing` still `QQ` — `t` joins the monoid rather than the coefficients —
so base change along an extension has no spelling at all.

For what it is worth on the doc-page half of this issue: `k (monoid R)` is
documented at `doc_rings.m2:253` and used at about fifteen sites in distributed
packages, including `QthPower.m2:797-800`, which carries one monoid across `ZZ`
and then `QQ`. It is the answer for polynomial rings and drops the relations for
quotients, which is worth saying explicitly wherever base change gets written up.
