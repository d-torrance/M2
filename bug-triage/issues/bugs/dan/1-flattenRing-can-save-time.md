### What the file asks for

`flattenRing` rewrites a tower of rings as a single quotient of a polynomial ring, handing back a ring
map alongside it. These five lines point at two efficiencies, the second depending on the first: the
flattened ring could reuse the engine's existing raw ring where the flattening does not really change
it, and once a map's source and target share a raw ring, applying it could return its argument
untouched instead of computing.

Both halves unmet, and the second is measurable without the first — so it is the easier one to act on.

### Ring-map application never short-circuits

`ringmap.m2` contains no raw-ring comparison, and `RingMap RingElement` (`:169-175`) always calls
`rawRingMapEval`. So `map(zzB, zzB)` — which satisfies `zzid == 1`, `source === target` and
`zzid === id_zzB`, and note that `ringmap.m2:31` already computes exactly that predicate for comparison
purposes — still costs **2.55 ms per application** on a 210-term polynomial, to return its argument.

### The first half is real but small

For a tower, `flattenRing` builds a fresh raw ring rather than reusing `R.FlatMonoid`:
`raw zzS =!= raw zzR` and `zzR.FlatMonoid =!= monoid zzS`, even though `describe` of the two monoids is
character-identical. But caching from `b25a44b6d1` (2008-10-24) already amortizes it — 4.0 ms on the
first call for a 16-variable tower, 4.1 µs afterwards.

### The unsettled part, which is what "when appropriate" is doing in the file

Whether reusing `R.FlatMonoid` is sound for Weyl algebras, `Inverses`, `SkewCommutative` or
`Join => false`. That needs a guard rather than a comparison, and it is the reason the first half is not
simply an optimization.

### Related

**#2133** is the adjacent caching issue.
