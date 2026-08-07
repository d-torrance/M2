Half of the file's concern has been addressed, and the two halves now disagree with each other, which
is worse than the original state.

### What landed

The `Module` case handles relations, via `coimage basis(p, symmetricAlgebra M)` at
`Core/multilin.m2:90` — essentially the approach sketched in the file.

### What did not

The `Matrix` case is still the raw call the file itself calls a "formative attempt" and suspects is
wrong:

```m2
symmetricPower(ZZ, Matrix) := Matrix => (i,m) -> map(ring m, rawSymmetricPower(i, raw m))
```

And the two disagree: `symmetricPower(2, m)` on a map of cokernels returns `R^1 <-- R^1`, forgetting
the relations that the module version respects.

### Why this matters more than it looks

`symmetricPower` of a map is supposed to be functorial with `symmetricPower` of its source and target.
When the module version quotients by relations and the matrix version does not, the map returned is not
a map between the modules the module version would produce — so composing the two operations gives
inconsistent answers rather than merely imprecise ones.
