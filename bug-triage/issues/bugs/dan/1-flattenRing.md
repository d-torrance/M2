Three asks in the file, with three different destinations. This issue is the first.

### (1) The opening question — unmet, and it errors

What should `flattenRing` do when the requested coefficient ring appears in the chain as `(k/I)[a]`?

```m2
i1 : zzB = (zzA/ideal(s^3))[t];
i2 : flattenRing(zzB, CoefficientRing => zzA)
     error: unable to flatten ring over given coefficient ring     -- newring.m2:184
i3 : flattenRing(zzB, CoefficientRing => QQ)
o3 = QQ[t,s]/s^3                                                   -- fine
```

There is a smaller reproducer of the same mechanism: `flattenRing(zzA, CoefficientRing => zzA)` also
errors. `flattenRing Ring` tests `k === R` at `newring.m2:198`, but the overriding methods at `:221-225`
and `:243` test only the *coefficient* ring — so the self-reference falls through to `unable()`.

Schubert2 calls the `CoefficientRing` form at five sites, so this is not a hypothetical corner.

### (2) Is met, and Dan's own "(or does it?)" is answered

Degrees and `degreeLength` are preserved variable by variable — a `{{1,0}}`/`{{0,1}}` tower flattens to
degrees `{{0,1,0,0},{0,0,1,0}}`, `degreeLength` 4 to 4 — and homogeneity survives the map. That came from
the 2008 degree-joining series `0126f8f60b..3f8ebb6ab1` plus `1da8ef003c` and `bf728fe883`;
`matrix2.m2:181` now relies on it.

### (3) Is verbatim still true, and lives elsewhere

`degreeLength` is never reduced and the order is the product order (`monoids.m2:681-682`). Its live
symptom is **#3887**, whose reporter works around it with `newRing(..., MonomialOrder => GRevLex,
Degrees => {1,1})`; that analysis has been added there as a comment.

### A documentation gap found on the way

`flattenRing-doc.m2` has **zero** mentions of degrees, homogeneity or monomial order — which is what
**#651** should absorb.
