`pushFwd` cannot describe one Galois field as a module over a smaller one, although the same extension
written as quotient rings works fine.

```m2
i1 : needsPackage "PushForward";

i2 : R = GF(729, Variable => symbol ga);

i3 : S = GF(27, Variable => symbol vv);

i4 : F = map(R,S,{ga^28});

o4 : RingMap R <-- S

i5 : isWellDefined F

o5 = true

i6 : pushFwd F
stdio:6:7:(3):[1]: error: no coefficient ring present
```

The map is accepted and `isWellDefined` confirms it, so the failure is not about the map. Spelling the
same fields as quotient rings instead, the push-forward goes through and gives the right answer:

```m2
A = ZZ/3[x]/(x^3+2*x+1);
B = ZZ/3[y]/(y^6+2*y^4+y^2+2*y+2);
G = map(B,A,{y^28});
first pushFwd G      -- a free module of rank 2
```

Rank 2 is correct: $[\mathbb{F}_{729} : \mathbb{F}_{27}] = 6/3 = 2$. So the computation is available and
only the `GF` spelling cannot reach it.

The error comes from `coefficientRing` being asked of a `GaloisField`:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/rings.m2#L56

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/1-GF.m2` opens with a
five-item checklist of "current problems with galois fields", carrying its own status markers:

```
-- (1) inclusion maps from one GF to another are difficult FIXED
-- (2) ring maps display with internal confusing information FIXED
-- (3) no factorization TO BE DONE
-- (4) no gcd's FIXED
-- (5) module structure of one GF wrt a smaller one TO BE DONE
--     this should be a special case of pushForward.
```

Four of the five are now done. Item (3) has been delivered since the file was written —
`factor` over `GF(729)[x]` splits `(x^2-a)(x-1)` correctly and the factorization multiplies back — and
items (1), (2) and (4) all check out, including the line the file flags at its item (1) as still
erroring, `F a` for a map between two Galois fields, which now returns a value. Item (5) is this issue.

Adjacent but distinct: [#4435](https://github.com/Macaulay2/M2/pull/4435) is open work by the same
author on letting Core `pushForward` accept more ring maps, but its blocker is a degree-map check
raising `not implemented yet: unexpected degree map of ring map`, and its diff contains no handling for
Galois fields.
