<!-- issue: #3961 -->
The mechanism here turns out to explain both the wrong `true` and the case that works.

`isWellDefined RingMap` checks one thing — that the relations of the source die:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/newring.m2#L259-L265

That is why `isWellDefined map(ZZ, ZZ/5)` gets it right and `isWellDefined map(ZZ[x], (ZZ/5)[x])`
does not. Asked directly, `ZZ/5` is a quotient, so the relation is there to be tested:

```m2
i1 : (ambient(ZZ/5), ideal(ZZ/5))

o1 = (ZZ, ideal 5)
```

`g I == 0` then asks whether `5` maps to zero in `ZZ`. It does not, so the answer is `false`, and for
the right reason. But once `ZZ/5` is a *coefficient* ring, the relation is no longer in the
flattening:

```m2
i1 : A = (ZZ/5)[y];

i2 : ambient first flattenRing(A, Result => 3)

o2 = A

i3 : ideal first flattenRing(A, Result => 3)

o3 = ideal ()
```

The flattened ring is `A` itself and `I` is zero, so `g I == 0` is vacuous and nothing ever looks at
the coefficient ring. The check is not so much ignoring the coefficient ring as never having been
given it.

### Two more instances

The same vacuous test passes for a source whose coefficient ring is not presented as a quotient at
all, so no relation could encode the obstruction even in principle — `flattenRing(QQ[x])` also gives
`ideal ()`:

```m2
i4 : isWellDefined map(ZZ, QQ[x], {1})

o4 = true

i5 : (map(ZZ, QQ[x], {1})) (1/2*x)
stdio:5:21:(3):[1]: error: cannot map rational to this ring
```

The second one is a different failure mode from the one reported here — it does not answer at all:

```m2
i6 : isWellDefined map(RR_53, CC[u], {1})
stdio:6:13:(3):[1]: error: cannot coerce CC value to ring type
```

That comes out of `Core/ringmap.m2` while applying the composite `g`, so `isWellDefined` raises
rather than returning `false`. Worth separating from the `true` case: code guarding with `if
isWellDefined f then ...` copes with a wrong `false`, but not with an error.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/anton/LINEAR-ALGEBRA/CCsubRR.m2`
annotated each of its lines with what was right and wrong, and the concrete misbehaviour it recorded
is gone: what it marked `crashes in linalg branch, gives 0 in 1.6.  Both are wrong...!` now errors
cleanly instead of crashing or returning zero, and the two it marked `just wrong` error rather than
returning a wrong answer. What survives is its twice-repeated

    REALLY: should give an error: cannot construct ring map from R --> ZZ

which is the enforcement question in #1011 rather than anything here. The reason for commenting on
this issue instead is the paragraph above: the tool for detecting the thing #1011 declines to enforce
is wrong on exactly these maps, and one of them it cannot answer at all.
