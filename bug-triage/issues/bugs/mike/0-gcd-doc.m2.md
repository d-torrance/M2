`gcd` refuses to work over a field extension built with `toField`, while working over the `GF`
spelling of the same field. Both rings below are fields of order 81:

```m2
i1 : A = GF(3,4, Variable => a);

i2 : R = A[x];

i3 : F = (a*x^3-2*a*x^2-x-1)*(x-a);

i4 : G = (a*x^3-2*a*x^2-x-1)*(x-a^2);

i5 : gcd(F,G)

      3    2       3    2      3    2
o5 = x  + x  + (- a  + a )x - a  + a

o5 : R

i6 : B = toField(ZZ/3[b]/(b^4-b^3-1));

i7 : S = B[y];

i8 : FF = (b*y^3-2*b*y^2-y-1)*(y-b);

i9 : GG = (b*y^3-2*b*y^2-y-1)*(y-b^2);

i10 : gcd(FF,GG)
stdio:10:3:(3):[1]: error: expected coefficient ring of the form ZZ/n, ZZ, QQ, or GF
```

`b^4-b^3-1` is irreducible over `ZZ/3` — `factor` returns it whole — so `toField` was told the truth
and `B` is a field. The same refusal occurs with a characteristic-zero base, e.g.
`toField(QQ[a]/(a^6-a^3-1))`.

### The documentation promises otherwise

The `gcd` node takes `ZZ`, `QQ` or any `RingElement`, and says nothing about the coefficient ring:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/Macaulay2Doc/operators.m2#L208-L236

Its examples are over `ZZ` and `QQ[x,y,z]`. There is no `Caveat`, so a reader has no way to learn that
the coefficient ring must be one of `ZZ/n`, `ZZ`, `QQ` or `GF` — which is the file's own complaint,
*"doc is lacking"*, still true after fifteen years.

### Three spellings, three behaviours

Worth seeing together, because the middle row is a separate open issue and the difference between the
rows is the point:

| ring | `gcd` |
| --- | --- |
| `GF(3,4)[x]` | correct |
| `(QQ[a]/(a^6-a^3-1))[t]`, no `toField` | returns `1` — a common divisor that is not greatest ([#4583](https://github.com/Macaulay2/M2/issues/4583)) |
| `toField(...)[x]`, any base | errors as above |

So the undeclared quotient answers wrongly and the declared field refuses; only `GF` works. A fix for
[#4583](https://github.com/Macaulay2/M2/issues/4583) would not necessarily address this one, since
that issue is about `gcd` proceeding when it should not.

### It may be a lost capability rather than a missing one

The bug file asserts these cases *succeed*, twice annotating them *"this calls the gcd via syzygies
routine"*:

```m2
A = toField(ZZ/3[a]/(a^4-a^3-1))
R = A[x]
F = (a*x^3-2*a*x^2-x-1)*(x-a)
G = (a*x^3-2*a*x^2-x-1)*(x-a^2)
assert(gcd(F,G) == a^-1 * (a*x^3-2*a*x^2-x-1)) -- this calls the gcd via syzygies routine
```

If a syzygy fallback once handled this, restoring it may be cheaper than extending the dispatch.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/0-gcd-doc.m2` is a 120-line
file whose header lists three complaints — *"doc is lacking"*, *"rawGCDRingElement is not being called
from the front end"*, and that the GF code *"works if the ring is declared via GF"*. The second is
[#4583](https://github.com/Macaulay2/M2/issues/4583); the first and third are this issue. One of its
other claims has since been fixed: over `GF(3,20)` it records both `gcd` and `factor` as *"gives
ERROR"*, and both work now.
