`monomialIdeal` picks up a spurious generator when the coefficient ring is a quotient, and the ideal
it produces is internally inconsistent — it contains a unit but does not contain 1.

```m2
i1 : A = QQ[w]/(w^4+w^3+w^2+w+1);

i2 : R = A[x,y];

i3 : I = monomialIdeal(x)

                          3    2
o3 = monomialIdeal (x, - w  - w  - w - 1)

o3 : MonomialIdeal of R

i4 : numgens I

o4 = 2

i5 : w^4 % I == 0

o5 = true

i6 : 1_R % I == 0

o6 = false

i7 : y % I == 0

o7 = false
```

`monomialIdeal(x)` should have one generator. The second is not a display artifact — `numgens` is 2 —
and it is a unit: in `A`, `w^4 + w^3 + w^2 + w + 1 = 0`, so `- w^3 - w^2 - w - 1` is `w^4`, and
`isUnit(w^4)` is true. An ideal containing a unit is the whole ring, yet `1_R` and `y` both fail to
reduce to zero in it. Whatever `I` is, it is not an ideal.

### The trigger is a quotient in the coefficient ring

| ring | `monomialIdeal` of the first variable |
| --- | --- |
| `QQ[u,v]` | `monomialIdeal u` |
| `(ZZ/5)[e,f]` | `monomialIdeal e` |
| `(QQ[p])[q,r]` | `monomialIdeal q` |
| `(QQ[w]/(w^4+w^3+w^2+w+1))[x,y]` | `monomialIdeal(x, -w^3-w^2-w-1)` |
| `(QQ[s]/(s^2))[t_1,t_2]` | `monomialIdeal(t_1, 0)` |

A flat ring, a finite field and a tower without a quotient are all fine. The last row makes the
mechanism plain: for `QQ[s]/(s^2)` the relation reduces to `0` in the quotient, and a zero generator
appears — so what is being added is the coefficient ring's defining relation.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/0-rawMonomialIdeal` reports
the same thing through the raw interface:

```m2
i43 : R = (QQ[w]/(w^4+w^3+w^2+w+1))[x,y];

i44 : newMonomialIdeal(R,rawMonomialIdeal(raw matrix {{x}}, 0))

                           3    2
o44 = monomialIdeal (x, - w  - w  - w - 1)
```

with the note *"this answer appears wrong, because of the appearance of the extra constant"*. That is
still exactly what happens, and the top-level `monomialIdeal` does it too, so it is not confined to
the raw call.

Worth noting for whoever picks this up: [#1589](https://github.com/Macaulay2/M2/issues/1589) proposes
removing `MonomialIdeal` from the top level in favour of hooks on `Ideal`, so a fix here may want to
account for that direction.
