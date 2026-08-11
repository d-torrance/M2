`(quotient, Ideal)` shares a documentation node with `comodule`, and that node says the output is a
module. It is not — for an ideal, `quotient` returns a ring:

```m2
i1 : R = QQ[a..d];

i2 : I = ideal(a,b,c,d^3);

o2 : Ideal of R

i3 : quotient I

           R
o3 = -------------
                3
     (a, b, c, d )

o3 : QuotientRing

i4 : comodule I

o4 = cokernel | a b c d3 |

                            1
o4 : R-module, quotient of R
```

The node keys four methods together and promises a `Module` for all of them:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/Macaulay2Doc/functions/comodule-doc.m2#L5-L27

Three of the four deliver that. `(quotient, Ideal)` returns a `QuotientRing`, so the `Usage` line
`comodule M` / `quotient M` presents two things as interchangeable when they are not, and the
`Outputs` clause is wrong for one of the four keys. The node's only example is `comodule I`, so the
rendered page never shows the case that misbehaves.

The declared return type in the source is wrong in the same way:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/matrix1.m2#L438-L439

`quotient Ideal := Module => opts -> I -> (ring I) / I` — annotated `Module`, returning a
`QuotientRing`. The neighbouring `comodule Ideal := Module => I -> cokernel generators I` on the line
above is correct.

### A second, softer point

The bug file adds *"also, the quotient page should perhaps say something"*, which seems worth passing
on. `quotient` carries three unrelated meanings, and nothing orients a reader among them:

* `quotient(I, J)` — the colon ideal $I : J$
* `quotient I` — the quotient ring `(ring I)/I`
* `quotient M` — the comodule of a module

The first is documented under its own nodes, the last two share the `comodule` page, and the middle
one is the one documented incorrectly.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/0-doc-quotient.m2` is three
lines long and says only *"doc for (quotient,Ideal) is wrong"* — which is still accurate, and now has
a location.
