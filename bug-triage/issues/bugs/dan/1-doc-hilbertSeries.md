The specific documentation gap the file names is still there.

All three operations work. On `monomialCurveIdeal(QQ[a..d], {1,3,4})`:

```m2
i1 : hilbertSeries (R/I)            -- returns a Divide
i2 : numerator oo                   -- 1-T^2-3T^3+4T^4-T^5
i3 : denominator hilbertSeries (R/I) -- (1-T)^4
i4 : value oo                       -- 1-4T+6T^2-4T^3+T^4
```

But `help hilbertSeries` mentions **none** of them. Its See-also list is `degreesRing`,
`reduceHilbert`, `poincare`, `poincareN`, `hilbertPolynomial`, `hilbertFunction`. The file's request is
*"Need to add references to numerator, value denominator, etc."*, and that is exactly what is absent.

### Why the current text is not enough

The prose does say the result is "a type of expression called a `Divide`", which is the hint — but a
reader then has to guess that `numerator`, `denominator` and `value` apply to it. Naming them is the
difference between knowing the type and being able to use the result.

### Neighbouring open issues, neither of them this

**#974** (`value` of a Hilbert series failing because the parts live in a Laurent ring) and **#1701**
(`hilbertSeries MonomialIdeal`).
