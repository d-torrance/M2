Fractions are not put in lowest terms when the base ring has quotient relations, so an arbitrarily
large integer factor is carried in both numerator and denominator.

```m2
i1 : R = frac(QQ[x]/(x^5-2)[y]);

i2 : F = (x/(y-2) + 1 + x*y)^4;

i3 : denominator F

                                           4                                          3
o3 = 1443087550851217253118659088405235200y  - 11544700406809738024949272707241881600y  - ...
```

That leading coefficient has 37 digits. The same computation over a base ring without relations comes
out in lowest terms:

```m2
i4 : S = frac(QQ[z]);

i5 : denominator (1/(z-2) + 1 + z)^4

      4     3      2
o5 = z  - 8z  + 24z  - 32z + 16
```

The factor is exactly removable. Writing `C` for that 37-digit integer, `denominator F == C*(y-2)^4`
is true, `numerator F` is divisible by `C`, and dividing it out leaves

```
x^4*y^8 + (-8*x^4+4*x^3)*y^7 + (28*x^4-32*x^3+6*x^2)*y^6 + ...
```

with coefficients of one or two digits. So `F` differs from its reduced form only by `C/C`.

### Cause

`FractionField::simplify` only cancels when `use_gcd_simplify` is set, and the constructor clears that
flag whenever the base ring is a quotient:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/e/rings/frac.cpp#L38-L46

`frac(QQ[x]/(x^5-2)[y])` flattens to a ring with one relation, so `use_gcd_simplify` is false, the
branch holding the content division — `divide_by_given_content` on numerator and denominator — is
skipped, and nothing removes the common factor. Two lines below the flag is set there is an
acknowledged `#warning "frac simplify: doesn't handle towers of fracs"`.

This looks like a consequence of `gcd` being unavailable over such rings rather than an independent
decision; see [#4622](https://github.com/Macaulay2/M2/issues/4622), where `gcd` refuses over an
extension because `factoryAlmostGood` walks past the defining relation.

### Relation to other work in this area

* [#4462](https://github.com/Macaulay2/M2/pull/4462) is open and reworks `frac.cpp`. It adds
  `simplify_unit_denominator`, which cancels when the denominator is a **unit**. The denominator here is
  `C*(y-2)^4`, which is not, so that path returns early and control reaches the same disabled branch.
  On reading the diff it therefore appears not to fix this — but that branch was not built and tested,
  so treat this as a reading rather than a measurement.
* [#3172](https://github.com/Macaulay2/M2/issues/3172) and
  [#3173](https://github.com/Macaulay2/M2/pull/3173) concern the same construction crashing when the
  gcd fails during simplification, which was made to raise instead.
* [#4461](https://github.com/Macaulay2/M2/issues/4461) is about canonical form, `1/2*y` against `y/2`.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/1-big-coeffs-in-fractions`
records a session whose output carries coefficients such as `438345528176005822616124236928000`,
which is what the file is named for.
