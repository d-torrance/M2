<!-- issue: #3973 -->
The same entry point fails in the zero ring, and there the element being refused really is invertible.

```m2
i1 : R = QQ[x]/1;

i2 : 1_R == 0_R

o2 = true

i3 : isUnit x

o3 = true

i4 : x * (1//x) == 1

o4 = true

i5 : x^0

o5 = 0

i6 : x^-1
stdio:6:2:(3): error: either element not invertible, or no method available to compute its inverse

i7 : 1_R^-1
stdio:7:5:(3): error: either element not invertible, or no method available to compute its inverse
```

The zero ring is correct to call `x` a unit: `1 = 0` there, so `0 * 0 = 0 = 1`. The inverse being
withheld is `0`, which `1//x` returns without complaint. Note `i7` — the ring cannot invert its own
identity element.

Four parts of M2 already agree, including the matrix path:

```m2
i8 : inverse matrix {{x}}

o8 = map(R^1, R^1, 0)
```

So `inverse` on a 1×1 matrix over `R` gives the right answer while `x^-1` on its only entry does not.

### Why it is the same defect as this issue

`Ring::power`, on a negative exponent, calls `invert` and then uses `is_zero` on the result as its
signal that `invert` failed:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/e/rings/ring.cpp#L127-L139

In the zero ring a correct inverse *is* zero, so the sentinel cannot tell success from failure. This
issue's `r^-1` reaches the same negative-exponent path and fails a little earlier, inside `invert`
itself; the shared route is `Ring::power`, which is why this is being added here rather than filed
separately.

That test was introduced by
[`d97f303f2d`](https://github.com/Macaulay2/M2/commit/d97f303f2d) (2006-09-23, "fixed: inverses,
negative powers of elements"), the commit that first taught `Ring::power` to handle a negative
exponent at all.

### It is not only pathological input

Nobody writes `QQ[x]/1`. But the zero ring is what a quotient becomes whenever the ideal turns out to
be everything, which is something a Gröbner basis computation discovers rather than the user
declaring:

```m2
i1 : A = QQ[x,y];

i2 : R = A/ideal(x, 1+x);

i3 : 1_R == 0_R

o3 = true

i4 : u = y_R;

i5 : isUnit u

o5 = true

i6 : u^-1
stdio:6:2:(3): error: either element not invertible, or no method available to compute its inverse
```

`ZZ/1` and `ZZ/5[]/1` behave the same way, so it tracks the zero ring rather than any particular
presentation.

### Two neighbours that are already fixed

The same sentinel used to misfire in the opposite direction — reporting success where it should have
failed — and both of those are genuinely resolved, verified on 1.26.06:

* [#1314](https://github.com/Macaulay2/M2/issues/1314), where `t^-1` in `QQ[t]/t^3` returned `1`, now
  raises.
* [#2208](https://github.com/Macaulay2/M2/issues/2208)'s inverse of `0` over `ZZ/5` now raises.

So `invert` was corrected in those cases and the zero ring is what the sentinel still gets wrong.

### Where this came from

`bugs/mike/1-non-unit-problem-in-zero-ring`, one of the 857 files removed with the `bugs/` directory
in [`d2c8d27826`](https://github.com/Macaulay2/M2/commit/d2c8d27826) and catalogued in
[#36](https://github.com/Macaulay2/M2/issues/36). The file is the transcript alone:

```text
    i5 : R = QQ[x]/1

    o5 = R

    o5 : QuotientRing

    i6 : x^-1
    stdio:6:2:(1):[0]: element is not invertible
```

The message has been reworded since; the behaviour has not. Its wording matches `d97f303f2d`'s
`ERROR("element is not invertible")` exactly, which dates the file to after September 2006.
