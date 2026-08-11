The [`"exterior algebras"`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/Macaulay2Doc/ov_rings.m2#L1242-L1257)
documentation node states a degree-dependent commutation rule:

> An exterior algebra is a polynomial ring $R$ where multiplication of the variables obeys the
> commutation relation $xy = (-1)^{\textrm{deg}(x)\textrm{deg}(y)}yx$. One notable consequence of this
> is that if $\textrm{deg}(x)$ is odd, then $x^2 = 0$.
>
> Here, $\textrm{deg}(x)$ is the degree of $x$ — or the first degree of $x$ in case $R$ is
> multi-graded.

The implementation ignores the degrees: every skew-commutative variable anticommutes with every
other, and every one squares to zero, whatever its degree. The two agree when all degrees are 1 —
which is the default, and every other example on that page — and disagree as soon as any degree is
even.

This is visible on the page's own example ring, from
[a few lines further down](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/Macaulay2Doc/ov_rings.m2#L1277),
where `a` and `b` have degree 2:

```m2
i1 : R = QQ[a,b,r,s,t, SkewCommutative=>true, Degrees=>{2,2,1,1,1}]

o1 = R

o1 : PolynomialRing, 5 skew commutative variable(s)

i2 : r*a == a*r

o2 = false

i3 : a*b == b*a

o3 = false

i4 : a*a

o4 = 0

o4 : R
```

By the documented rule, `i2` should be true — $(-1)^{1\cdot 2} = +1$ — and so should `i3`, since
$(-1)^{2\cdot 2} = +1$. And `a` has even degree, so nothing in the stated rule forces `a*a` to
vanish. All three come out the other way.

### Which side is wrong is not obvious from here

Both behaviours are defensible and both are old, so this is reported as a contradiction rather than
as a diagnosis:

* If `SkewCommutative` is meant to build a plain **exterior algebra**, the implementation is right and
  the rule sentence should be corrected or restricted to the all-degrees-1 case. The option's name and
  25 years of consistent behaviour point this way.
* If it is meant to build a **graded-commutative** (super) algebra, the documentation is right and
  even-degree generators should commute and not square to zero. That reading would change results for
  every exterior algebra with non-unit degrees, so it is not something to do lightly.

The documented rule is not a recent slip. The current phrasing dates from
[#4252](https://github.com/Macaulay2/M2/pull/4252) (2026-05-03), but that PR only restyled the node —
it changed one file, `ov_rings.m2`, and no code. The sentence it replaced said the same thing:

```
mildly non-commutative, in that, for every x and y in the ring,
y*x = (-1)^(deg(x) deg(y)) x*y, and that for every x of odd degree, ...
```

which goes back to commit `e4d91062d9`, 2001-05-18.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/0-SkewCommutative` records
the same observation with a degree-2 variable and asks the question this issue is passing on:

```m2
i18 : QQ[a,b,c,SkewCommutative =>true, Degrees=>{1,2,3}]

i20 : b^2

o20 = 0					<<<<<< is this really what we intend ?
```

Worth noting that the same author wrote both that file and, in 2001, the documented rule it appears
to contradict.

Not the same as [#3123](https://github.com/Macaulay2/M2/issues/3123), which is about variables of a
base exterior algebra anticommuting with those of an extension in a *tower* — a different convention
question, and independent of degrees.
