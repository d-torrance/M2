`discriminant(f, x)` returns the resultant of `f` and `f'` without either of the normalizations that
turn a resultant into a discriminant, and the documentation does not say so. The standard definition is

$$\operatorname{disc}(f) \;=\; \frac{(-1)^{n(n-1)/2}}{a_n}\,\operatorname{Res}(f, f')$$

for $f$ of degree $n$ with leading coefficient $a_n$. M2 returns $\operatorname{Res}(f, f')$:

```m2
i1 : R = QQ[a,b,c,x]

o1 = R

o1 : PolynomialRing

i2 : discriminant(x^2+b*x+c, x)

        2
o2 = - b  + 4c

o2 : R

i3 : discriminant(a*x^2+b*x+c, x)

          2     2
o3 = - a*b  + 4a c

o3 : R
```

`o2` is the negative of the familiar $b^2-4c$, and `o3` is $-a\,(b^2-4ac)$ — off by the sign and by the
leading coefficient.

The sign discrepancy follows $(-1)^{n(n-1)/2}$ exactly, so it appears at some degrees and not others.
Against the product-of-differences formula:

| degree | exponent $n(n-1)/2$ | expected factor | M2 |
| --- | --- | --- | --- |
| 2 | 1 | $-1$ | `disc((x-a)(x-b))` $= -(a-b)^2$ |
| 3 | 3 | $-1$ | `disc((x-a)(x-b)(x-c))` $= -\prod^2$ |
| 4 | 6 | $+1$ | `disc` of the quartic $= +\prod^2$, agrees |

Degree 4 agreeing is what rules out a plain sign error and points at the missing factor.

### Cause

The implementation is a single line, with no normalization applied:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/Elimination.m2#L127-L128

### The documentation does not disambiguate

The doc node gives the output only as *"the discriminant of `f` with respect to `x`"*:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/Elimination.m2#L214-L234

and its own example is a quadratic, so the rendered page displays the sign-flipped value with nothing
to explain it. A reader has no way to tell which convention is in force.

### Two ways to resolve, and they are not equivalent

Recorded without a recommendation, since the trade-off belongs to whoever owns this code:

* **Document the convention.** Cheapest, breaks nothing. `resultant(f, f')` is a legitimate quantity
  and callers have depended on this output for a long time.
* **Normalize the output.** Makes `discriminant` return the discriminant, and would change results for
  every existing caller — including at degrees where nothing looks wrong today, since the leading
  coefficient factor applies at every degree.

Not the same as [#4449](https://github.com/Macaulay2/M2/issues/4449), which is `discriminant` raising
`expected nonzero polynomials` when `diff(x,f)` vanishes identically in characteristic $p$.

Worth flagging for whoever picks this up: [#4472](https://github.com/Macaulay2/M2/pull/4472) is open
against that issue and rewrites this same line, adding an early `if diff(f,x) == 0 then return 0`
guard around the `resultant` call. It does not change the normalization, so the two are independent —
but they touch the same two lines and would want coordinating.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/0-discriminant` records the
degree 2 and 3 identities above and asks simply: *"Is the sign right?"*
