When `toField` is applied to a ring that is not in fact a field, dividing by a zero divisor is
diagnosed well over a multivariate quotient and poorly over a univariate one. Adding an unused second
variable to the same ring changes both the error message and whether the offending element is
recorded:

```m2
i1 : A = ZZ/101[u]/(u^2-1);

i2 : k = toField A;

i3 : 1/(u-1)
stdio:3:1:(3):[1]: error: either element not invertible, or no method available to compute its inverse

i4 : getNonUnit k

i5 : B = ZZ/101[p,q]/(p^2-1);

i6 : l = toField B;

i7 : 1/(p-1)
stdio:7:1:(3):[1]: error: a non unit was found in a ring declared to be a field

i8 : getNonUnit l

o8 = p - 1

o8 : l
```

`u^2-1 = (u-1)(u+1)`, so `u-1` is a zero divisor and erroring is correct in both cases. The difference
is what the user is told. Over `B` the message names the actual situation and `getNonUnit` hands back
the element, which is exactly what that function exists for. Over `A` the message is the engine's
generic fallback and `getNonUnit` returns `null`, so there is no way to find out which element caused
it.

The same split appears with `QQ[s]/(s^2-1)` (generic message, `getNonUnit` null) versus
`ZZ/101[w,z]/(w*z)` (field-specific message, `getNonUnit` returns `w+z`), so it tracks the number of
variables rather than the coefficient ring or the shape of the relation.

### Where the paths diverge

The good path runs through `Ring::set_non_unit`, which records the element and then raises:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/e/rings/ring.cpp#L88-L95

The univariate path never reaches it, and errors from the generic fallback in `Ring::power` instead:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/e/rings/ring.cpp#L127-L139

Since `get_non_unit` returns zero unless `set_non_unit` has run, `getNonUnit` comes back empty.

This looks like the tail of the same machinery fixed in
[#4259](https://github.com/Macaulay2/M2/pull/4259) (`4cbb7fef7d`, 2026-05-05), which made
`set_non_unit` throw rather than only set the engine's error flag, and which set the recorded element
before throwing so that `getNonUnit` is populated on the way out. That fix works on the multivariate
path; the univariate one does not get there.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/0-GF-division.m2` works
through division in `toField` rings and ends:

```
-- bug: one of these operations should say something about finding a zero divisor!!
```

That request is now met over multivariate quotients and unmet over univariate ones. The rest of the
file has resolved: its first block, over the genuine field `ZZ/101[a]/(a^2+a+1)`, passes all six of
its assertions, and the two assertions it marks `-- still failing` in the second block are both asking
for the wrong thing — `1//(1-a)` returning `0` is correct division with remainder, since
`1%(1-a) == 1` and `0*(1-a) + 1 == 1`, and `1/(a-1)` failing an equality test is a consequence of its
raising, which it should.
