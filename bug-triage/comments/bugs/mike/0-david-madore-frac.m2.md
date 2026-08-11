An older instance of the same `/` versus `^-1` split, from 2008, where it used to be a crash rather
than an error.

```m2
i1 : R0 = (ZZ/32003)[u];

i2 : K0 = frac R0;

i3 : K = K0[v]/(v^2+u^2);

i4 : toField K;

i5 : 1/v
stdio:5:1:(3):[1]: error: not implemented yet: fraction fields of polynomial rings over rings other than ZZ, QQ, or a finite field

i6 : v^-1

        1
o6 = - --*v
        2
       u

o6 : K

i7 : 1//v

        1
o7 = - --*v
        2
       u

o7 : K
```

Same shape as `1/x_R` against `x_R^-1` in the report above, and consistent with the diagnosis in this
thread that `/` reaches for a fraction field while `^-1` looks for an inverse. `//` finds it too.

Two things this example adds.

**The inverse plainly exists, and the ring really is a field.** `v^2 = -u^2`, and $-1$ is not a square
mod 32003, so `v^2+u^2` is irreducible over `K0` — `factor` leaves it alone. So the failing case here
is not a ring that was wrongly declared a field; it is a genuine field in which `/` still cannot
divide.

**Whether it fails depends on something invisible.** Assigning the ring that `toField` returns makes
`1/v` work, because the assignment triggers `use`, which re-promotes `v` into the new ring:

```m2
L = toField K;   -- assigned:  1/v  gives -(1/u^2)*v
toField K;       -- discarded: 1/v  raises as above
```

`toField` returns a new ring rather than modifying its argument, so `toField K` on its own leaves `v`
belonging to the old, undeclared `K` — which is what the bug file does, and what makes it look like
`toField` had no effect.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36).
`bugs/mike/0-david-madore-frac.m2` records this from an email of David Madore, 4 May 2008, and both
lines were then hard crashes — annotated `-- actual crash` and, later, `v^-1  -- crashes! still
crashes, 22 May 2017`. Neither crashes now: `v^-1` returns the right answer and `1/v` raises. The
file's closing wish was *"it should be checked that a fraction field isn't over another fraction
field -- or better, get it to work!"*, and the first half is what the current error message does.
