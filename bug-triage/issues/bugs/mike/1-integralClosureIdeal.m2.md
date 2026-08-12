`integralClosure I` can be intractable where `integralClosure(J, d)` returning the same ideal is instant,
because the one-argument form always passes the ideal it is handed straight to the Rees algebra.

Over `ZZ/101[a,b]`, with `i = ideal(a^4, a^3*b, a*b^3, b^4)`:

```m2
i2 = i^2;                      -- 9 monomial generators, a^8 .. b^8
integralClosure i2             -- no result after 240 seconds
integralClosure(i, 2)          -- 0.26 seconds
```

Both compute the same ideal. The second argument of `integralClosure(Ideal, ZZ)` is the power, so
`integralClosure(i, 2)` *is* the integral closure of `i^2`, and it comes back with

```
ideal(b^8, a*b^7, a^2*b^6, a^3*b^5, a^4*b^4, a^5*b^3, a^6*b^2, a^7*b, a^8)
```

which is `i^2` itself — so the answer to the whole question is that `i^2` is already integrally closed,
reached instantly by one entry point and not at all by the other.

For a check on what the second argument means, independent of this example, the closure of
`ideal(a^3, b^7)^2 = ideal(a^6, a^3*b^7, b^14)` is the monomials `a^i*b^j` with `i/6 + j/14 >= 1`, and that is
exactly what `integralClosure(ideal(a^3,b^7), 2)` returns:

```m2
i1 : integralClosure(ideal(a^3,b^7), 2)

o1 = ideal (a^6, a^5*b^3, a^4*b^5, a^3*b^7, a^2*b^10, a*b^12, b^14)
```

### Where the difference comes from

The one-argument and two-argument forms are both thin wrappers on the same three-argument method:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/IntegralClosure.m2#L873-L875

So `integralClosure i2` runs `integralClosure(i2, (i2)_0, 1)` while `integralClosure(i, 2)` runs
`integralClosure(i, i_0, 2)`. That method trims its input and hands it to `integralClosureOfIdeal`, which
builds `reesAlgebra(I, a)` and takes the integral closure of *that* ring. The two calls therefore differ in
the size of the ring being normalized: a Rees algebra on `i2`'s 9 generators with denominator `a^8`, against
one on `i`'s 4 generators with denominator `a^4`. Reading the source, that is where the cost goes — this is
not a profile, and the slow call was never allowed to finish, so it is possible the expense lies elsewhere
inside the normalization.

Two smaller observations from the same runs:

* `integralClosure(i2, 1)` also fails to finish in 240 s, which is consistent with the above: it is the same
  call as `integralClosure i2`.
* `integralClosure i` itself is instant (0.25 s), returning `ideal(b^4, a*b^3, a^2*b^2, a^3*b, a^4)`. The
  difficulty appears with the squared generating set, not with the ring or the field.

### Why this is worth an issue rather than a note about expected cost

`i` is a monomial ideal in two variables whose closure is itself. Nothing about the input suggests it is
hard, a user has no way to tell an intractable call from a hung one, and the cheap route is only reachable if
you happen to know the ideal is a power *and* know that the `ZZ` argument means the exponent. A caller
holding `i2` and not its square root has no way in at all.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/1-integralClosureIdeal.m2` is a
scratch file of `integralClosure` examples; the line

```m2
integralClosure (i^2) == i^2 -- i^2 is already integrally closed
  -- this line above doesn't seem to finish in small time
```

records the same observation, undated but from a file whose surrounding timings are annotated
"the timings here refer to some older version of integral closure".
