<!-- issue: #3449 -->
This limitation was reported independently twenty years ago, and the route to it may be worth recording here: it was hit
while trying to compute the kernel of a ring map, rather than while inverting a variable directly.

From `bugs/dan/0-bugs-iswanson.m2`:

```m2
R = ZZ/101[x,y]
K = frac R
S = K[u,v]
I = ideal(u^2, u*v, v^2)
gb I
A = ZZ/101[a,b,c]
f = map(frac S, A, {u^3/v^4, u^2/v^2, (u+v)/v^4})
kernel f

-- the map above gives error message
```

The reporter attributed the failure to the map, which is the natural reading — the error appears on the `map` line. It is
not the map. `frac S` is where it stops, before any map exists:

```m2
i1 : R = ZZ/101[x,y]; K = frac R; S = K[u,v];

i4 : frac S
stdio:4:1:(3): error: not implemented yet: fraction fields of polynomial rings over rings
              other than ZZ, QQ, or a finite field
```

raised from `m2/enginering.m2:350` — the same message as in this issue, since `K` is itself a fraction field and so is
none of `ZZ`, `QQ` or a finite field.

What this adds beyond a second reproducer is the use case. Building the total ring of fractions of `frac(k[x,y])[u,v]` in
order to define a map from a polynomial ring and take its kernel is a natural thing to want, and it is inaccessible for
this reason. The other half of the same file's request — the same construction over `frac(r)` for a plain polynomial ring
`r`, with `MonomialOrder => Eliminate 4` — works fine today and returns a 20-generator ideal, so the obstruction really is
the iterated fraction field rather than anything about kernels of ring maps.

### Where this came from

`bugs/dan/0-bugs-iswanson.m2`, one of the 857 files removed with the pre-GitHub `bugs/` tree in d2c8d27826 and catalogued
in #36. It is recorded as a duplicate of this issue rather than filed separately. The file's seven other requests are
settled: six are met, including the monomial exponent limits, which now cap at exactly the `2^15-1` and `2^7-1` the file
asked for and raise errors instead of wrapping silently to `1`.
