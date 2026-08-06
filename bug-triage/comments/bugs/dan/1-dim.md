`dim` reaches this by a second route, with the identical message:

```m2
i1 : R = ZZ[x];

i2 : rank coker vars R
stdio:2:4:(3):[1]: error: codim: expected an affine ring (consider Generic=>true to work over QQ)

i3 : dim(R/ideal(promote(2,R)))
stdio:3:3:(3):[1]: error: codim: expected an affine ring (consider Generic=>true to work over QQ)
```

So it is not only `rank` that has no way to pass `Generic => true` through — `dim` of a quotient of `ZZ[x]` is blocked by the same plumbing, and the answers are not in doubt: `ZZ[x]/2` is `(ZZ/2)[x]`, of dimension 1, and `ZZ[x]/(2,x)` is `ZZ/2`, of dimension 0.

This comes from a 2011 bug file (`bugs/dan/1-dim`) reporting the `dim` cases, attributed there to Bart Snapp. Its four examples still behave as follows on 1.26.06-40-gd8e86d689d:

| | |
| --- | --- |
| `dim(A/ideal(2))` | `error: expected ideal of the same ring` |
| `dim(A/ideal(two))` | `codim: expected an affine ring` |
| `dim(A/ideal(2,x))` | `codim: expected an affine ring` |
| `dim(A/ideal(two,x))` | `codim: expected an affine ring` |

where `A = ZZ[x]` and `two = promote(2,A)`. The first is a separate and more arguable matter — `ideal 2` on its own is an ideal of `ZZ`, not of `A`, so the quotient is rejected before `dim` is reached. The other three build the quotient without complaint and then fail in `codim`.

The bug file ends "at least now, an error is given. can we compute the right answer?", which is still the open question, and it looks like the same one this issue asks.
