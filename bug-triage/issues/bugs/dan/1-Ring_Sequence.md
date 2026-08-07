Not installed: `R_(1..3)` still errors with "no method for binary operator `_`" on
`(PolynomialRing, Sequence)`.

### This is convenience rather than a missing capability

Two spellings already do the job, one of them literally the file's own body:

```m2
i1 : R = QQ[a,b,c,d];
i2 : (R_*)_{1,2,3}
o2 = {b, c, d}
i3 : apply(1..3, i -> R_i)
o3 = (b, c, d)
```

`R_*` postdates the file and covers most of what David liked about the idea.

### One detail for whoever takes it

The adjacent slot is occupied, and differently: `R_{1,2,3}` means the **monomial with that exponent
vector**, `a*b^2*c^3`. So `Ring _ List` is not available, while `Ring _ Sequence` is free — which is
convenient, but means the two bracket spellings would mean quite different things, and that asymmetry
should be documented if this goes in.

**#3515** is a different notation request (partial application of functions), not this.
