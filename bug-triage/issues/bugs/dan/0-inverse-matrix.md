### What the file reports

An inverse only exists for a square matrix. `inverse` — and `M^-1`, which is the same operation
spelled as a power — never checks, so a 1×2 matrix comes back with a 2×1 "inverse". The stray `o34`
line at the top of the file belongs to an unrelated example, since Dan was collecting several things
he thought should error; the `inverse` lines are the one this issue is about.

### It still behaves that way

```m2
i1 : inverse matrix {{1,2}}
o1 = | 1 |
     | 0 |
                2        1
o1 : Matrix ZZ  <--- ZZ

i2 : (matrix {{1,2}})^-1              -- the same answer, by the other spelling

i3 : inverse matrix {{1},{2}}         -- the tall matrix, correctly refused
stdio:3:1:(3): error: matrix not invertible
```

So a wide matrix gets an answer, a tall one gets an error, and the answer is not an inverse of
anything: `matrix {{1,2}} * matrix {{1},{0}}` is the 1×1 identity, but the product the other way round
is not the 2×2 identity. The word "inverse" claims both.

### Why the two orientations differ

What `inverse` computes is a solve rather than an inversion. For a wide matrix a right inverse exists
and is found; for a tall one nothing satisfies the system, so the error fires. The defect is that
`inverse` never insists on a square matrix first, so a one-sided solution is returned under a name
that promises a two-sided one.

### Related

**#3738** reaches a wrong inverse by a different path, so the two are worth reading together but are
not the same defect.
