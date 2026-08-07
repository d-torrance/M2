Still reproduces. A non-square matrix gets an "inverse" instead of an error:

```m2
i1 : inverse matrix {{1,2}}
o1 = | 1 |
     | 0 |
                2        1
o1 : Matrix ZZ  <--- ZZ

i2 : (matrix {{1,2}})^-1              -- same answer by the other spelling
```

while the transpose is diagnosed correctly:

```m2
i3 : inverse matrix {{1},{2}}
stdio:3:1:(3): error: matrix not invertible
```

So one orientation errors and the other returns a matrix that is not an inverse of anything —
`matrix {{1,2}} * matrix {{1},{0}}` is the 1×1 identity, but the product the other way is not the 2×2
identity, which is what "inverse" claims.

### Why the two orientations differ

The computation is a solve rather than an inversion: for a wide matrix a right inverse exists and is
found, for a tall one nothing satisfies the system and the error fires. The bug is that `inverse` does
not first insist on a square matrix, so a one-sided solution is returned under a name that promises a
two-sided one.

### Related

**#3738** reaches a wrong inverse by a different path, so the two are worth reading together but are
not the same defect.
