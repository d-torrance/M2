A smaller and deterministic reproducer for the second half of this issue, found while triaging a
different file in the same directory.

```m2
needsPackage "NumericalAlgebraicGeometry"
n = 3;
R = CC[x_1..x_n, y_1..y_n];
couple = i -> sum(toList(1..n) - set{i}, j -> x_i*y_j - x_j*y_i) - y_i;
I = ideal({sum(1..n, i -> y_i)}
    | apply(toList(1..n-1), i -> couple i)
    | apply(toList(1..n), i -> x_i^2 + y_i^2 - 1));
numericalIrreducibleDecomposition I
```

```
witness-set.m2:26:10:(2):[19]: error: check failed
 -- 1.05924s elapsed
```

Six equations in six unknowns, about a second, and it failed on **6 of 6** runs in separate processes
with independent seeds — where the sphere example already in this issue fails 2 times in 6 and
otherwise runs past four minutes.

It is not a size effect. The same family at `n = 2` and `n = 4` both succeed:

| n | unknowns | equations | Bézout | result | elapsed | RSS change |
| --- | --- | --- | --- | --- | --- | --- |
| 2 | 4 | 4 | 8 | `dim 0`, six points | 0.43 s | +7.7 MB |
| 3 | 6 | 6 | 32 | **`check failed`** | 1.06 s | — |
| 4 | 8 | 8 | 128 | `dim 0` and `dim 2`, the latter of degree 14 | 5.47 s | −23.5 MB |

These are Kuramoto-oscillator equilibria on a complete graph: each `x_i^2 + y_i^2 - 1` is a circle,
each coupling term is `x_i y_j - x_j y_i`, and the one linear equation normalises the frequencies.
That is a standard benchmark family for numerical algebraic geometry rather than anything contrived,
which is part of why the `n = 3` failure seems worth having.

### Where it came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36).
`bugs/anton/MEMORY-LEAKS/mike-NAG.m2`, from 2019, runs the `n = 7` member of this family and notes
that `numericalIrreducibleDecomposition` there *"uses > 20 GB after several hours"*. That particular
claim can no longer arise: the file builds its ideal over `ZZ/nextPrime 10000`, and the call is now
refused immediately —

```
NAGtypes.m2:191:20:(2):[13]: error: expected coefficients that can be converted to complex numbers
```

— in 0.00013 s, so nothing runs at all. Rebuilding the same system over `CC` and shrinking it is what
produced the table above, and no member of it showed memory growth.
