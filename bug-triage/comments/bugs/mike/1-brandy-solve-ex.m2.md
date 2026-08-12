<!-- issue: #4556 -->
Evidence on the open question in the footnote above — whether the zeros are unintended rather than a
deliberate convention for inexact fields.

They were reported as a bug in 2008. `bugs/mike/1-brandy-solve-ex.m2`, one of the files removed with
the `bugs/` directory in d2c8d27826 (#36), ends with:

```m2
--Also the "inverse" function will compute something, even for a singular matrix.
A = matrix {{1.,2,3},{4,5,6},{1,1,1}};
assert try(inverse A; false) else true
```

The assertion is that `inverse` **raises** on that matrix, so whoever wrote the file expected an error
and recorded the silent answer as wrong. That is not a proof of intent, but it is a contemporaneous
statement from inside the project, and no counter-statement has turned up.

It still fails, on the same matrix:

```m2
i1 : A = matrix {{1.,2,3},{4,5,6},{1,1,1}};

i2 : det A

o2 = -0

i3 : inverse A

o3 = 0

                3         3
o3 : Matrix RR    <-- RR
              53        53

i4 : A * inverse A

o4 = 0

                3         3
o4 : Matrix RR    <-- RR
              53        53
```

`A * inverse A` being the zero matrix rather than the identity is perhaps the plainest way to put it.
The same matrix over `QQ` and over `ZZ/11` raises `matrix not invertible` as expected, and a
non-singular real matrix gives `A * inverse A` equal to the identity to within 2e-15 — so the defect is
specific to singular matrices over the inexact fields, as this issue describes.

### The rest of that file

For completeness, since it is mostly about `solve` rather than `inverse`: its `solve` complaints do not
reproduce. It asserts that `solve(A,b)` should raise when the system is inconsistent, and instead of a
bogus answer `solve` now returns `null` — for `A` of rank 2 with `b` outside its column space, and
likewise in its second example. The `solve` documentation states its restriction explicitly ("over `RR`
or `CC`, the matrix `A` must be a square non-singular matrix"), and these examples are over `ZZ/11`,
where returning `null` for no solution seems a reasonable answer rather than a defect.
