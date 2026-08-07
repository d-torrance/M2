The inconsistency the file reports still stands, and it is easy to see side by side:

```m2
i1 : R = QQ[x,y]; M = R^1/(x^2); N = R^1/(x);
i2 : f = map(N, M, matrix{{1_R}}); g = map(N, N, matrix{{1_R}});
i3 : isFreeModule source f
o3 = false

i4 : first quotientRemainder(f, g)
o4 = matrix {{1}}                                -- accepted

i5 : f % g
     error: expected maps from free modules      -- Core/matrix2.m2:482
```

So `quotientRemainder` answers for a non-free source and `%` refuses the same input. `g \\ f` errors on
it too.

### The mathematical question the file raises

Dan's own framing is worth keeping: when the source of the first map is not free, the quotient *may not
be well defined*, so the right answer might be to make `quotientRemainder` stricter rather than `%`
more permissive. What is clearly wrong is having the two disagree silently.

### There is an announced fix that has not arrived

**#3222** (merged 2024-05-10) added the deprecation warning for `Matrix \\ Matrix` explicitly *"in
preparation for a PR in the next release which will change `g \\ f` to call `quotient'(f, g)` … and
which will add a new algorithm that works for arbitrary modules rather than only free modules, as well
as maps of coherent sheaves"*, crediting **#1448** as the design discussion with @mikestillman. The
deprecation landed; the algorithm did not.
