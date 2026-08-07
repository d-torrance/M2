This has an announced fix that has not arrived, which seems worth recording on the issue.

**#3222** (merged 2024-05-10) added the deprecation warning for `Matrix \\ Matrix`, and says why:

> This PR is in preparation for a PR in the next release which will change `g \\ f` to call
> `quotient'(f, g)` instead of being a synonym for `f // g = quotient(f, g)`, and which will add a
> new algorithm that works for arbitrary modules rather than only free modules, as well as maps of
> coherent sheaves. This change was suggested in #1448 during work on PrimaryDecompositions with
> @mikestillman, and recently came up again during work on Varieties with @ggsmith and others.

"A new algorithm that works for arbitrary modules rather than only free modules" is exactly what this
issue asks for, so the design discussion is #1448 rather than anything that needs settling here.

Measured today, two years later, the inconsistency is unchanged:

```m2
i1 : R = QQ[x,y]; M = R^1/(x^2); N = R^1/(x);
i2 : f = map(N, M, matrix{{1_R}}); g = map(N, N, matrix{{1_R}});
i3 : isFreeModule source f
o3 = false

i4 : first quotientRemainder(f, g)
o4 = matrix {{1}}                                -- answers happily

i5 : f % g
     error: expected maps from free modules      -- Core/matrix2.m2:482
```

So `quotientRemainder` accepts a non-free source and `%` refuses the same input, which is the
discrepancy this issue reports. `g \\ f` also errors on it. The deprecation half of #3222 landed; the
algorithm half is what is outstanding.
