<!-- issue: #4518 -->
Two files from the `bugs/` tree give this a concrete cost, and the numbers argue that the fixed strategy
table is not only an extensibility problem — it already hides the fastest available algorithm and exposes a
broken one.

`bugs/mike/devel-f4-eg1.m2` (removed with `bugs/` in d2c8d27826, #36) is a 29-generator binomial ideal in
32 variables over `ZZ/32003`, written in 2008 to ask whether `Algorithm => LinearAlgebra` was buggy. Every
route below returns the identical Gröbner basis of **3626 elements**, on 1.26.06-40-gd8e86d689d:

| route | wall | cpu |
| --- | --- | --- |
| `groebnerBasis(J, Strategy => "MGB")` | **4 s** | 3.9 s |
| `gb J` (default) | 21 s | 22.1 s |
| `gb(J, Algorithm => LinearAlgebra)` | 44 s | 178.6 s |
| `groebnerBasis(J, Strategy => "F4")` | 173 s | 253.2 s |
| `gb(J, Algorithm => ParallelF4)` | `error: unknown engine error` | — |

Three things follow that bear on this issue rather than on the original file.

**The fastest algorithm here is not reachable from `gb` at all.** `MGB` is 5× faster than the default and
43× faster than `groebnerBasis`'s own `"F4"`, but `processAlgorithm` (`m2/gb.m2:118-139`) accepts only
`Homogeneous`, `Inhomogeneous`, `Sugarless`, `Homogeneous2`, `LinearAlgebra`, `Toric`, `Test` and
`ParallelF4`. So `gb` and `groebnerBasis` expose disjoint sets, and which one a caller reaches for decides
whether a 4-second computation takes 4 seconds or 21.

**The TODO in that same function is answered, and the answer is no.** `m2/gb.m2:113-117` asks "F4 is still
used by `groebnerBasis`, is it the same as `LinearAlgebra`?" On identical input they differ by 4× in wall
time — 173 s against 44 s — so they are not the same path. That matters for any consolidation of the
dispatch surface, because the two names are currently easy to mistake for aliases.

**One value in the fixed table is selectable but non-functional.** `Algorithm => ParallelF4` fails on the
smallest input I could construct —

```m2
i1 : R = ZZ/101[a,b,c];

i2 : gb(ideal(a^2-b*c, a*b), Algorithm => ParallelF4)
Number of monomials: 3
--hash table info--
  size of hashtable = 524288
  ...
--- Input Polynomials ---
a2-bc ab
stdio:2:1:(3): error: unknown engine error
```

— and on every finite-field case I tried, while printing engine debug output to stdout with `gbTrace`
unset. This is not a complaint about the underlying work: `e/gb-f4/` is plainly mid-construction, last
touched 2026-06-02, with commit messages like "resolve conflicts, compiles", and
`e/gb-f4/testing.m2` is the authors' own scratch file containing this same call. The point for this issue
is that the table has no notion of a strategy being unavailable. `Toric` at least calls `warnexp()`
(`m2/gb.m2:87`); `ParallelF4` is marked "also experimental" in a code comment only, so from a released M2
it looks exactly as legitimate as `LinearAlgebra`.

A registration mechanism of the kind this issue asks for would presumably let a strategy declare whether
it applies at all, which is the same check `Saturation.m2` already makes when its hooks return `null` on
inputs they cannot handle. That seems worth keeping in scope alongside the ring-registration question,
since it is the difference between an unimplemented strategy declining and it emitting a debug dump.

<sub>Measurements are single runs on one 12-core machine and the wall figures for the parallel routes will
vary; the `MGB`-versus-default and `"F4"`-versus-`LinearAlgebra` gaps are large enough that ordering should
be stable, but please do not read the exact factors as benchmarks. `cpu` exceeding `wall` for
`LinearAlgebra` is that strategy's parallelism (#3554); note that it was unaffected by setting
`allowableThreads = 1`.</sub>
