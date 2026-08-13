<!-- issue: #488 -->
Still leaking ten years on, and the `IC.cache.quotient` question this issue was opened to probe has an
answer: that field is not the retainer.

`bugs/mike/git-468-memory-leak.m2`, removed with `bugs/` in d2c8d27826 (#36), is Mike's copy of the same
Christian Bopp example, written against #468 three weeks before this issue was opened. Two things have to be
fixed before it will run at all: `RandomCanonicalCurves` has been merged into `RandomCurves`, and
`FastNonminimal` is no longer an option of the `res` you get in a default session — it survives only in
`OldChainComplexes`, and the current spelling is `Strategy => Nonminimal`.

Measured as peak RSS (`/usr/bin/time -v`) at N=20 against N=200, everything else held fixed, on
1.26.06-40-gd8e86d689d:

| work done per iteration | N=20 | N=200 | MB per iteration |
| --- | --- | --- | --- |
| generate the curve only | 309.3 MB | 310.1 MB | flat |
| + `gens gb IC` | 309.3 MB | 310.2 MB | flat |
| + `minimalBetti IC` — this issue's code | 333.7 MB | 499.0 MB | **0.92** |
| + `minimalBetti IC`, then `IC.cache.quotient = null` | 333.7 MB | 500.2 MB | **0.93** |
| + `res(…, Strategy => Nonminimal)`, `betti(…, Minimize => true)` | 355.9 MB | 686.3 MB | **1.83** |

Growth is linear across a fortyfold range — 328, 356, 411, 502, 689 MB at N = 5, 20, 50, 100, 200 for the
last row — with no plateau anywhere.

**The `gens gb` control is the informative one.** It runs on the same ideals, in the same ring, and caches its
result on the same objects, and it does not grow at all over 200 iterations. So neither ring accumulation nor
"results are cached on the ideal" explains this; whatever is retained is specific to the nonminimal
resolution path.

**`IC.cache.quotient = null` makes no difference** — 500.2 MB against 499.0 MB, inside run-to-run noise. That
was the hypothesis under test in the code above, so it can be struck off, which may be part of why the
original result read as perplexing: the workaround was being applied to something that was never holding the
memory.

**Two caveats, since this is an RSS measurement.** Boehm never returns pages to the OS, so these figures are a
high-water mark of heap *demand* rather than of live data; the reason I still read it as retention is that
the identical machinery with `gb` substituted is flat, and a fragmentation or warm-up effect would show
diminishing increments rather than a constant 1.83 MB per iteration out to N=200. And I have not established
whether the memory is unreachable-but-uncollected or reachable-but-retained — that distinction matters for
the fix and needs a tool I did not use here.

For anyone reproducing: the loop is 0.73 s per iteration, so N=200 is about two and a half minutes, and the
`gb` control is worth running first since it establishes the flat baseline in the same session shape.

<sub>#468, which the original file was named for, was closed on 2016-05-29; this issue opened 2016-06-16 and
is the live one. #424 and #469 are the other 2016 nonminimal-resolution issues, both closed, and #3068 is a
caching-versus-`Minimize` error rather than a leak.</sub>
