`rawLinAlgDeterminant` on a matrix over the FLINT integer ring segfaults, and corrupts the heap on the way
out, whenever any entry is zero. The 2-by-2 identity matrix is enough:

```m2
i1 : debug Core

i2 : rawLinAlgDeterminant raw mutableMatrix matrix(ZZFlint, {{1,0},{0,1}})
-- SIGSEGV
-* stack trace, pid: 233862
 3# 0x00007E5E73BA6DA2 in /lib/x86_64-linux-gnu/libgmp.so.10
 4# __gmpn_sqrtrem in /lib/x86_64-linux-gnu/libgmp.so.10
 5# __gmpz_sqrtrem in /lib/x86_64-linux-gnu/libgmp.so.10
 6# fmpz_sqrtrem in /lib/x86_64-linux-gnu/libflint.so.18
 7# 0x00007E5E73D4E920 in /lib/x86_64-linux-gnu/libflint.so.18
 8# fmpz_mat_solve_bound in /lib/x86_64-linux-gnu/libflint.so.18
 9# fmpz_mat_solve_dixon in /lib/x86_64-linux-gnu/libflint.so.18
10# fmpz_mat_det_divisor in /lib/x86_64-linux-gnu/libflint.so.18
*-
Fatal glibc error: malloc.c:2599 (sysmalloc): assertion failed: (old_top == initial_top (av) &&
old_size == 0) || ((unsigned long) (old_size) >= MINSIZE && prev_inuse (old_top) &&
((unsigned long) old_end & (pagesize - 1)) == 0)
```

The `glibc` line is the part I would weight most: this is not a clean null dereference but heap corruption,
so a call that does *not* crash outright may already have damaged unrelated allocations.

### The trigger is a zero entry

| matrix over `ZZFlint` | `det` over `ZZ` | `rawLinAlgDeterminant` |
| --- | --- | --- |
| `{{1,1},{1,2}}` | 1 | 1 |
| `{{0}}` | 0 | 0 |
| `{{90,11,91},{85,34,1},{69,68,80}}` | 477133 | 477133 |
| `{{1,0},{0,1}}` | 1 | **SIGSEGV** |
| `{{5,0},{0,5}}` | 25 | **SIGSEGV** |
| `{{1,2},{3,0}}` | −6 | **SIGSEGV** |
| `{{90,11,91},{85,34,0},{69,68,80}}` | 482494 | **SIGSEGV** |

The last two rows differ in one entry: changing the single `0` to a `1` turns a crash into the correct
answer. Zero-free matrices appear to be fine at every size I tried; a dense random 1000-by-1000 also
crashes, which is where I first hit this.

### This is known, but recorded nowhere findable

The tests for it are deliberately switched off. `packages/EngineTests.m2:205-207` reads

```m2
ringsPID = {
    "ZZ"
    --"ZZFlint"
    }
```

and the corresponding block in `packages/EngineTests/LinearAlgebra.Test.FLINT.m2:200-206` is a bare `///`
string rather than `TEST ///`, so it never runs:

```m2
///
  debug Core
  -- Most of this code is designed for fields...
  R = ZZFlint
  testDeterminant R
  testMult R
///
```

Both were done by [`50058c0907`](https://github.com/Macaulay2/M2/commit/50058c0907), Mike Stillman,
2014-06-09, whose message is *"Changes to tests. One test is still failing: crashing bug."* So the crash has
been known for twelve years, and the only record of it is that commit message plus two commented-out lines.
Nothing in the issue tracker mentions `ZZFlint` or `QQFlint` at all.

### Two ways to close this, and I think either is fine

**Fix it and re-enable the tests.** `testDeterminant` and `testMult` are already written for this ring and
would exercise it the moment `"ZZFlint"` goes back into `ringsPID`. Whether the fault is in M2's call into
`fmpz_mat_det`/`fmpz_mat_det_divisor` or in FLINT itself I have not established — the stack bottoms out in
GMP's `sqrtrem` beneath FLINT's Dixon solver bound computation, so a wrong dimension or an uninitialised
`fmpz_mat` handed across the boundary would fit.

**Or remove the ring.** `ZZFlint` is not in `m2/exports.m2` and has no documentation node, so it is
unreachable without `debug Core`; it exists as `ZZFlintRing` in `m2/flint.m2:6-13`. A half-present
experimental ring that corrupts the heap is arguably worse than no ring, and twelve years without work
suggests nobody is depending on it. `QQFlint` is in the same position — `EngineTests.m2:201` carries
`-- QQFlint not working yet`, and its own test block at `LinearAlgebra.Test.FLINT.m2:208` is likewise inert
and annotated `-- testRank R --FAILS`.

What I would not recommend is leaving it as it is, since the current state costs a reader the same
investigation twice.

### Not these

[#487](https://github.com/Macaulay2/M2/issues/487) was a determinant segfault, but at the M2 level over
polynomial rings; it was fixed by [#2546](https://github.com/Macaulay2/M2/pull/2546), the `Strategy =>
Dynamic` cofactor implementation, and [#3304](https://github.com/Macaulay2/M2/pull/3304) added the
regression test for it. [#1136](https://github.com/Macaulay2/M2/issues/1136) does name
`rawLinAlgDeterminant`, but over `ZZ/2[a..d]`, and reports a questionable answer rather than a crash.
Searched titles for `ZZFlint`, `QQFlint`, `flint`, `determinant`, `rawLinAlgDeterminant`, `fmpz`, `sqrtrem`,
`heap`, `malloc`, `glibc` and `linear algebra`, bodies for `ZZFlint`, and comments for `ZZFlint`, `QQFlint`,
`rawLinAlgDeterminant` and `fmpz_mat_det_divisor`.
