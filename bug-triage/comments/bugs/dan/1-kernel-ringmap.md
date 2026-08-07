Provenance for the guard this issue names, which turns out to be a scoped decision rather than an
oversight.

`canUseHilbertHint` was introduced by **#3942** (merged 2026-06-04), the PR that fixed the crash in
#1935 and #3937 — before it, a wrong Hilbert hint aborted the process rather than raising an error.
That PR's description records the scope it chose:

> - A function `canUseHilbertHint` has been added, documented, which allows one (and the GB code) to
>   decide if in a given situation, the Hilbert hint is usable.
> - The abort when discovering a bad Hilbert function is removed. Instead, an exception is issued.
> - Note: multi-graded rings will not use any Hilbert hint. If desired, we can likely add this, but
>   it will take some effort and time.

So the predicate has two deliberate restrictions, and its author already priced extending one of
them. This issue is about the other: `canUseHilbertHint Matrix` additionally requires
`isHomogeneous m`, which is what excludes the inhomogeneous case Mike's 2008 recipe was written for.
Worth knowing before anyone treats it as a missing check to be relaxed casually — the safety it
provides is what stopped the crash in #1935.
