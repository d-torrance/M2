A reproduction where the missing alternate stack costs a diagnostic, in case it is useful for
motivating this.

A one-line file in the pre-GitHub `bugs/` tree (`bugs/dan/1-long-list-parsing`, being triaged in
#36) reads, in full:

> with stack size set to 8192K lists of length 90000 can't be parsed

That threshold has moved — 90000 parses now — but the failure mode above it is a bare `SIGSEGV`
with no output at all. Measured with `ulimit -s 8192`, the default on this machine, on files
containing a single list literal:

| list length | result |
| ---: | --- |
| 90000 | parses |
| 150000 | parses |
| 175000 | **SIGSEGV**, exit 139, no message |
| 300000 at `ulimit -s 16384` | parses |

So the limit is parser recursion depth scaling with the length of the literal, and doubling the
stack doubles the capacity. What is notable for this issue is the silence: M2's handler prints a
useful trace for other crashes — a Gröbner basis computation over an engine tower ring, for
instance, gives

```
-- SIGSEGV
 3# GaussElimComputation::insert(gm_elem*) at e/gauss.cpp:50
 ...
```

— but produces nothing here, because the stack the handler would run on is the one that just
overflowed. That is precisely the case `sigaltstack` addresses, and it is not an exotic one:
machine-generated M2 code reaches these sizes, since `toExternalString` of a large list written to
a file and read back is an ordinary workflow.

With an alternate stack the same input could at least report where it died, and ideally the parser
could raise "expression nested too deeply" before reaching that point.
