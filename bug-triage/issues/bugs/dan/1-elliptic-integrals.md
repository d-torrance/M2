Neither check was done, and the source says so.

### The sign of the second period

`EllipticIntegrals.m2:380-381` still asserts a **negative** imaginary part for `D.Period'`,
byte-identical to the file, under a live comment:

```m2
-* actually, the paper has the complex conjugate of this period ... *-
```

against Grayson's 1989 *Archiv der Mathematik* period cited at `:15-18`. Two neighbouring assertions
dodge the question with `abs` (`:320`, `:322`).

The real part drifted from `.0p200` to `2.37e-66` in `28a631f318` (2026-05-19) — a test repinned to new
`sqrt(CC)` output rather than an answer, and below the noise floor at 200 bits anyway.

### The branch cuts were never looked at

`git grep "branch cut"` is empty tree-wide, and `E.log` (`:226`) still picks quadratic roots by
magnitude — `quadnorm eqn' > quadnorm eqn` at `:239` — with no stated convention.

### One part of the file is settled

Its `{* *}` comment syntax: `f2c87ac678` converted this very file, and `bc59b7532b` made the old syntax
a hard lexer error (`d/lex.d:308-309`).

### Related

**#4416** is the neighbouring arithmetic-geometric-mean question, about infinite arguments.
