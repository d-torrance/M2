Reproduces cold, and the order of the file's two lines matters — which is worth knowing before testing it.

```m2
i1 : X11 .. a
     error: 'X11' is not one of the symbols known to 'vars'
i2 : vars(-11)
o2 = X11
i3 : X11 .. a
o3 = (X11, X10, ..., X1, a)          -- works now
```

So `X11 .. a` fails in a fresh session and succeeds only after `vars(-11)` has memoized into
`varIndices`. Running the file's lines in the *other* order reads as fixed.

### The cause: a one-way map

`varName` (`indeterminates.m2:8-13`) defines the whole naming scheme — `X<n>` for negative indices,
`x<n>` for indices ≥ 52 — while `reverseVars` (`:24-33`) inverts only single letters,
`match("^[a-zA-Z]$", s)` at `:28`, plus whatever a prior `vars ZZ` call happened to register.

So `vars` generates names it cannot invert. It is not only the negative case: `vars(52..54)` is
`(x0, x1, x2)` and `x0 .. x2` fails the same way.

### The fix is the two missing inverses

`^X([0-9]+)$` and `^x([0-9]+)$`. They cannot collide with the single-letter case, since `x` alone is
index 23 and `x0` is index 52.

### Distinct from the other `..` issues

**#4510** and **#2020** are about `..` returning the wrong *type*, not about failing to resolve a name.
