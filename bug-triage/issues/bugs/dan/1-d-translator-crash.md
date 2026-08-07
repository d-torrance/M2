Reproduced. Copy any `.d` file to `string.d` and run `scc1` on it:

```
chk.c:1421: assertion failed
```

exit 1. Controls with identical content under other names — `notstring.d`, `foo.d`, `strings.d` —
produce ordinary "symbol never defined nor declared" diagnostics and no assertion. **So the filename
alone decides it**, which is the file's claim.

The crash mode is an assertion rather than the 2009 segfault because this build has assertions enabled;
without them it would presumably still be a segfault.

### Likely cause

`scc1` appears to derive a package name from the filename, and `string` collides with the built-in
string type — so the compiler ends up with two things of that name and trips an invariant rather than
reporting a conflict.

### Why it is worth filing despite being obscure

It is a minimal reproducer needing no rebuild, and the failure is an assertion with no useful message, so
anyone who trips it has nothing to go on. `scc1` bugs are actively tracked — #1197, #4525 and #4542 open,
#1134 and #1420 closed — and none of them is this.
