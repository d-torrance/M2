Reproduces — and it is not merely counter-intuitive, as the file says. It **contradicts the
documentation**.

`ov_debugging.m2:102-107` promises that after typing `end`, *"the debugger will be entered again … at the
point inside the function `g` from which the function `f` was called"*, and that `listLocalSymbols` will
then show `g`'s local variables.

### Side by side on 1.26.06

With the file's own functions, which declare no local variables, `end` returns straight to the top-level
prompt and `g` is **never entered**:

```m2
f = () -> (1/0; 4)
g = () -> (3; if true then f(); 5)
g()
-- debugger stops in f; type end
-- back at the top level, g skipped
```

With the same call structure but a local variable in each function, `end` re-enters the debugger inside
the caller and `listLocalSymbols` shows it.

So the skip is specifically the **empty-frame** case: the documented walk up the stack silently does not
happen for any function that happens to declare no locals. Four lines reproduce it.

### Related but distinct

`1-debugger-and-tail-recursion` is the case where frames are *elided* rather than empty.
