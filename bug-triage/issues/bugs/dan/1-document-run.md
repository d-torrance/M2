Half met, and the other half is **contradicted** by the current text.

### The return-code half is documented

`ov_system.m2:855-867` documents the encoding including "2 for interrupt", added by `42ade68981`
(2014-08-26).

### The interrupt-ignoring half says the opposite

`ov_system.m2:851-854` reads: *"signals invoked by control characters at the terminal will go both to it
and to Macaulay2."* But `run` is libc `system()` (`d/scclib.c:850-855`), which sets `SIGINT` and
`SIGQUIT` to `SIG_IGN` **in the parent** for the child's duration. Measured by having M2 signal itself
from inside `run`:

```m2
i1 : ppp = processID()
i2 : rrr = run("kill -INT " | toString ppp)
o2 = 0
i3 : print "SURVIVED"
SURVIVED
```

Delivery is real — same process group — but M2 ignores it. So Ctrl-C kills only the command, `run`
returns 2, and the documentation tells the reader to expect otherwise.

### Related

**#1082** asks to redesign what `run` and `wait` return and to stop shelling through `/bin/sh`, which
would change what needs documenting here — so the two are worth settling in order.
