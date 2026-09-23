### What the file reports

A method function created by `method(Options => ...)` receives its options first and its arguments
second, so its methods are written `opts -> (m, n) -> ...`. Parenthesize that first part — write
`(opts) -> (m, n) -> ...` — and you have a *one*-argument function that returns a function, which is
not what a two-argument method needs. M2 notices and errors, except when a return type is supplied as
well, in which case it installs nothing and says nothing. The file is Dan's own explanation of why the
check is bypassed, ending in the workaround: write `opts`, not `(opts)`.

Still reproduces, and the failure is silent, which is the part worth fixing:

```m2
i1 : g = method(Options => {});
i2 : g (ZZ, ZZ) := List => (opts) -> (m, n) -> foo
o2 = List => {*Function[...]*}
o2 : Option

i3 : methods g
o3 = {}                                  -- nothing was installed
```

With the return type omitted, the same line is rejected outright:

```m2
i4 : g (QQ, QQ) := (opts) -> (m, n) -> foo
stdio:4:12:(3): error: expected method for binary operator to be a function of 2 variables
```

So supplying a `List =>` return type turns a diagnosed error into a no-op.

### Why the check is bypassed

The sanity check that catches the arity mismatch in the untyped case never runs in the typed one: what
is being assigned is then `List => f` rather than `f`, and the arity of `f` inside that `Option` is
never examined.

### Related

**#2864** is a different confusion in the same area (options and method installation), so the two are
worth reading together while touching this code.
