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

### The mechanism, from the file

`(opts) -> (...)` insists on exactly one argument, while `opts -> (...)` does not — the parenthesized
form is a one-argument function whose body happens to be another function. The sanity check that
catches this for the untyped case is bypassed when the value is an `Option`, because what is being
assigned is then `List => f` rather than `f`, and the arity of `f` is never examined.

### Related

**#2864** is a different confusion in the same area (options and method installation), so the two are
worth reading together while touching this code.
