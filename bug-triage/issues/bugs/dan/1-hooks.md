Filed as the general gap rather than the D-module one, because the general gap is what blocks anybody.

### `Hom` is hookified; `Ext` is not

```m2
hooks(Hom, Module, Module)     -- Strategy => Default, Strategy => Syzygies   (e0ffe77743, 2023-12-12)
hooks(Ext, Module, Module)     -- empty
```

`Ext` in Core is only a stub raising `missingPackage` (`complexes.m2:78,93`), with the real
implementations installed as **plain methods** in Complexes and OldChainComplexes. So no package can add
an `Ext` strategy without overriding the method outright.

### Why the D-module framing is dropped

`WeylAlgebras` does use the mechanism — it hooks `codim` at `WeylAlgebras/Dbasic.m2:270-271`, since
`19c0b76f7d` (2020-11-18) — so the pattern was known and simply not chosen for `Hom` or `Ext`. The
D-module `Hom` and `Ext` shipped instead as standalone `DHom` and `DExt` in `BernsteinSato`, taking an
extra weight-vector argument, so whether they are substitutable for `Hom` and `Ext` is a mathematical
question this triage could not settle.

Anton, whom the file names, filed nothing about it in eighteen years — which is a reason to state the
request generally rather than to attribute it.
