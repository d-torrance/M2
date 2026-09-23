### What the file asks for

When Macaulay2 resolves a name it searches the dictionaries listed in `dictionaryPath`, in order.
`OutputDictionary` is one of them, and it holds the symbols `o1`, `o2`, … that the interpreter creates
for the values of your output lines. `newPackage` sets

```m2
dictionaryPath = {Core.Dictionary, OutputDictionary, PackageDictionary}
```

at [`m2/packages.m2:403`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/packages.m2#L403),
so a package's source is read with the session's output symbols in scope. Dan's note is that it should
not be, with one exception: `User`, the package that stands in for the interactive session itself.
Still the case today.

### Why it matters

A package's own code can therefore resolve a name to an output symbol left over from whatever the
user happened to evaluate first — which makes loading a package depend on the history of the session
that loads it. The exception the file grants, `User`, is the one place where reaching those symbols is
the point.

### Notes for whoever picks this up

**#1427** proposes the same trim-and-restore fix (remove `OutputDictionary` from `dictionaryPath` for
the duration of the load, put it back afterwards), so the two should be settled together. The care
needed is in restoring the path on the error path too, since a package that fails to load must not
leave `dictionaryPath` altered for the rest of the session.
