Still reproduces: `OutputDictionary` is on `dictionaryPath` while a package loads.

### Why it matters

`OutputDictionary` holds the symbols `o1`, `o2`, … from the interactive session. With it on the path
during a package load, a package's own code can resolve a name to an output symbol from whatever the
user happened to evaluate before loading — which makes package loading depend on session history. The
exception the file grants, the `User` package, is the one place where reaching those symbols is the
point.

### Notes for whoever picks this up

**#1427** proposes the same trim-and-restore fix (remove `OutputDictionary` from `dictionaryPath` for
the duration of the load, put it back afterwards), so the two should be settled together. The care
needed is in restoring the path on the error path too, since a package that fails to load must not
leave `dictionaryPath` altered for the rest of the session.
