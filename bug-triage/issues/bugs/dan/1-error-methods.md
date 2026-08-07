Still true, and measurably worse than when the file was written: its transcript shows 102 entries;
`methods symbol =` now returns **154**, of which exactly **4** are real assignment methods
(`RawMutableMatrix _ Sequence`, `MutableMatrix _ Sequence`, `IndexedVariableTable _ Thing`,
`Symbol _ Thing`).

`robust.m2:64-130` installs a `Thing OP Thing` method per flexible operator whose entire body builds the
"no method for" message.

### One distinction the file conflates

74 of the 154 are `((op,=),Type,Type)` and `((op,=),Type)` inheritance dispatchers from
`methods.m2:567-578`. Those do real work, so "stop installing them" is not available for that group —
only for the message stubs.

### A one-line partial mitigation exists

150 of the 154 already satisfy `isUndocumented`, yet `documentableMethods` (`help.m2:143`) returns all
154: `isDocumentableMethod` asks whether each component of the key is an *exported type*, never whether
the key itself was marked undocumented — and `Thing` and `Type` are exported. Adding that test would
filter the stubs out of the listing, at the cost of a documentation-database lookup per key.

### Related

**#1331** asks for more output here, **#1604** is the inverse complaint, and **#3108** was fixed by
filtering unexported types (`10529388c6`) — which provably leaves every stub in place, since `Thing` is
exported.
