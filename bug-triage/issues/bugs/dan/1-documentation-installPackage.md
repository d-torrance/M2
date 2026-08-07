The parenthetical in the file was built; the titular ask was not — and the ask needs restating before it
is acted on.

### What was built

`CacheExampleOutput` arrived in `aa8a540a3d` (2008-12-14), plausibly *before* this file, and stashes
example output per node: `installPackage.m2:522-541` keys each file `examples/<node key>.out`, with a hash
guard at `:527-531`. Dan endorses exactly that split in **#1924**.

### What was not

The Usage line is still `installPackage PackageName` (`package-doc.m2:448-452`) — only the case has
drifted from the file's `PACKAGENAME`.

### Why the literal request would make the page worse

`installPackage "FOO"` names one of the two accepted argument types and hides the other:
`lookup(installPackage, Package)` is non-null, and `installPackage FirstPackage` works. The metavariable
covers both.

### But the underlying need is live, and in style

Of 260 Usage lines in `Macaulay2Doc/functions`, 248 are bare metavariables and the 12 with quotes all
quote a placeholder precisely where quoting matters — `export {"symbol1", ...}` is the model. So a Usage
line that shows the string form *as one of the two* would be consistent with house style; replacing the
metavariable outright would not.
