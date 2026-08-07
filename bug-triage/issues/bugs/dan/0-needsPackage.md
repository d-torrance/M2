89% done, and what remains is a finite, checkable list rather than an open-ended sweep.

### Where it stands

- **233 of 300** distributed packages already use `PackageImports` or `PackageExports`.
- **34** still call `needsPackage` before `beginDocumentation`.
- Every other `needsPackage` hit is inside a `TEST` block, where it is correct.

`Benchmark.m2` is the pattern for that last point: it appears at line 13 in Dan's 2012 grep and now
only at line 266, inside a `TEST`.

### The remaining 34 are not a mechanical conversion

`PackageImports` loads a package **without importing its symbols**. So a package that actually uses
the imported package's exported names cannot simply switch to `PackageImports` — it has to use
`PackageExports` (which re-exports them to *its* users, a different meaning) or keep `needsPackage`.
That missing middle is what **#4502** describes: a way to load and import without re-exporting.

So this issue is blocked on #4502 for some of the 34, and mechanical for the rest — worth separating
the two groups before starting.
