Closed as fixed, with the record of what actually happened kept here because it is not obvious from
the code today.

### The first ask: tell the packager which programs configure found

Met more automatically than the file proposed. `FILE_PREREQS` (`0d2d059660`, 2020) accumulated the
`command -v` paths of the external programs configure locates, and drove the runtime dependencies of
the `.deb` and `.rpm` builds — so a distribution packager got the list without reading configure's
output.

That use has since been scrapped in favour of hardcoded dependencies per build system, which leaves
`distributions/freebsd/Makefile.in:60` as the only remaining reader of `FILE_PREREQS`.

### The second ask: stop hard-coding program paths in packages

Met separately, by `findProgram` (#1389). Packages now locate their helper programs at runtime
through `findProgram`/`programPaths` (`m2/programs.m2`) rather than carrying a path.

### Why this matters for anyone reading the file

The file's suggestion — that configure print an emphatic message asking the package assembler to add
the prerequisite — was overtaken by making the information machine-readable instead, then partly
walked back. If per-distribution dependency lists ever become a maintenance problem again,
`FILE_PREREQS` is the mechanism that already exists.
