A file in the old `bugs/` tree asks for the same thing, and records the specific
blockers behind "we don't have a way to build it":

> - it uses a dynamic readline library that can only be provided by fink and
>   probably will not be copied into the install location
> - it offers no way to provide a directory tree containing a readline library
>   we've compiled
> - it writes a file into the user's home directory called `.polymake`
> - it asks questions

Those four are still in `configure.ac` today, as a comment near the `PROGLIST`
definition, and `M2/libraries/polymake/Makefile.in` still opens with "we haven't
succeeded in making this build process work". Polymake appears nowhere in
`libraries/Makefile.in`, so nothing is built.

Two things in the file that are not in this issue:

> Nov 8, 2010: Michael Joswig reports they've addressed those problems by
> reworking the build process

and, for the home-directory objection specifically:

> work around the problem of leaving stuff in the home directory by setting
> `POLYMAKE_USER_DIR` to ".", or to a subdirectory of the build/source directory

**I have not verified either.** The first is a 2010 report about the state of
polymake in 2010, relayed here rather than confirmed, and the build has surely
changed again since. But if it held even roughly, the objections recorded in
`configure.ac` have been stale for well over a decade, and the question of whether
M2 can depend on an installed polymake may be easier now than the comment there
suggests.
