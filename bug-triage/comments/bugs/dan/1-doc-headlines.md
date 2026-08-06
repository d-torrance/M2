There is a request from the other direction on record, which may be worth deciding together with this one.

A note in the pre-GitHub `bugs/` tree (`bugs/dan/1-doc-headlines`, being triaged in #36) asks for the opposite resolution:

> make Headline support not just strings, but everything, including TeX and so on
>
> also in SimpleDoc

So one request is to reject non-strings in headlines, and the other is to accept them.

As things stand the stricter reading is what the code does. `document` requires a string:

https://github.com/Macaulay2/M2/blob/79c7ac4fc5d4de37a18aae03ac9eb8c0db1e3c7b/M2/Macaulay2/m2/document.m2#L601-L604

and `SimpleDoc` enforces the same through `singleString`:

https://github.com/Macaulay2/M2/blob/79c7ac4fc5d4de37a18aae03ac9eb8c0db1e3c7b/M2/Macaulay2/packages/SimpleDoc.m2#L81

which raises the question of how `linSpace` acquired one — presumably by a path that bypasses `getHeadline`, and that may be the more useful thing to pin down here, since whichever way the design goes, a headline that reaches the HTML generator without passing that check is a hole either way.

For what it is worth, the two requests are not quite symmetric: allowing TeX in a headline means every consumer of headlines has to render it, and headlines appear in contexts that are not HTML — `help` output in a terminal, `about` results, info files, and the one-line summaries in package tables of contents. That is an argument for this issue's position rather than the bug file's, but it is the maintainers' call.
