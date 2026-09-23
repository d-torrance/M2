### What the file asks for

A `TAGS` file is what lets an editor jump from a name to its definition. `etags` can include one tags
file inside another, so tables built for separate directories can be presented as a single one. This
two-line file observes that and asks M2 to use it, so that one `TAGS` covers the whole tree instead of
one per directory.

Unmet, and the specific mechanism the file proposes is nowhere in the tree.

### The state today

**Six** directories build `TAGS` independently — `c/`, `d/`, `e/`, `m2/`, `system/`,
`html-check-links/` — and `m2/` additionally hand-rolls `TAGS.doc` for the Macaulay2Doc sources by
piping `find` into a shell loop (`m2/Makefile.in:43-47`). There is no `etags --include` anywhere, and no
umbrella target. So "better tag files for everything" does not exist.

### Why this is worth doing rather than marginal

The value of a tags file is jumping to a definition *wherever* it lives, and the current arrangement
gives six tables that each cover one directory — precisely the case `--include` exists for. Evidence
that the targets are used rather than vestigial: the `m2` `TAGS` rule was repaired recently, in
**#4233**, for an argument-list-too-long failure. Somebody was running it.
