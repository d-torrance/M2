The html half of this was fixed in direct response to the file — and then silently undone.

### The history

`f1c1dd78f1` (2007-12-23) moved every `[symmetricAlgebra, Opt]` key into the node's `Key` list, which
rendered because the node still had a top-level `Usage`. Then `9be2d70841` (2009-01-23) restructured the
node into three `SYNOPSIS` blocks and deleted that `Usage`.

`processUsage` returns early without a `Usage` (`document.m2:545-548`), and `SYNOPSIS` has **no Options
slot at all** (`:758-768`). So today:

- `help symmetricAlgebra` has no "Optional inputs" section,
- `help newRing` lists all 19 of its options with links.

All 18 of `symmetricAlgebra`'s option keys resolve — the content is documented; the page just does not
show it.

### Why the warning half fails open

That same early return exempts a `SYNOPSIS`-only node from the check, so ask (a) — the warning firing for
only 3 of the options — is not a warning bug. Generically it was met by `24478f749a`:
`document.m2:564` appends every option the author did not mention, and `validate.m2:76-81` warns about
the ones with no documentation. It simply never runs on this node.

### Related, from the other side

**#2516** is the same machinery seen inversely: a user getting warnings for pass-through options.
