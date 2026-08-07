The rename was never done: `minimizePresentation` does not exist.

### What the file proposes

Two steps, and the second is the point of the first: rename `minimalPresentation` to
`minimizePresentation` — a verb, for the operation — and then use the freed name `minimalPresentation`
for *the presentation that operation produces*, a noun for the object.

### Why it is a decision rather than a patch

`minimalPresentation` is exported, documented, used across distributed packages and called by users, so
this is an interface break with a deprecation period, not a rename. That is presumably why it has sat:
the naming argument is good and the cost is real.

Worth noting the tree has since done exactly this kind of thing once — PR #3742 (merged 2025-04-21)
turned `intersection` into a synonym for `intersect`, closing #2257 — so there is a precedent for
managing a name change by synonym rather than by replacement. The wrinkle here is that the old name is
wanted for something *else*, which a synonym cannot express.
