Half met by `4f82b03a94` (2020-08-06), twelve years after the file.

### What is documented

The `[f, Opt]` key syntax (`document-doc.m2:149-153`, `ov_documentation.m2:176`) and a dedicated
**"optional argument documentation template"** node (`:513-535`).

### What is not

How an option's **type, default and description** reach a page. `[document, Inputs]` (`:233-263`) lists
seven accepted item forms and never uses the word "option" — so nothing states that an `Option`-shaped
item is pulled out into the separate "Optional inputs" section, that the default is filled in
automatically, or that omitted options are auto-listed. That half is documented **only** for the
SimpleDoc format (`SimpleDoc/doc.txt:161-165`, `3cb2096b13`).

### And one sentence is actively wrong

`document-doc.m2:636` says of `SYNOPSIS` that *"the options are used just as with `document`"*. They are
not: `SYNOPSIS` has no Options slot, and without `BaseFunction => f` an option item falls to
`document.m2:516` — a bare `Name => …` with no type, no default and no link.

Not on the wiki either: the style guide's "Use of optional arguments" section is about code conventions.

### Related

**#1466** is the next layer up (documenting an option's admissible values); **#1220** is the closed issue
whose fix delivered the half that is met.
