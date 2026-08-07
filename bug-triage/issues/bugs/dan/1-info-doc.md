Half met, half never attempted — and the check that would have caught the gap has been switched off
since 2009.

### The machinery is there

A full Next/Prev/Up spine over all **2010** nodes of `Macaulay2Doc.info`, a valid tag table
(`installPackage.m2:411-414`), and **236** `* Menu:` blocks from `format.m2:315-331`, whose code comments
explain why Up links and unwrapped menu items matter for the emacs reader.

### But the flagship manual's Top node has no menu at all

The next node begins at line 169 and the first menu anywhere is at line **442**. So at the manual's entry
point, `m`, the digit keys and standalone info's table of contents do nothing — only follow-reference
works.

It is package-dependent: `Varieties.info` *has* a Top menu. Only `Subnodes` feeds the menu generator, and
`Macaulay2Doc`'s `ov_top.m2` uses hand-rolled `UL` lists instead.

### The index half is simply absent

**Zero** `Index` nodes in any M2 info file — while `makeSortedIndex` (`installPackage.m2:421-441`) builds
exactly that for html over the same node list. The data is computed and never rendered into info.

### Why this went unnoticed for seventeen years

`packages/Makefile.in:121-122` disables `check::check-info` with the comment that `Info-validate`
*"doesn't work well enough to be useful"* — commented out by `f766cd4dfa8` on 2009-01-06.
`Info-validate` is the tool that flags nodes reachable only by cross-reference.

### Related

`2-toc-index-info-doc` is the same ask with a proposed mechanism, still untriaged; **#4554** is the
sibling about the `dir` file *beside* the `.info` rather than navigation inside it.
