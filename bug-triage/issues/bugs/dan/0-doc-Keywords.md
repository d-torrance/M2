The four examples in the file are all aliases from *other systems* — `solve` for `RationalPoints`,
`Normalform` for `%`, `inList`/`element` for `member`, `Inheritance`/`Parent` for `showStructure` — so
what is being asked for is discovery by synonym: a user who knows another system's vocabulary should
find M2's node.

### What search does today

```m2
about "Normalform"                 -- nothing, even with Body => true
about "solve"                      -- 220 hits, swamped by resolve/resolution
```

So the aliases are unreachable, and the one word that is present is diluted past usefulness.

### What Keywords is, and is not

`newPackage`'s `Keywords` option exists, but it is per *package* and holds subject categories for
browsing the package list — not per-node search aliases. Nothing today lets a documentation node
declare "also findable as …".

### Related

**#3689** is the diacritic special case of the same want: `about "Groebner"` should find the node
titled "Gröbner bases". Accent folding alone would fix that one; author-declared aliases would subsume
it, so it is worth deciding whether to do the general thing.

### Notes for whoever picks this up

The file suggests `\index{foo}` on the LaTeX MakeIndex model, aiming at a mergeable master index.
Note that `makeSortedIndex` (`installPackage.m2:421-441`) already builds a per-package index over the
node list for html, so there is existing machinery to hang aliases on rather than a new subsystem to
build.
