This collects the "modernize this node" requests from a single wishlist file, because they are one condition rather than
fourteen: a set of operator documentation nodes still written in the pre-`Headline` style, `TT "x op y", " -- ..."`, with
no `Usage`, `Inputs`, `Outputs` and often no example. In several cases the node's own sibling — same file, same shape,
sometimes ten lines away — is fully modern, which is what makes these worth fixing as a batch.

**Not in scope, to be explicit about it.** Bare-symbol nodes carrying only a `Headline` are the deliberate convention
here: they are overview pages, headlined "a binary operator, usually used for …" with `Subnodes` and `SeeAlso`, and the
detail lives in the method nodes beneath. `symbol |`, `symbol ~` and `symbol @@` are all correct as they stand. Nothing
below asks for `Usage` on an overview node.

### Nodes with no `Headline` at all

| node | file | note |
| --- | --- | --- |
| `(exp, RingElement)` | `operators.m2` | has `Usage`, `Inputs`, `Outputs`; only the headline is missing |
| `true` | `operators.m2` | body is `TT "true", " is a value indicating truth."` |
| `false` | `operators.m2` | likewise |
| `{"left shift", (symbol <<, ZZ, ZZ), …}` | `operators/shift.m2` | has `Usage`, `Inputs`, `Outputs`, examples |
| `{"right shift", (symbol >>, ZZ, ZZ), …}` | `operators/shift.m2` | likewise |
| `GlobalReleaseHook` | `ov_repl.m2:279` | see below |

The two shift nodes are the cheapest fixes in the list — everything except the headline is already written.

### Siblings that diverged

**`GlobalAssignHook` and `GlobalReleaseHook`**, `ov_repl.m2:255` and `:279`, three lines apart. The first has `Headline`,
`Usage`, `Inputs`, `Consequences`, `EXAMPLE` and `SeeAlso`. The second opens
`TT "GlobalReleaseHook", " -- a method name that is consulted when an assignment to a global variable is about to occur."`
and has none of them. Its headline is derivable from that very sentence.

**`===`, `=!=`, `!=` and `==`** in `operators/equality.m2`:

- `===` at `:4` — `Key => {symbol ===, (symbol ===, Thing, Thing)}`, with `Headline`, `Usage`, `Inputs`, `Outputs` and
  three `EXAMPLE` blocks.
- `=!=` at `:51` — the *identical* Key shape, `{symbol =!=, (symbol =!=, Thing, Thing)}`, but only a `Headline`, then
  `TT "x =!= y", " -- returns true or false ..."` and "See `===` for details."
- `!=` at `:149` — `Headline` only, then `TT "x != y", " -- the negation of x == y"` and a `PARA{}` with nothing after it.
- `==` at `:63` — has `Headline` and `Usage` but no `Inputs` or `Outputs`, unlike `===` beside it.

**`.` and `.?`** in `ov_hashtables.m2:876` and `:893`, both keyed by strings. `.` has an example but no
`Usage`/`Inputs`/`Outputs`; `.?` has neither those nor any example. The `hash` node at `:902`, twenty lines below, is
fully modern. Separately, `.`'s `SeeAlso` is `{"#", ".?", "global"}` and omits `MutableHashTable`, although the example it
shows is `x = new MutableHashTable`.

**`=>`** at `functions/options-doc.m2:5` — `Headline => "construct an option"`, then one old-style line and no example.
The `Option` node immediately below in the same file is a modern `doc ///` node and does carry one (`o = Limit => 5`,
`peek o`).

### Nodes where only a headline was ever written

- **`->`**, keyed by the string `"->"` at `ov_methods.m2:284`. Headline "make a function", then the whole body is
  `TT "x -> e", " -- denotes a function ..."`, `BR{}`, `TT "(x) -> e", …`, a `UL` of three cases, and so on. No `Usage`,
  `Inputs`, `Outputs` or `EXAMPLE`, for one of the first operators a new user meets.
- **`symbol ^**`**, `operators/tensor.m2:22` — a `Headline` and nothing else. `operators/tensor.m2` holds only four keys,
  all bare symbols, so neither `Module ^** ZZ` nor `CoherentSheaf ^** ZZ` has a node, and `**` itself has no
  method-level documentation.
- **`symbol (*)`**, `operators.m2:566` — a `Headline` and no body, and unlike `|` or `~` there are no method nodes
  beneath, so following the link tells a reader nothing beyond the headline.
- **`symbol ,`**, `operators.m2:612` — a `Headline` and nothing else. Being punctuation it has no methods to carry the
  detail, so the headline is all there is.

### Three small things to fix in passing

- `operators/equality.m2:112` is `HEADER3 "Rings"` with nothing under it, immediately followed by `HEADER3 "Modules"`, so
  the rendered page shows an empty section heading.
- `operators/equality.m2:48`, `:146` and `:154` still list `"operators"` in their `SeeAlso`. Those are the last three in
  Macaulay2Doc; the equivalent entries were removed from the other operator nodes long ago.
- `operators.m2:496`, the `!` node, is otherwise modern but declares `Inputs => {"n" => ZZ}` without saying the argument
  must be non-negative — `(-1)!` is an error while `4!` is 24. Stating the domain alongside the type would close it.
- `ov_lists.m2:889-890` has two consecutive single-string `EXAMPLE` calls, which render as two boxes; combining them into
  one `EXAMPLE` with a two-element list is the whole fix.

### One thing checked and found correct

The `==` node's `Caveat` at `:142-145`, warning that whether `==` returns true "is not necessarily related to whether the
comparison operator `?` returns `symbol ==`", is accurate and should stay. Over `QQ[x,y]`:

```m2
i1 : image matrix{{x}} == image matrix{{y}}
o1 = false

i2 : (image matrix{{x}}) ? (image matrix{{y}})
o2 = ==
```

because `Module ? Module` (`m2/modules.m2:370`) compares rank then degrees, and both are rank 1 with degrees `{{1}}`.

### Provenance

These are fourteen entries from `bugs/dan/doc-changes`, a 100-line review of the operator documentation. The file's other
nineteen entries were checked in the same pass and are recorded in the catalogue: most have been met, `_`
(`operators/underscore.m2`, now 568 lines and 19 nodes) most comprehensively, and two are obsolete — `# File` no longer
exists, and `Module ~`, `Ideal ~` and `Ring ~` are gone with sheafification now spelled `sheaf`.

Searched titles for `headline`, `operator`, `Usage`, `Inputs`, `modernize` and `checkDoc`, and comments for
`GlobalReleaseHook`, "modernize the documentation", "missing a headline" and "old style documentation". Nothing tracks
this. #3194 "Operator wishlist" is about adding new operators, not documenting existing ones, and #2372 concerns hypertext
*inside* headlines rather than missing ones.
