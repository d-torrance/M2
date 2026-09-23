### What the file reports

`rowMult`, `rowAdd`, `rowSwap` and their column counterparts are the elementary row and column
operations on a mutable matrix. They work in place, modifying the matrix they are given rather than
returning a new one. This two-line file notes that `rowMult` nevertheless returns a value its
documentation never mentions, and guesses that the rest of the family does the same.

True as reported, and the file's suspicion that *"the other col and row functions might also"* is
confirmed for the whole family.

`rowMult(m, i, a)` returns the `MutableMatrix` it just modified. `help rowMult` documents Usage, Inputs
and Consequences, and has **no Outputs section** — so the return value appears only incidentally, as
`o3` in the example.

### The whole family behaves the same way

`rowMult`, `columnMult`, `rowAdd`, `columnAdd`, `rowSwap`, `columnSwap` **and** `rowPermute` all return
the `MutableMatrix` rather than `null`, and none of those seven documentation nodes has an Outputs
section.

### Small and closable

Two acceptable fixes: document the return value in an Outputs section, or state in Consequences that the
matrix is returned for convenience. Either settles all seven nodes, since they share the pattern.
