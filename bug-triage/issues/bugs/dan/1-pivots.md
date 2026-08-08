
`pivots` is unchanged from the code quoted above — `matrix2.m2:10-19`, `-- I wish this could be in
the engine` comment included. It walks each column and returns the position of the first non-zero
entry, searching from the top or the bottom according to a direction derived from
`leadTerm matrix {{1_R},{1_R}}`.

Both of the file's requests are open, and the documentation one has got worse rather than staying
still.

### The precondition is undocumented, and the documented example breaks it

`pivots-doc.m2` is marked `--- status: DRAFT`. It describes the output as *"positions (r,c) which are
the positions of the non-zero lead terms (in each column)"*, mentions no assumption about the input,
and its only `Caveat` is *"Should be implemented in the engine"* — which is the author's other wish,
not this one.

Its worked example is this:

```m2
i1 : f = matrix{{1,3,0,0,3,5,2,0,0},{0,0,0,1,3,6,7,8,0}}
i2 : pivots f
o2 = {(0,0), (0,1), (1,3), (1,4), (1,5), (1,6), (1,7)}
```

Row 0 appears twice and row 1 five times. That matrix is not in column echelon form, so the
documentation's own illustration is the case the note warns about — and a reader has no way to learn
that the result is only meaningful when each row is hit at most once.

### Which row you get is not always determined by the matrix alone

For a column with more than one non-zero entry the answer depends on the `Down`/`Up` direction, which
is inferred from the ring:

```m2
i1 : R = QQ[x];
i2 : pivots matrix{{x,1},{1,x}}
o2 = {(1,0), (1,1)}
```

Both columns report row 1. Over a ring where the direction comes out `Down`, the same matrix would
report row 0 twice.

### Nothing in the tree is currently at risk

Worth saying plainly, because it bounds the urgency. `pivots` has four callers and all are safe:

- `matrix2.m2:176`, `modules2.m2:181` and `modules2.m2:203` apply it to the output of
  `smithNormalForm`, which is canonical;
- `modules2.m2:227`, in `factor Module`, applies it to `presentation minimalPresentation M` and only
  for `ZZ`, `k[x]`, or a field.

So the exposure is to someone calling an exported, documented function on a matrix of their own.

### What would close it

- State the assumption in `pivots-doc.m2`, and replace or annotate the example so it does not
  illustrate the forbidden case.
- Optionally the file's second request: a cheap guard that notices two columns claiming the same row.
  The note puts it as *"maybe we could add some quick checks"*, and given the four call sites above it
  is a safeguard for future callers rather than a fix for a present bug.

### Not the same as the other pivot issues

[#613](https://github.com/Macaulay2/M2/issues/613) is about `reduce_by_pivots` in the engine, and
[#2511](https://github.com/Macaulay2/M2/issues/2511) is about `reducedRowEchelonForm`. Neither
concerns `pivots Matrix`.
