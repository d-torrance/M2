The request came with its own closing condition — *"When the bug gets fixed, the comments need to be removed"* — and that
condition is now met, so this is a small piece of housekeeping that has simply been waiting a long time for someone to
notice.

[`Macaulay2Doc/functions/hilbertPolynomial-doc.m2:78-86`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/Macaulay2Doc/functions/hilbertPolynomial-doc.m2#L78-L86)
still ends the `(hilbertPolynomial, Module)` node like this:

```m2
     PARA{},
--     "These Hilbert polynomials can serve as ",
--      TO2 (hilbertFunction,"Hilbert functions"),
--      " too since the values of the Hilbert polynomial eventually are
--      the same as the Hilbert function. ",
--      EXAMPLE {
-- 	  "apply(5, k-> h(k))",
-- 	  "apply(5, k-> hilbertFunction(k,M))"
-- 	  }
     }
```

### The commented-out code works

Run against that node's own setup:

```m2
i1 : R = QQ[a..d];

i2 : M = module monomialCurveIdeal(R, {1,3,4});

i3 : h = hilbertPolynomial M;

i4 : apply(5, k -> h(k))
o4 = {0, -1, 1, 7, 18}

i5 : apply(5, k -> hilbertFunction(k, M))
o5 = {0, 0, 1, 7, 18}
```

No error, and the two lists make the prose's point exactly: they differ only at `k = 1` and agree from `k = 2` onward,
which is what "the values of the Hilbert polynomial eventually are the same as the Hilbert function" means. Over a wider
range they stay equal — `{0, -1, 1, 7, 18, 35, 59, 91, 132, 183, 245, 319}` against
`{0, 0, 1, 7, 18, 35, 59, 91, 132, 183, 245, 319}`.

### The sibling node already does this

The node immediately above, at
[`:50-58`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/Macaulay2Doc/functions/hilbertPolynomial-doc.m2#L50-L58),
runs the same two lines uncommented with a ring instead of a module, and behaves the same way:

```m2
i6 : S = R/monomialCurveIdeal(R, {1,3,4});

i7 : h2 = hilbertPolynomial S;

i8 : apply(5, k -> h2(k))
o8 = {1, 5, 9, 13, 17}

i9 : apply(5, k -> hilbertFunction(k, S))
o9 = {1, 4, 9, 13, 17}
```

So the surviving comment markers are the only thing distinguishing the two nodes on this point, and there is no longer a
reason for the module version to be the silent one.

### Two smaller things in the same lines

- The `PARA{}` on the line before the commented block now has nothing following it, so the node ends by emitting an empty
  paragraph. Whoever removes the comment markers will want to keep that `PARA{}`; whoever decides against restoring the
  text should drop it.
- The file's header still carries the original author's note, `--- notes: show ex of eventually = HF`, directly describing
  the block below it. That can go at the same time.

I have not opened a pull request because the choice between restoring the text and deleting it is a documentation-content
decision rather than a mechanical one, though the evidence above points at restoring.

### Related

[#4564](https://github.com/Macaulay2/M2/issues/4564) is the neighbouring page from the same triage — `hilbertSeries` not
documenting `numerator` and `denominator`. Different gap, adjacent node.

Searched titles and bodies for `hilbertPolynomial` and `hilbertFunction`, and comments for `hilbertPolynomial` and
"commented out of the documentation"; nothing covers this.
