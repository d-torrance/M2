`prune` and `minimalPresentation` silently drop a `Lex` order, returning a ring that orders its monomials by
`GRevLex` instead:

```m2
i1 : R = QQ[a,b,c,d, MonomialOrder => Lex];

i2 : S = R/ideal(a - b^2, c - d^3);

i3 : P = minimalPresentation S;

i4 : (monoid P).Options.MonomialOrder
o4 = {MonomialSize => 32, Lex => 0, Position => Up, GRevLex => {1, 1}}
```

Two variables survive, `b` and `d`, so the order should be `Lex => 2`. Instead the `Lex` block is emptied to
`Lex => 0` and a `GRevLex` block is appended, which is what actually orders the ring. The consequence is a
different answer, not just a different printout:

```m2
i5 : use P; leadTerm(P_0 + P_1^3)
o6 = d^3

i7 : T = QQ[b,d, MonomialOrder => Lex]; use T; leadTerm(b + d^3)
o9 = b
```

`Lex` takes the earlier variable; `GRevLex` takes the higher degree. So anything order-dependent computed in
the presented ring — Gröbner bases, `leadTerm`, elimination — differs from what the user asked for, with no
warning.

**`prune` is affected identically.** It shares all nine of `minimalPresentation`'s methods, and
`(monoid prune S).Options.MonomialOrder === (monoid minimalPresentation S).Options.MonomialOrder` is true on
this example. Since `prune` is much the more commonly used spelling, that is probably how anyone will meet
this.

### Which orders are affected

Only a `Lex` block spanning every variable. Surveyed on the same quotient:

| original order | presented order | |
| --- | --- | --- |
| default, `GRevLex => {1,1,1,1}` | `GRevLex => {1,1}` | correct |
| `{2,2}`, two `GRevLex => {1,1}` blocks | two `GRevLex => {1}` blocks | correct |
| `Weights => {1,2,3,4}` | `Weights => {2,4}` | correct — `b` had 2, `d` had 4 |
| `Eliminate 2` | `Weights => {1}`, `GRevLex => {1,1}` | correct — eliminates `b`, keeps `d` |
| `{Lex => 2, GRevLex => 2}` | `{Lex => 1, GRevLex => {1}}` | correct |
| **`Lex`** | **`Lex => 0`, `GRevLex => {1,1}`** | **wrong** |

So a `Lex` block *inside* a product order is handled properly; it is the single-block case that fails.

### Where it comes from

`monOrder` in [`m2/minPres.m2:55-75`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/minPres.m2#L55-L75)
begins:

```m2
monOrder := (Ord, l) -> (
     -- 2 arguments: a monomial order and a list of variables.
     -- return:  a new monomial order corresponding to the subset of
     --          of variables in l.
     if (Ord === Lex or Ord === GRevLex or Ord === RevLex)
     then newOrd := Ord
     -- The order for the original ring is a product order.
     else (...)
```

That first test can no longer succeed. `Ord` is `(monoid R).Options.MonomialOrder`, which today is a list —
`{MonomialSize => 32, Lex => 4, Position => Up}` — and never the bare symbol `Lex`, so every order falls into
the branch written for product orders. For most orders that branch happens to produce the right answer; for a
single `Lex` block covering all the variables it does not.

I have not traced the arithmetic inside that branch to the line that miscomputes, so the diagnosis above is
"the guard is stale and the fallback is wrong for this shape" rather than a specific fix. Someone who knows
`monOrder` will see it faster than I would.

### Related

[#4621](https://github.com/Macaulay2/M2/issues/4621) is the complementary problem from the other side: the
engine *drops* an empty monomial ordering block, so `selectInSubring` miscounts. This issue is about
`minPres` *generating* such a block. They want fixing independently — preserving the empty `Lex => 0` block
would still leave the ring ordered by the `GRevLex` block that should not be there.

Not [#346](https://github.com/Macaulay2/M2/issues/346) or its duplicate
[#347](https://github.com/Macaulay2/M2/issues/347), which are `minPres` raising "key not found in hash table"
on an ideal with very large coefficients, nor [#196](https://github.com/Macaulay2/M2/issues/196),
`prune ideal R` erroring; all three are closed. Searched titles for `minimalPresentation`, `minPres` and
`monomial order`, bodies for `monOrder`, `minPres` and `MonomialOrder`, and comments for
`minimalPresentation monomial`, `monOrder` and `minPres`.

### Provenance

One request from `bugs/dan/0-bugs-ataylor.m2`, a wishlist file removed with the `bugs/` tree in d2c8d27826
and catalogued in #36:

> minPres still needs to have how it deals with monomial orders repaired.

Still true. The other three requests in that file — that `prune` and `minPres` be synonyms of
`minimalPresentation`, that synonyms be highlighted in Emacs, and that they appear in the documentation
index — have all since been met.
