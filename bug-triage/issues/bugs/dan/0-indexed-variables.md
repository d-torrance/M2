Still reproduces, and one call earlier than the file reports.

```m2
i1 : R = QQ[x_0..x_3];
i2 : class \ (R_0..R_3)
o2 = (R, R, R, R)

i3 : S = QQ[x_0..x_3];                    -- a second ring reusing the same symbols
i4 : class \ (R_0..R_3)
o4 = (IndexedVariable, IndexedVariable, IndexedVariable, IndexedVariable)
```

The file shows this happening on the third call; today it happens on the first call after the second
ring is created, and it stays that way for the rest of the session. So `R_0..R_3` stops producing
elements of `R` — it produces the bare indexed variables instead.

### Why it happens

`..` on ring elements is implemented in `m2/dotdot.m2` by taking the two endpoints back to symbols and
walking between them. When a second ring reuses `x_0..x_3`, the symbols now point at the *new* ring,
so the round trip no longer lands in `R`, and the code falls back to yielding `IndexedVariable`s
rather than erroring.

### Related, and deliberately kept separate

**#2771** covers the shadowing *warnings* in this area, which is a different complaint. Also relevant:
`tests/normal/dot-dot.m2:128` asserts the `IndexedVariable` fallback for a shadowed ring, added in
2009 — so a fix has to change a regression test that has stood for sixteen years, and the behaviour is
deliberate rather than accidental.
