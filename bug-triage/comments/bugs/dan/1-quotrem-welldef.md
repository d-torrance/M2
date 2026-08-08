A second file from the pre-GitHub `bugs/` tree reports this, and it carries a sharper example than
the one above — a *pair* where one direction is well defined and the other is not, so the failure is
not simply "a non-free source is dubious" but depends on which way you divide.

`bugs/dan/1-quotrem-welldef`, over `ZZ/101[x]`, still on 1.26.06-40-gd8e86d689d:

```m2
R = ZZ/101[x]
f = inducedMap(R^1/x, R^1/x^2)
g = inducedMap(R^1/x, R^1/x^3)

first quotientRemainder(g, f)      -- matrix {{1}}
isWellDefined oo                   -- true

first quotientRemainder(f, g)      -- matrix {{1}}
isWellDefined oo                   -- false
```

The asymmetry is exactly what one would want:

- `quotientRemainder(f, g)` needs `q : R¹/x² → R¹/x³`, and `1` sends `x²` to `x²`, which is nonzero
  in `R/x³`. Not a map.
- `quotientRemainder(g, f)` needs `q : R¹/x³ → R¹/x²`, and `1` sends `x³` to `x³`, which *is* zero in
  `R/x²`. Fine.

So `1` is returned in both cases and is only correct in one. That makes the pair usable as a
regression test: whatever the resolution here, `isWellDefined` on the quotient should agree with which
of the two directions was asked for.

The file is Dan Grayson's, and it opens by treating it as an oversight rather than a design question:

> Here is a rather silly bug of mine from long ago. Did I just not think about whether the resulting
> maps are well-defined? Hmm...

Worth having alongside the `Matrix % Matrix` inconsistency above, since the two point the same way: one
of the pair refuses non-free sources, the other accepts them and can return something that is not a map.
