### What the request is about

`?` is Macaulay2's comparison operator: `a ? b` answers `<`, `=` or `>`, and `sort` is built on it.
This entry is a single line from a list of documentation changes Dan was keeping, and it sits between
the `==` and `!=` entries, among the comparison operators — which is what fixes it as a request for
the *binary* comparison on ideals rather than the unary `? I` that prints brief documentation.

That binary form is not defined, so a list of ideals cannot be sorted:

```m2
i1 : R = QQ[x,y];

i2 : sort {ideal x, ideal(x,y), ideal(x^2)}
stdio:2:1:(3): error: no method for binary operator ? applied to objects:
--            ideal x (of class Ideal)
--       ?    ideal (x, y) (of class Ideal)

i3 : sort {monomialIdeal x, monomialIdeal(x,y)}      -- same
```

whereas the corresponding modules sort fine:

```m2
i4 : sort {module ideal x, module ideal(x,y)}
o4 = {image | x |, image | x y |}
```

### Why the module case works and this is the natural gap

The method that makes `i4` work exists for exactly this purpose. `m2/modules.m2:370`:

```m2
-- used for sorting a list of modules
Module ? Module := (M, N) -> if rank M != rank N then rank M ? rank N else degrees M ? degrees N
```

So it is an arbitrary total order on rank then degrees — deliberately *not* containment — provided so that lists of
modules can be sorted into a canonical arrangement. `CoherentSheaf ? CoherentSheaf` exists too. Ideals are the more
commonly handled object of the three, and they are the one that cannot be sorted.

To be clear about what is and isn't missing: the *unary* `?` works on ideals already, inheriting `briefDocumentation` from
`(?, Thing)`, so `? I` and `? Ideal` both print brief documentation. This is only about the binary comparison.

`lookup(symbol ?, Ideal, Ideal)` returns a function rather than `null`, which is not evidence of a method — it is the
installed "no method" error at `m2/robust.m2:88`, the subject of #4572.

### Not a duplicate of #660

[#660](https://github.com/Macaulay2/M2/issues/660) "comparison of ideals" is about `==` on ideals being slower than
necessary because it does two `isSubset` calls, and asks why some code there is commented out. That is mathematical
equality and its cost; this is an arbitrary ordering for sorting. They would be fixed in different places, though anyone
touching ideal comparison may want to look at both.

### The rest of the file

The remaining 32 entries of `bugs/dan/doc-changes` were triaged in the same pass and are recorded in
the catalogue.

Searched titles for `compare ideal`, `sort ideal` and `codim`, and comments for `Ideal ? Ideal`; nothing tracks this.
