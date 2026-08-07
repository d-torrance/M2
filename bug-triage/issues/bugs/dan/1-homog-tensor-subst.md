Reproduces byte for byte:

```m2
i1 : A = QQ[a]; B = QQ[b]; C = A ** B;
i2 : f = substitute(vars A, C);
i3 : isHomogeneous f
o3 = false
i4 : degrees f
o4 = {{{0, 0}}, {{0, 0}}}          -- while degrees C is {{1,0},{0,1}}
```

### The file's own framing is right, and the information is derivable

*"if only `substitute` would pay attention to how `C` was constructed from `A`"* — and it can be:

```m2
map(C, A, {C_0}, DegreeMap => d -> d | {0})     -- homogeneous, degrees {{{0,0}},{{1,0}}}
```

`map(C, A)` carries no `DegreeMap`; supplying the obvious one fixes it.

### The tensor already computes what is needed

`monoids.m2:692` applies `d -> join(d, N0)` to the **first** factor's degrees inline, and `:694` stores
only the **second** factor's map as `opts.DegreeMap`.

I predicted from that asymmetry that `substitute(vars B, C)` would work where `vars A` fails. It does
**not** — both are non-homogeneous with zero degrees — so the stored map is not consulted either, and the
fix is to plumb it through rather than to add the missing half.

### Pattern worth noting

This is the second row of its cohort where the needed information exists and the front end does not use
it, after #4583's minimal polynomial. **#2905** is the adjacent tensor-and-degrees bug that three settled
rows already point at.
