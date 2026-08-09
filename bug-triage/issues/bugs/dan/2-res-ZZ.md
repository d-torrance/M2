Over a polynomial ring, `freeResolution` hands the given presentation matrix back as `dd_1` without
minimizing it, so a module that is zero can get a free resolution of length 2.

The 2006 file below asks, as its second numbered task, "find out why res plugged in the presentation
matrix as `dd_1` in the first example and make it do better". Running its own example:

```m2
i1 : R = ZZ[];

i2 : f = random(R^6, R^9);

i3 : C = freeResolution coker f;

i4 : apply(toList(0..length C), i -> rank C_i)

o4 = {6, 9, 3}

i5 : C.dd_1 == f

o5 = true
```

`coker f` is the **zero module** — the Smith normal form of `f` is six units, and
`minimalPresentation` agrees, giving a 0×0 presentation. So that is a length-2 free resolution of 0,
whose first differential is the input.

### It is not about `ZZ`

`ZZ` is the one ring where this comes out right, which is what makes it easy to misread. The same
zero module over four rings:

```m2
ZZ    : freeResolution coker id_(ZZ^3)     ->  {0}
ZZ[]  : freeResolution coker id_(R^3)      ->  {3, 3}
ZZ[x] : freeResolution coker id_(R^3)      ->  {3, 3}
QQ[x] : freeResolution coker id_(R^3)      ->  {3, 3}
```

`QQ[x]` behaves exactly like `ZZ[]`, so this is a polynomial-ring issue, not a `ZZ` one.

### Why `ZZ` is different

`resolutionOverZZ` is the only strategy that prunes — it computes `minimalPresentation M` — and it
declines on anything that is not literally `ZZ`:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/Complexes/FreeResolution.m2#L417-L419

`ZZ[]` is not `ZZ`, so the hook returns `null` and `Strategy => 1`, the engine resolution, answers
instead. Confirmed by reading `M.cache.ResolutionObject.Strategy` after the call: `null` over `ZZ`,
`1` over `ZZ[]`.

The commented-out LLL hook a few lines below carries the same top-of-the-tower guard,
`if ring M === ZZ then ...`, and already cites #3785 next to it.

### `minimalBetti` inherits it

```m2
i1 : S = QQ[x,y];

i2 : m = matrix{{1_S, x}};      -- homogeneous; coker m is the zero module

i3 : minimalBetti coker m

o3 = BettiTally{(0, {0}, 0) => 1, (1, {0}, 0) => 1}
```

`minimize` applied to the same complex does give `{0}`, so the machinery to cancel the unit entry
exists and is simply not reached.

### Relation to existing issues

#3785's own example (`res coker id_(ZZ^3)`) passes today, because `res` is now `freeResolution` and
`ZZ` takes the pruning hook. Its question — what the answer should be — is unresolved for every
other ring. #3802 collects the surrounding `LLLBases` problems, but the hook at issue here is in
`Complexes`, and the LLL one is commented out.

The file's other two tasks are met: (1) resolutions do continue past the global dimension when
needed — over `ZZ[]`, gdim 1, the example above has length 2 — and (3) over `ZZ[x,y]` they stop at
length 3, which is the global dimension.
