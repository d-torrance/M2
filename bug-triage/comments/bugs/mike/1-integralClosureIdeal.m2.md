<!-- issue: #3078 -->
The ring map this issue points at is not merely missing a `DegreeLift` — when the source ring admits no map
to the target at all, `basis` builds the **zero map** and stores it, without complaint.

```m2
i1 : kk = ZZ/101; S = kk[a,b]; R = kk[c,d];

i4 : P = R^1/(ideal vars R)^3;

i5 : B = basis(P, SourceRing => S);

i6 : keys B

o6 = {source, RingMap, RawMatrix, target, cache}

i7 : B.RingMap

o7 = map(R,S,{0, 0})

o7 : RingMap R <-- S
```

`S = kk[a,b]` and `R = kk[c,d]` share only their coefficient field, so there is no map `R <- S` to find, and
`map(R,S)` with no images given produces the zero map rather than refusing. `basis` accepts it and returns a
matrix over `R` whose stored `RingMap` is that zero map; `source B` is nonetheless `S`.

This is the same `phi` the issue already blames, two lines up from the `DegreeLift` problem:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/basis.m2#L249-L252

and the `TODO` sitting directly above it describes exactly this case:

```m2
-- TODO: check that S is compatible; i.e. there is a map R <- S
-- perhaps the map should be given as the option instead?
S := if opts.SourceRing =!= null then opts.SourceRing else R;
phi := map(R, S);
```

So the note in this issue that the offending map is one "that the user can't provide" has a second
consequence beyond lost homogeneity: with no way to supply the map and no check that one exists,
`SourceRing` silently accepts a ring that is unrelated to the module's, and the resulting matrix carries a
`RingMap` that maps everything to zero. The second half of that `TODO` — passing the map instead of the ring —
would settle both symptoms at once.

### Provenance

This was reported once before. `bugs/mike/1-integralClosureIdeal.m2`, one of the files removed with the
`bugs/` directory in d2c8d27826 (#36), carries a `--Some bugs:` block ending with a `--BASIS problems:`
section, whose last lines are

```m2
B=basis(P,SourceRing => S)
keys B -- "RingMap" is a key
B.RingMap => f --returns f, does NOT set the key
B.RingMap -- thinks the map of rings is 0.
```

on exactly the `S`, `R` and `P` above. The file is undated; its neighbours in that directory are from 2008–2010.
