`prune` on a quotient ring recomputes everything on every call. The module version caches; the ring
version does not.

```m2
S = ZZ/32003[a,b,c,d];
I = ideal random(S^1, S^{5:-8});
R = S/I;
M = coker vars R;
for i to 4 do elapsedTime prune R;
for i to 4 do elapsedTime prune M;
```

```
 -- .868023s elapsed      <- prune R
 -- .838359s elapsed
 -- .89107s elapsed
 -- .874442s elapsed
 -- .888008s elapsed

 -- .082846s elapsed      <- prune M
 -- .00000909s elapsed
 -- .00000235s elapsed
 -- .00000223s elapsed
 -- .00000291s elapsed
```

Each `prune R` costs about what the Gröbner basis of `I` costs on its own (0.95 s here), and
`prune R === prune R` is false, so a fresh ring is built every time.

### Cause

`minPressyRing` caches the isomorphism *maps* on `R` but never the ring it just computed:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/minPres.m2#L390-L404

`R.minimalPresentationMap` and `R.minimalPresentationMapInv` are set — verified: `R.?minimalPresentationMap`
is false before the first call and true after — but the returned `finalRing` is discarded, so the next
call redoes `minPressy(ideal R, ...)` and overwrites those maps with equivalent ones.

Modules already have exactly this caching, through `hasMinPres`:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/modules2.m2#L129

added in [#4134](https://github.com/Macaulay2/M2/pull/4134). Rings were not given the analogue, which
is why the two columns above differ by five orders of magnitude after the first call.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/0-quotient-rings.m2` is
Mike's 2009 timing session on Gröbner basis information being lost around quotient rings, sent to Dan
with the note *"we are forgetting groebner basis information about our quotients"*. Its line 9 is

```m2
time prune R; -- recomputes GB FIX
```

The rest of that file has since been addressed, which is worth saying so nobody re-opens it wholesale:
`R = S/I` no longer copies the Gröbner basis (0.024 s), `trim R` caches (0.86 s then 0.019 s),
`flattenRing R` is instant, and `C = R[x,y]` is fast. `flattenRing C` still costs a full Gröbner basis
on first call, about 1.0 s, but caches thereafter.
