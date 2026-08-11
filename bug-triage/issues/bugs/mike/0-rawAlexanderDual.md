The Alexander dual of a monomial ideal comes back empty over a tower ring, where the same ideal over a
flat ring gives the right answer:

```m2
i1 : R = QQ[x][y];

i2 : I = monomialIdeal(x*y)

o2 = monomialIdeal(x*y)

o2 : MonomialIdeal of R

i3 : dual I

o3 = monomialIdeal ()

o3 : MonomialIdeal of R

i4 : S = QQ[x,y];

i5 : J = monomialIdeal(x*y)

o5 = monomialIdeal(x*y)

o5 : MonomialIdeal of S

i6 : dual J

o6 = monomialIdeal (x, y)

o6 : MonomialIdeal of S
```

`o3` should be `(x, y)` as `o6` is. Flattening the tower's own ideal confirms it — `dual monomialIdeal p I`
for `(R', p) = flattenRing R` gives `monomialIdeal (y, x)` — so the two routes to the same
mathematical object disagree, and the tower route returns the zero ideal.

Nothing signals a problem: `monomialIdeal(x*y)` is accepted, prints back unchanged, and reports
itself as a `MonomialIdeal of R`. A caller receives an empty ideal with no error.

If a monomial ideal in `QQ[x][y]` is not a meaningful object — `x` lives in the coefficient ring, so
`x*y` is arguably not a monomial of `R` at all — then the right behaviour is to refuse it at
construction rather than to accept it and return nothing.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/0-rawAlexanderDual` reports
this same input crashing:

```
    i4 : dual I

    Program received signal SIGSEGV, Segmentation fault.
    0x40adb9ac in __gmpz_set () from /usr/lib/libgmp.so.3

    (gdb) up
    #1  0x0843c11b in Frobby::alexanderDual (...) at src/frobby.cpp:225
    #2  0x0821dad1 in rawAlexanderDual (...) at .../e/x-monideal.cpp:380
```

with the guess *"perhaps rawAlexanderDual is giving bad data to frobby that makes it crash"*. The
crash is gone; what replaced it is the silent empty answer above.

Structurally the same shape as [#3389](https://github.com/Macaulay2/M2/issues/3389), where
`coefficients` over `QQ[x][y]` returned nonsense while the flat ring was fine — a different function,
the same tower-versus-flat divergence. That one was closed; this one is separate.
