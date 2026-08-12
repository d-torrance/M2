`kernel RingMap` accepts `DegreeLimit`, `SubringLimit` and `Strategy`, and honours them. The two
one-line functions in `ringmap.m2` that exist only to call it accept no options at all, so those
controls cannot be reached through them.

```m2
i1 : R = ZZ/101[a..d]; S = ZZ/101[s,t];

i3 : F = map(S,R,matrix{{s^4, s^3*t, s*t^3, t^4}});

i4 : kernel F

              3     2      2    2         2      3     2
o4 = ideal (b*c - a*d, c  - b*d , a*c  - b d, b  - a c)

o4 : Ideal of R

i5 : kernel(F, SubringLimit => 1)

o5 = ideal(b*c - a*d)

o5 : Ideal of R

i6 : coimage(F, SubringLimit => 1)
stdio:6:1:(3): error: no method found for applying coimage to: ...

i7 : isInjective(F, SubringLimit => 1)
stdio:7:1:(3): error: no method found for applying isInjective to: ...
```

`options coimage` and `options isInjective` are both `null`.

### Where it comes from

Both are single expressions over `kernel f`, declared with no option table:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/ringmap.m2#L375-L377

so `coimage f` always runs the full `kernel f`, and there is no spelling of "compute the coimage,
stopping after the first *n* subring generators" or "…up to degree *d*". `kernel` itself is fully
hooked and strategy-aware:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/ringmap.m2#L231-L236

### Why it is worth a line of code

For an elimination that does not finish, a partial answer is the only answer available, and `coimage`
is the documented way to compute the ideal of the image of a ring map. The workaround is to call
`kernel` directly and form the quotient by hand — which is exactly what `coimage` is, so nothing is
lost mathematically; but the convenience wrapper is the thing a caller reaches for, and it silently
has no way to express the bound.

There is no `image RingMap`, so `coimage` and `isInjective` are the whole of the affected set.

### A caution for whoever measures this

`kernel RingMap` caches on the ring map, and the underlying `gb` caches on the graph matrix, so
re-testing option variants against one `F` in one session will show the options being *ignored* when
they are not:

```m2
-- misleading: kernel F has already completed and cached the gb
kernel F                     -- 4 generators
kernel(F, SubringLimit => 1) -- 4 generators, apparently ignoring the option
```

Build a fresh ring and map per variant and the options behave correctly — `SubringLimit => 1` gives
one generator, `=> 2` gives two. Worth stating because it is the first thing a reader will try.

### Where this came from

`bugs/mike/1-pushForward1`, one of the 857 files removed with the `bugs/` directory in
[`d2c8d27826`](https://github.com/Macaulay2/M2/commit/d2c8d27826) and catalogued in
[#36](https://github.com/Macaulay2/M2/issues/36). Dan Grayson's note asks:

> Mike, pushForward1 has been supplanted by coimage, but pushForward1 had many options, no longer
> available. Is that a problem?

followed by `options pushForward1`, listing eight, and then a bare `options coimage`.

Answering it: mostly not. `pushForward1` was removed in favour of `relations coimage map(M,f)`
(`changes.m2:1827`), and of its eight options, `DegreeLimit` and `Strategy` are on `kernel` by name,
`BasisElementLimit`'s analogue for an elimination is `SubringLimit`, and `UseHilbertFunction` is
better than restored — the `AffineRing` strategy calls `canUseHilbertHint` itself and then asserts the
hint was used, so it is automatic rather than a user's decision. `MonomialOrder`, `PairLimit`,
`StopBeforeComputation` and `StopWithMinimalGenerators` have no successor.

The one gap that is not merely a rename is the one above: the options exist, and the wrappers cannot
pass them on.

Note that `pushForward` is *not* the successor and is not what this is about — it requires the map to
be module-finite, which `pushForward1` never did.

### Searches this rests on

Titles and bodies for `coimage`, `pushForward`, `pushForward1`, `kernel`, `SubringLimit`; comments via
`gh search issues` for `pushForward1`, `coimage`, `minPres`. Pull requests included, not filtered.
[#3284](https://github.com/Macaulay2/M2/issues/3284) is the nearest open neighbour — `kernel`
misbehaving under `pushFwd` on multigraded input — and is about answers rather than options.
`pushForward` is under active development
([#4366](https://github.com/Macaulay2/M2/issues/4366),
[#4435](https://github.com/Macaulay2/M2/issues/4435),
[#4465](https://github.com/Macaulay2/M2/issues/4465),
[#4176](https://github.com/Macaulay2/M2/issues/4176)), all widening which ring maps are accepted,
which is a different axis from this.
