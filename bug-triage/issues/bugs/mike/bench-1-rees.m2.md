`reesIdeal(I, a)` — the two-argument form, whose comment in the source reads "the following method, usually
faster" — does not finish on a 7-variable monomial curve where the one-argument form takes two seconds:

```m2
i1 : needsPackage "ReesAlgebra";

i2 : S = ZZ/101[vars(0..6)];

i3 : i = monomialCurveIdeal(S, {4,8,11,13,15});

o3 : Ideal of S

i4 : elapsedTime reesIdeal i;                  -- 24 generators
 -- 1.93899s elapsed

i5 : elapsedTime reesIdeal(i, i_0);            -- killed at 15 minutes
```

`i_0` is `e^2-d*f`, a nonzero element of a domain, so it is a non-zerodivisor and the method's precondition
holds. The file this comes from records the same behaviour in 2013 — "doesn't finish before I get impatient
(10 min?)" — so this is not a regression.

### Where the time goes

```m2
reesIdeal(Module, RingElement) := Ideal => o -> (I, I0) -> (
    if o.Trim == true then I' := trim I else I' = I;
    K' := if o.Jacobian == true then expectedReesIdeal I' else (
    K' = symmetricAlgebraIdeal I';
    R := ring K';
    IR := substitute(I0, R);
    trim saturate(K', IR)                                    -- <-- unbounded
    ))
```

| step | cost |
| --- | --- |
| `trim i`, then `trim module` | 0.0008 s, 0.0003 s |
| `symmetricAlgebraIdeal` | 56.8 s |
| `saturate(K', IR)` | **> 180 s, no result** |

### The saturation happens in a tower, which disables most of `saturate`

`symmetricAlgebraIdeal` returns an ideal in `S[w_0..w_7]` — a polynomial ring whose coefficient ring is
itself a polynomial ring:

```m2
i6 : R = ring symmetricAlgebraIdeal trim module trim i;

i7 : numgens R, coefficientRing R
o7 = (8, S)
```

`saturate`'s strategies gate on `isFlatPolynomialRing`, defined at `Saturation.m2:72` as

```m2
isFlatPolynomialRing = R -> isPolynomialRing R and (isField(kk := coefficientRing R) or kk === ZZ)
```

which is false here because `coefficientRing R` is `S`. It is checked at `Saturation.m2:533`, `555` and
`579`, so `Linear`, `Bayer`, `GRevLex` and `Eliminate` all decline the input — each returns in under a
millisecond — leaving only `Iterate`, which is the naive loop.

Flattening the ring first, and changing nothing else, makes the same saturation finish:

| ring | `isFlatPolynomialRing` | `saturate(K, i_0)`, default options |
| --- | --- | --- |
| `S[w_0..w_7]`, 8 vars over `S` | false | > 180 s, no result |
| `flattenRing` of it: 15 vars over `ZZ/101` | true | **9.9 s, 24 generators** |

The 24 generators agree with what `reesIdeal i` returns, so this is the same answer by a different route.

### What I could not determine

In the *flat* ring, `Strategy => Eliminate` and `Strategy => Iterate` each also exceeded 300 s, yet the
default chain returned in 9.9 s. So the measurement above shows that flattening makes the step fast, but
not which strategy is responsible for that, and the mechanism I can name — the four strategies declining
the tower — does not by itself account for the flat-ring default being faster than any strategy I could
select by hand. Worth someone with more knowledge of the hook order looking at, since it may point at a
second, separate problem in how `(saturate, Ideal, RingElement)` dispatches.

### One caveat about what fixing this would buy

Even with the saturation flattened, the two-argument form would total roughly 67 s on this example against
1.94 s for `reesIdeal i`, so the "usually faster" claim above the method still would not hold here. The
change is from *does not finish* to *finishes*, not from slow to fast. The file's second example runs the
other way — `reesIdeal(i)` 8.4 s against `reesIdeal(i, i_0)` 1.4 s on
`ideal random(S^1, S^{-3,-4,-5,-6})` in four variables — so the method is genuinely worth having; it just
should not be able to hang on a seven-variable monomial curve.

### Searches this rests on

Titles and bodies for `reesIdeal`, `ReesAlgebra`, `symmetricKernel`, `symmetricAlgebraIdeal`, `saturate`,
`flattenRing`, `tower` and `isFlatPolynomialRing`, and comments via `gh search issues` for `reesIdeal`.
Nothing mentions `reesIdeal` outside the package's own history.
[#4308](https://github.com/Macaulay2/M2/issues/4308) is `(saturate, Module, Ideal)` being broken, a
different method; the open `flattenRing` issues
([#2133](https://github.com/Macaulay2/M2/issues/2133),
[#3887](https://github.com/Macaulay2/M2/issues/3887),
[#4578](https://github.com/Macaulay2/M2/issues/4578),
[#4579](https://github.com/Macaulay2/M2/issues/4579)) are about `flattenRing` itself rather than about
callers that fail to use it.
