<!-- issue: #150 -->
The user-visible consequences of this, twelve years on, and one wishlist request that turns out to be blocked
by exactly what you describe.

`Ext` and `Tor` are guarded off over noncommutative rings, while `Hom` — the thing this issue says cannot be
an $R$-module without an antiautomorphism — goes through with no check at all:

```m2
i1 : E = QQ[e_1,e_2,e_3, SkewCommutative => true];

i2 : M = E^1/(ideal e_1); N = E^1/(ideal e_2);

i4 : Hom(M, N)
o4 = subquotient (matrix {{e_1}}, matrix {{e_2}})

i5 : Ext^1(M, N)
stdio:5:1:(3): error: 'Ext' not implemented yet for noncommutative rings.

i6 : Tor_1(M, N)
stdio:6:1:(3): error: 'Tor' not implemented yet for noncommutative rings.
```

on 1.26.06-40-gd8e86d689d. The guard is in five places, all with the same message:

| file | line | functor |
| --- | --- | --- |
| `Complexes/Tor.m2` | 8, 28 | `Tor` |
| `Complexes/ChainComplex.m2` | 1189 | `Ext` |
| `Complexes/ChainComplexMap.m2` | 719, 738 | `Ext` |

`Depth.m2:63` carries the same pattern for `depth`. Nothing guards `Hom`.

**Which suggests the asymmetry runs the wrong way.** Reading this issue, `Hom` is the one that needs the
antiautomorphism M2 does not have, so `Hom` succeeding while `Ext` and `Tor` refuse looks like the opposite of
what the mathematics wants. Whether the right fix is to guard `Hom` too, or to add the antiautomorphism
provision and lift all of them, is a question for someone who knows this code — but it seemed worth recording
that the current behaviour is inconsistent in that specific direction rather than merely incomplete.

### A 2003 wishlist asked for `Tor` here

`bugs/dan/0-bugs-eisenbud.m2`, one of the 857 files removed with the `bugs/` tree in d2c8d27826 and
catalogued in #36, contains the one-line request

> Tor should allow noncommutative rings (exterior algebra)

I was going to file that as its own issue until this one turned up, at which point it became clear that the
request is blocked on the structural gap described here rather than on anybody simply not having got round to
`Tor`. So it is recorded as a duplicate of this issue instead.

### And a recent attempt lapsed

[#4459](https://github.com/Macaulay2/M2/pull/4459), "Compute Hom and Ext over SkewCommutative rings", was
opened in June 2026 and closed **without being merged** — its description opens "NOT READY FOR REVIEW AT ALL"
and it carried changes from #4435 and #4176 that were awaiting review. Useful mainly as evidence that the
capability is still wanted by people other than the original requester, and that a partial attempt exists in
the history for anyone picking this up.
