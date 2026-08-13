<!-- issue: #2192 -->
This was first reported in 2016, and the behaviour has changed since — for the better, but not in a way that
closes it.

`bugs/mike/git-issue475-openblas.m2`, one of the files removed with `bugs/` in d2c8d27826 (#36), carries
this as a block of asserts. Its filename points at #475 (openblas) and the first and last thirds of the file
really are about `ZZp` strategies and `Ffpack`/`Flint`, but the middle is exactly the defect here:

```m2
kk = ZZ/101
M = mutableMatrix(ZZ, 2, 2)
M_(1,1) = 4
assert(M != 0)
assert(matrix promote(M, kk) == promote(matrix M, kk)) -- false
-- the following should not be 0!
assert(promote(M,ZZ,kk) != 0)

-- lift has the same problem
M1 = mutableMatrix(kk, 2, 2)
M1_(1,1) = 4
assert(M1 != 0)
assert(matrix lift(M1, ZZ) == lift(matrix M1, ZZ)) -- false
-- the following should not be 0!
assert(lift(M1,kk,ZZ) != 0)
```

Three things in that worth adding to this issue.

**The 2016 symptom was a silent wrong answer; today it raises.** The `-- false` and `should not be 0!`
annotations record that `promote` and `lift` were *returning zero matrices*. On 1.26.06-40-gd8e86d689d they
error instead — `promote(M, kk)` gives `MutableMatrix promote not implemented yet` from `enginering.m2:129`,
matching the message quoted above. So anyone picking this up should know there is no longer a wrong-answer
path to reproduce, only a missing method. That is a genuine improvement and it means the risk profile of
this issue is lower than the 2016 file implies.

**`lift` is broken in the same way, and its message is misleading.** This issue's title and body are about
promotion, but the other direction fails too, and less helpfully:

```m2
i1 : kk = ZZ/101; M1 = mutableMatrix(kk, 2, 2); M1_(1,1) = 4;

i4 : lift(M1, ZZ)
stdio:4:1:(3): error: cannot lift given matrix

i5 : lift(matrix M1, ZZ)
o5 = | 0 0 |
     | 0 4 |
```

`cannot lift given matrix` reads as a statement that the lift is mathematically impossible, but the
immutable version of the same matrix lifts fine, so the obstruction is only the missing `MutableMatrix`
method. That comes from `enginering.m2:138`, a few lines from the `promote` case at `:129`. Whatever fixes
one should probably fix the other, and the `lift` message wants rewording either way.

**There is an exact workaround, for anyone who lands here from a search.** Round-trip through the immutable
type:

```m2
mutableMatrix promote(matrix M, kk)   -- {{0, 0}, {0, 4}}
mutableMatrix lift(matrix M1, ZZ)     -- {{0, 0}, {0, 4}}
```

That is only useful for matrices small enough that materialising an immutable copy is acceptable, which is
not the case that motivates mutable matrices — the same 2016 file goes on to build a 500-by-600 mutable
matrix over `ZZ` and promote it to a `Flint` `ZZ/32003`, which is presumably the shape anyone actually wants
this for.

<sub>The 2016 file also asserts `M != 0` and `M1 != 0` before each of the failing pairs; both of those still
pass, so the mutable matrices themselves are fine and it is only the coercion that is absent.</sub>
