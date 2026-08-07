Unchanged, and the asymmetry with `ideal` is what makes the message wrong rather than merely terse:

```m2
i1 : ideal {}
o1 = ideal()                                              -- accepted

i2 : monomialIdeal {}
stdio:2:1:(3): error: expected a polynomial ring without quotient elements
```

The complaint the error makes is not the caller's problem. There is no ring in the input at all —
`{}` names none — so "expected a polynomial ring without quotient elements" describes a ring that was
never supplied, and a reader is left looking for a quotient ring they did not use.

For contrast, the same empty ideal is reachable as soon as a ring is in scope:

```m2
i3 : R = QQ[x,y];
i4 : monomialIdeal 0_R
o4 = monomialIdeal()                                      -- fine
```

### What a fix would look like

Either accept `monomialIdeal {}` the way `ideal {}` is accepted — note that `ideal {}` answers over
`ZZ`, so the two would then agree — or raise the error `ideal {}` would raise if it were rejected,
naming the real problem: an empty list carries no ring, so there is nothing to build the ideal in.
The second is the smaller change and arguably the more honest one, since the empty monomial ideal
over `ZZ` is not obviously useful.

Related decision on the record: **#3328** deliberately declined to give `intersect` and `tensor`
0-argument forms because defaulting the ring to `ZZ` "causes hard to find bugs" (Mike Stillman), so
whichever way this goes it should probably be consistent with that.
