This machinery has a second site with the same gap, from a file that predates the issue.

`bugs/dan/1-monoid-degreeLift` in the pre-GitHub `bugs/` tree (being triaged in #36) asks that the
monoid code compute a degree lift *the way `ringmap.m2` does*, and quotes both sides. Both quotes
are still byte-identical to the current source. The monoid side, now `m2/monoids.m2:695-711`, gives
up instead of computing:

```m2
opts.DegreeLift = (
     if lm === null then (
          if dm === identity then (
               d -> (
                    for i from #N0 to #M0-1 do if d#i =!= 0 then degreeNoLift();
                    drop(d,#M0-#N0)))
          else x -> error "degree lift function not provided (DegreeLift option)")
     else lm);
```

while the ring-map side, now `m2/ringmap.m2:98-104`, inverts the degree map:

```m2
else (d -> (
          (q,r) := quotientRemainder(transpose matrix {d}, degmapmatrix);
          if r != 0 then error "degreeLift: degree not liftable";
          flatten entries q)));
```

and that does work in the singly-graded case — for a doubling `DegreeMap`, `f.cache.DegreeLift {2}`
returns `{1}`.

**The reason this is a comment here rather than a new issue** is that the implementation the file
says to copy is the one this issue reports as broken, and it still reproduces exactly as written
above:

```m2
i1 : A = QQ[x,y, Degrees => {{1,0}, {0,1}}];
i2 : B = newRing(A, Degrees => reverse degrees A);
i3 : f1 = map(A, B, gens A, DegreeMap => reverse);
i4 : isHomogeneous f1, isHomogeneous inverse f1
o4 = (true, false)
i5 : f2 = map(A, B, gens A, DegreeMap => reverse, DegreeLift => reverse);
i6 : isHomogeneous inverse f2
o6 = false
```

So propagating it into `monoids.m2` would spread the multigraded defect to a second place. Whatever
fixes the lift here presumably wants to serve both call sites, which makes the monoid gap part of
this issue's scope rather than a separate request.
