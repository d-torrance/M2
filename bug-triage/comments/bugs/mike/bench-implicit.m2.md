<!-- issue: #4590 -->
A second file from the same directory gives the inhomogeneous case a price, which this issue currently
argues for from the source alone.

`bugs/mike/bench-implicit.m2`, also removed with `bugs/` in d2c8d27826 (#36), is an implicitization
benchmark whose map is exactly the shape described above — degree-1 source variables, quadric images, so
nothing in sight is homogeneous:

```m2
n = 5
R = ZZ/32003[vars(0..n-1)]
S = ZZ/32003[vars(26..26+n)]
phi = map(R,S,random(R^1,R^{(numgens S):-2}))
gbTrace=3
ker phi;
```

The only change worth making to it is to regrade `S` so that the map becomes graded. That leaves `ker phi`
the same set of polynomials — the grading of `S` has no bearing on which polynomials go to zero — and alters
only whether the algorithm can see it as homogeneous:

|  | `isHomogeneous phi` | graph ideal homogeneous | `canUseHilbertHint` | `ker phi` |
| --- | --- | --- | --- | --- |
| `S = ZZ/32003[vars(26..31)]` | false | false | **false** | no result in 600 s |
| `S = ZZ/32003[vars(26..31), Degrees => {6:2}]` | true | true | **true** | **196 s** |

`canUseHilbertHint` is the predicate at `gb.m2:271` named above, and the fast row is the one where it
returns true. The 196-second answer is a single generator, degree 32 in the regraded ring, so the degree-16
implicit equation of the image hypersurface. Reproduced on 1.26.06-40-gd8e86d689d with `setRandomSeed 42`.

### What this does and does not bound

**It does not measure what implementing the homogenize/dehomogenize path would buy.** Regrading buys
homogeneity in general, and a graded Gröbner basis gains degree-by-degree progress and early termination
quite apart from any Hilbert hint. So 196 s against 600 s is a ceiling on the prize, not an estimate of it.

**The 600 s is a lower bound**, since that run was killed rather than finishing.

Read narrowly, then: on a five-line example in six variables, the gap between having the hint machinery
available and not having it is at least a factor of three and possibly unbounded, on a computation that is
otherwise small. That seemed worth recording next to the code reading, since an issue with no numbers on it
is easy to file under "would be nice".

One incidental warning for anyone re-running the file: `vars(26..31)` is `A..F`, so a matrix held in a
variable named `F` is silently clobbered the moment `S` is created. The original avoids this only by
inlining the matrix into the `map` call, and it cost me two confusing runs.
