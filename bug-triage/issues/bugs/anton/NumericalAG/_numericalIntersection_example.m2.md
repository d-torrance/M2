Two witness-set checks in
[`witness-set.m2`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/NumericalAlgebraicGeometry/witness-set.m2)
fail intermittently on small inputs, and between them they produce a wrong answer, no answer, and an
error, for the same input on different runs. Both checks are marked provisional in the source, so
this may be less "these are broken" than "these were never finished".

## 1. `numericalIntersection` gives four different answers

```m2
needsPackage "NumericalAlgebraicGeometry"
CC[x,y,z]
sph = (x^2+y^2+z^2-1);
I = ideal {sph*(x-1)*(y-x^2), sph*(y-2)*(z-x^3)};
for i from 1 to 12 do (
    V := numericalIrreducibleDecomposition I;
    r := try (
        V12 := numericalIntersection(last V#1, first V#2);
        concatenate("dims ", toString sort keys V12, "  degs ",
            toString flatten apply(sort keys V12, k -> apply(V12#k, degree)))
        ) else "ERROR";
    << "run " << i << ": " << r << endl << flush;
    )
```

Eleven completed runs on `1.26.06-40-gd8e86d689d (development)`:

| outcome | runs |
| --- | --- |
| one component, `dim 0`, degree 1 | 5 |
| no components at all | 2 |
| error (below) | 2 |
| one component, `dim 0`, degree 3 | 1 |
| one component, `dim 0`, degree 2 | 1 |

`w1` is a curve (`dim 1`, degree 3) and `w2` is the sphere (`dim 2`, degree 2), so a finite set of
points is the right shape; how many is what the runs disagree about. The empty answer is the worst of
them, because the caller cannot tell it from a genuinely empty intersection.

The error is `assertion failed`, from

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/NumericalAlgebraicGeometry/witness-set.m2#L131-L133

That branch of `movePoints` is taken only when `status P === Singular`, which is why it fires
intermittently — whether a witness point comes out singular depends on the random slice chosen at

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/NumericalAlgebraicGeometry/intersection.m2#L161-L164

## 2. `numericalIrreducibleDecomposition` fails its own witness-set check

```m2
needsPackage "NumericalAlgebraicGeometry"
CC[x,y,z]
sph = x^2+y^2+z^2-4;
f1 = sph*(y^2-x^4);
f2 = sph*(y-1)*z;
f3 = sph*(z-1)*z;
elapsedTime numericalIrreducibleDecomposition ideal(f1,f2,f3)
```

Six runs, each a fresh process:

| outcome | runs |
| --- | --- |
| `error: check failed` after ~1.5 s | 2 |
| still running after 240 s | 4 |

The error is

```
witness-set.m2:26:10:(2):[19]: error: check failed
```

from

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/NumericalAlgebraicGeometry/witness-set.m2#L24-L26

so a witness point came back with a residual over `1000*DEFAULT.Tolerance`. As with the assertion
above, the author's own comment on the line says the check is not settled: *"should we check this
differently: e.g., Newton method convergence?"*

The runtime spread is worth flagging separately, without a claim about its cause: this is three
polynomials in three variables, and the runs that do not fail fast do not finish in four minutes. One
earlier attempt ran fifty minutes without completing a single decomposition. I have not established
what those runs are doing, so this is an observation rather than a diagnosis.

## Not the same as #151 or #1456

[#151](https://github.com/Macaulay2/M2/issues/151) is also about `NumericalAlgebraicGeometry` being
nondeterministic, but it is a `SIGSEGV` traced to a gc/mpfr interaction at the C level, and
[#1456](https://github.com/Macaulay2/M2/issues/1456) is a `SIGSEGV` in `PathTracker::make`. Both of
these are M2-level checks on randomized code paths, and the wrong answers in the first section are
returned with no error at all.

## Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). Both examples are Anton's, from
`bugs/anton/NumericalAG/_numericalIntersection_example.m2` and
`bugs/anton/NumericalAG/_numericalID_example.m2`, and both files record the failure they still show.
The first pasted an output ending `a numerical variety with components in` with nothing following,
which is how an empty `NumericalVariety` prints; the second pasted a backtrace whose error is
`witness-set.m2:13: error: check failed` — the same check, at the line it occupied then.
