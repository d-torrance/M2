`track` and `trackHomotopy` both record a `NumberOfSteps` in each solution's cache, and
[the documentation](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/NumericalAlgebraicGeometry/doc.m2#L445)
gives it one meaning for both — *"number of steps taken on the corresponding homotopy path"*. The two
compute it by different conventions.

`trackHomotopyM2engine` records the count itself:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/NumericalAlgebraicGeometry/track.m2#L677-L682

`track`'s own loops subtract one, in two places, and say why:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/NumericalAlgebraicGeometry/track.m2#L578-L582

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/NumericalAlgebraicGeometry/track.m2#L1019-L1023

So one reports the number of steps and the other the number of points minus one. Whichever is
intended, both cannot be, and a caller reading `NumberOfSteps` gets a quantity that depends on which
entry point produced the solution.

### Reproducing

```m2
debug needsPackage "NumericalAlgebraicGeometry"
n = 2; d = 2;
R = QQ[x_0..x_(n-1)]
eps = 10^-3
T = apply(n, i -> if i == 0 then x_i^d - eps^d else (x_i-i)^d - eps^(d-1)*x_i)
(S, solsS) = totalDegreeStartSystem T
H = segmentHomotopy(S, T, gamma => 1+pi*ii)
tsm = minimalStepSize 53
sols  = trackHomotopy(H, solsS, tStepMin => tsm, CorrectorTolerance => 1e-8, EndZoneFactor => 0)
sols' = track(S, T, solsS, tStepMin => tsm, CorrectorTolerance => 1e-8, EndZoneFactor => 0.)
apply(sols,  p -> p.cache.NumberOfSteps)   -- {22, 23, 22, 23}
apply(sols', p -> p.cache.NumberOfSteps)   -- {23, 23, 23, 23}
```

Both return the same four solutions. The two solution records differ in shape as well, which is how
you can tell which branch produced each: `trackHomotopy` gives
`{H, LastIncrement, LastT, NumberOfSteps, SolutionStatus}` and `track` gives
`{ConditionNumber, LastT, NumberOfSteps, SolutionStatus, SolutionSystem, Tracker}`.

### What fixing the convention would not do

It would not make those two lists agree, and the issue should not be read as claiming otherwise.
`trackHomotopy` reports *fewer* steps here, where the off-by-one alone would make it report one more,
so the underlying iteration counts differ too — these are genuinely separate steppers that happen to
land on the same solutions. Only the naming is being reported here: the same documented field means
two things.

`Precision` is not involved. `trackHomotopy` gives `{22, 23, 22, 23}` with or without
`Precision => infinity`, and `track` does not accept the option at all.
