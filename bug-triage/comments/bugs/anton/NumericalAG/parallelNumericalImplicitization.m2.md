Still reproducing in 2026, and it does not appear to be about threads.

On `1.26.06-40-gd8e86d689d (development)`, `numericalImageDegree` returns `1` instead of `60` in a
quarter of runs on this input:

```m2
needsPackage "NumericalImplicitization"
R = CC[s,t]; F = flatten entries basis(60,R);
allowableThreads = 4
for i from 1 to 6 do (
    a := numericalImageDegree(F, ideal 0_R, MaxThreads => 1, Verbose => false);
    << "serial   run " << i << " -> " << toString a << endl << flush;
    )
for i from 1 to 6 do (
    b := numericalImageDegree(F, ideal 0_R, MaxThreads => 4, Verbose => false);
    << "parallel run " << i << " -> " << toString b << endl << flush;
    )
```

```
serial   run 1 -> 60      parallel run 1 -> 60
serial   run 2 -> 60      parallel run 2 -> 60
serial   run 3 -> 1       parallel run 3 -> 1
serial   run 4 -> 60      parallel run 4 -> 60
serial   run 5 -> 1       parallel run 5 -> 60
serial   run 6 -> 60      parallel run 6 -> 60
```

Three failures in twelve, two of them at `MaxThreads => 1`. Since the serial and parallel halves fail
at about the same rate, parallelism does not look implicated — which matters here, because the bug
file this came from was written about the parallel path specifically.

Two things worth noting about the shape of the failure. The wrong answer is `1`, not a near miss like
the `17` instead of `18` in the transcript above, so the run is not stopping one point short — it is
finishing the trace test having found essentially nothing. And it is silent: the function returns
normally, so a caller gets a plausible small integer with no indication anything went wrong.

I have not varied the LAPACK version, so this says nothing either way about the attribution in the
title; it only shows the symptom is still present five years on, on a current build.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36).
`bugs/anton/NumericalAG/parallelNumericalImplicitization.m2` runs the code above and has a long
transcript pasted after its `end` line, in which the recorded failure is a different one —
`error: degree list should be of length 6` during parallel tracking, which is #3239, closed as fixed
in #4378. That error no longer occurs. The wrong image degree is what remains.
