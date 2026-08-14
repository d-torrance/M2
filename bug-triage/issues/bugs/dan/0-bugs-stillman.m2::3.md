`conjugate` has no method for `Matrix` or `MutableMatrix`, and there is no conjugate transpose under any
spelling — in a system whose `SVD`, `eigenvalues`, `eigenvectors` and `solve` over `CC` are all LAPACK-backed
and all defined in terms of the adjoint.

### What is there

```m2
i1 : methods conjugate

o1 = {0 => (conjugate, Constant) }
     {1 => (conjugate, Partition)}
     {2 => (conjugate, CC)       }
     {3 => (conjugate, CCi)      }
     {4 => (conjugate, Number)   }

i2 : conjugate matrix{{ii}}
stdio:2:1:(3): error: no method for adjacent objects: ...

i3 : conjugate mutableMatrix{{ii}}
stdio:3:1:(3): error: no method for adjacent objects: ...
```

And `transpose` does not conjugate, so the composition is unavailable too:

```m2
i4 : transpose matrix{{ii,2*ii}}

o4 = | ii  |
     | 2ii |
```

The workaround is `matrix apply(entries m, r -> conjugate \ r)`.

### This is already assumed to exist, in two places in this repository

**`EngineTests` defines it locally in order to test `SVD`.**
[`packages/EngineTests/LinearAlgebra.Test.eigen.m2:3-13`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/EngineTests/LinearAlgebra.Test.eigen.m2#L3-L13):

```m2
conjugate Matrix := (M) -> (
    L := entries M;
    map(target M, source M, L/(L1 -> L1/conjugate))
    )
...
     errU := norm((conjugate transpose U) * U - 1);
     errV := norm(Vt*(conjugate transpose Vt) - 1);
```

That is the engine's own test that `SVD` returns unitary factors, and it cannot be written without first
supplying the missing method. It is four lines, and it is the implementation this issue is asking for.

**A distributed package assumes it and is broken.**
[`DeterminantalRepresentations.m2:378-384`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/DeterminantalRepresentations.m2#L378-L384):

```m2
approxKer Matrix := Matrix => opts -> A -> (
    d := numcols A;
    (S,U,Vh) := SVD A;
    n := #select(S, s -> clean(opts.Tolerance, s) == 0);
    conjugate transpose Vh^{d-n..d-1}
)
```

The package does not define `conjugate Matrix` — that line is its only occurrence of the word — so
`approxKer` cannot return:

```m2
i1 : needsPackage "DeterminantalRepresentations";

i2 : approxKer matrix{{1_CC, 2, 3},{4, 5, 6},{7, 8, 9}}
stdio:2:9:(3): error: no method for adjacent objects: ...
```

`approxKer` is called from `cubicSurfaceDetRep`, which `detRep` dispatches to for a cubic in four variables —
and `detRep` is exported and documented. `cubicSurfaceDetRep`, `linesOnCubicSurface` and `doubleSixes` are all
in the package's `undocumented` list, so no example runs that branch under `check`, which is presumably how it
has stayed this way.

**What I did not establish.** I could not drive `detRep` end to end into `approxKer`. My cubic-surface input
failed earlier, inside `linesOnCubicSurface`, with `array index 0 out of bounds 0 .. -1` — a different problem,
and I did not pursue it. So what is demonstrated is that `approxKer` cannot succeed for any argument, not that
a particular `detRep` call reaches it.

### Naming

`adjoint` is taken: `m2/Hom.m2:85` uses it for the Hom-tensor adjunction, with `adjoint'` for the other
direction. So the conjugate transpose needs a different name — `conjugateTranspose`, or an option on
`transpose`.

### Provenance

The request is the first of three "RR and CC stuff" items in a 2005 scratchpad:

> `-- (1) conjugate should work on a MutableMatrix, or Matrix`
> `--     same with conjugate-transpose?`

Its sibling `(2)`, "`U^-1`, for `U` a matrix over `CC`, is WRONG", was checked at the same time and is fixed
for invertible matrices; the singular case, where `inverse` silently returns the zero matrix, is
[#4556](https://github.com/Macaulay2/M2/issues/4556).
