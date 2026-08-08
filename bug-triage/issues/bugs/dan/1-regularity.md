
Unmet in every particular, and the mechanism it asks for has since become M2's standard way of doing
this — so it is cheaper now than when the note was written.

### There is no dispatch point

`regularity` is declared in Core with one option:

```m2
regularity = method(TypicalValue => ZZ, Options => { Weights => null })   -- betti.m2:244
```

No `Strategy`, no `Algorithm`, and no `addHook`/`runHooks` anywhere for it. The working methods live in
`Complexes`:

```m2
regularity Ideal   := opts -> I -> ... 1 + regularity betti(freeResolution liftModule comodule I, opts)
regularity Module  := opts -> M -> regularity betti(freeResolution liftModule minimalPresentation M, opts)
regularity Complex := opts -> C -> ...
```

(`Complexes/ChainComplex.m2:640-654`.) Every route goes through `betti(freeResolution …)`, hard-coded,
with nowhere for an alternative algorithm to attach.

### So the package has to use a different name

`packages/Regularity.m2` exists and exports `mRegularity`, `CM`, `MonCurve` (`:22`). A user who writes
`regularity I` cannot reach it. The package's own tests compare the two by hand:

```m2
assert(mRegularity(I, MonCurve => true) == regularity I)
```

which is the shape of thing a `Strategy` exists to avoid.

The fragmentation is still growing: #4191 is an open pull request adding `weightedRegularity` to
`Depth.m2`, a third name for a closely related computation.

### Why this is easier than in 2009

When the note was written, hooks were a new idea. They are now the conventional pattern for exactly this
situation — `freeResolution`, `pushForward`, `minimalPrimes` and `kernel` all dispatch through
`runHooks`, and each lets a package register an algorithm under a `Strategy` name without touching the
caller. Making `regularity` one of them is following a paved road.

It also answers the note's second clause for free — *"The value of Strategy should be checked, somehow"* —
since `runHooks` reports when no registered strategy accepts the input, rather than silently doing
something else.

### Related

#3321 (closed) was about `regularity` for modules over quotient rings, not about dispatch.
