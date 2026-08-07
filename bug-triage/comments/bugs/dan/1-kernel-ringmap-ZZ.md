Two small updates on this, from a 2008-era file that reports the same thing.

`bugs/dan/1-kernel-ringmap-ZZ` in the pre-GitHub `bugs/` tree (being triaged in #36) has the same
transcript as this issue — `R = ZZ[x]`, `f = map(R,R,{0})`, then `ker f` refusing — so this issue is
where it belongs rather than in a new one.

**The wording has moved**, which matters if anyone is grepping for it. The old message was
`kernel Ringmap: not implemented yet`. Since `kernel RingMap` became hook-based it is:

```m2
i1 : R = ZZ[u];
i2 : ker map(R,R,{0_R})
     error: kernel: no method implemented for this type of ring map
```

raised from `Core/methods.m2:695` when no registered strategy returns a value, rather than from a
dedicated "not implemented" branch.

**There is now a `ZZ` strategy, but it answers a different question.** `m2/ringmap.m2:336`
registers

```m2
ZZ => (opts, f) -> if source f === ZZ then ideal char target f,
```

so it fires only when the *source* is `ZZ` itself. A map `ZZ[u] -> ZZ[u]` has source `ZZ[u]`, falls
past it to `Default`, and fails — so the gap this issue describes is exactly the one still open,
and the strategy table is where a fix would now be registered rather than in `kernel` itself.

For context on that table: it currently holds `FractionField` (`:241`), `"AffineRing"` (`:307`),
`ZZ` (`:336`) and `Default` (`:338`), and the `"AffineRing"` strategy requires
`isField coefficientRing R` — which is the specific reason `ZZ` coefficients fall through.
