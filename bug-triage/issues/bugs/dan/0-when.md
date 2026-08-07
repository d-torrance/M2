`scc1` still miscompiles it, and the failure is silent dead code rather than a diagnostic.

With every case of the union covered:

```
A := {+ x:int };
B := {+ y:int };
C := A or B;
f(x:C):int := when x is A do 1 is B do 2 else 3;
```

`chk.c:783-794` labels only the *uncovered* types, so with `A` and `B` both handled there is no label
left for the `else` body — and it is emitted inside the switch with no case label at all. The result is
unreachable code that nothing diagnoses.

### Why it is worth fixing rather than avoiding

An `else` that cannot run is exactly the situation where a compiler should say so. As it stands, a `.d`
author who writes a defensive `else` gets no warning that it is inert, and a later change that *adds* a
member to the union will silently start routing through the newly-reachable branch — so the bug is
latent rather than merely cosmetic.

Adjacent: **#4542** is the other `when` defect in `scc1` (a statement-form `when` whose branches all
return is not recognized as returning), so the two are worth looking at together.
