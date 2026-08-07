The raw hooks exist and are complete; what is missing is any front end. And there are **two**
implementations, which is the first thing to settle.

### What exists

| | file | lines | base ring | reachable how |
| --- | --- | ---: | --- | --- |
| `Tower` | `e/rings/tower.cpp` | 560 | `ZZ/p` | `rawTowerRing*`, and the monoid option `Constants => true` |
| `ARingTower` | `e/basic-rings/aring-tower.{cpp,hpp}` | 438 + 614 | **`ARingZZpFFPACK` only** | `rawARingTower*` — no callers anywhere |

Both wrap the same arithmetic core, `DPoly`/`DRing` in `e/rings/dpoly.hpp`, and all six raw entry
points are wired into `d/interface.dd`. None is exported, so from a user session `rawTowerRing(...)`
is a `Symbol` until you `debug Core`. Both print as `Tower[ZZ/5[x,y,z]]`, so they are
indistinguishable from above.

Note that `ZZ[x][y][z,w]` does **not** reach either: the engine sees one flat `PolyRing` over `ZZ`
with four variables, and the tower is front-end bookkeeping.

### What the towers are for, and the one thing they do better

From the top of `dpoly.hpp`: *"Code for univariate polynomials over algebraic extensions of QQ and
over finite fields … monic gcd mod p over extension fields, modular gcd algorithm."* And that shows
up measurably. For two polynomials sharing the factor `x^5 + a*x + b` over `F5[a]/(a²-2)[b]/(b²-a)`:

| | result |
| --- | --- |
| `Tower`, via `rawTowerQuotientRing` | `x5+ax+b` — **correct** |
| flat ring + factory | **`1` — wrong** (and `f % d == 0`, `g % d == 0` both hold) |

That wrong answer is #4583. The tower representation is immune to it because the extension chain is
part of the ring rather than something factory must be told.

### What is broken on the only reachable route

`(ZZ/5)[x,y,z, Constants => true]` (`polyrings.m2:163-164`, carrying `-- TODO: document this`) gives
a `PolynomialRing` over a tower with **no relations**, where those univariate algorithms do not apply
and fail silently: `rawGCD(raw x, raw x)` is `0`, `x^2 % x` is `x^2`, `x^2 // x` is `0`,
`promote(1_(ZZ/5), R)` errors, and `gens gb ideal(x^2-y)` **segfaults** in
`GaussElimComputation::insert` (`e/gauss.cpp:50`). Test coverage is two tests in
`unit-tests/RingTowerTest.cpp`.

### What Dan asked for

`engineTowerRing`, a `class EngineTowerRing`, quotients by a tower of relations, and working
`promote`/`lift` — of which `promote` is precisely what fails first today.
