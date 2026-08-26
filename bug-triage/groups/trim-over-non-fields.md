# Group 1: minimal generators over rings that are not affine

**Primary file:** `M2/Macaulay2/m2/matrix2.m2`, lines 198-320
**Secondary:** `M2/Macaulay2/m2/gb.m2:209-241`
**Selected by:** `bin/cluster`, top-ranked candidate (score 16.4, and first under six of the
seven weightings tried — see [Why this group](#why-this-group))

Six issues, **three** distinct causes, one file. Two of the six are silent wrong answers in
`trim`, and **both are fixed by a one-line change that has been written and run** (below).

| # | type | filed | severity | one line |
| --- | --- | --- | --- | --- |
| [#747](https://github.com/Macaulay2/M2/issues/747) | Bug | 2018-03 | **wrong answer** | `trim` over `ZZ[a,x,y]` silently drops a generator |
| [#4095](https://github.com/Macaulay2/M2/issues/4095) | Bug | 2026-01 | **wrong answer** | same defect over `ZZ[x]`; `trim I` is not even a subideal-preserving map |
| [#837](https://github.com/Macaulay2/M2/issues/837) | Bug | 2018-08 | redundant output | `mingens` over `ZZ[i]/(i^2+1)` returns a redundant generator |
| [#256](https://github.com/Macaulay2/M2/issues/256) | Bug | 2015-05 | **wrong answer** | `f // gb g` silently returns 0 instead of the quotient |
| [#278](https://github.com/Macaulay2/M2/issues/278) | Task | 2015-06 | — | the `gb trim M` optimisation, commented out in 2015 |
| [#105](https://github.com/Macaulay2/M2/issues/105) | Task | 2014-03 | doc | should `mingens` error on inhomogeneous input? |

All six were **re-verified on 2026-08-26** against a `development` build — see
[Testing notes](#testing-notes), which contains the one thing that will waste your afternoon if
you skip it.

---

## Cause (a) — `StopWithMinimalGenerators` assumes a field of coefficients

Covers **#747** and **#4095**.

`matrix2.m2` defines the same helper twice, once inside `mingens Module` (:198) and once inside
`trim Module` (:231), byte-identical:

```m2
mingb := m -> gb (m, StopWithMinimalGenerators=>true, Syzygies=>false, ChangeMatrix=>false);
```

`StopWithMinimalGenerators` stops the Groebner computation as soon as the generators are minimal.
That test is a statement about lead terms, and it is only sound when the coefficients form a
field. Over `ZZ` a generator can fail to be redundant for reasons no lead term records, so the
computation stops early and the generator is thrown away — with no error, and no warning.

The only guard against this is at :281, inside `trim Module`:

```m2
if ring M === ZZ then ( ... LLL ... )
```

`ring M === ZZ` is an identity test on the ring itself, so `ZZ[x]` fails it, `ZZ[x,y]` fails it,
and `ZZ[i]/(i^2+1)` fails it. This is the failure mode `README-bugs-directory.md` singles out:
*"A guard that checks the top of a ring tower is the recurring defect here."*

### What it looks like

```m2
R = ZZ[a,x,y]
I = ideal(a^2+5, (1+a)*x - 2*y, 3*x - (1-a)*y)
J = ideal(x*y - x^2 - y^2 - 1)
K = ideal(3, 1-a)
trim(trim(I+K)+J)   -- | 3 x-y a-1 y^2+1 |
trim(trim(I+J)+K)   -- | 3 x-y a-1 |        <- y^2+1 is gone
```

The second is a **strict subideal** of the first (`isSubset` one way and not the other, checked),
so this is not a presentation difference. #4095 is sharper still:

```m2
S = ZZ[x]
trim ideal(x^2+x+1, x^4+x^3+x^2+x+1)   -- ideal(x+1), but the ideal is the unit ideal
trim ideal(x^2+1, x^3+1)               -- ideal(x-1), but the ideal is ideal(2, x+1)
```

In the second the constant is dropped *and* the linear form changes. The report and the
workaround in its thread both frame this as being about the unit ideal; it is not.

### Candidate patch — written, run, and it works

```diff
--- a/M2/Macaulay2/m2/matrix2.m2
+++ b/M2/Macaulay2/m2/matrix2.m2
@@ -195,7 +195,7 @@ mingens Module := Matrix => opts -> M -> M.cache.mingens ??= ...
 	if opts.Strategy === null then opts = opts ++ { Strategy => Complement };
-	  mingb := m -> gb (m, StopWithMinimalGenerators=>true, Syzygies=>false, ChangeMatrix=>false);
+	  mingb := m -> gb (m, StopWithMinimalGenerators=>isAffineRing ring M, Syzygies=>false, ChangeMatrix=>false);
@@ -228,7 +228,7 @@ trim Module := Module => opts -> M -> ...
- 	  mingb := m -> gb (m, StopWithMinimalGenerators=>true, Syzygies=>false, ChangeMatrix=>false);
+ 	  mingb := m -> gb (m, StopWithMinimalGenerators=>isAffineRing ring M, Syzygies=>false, ChangeMatrix=>false);
```

Both occurrences, one predicate. **`isAffineRing` is exactly the right test and it is already
used in this file at :205**, so this is reuse rather than a new predicate:

| ring | `isAffineRing` | behaviour today |
| --- | --- | --- |
| `ZZ`, `ZZ[x]`, `ZZ[x,y]`, `ZZ[i]/(i^2+1)`, `(ZZ[x])[y]` | false | broken |
| `QQ`, `QQ[x]`, `ZZ/7[x]`, `QQ[x]/x^2`, `GF 9`, `(GF 9)[x]`, `frac(QQ[u])`, `(frac(QQ[u]))[v]` | true | fine |

The correspondence is exact on every ring tried — every broken ring is non-affine and every
working ring is affine.

**Result, measured.** With the patch, all three wrong answers become right: #747's three
orderings agree and keep `y^2+1`; `trim ideal(x^2+x+1, x^4+x^3+x^2+x+1)` becomes `ideal 1` with
`T == I` true and `1_S % T` zero; `trim ideal(x^2+1, x^3+1)` becomes `ideal(2, x+1)`. This is
the fix #747's own triage note predicted — *"forcing gens gb first fixes both this and #4095"* —
reached from the other direction, by not stopping early rather than by pre-computing.

**Blast radius.** All 32 tests under `tests/normal/` that mention `trim`, `mingens` or
`minimalPresentation` were run before and after, and the pass/fail set is **identical**: 30 pass,
and `000-core.m2` and `hom.m2` fail *both* ways. Those two failures are pre-existing and unrelated
— `000-core.m2` dies at `variables.m2:74: baseName: no base name available`, reached through
`polyrings.m2` and `monoids.m2`, nowhere near this code. No test got slower by more than noise;
the four largest deltas were all *faster* under the patch, which is measurement noise on a 5-second
startup, not a speedup.

**Cost.** The patch trades a fast wrong answer for a slower right one, and the slowdown is real
but small in absolute terms:

| | unpatched | patched |
| --- | --- | --- |
| `trim` over `ZZ[a..e]`, 5 generators | 0.00088s | 0.00605s |
| `trim` over `ZZ[x,y]` | 0.00069s | 0.00077s |
| `trim` over `QQ[a..e]` (unaffected path) | 0.00085s | 0.00071s |
| `mingens` over `ZZ[x]` | 0.00034s | 0.00034s |

The `QQ` row is the one that matters for regression risk: the affine path is untouched, as
intended. **This is a small sample and the honest caveat is that nobody has run this against a
large `ZZ` computation** — if a slowdown shows up anywhere it will be there, and it is worth one
deliberate check before the PR.

---

## Cause (b) — nothing stops you asking a Groebner basis for data it does not have

Covers **#256**, and rewrites what **#278** is.

`gb.m2:209`:

```m2
quotient(Matrix, GroebnerBasis) := Matrix => o -> (n, G) -> (
    -- this gb might not be one with change of basis matrix attached...
    -- so it is best for the user not to use it
    R := ring G;
    (rem, quot, cplt) := rawGBMatrixLift(raw G, raw n);
    map(R, quot))
```

The comment already knows. `ChangeMatrix` defaults to `false`, so `rawGBMatrixLift` has no change
matrix to lift through and returns a zero-column result, which `map` turns into a map from the
zero module. Verified:

```m2
B = QQ[x,y]; g = matrix{{x^2, x*y}}; f = matrix{{x^3}}
f % g                             -- 0, so f IS in the image
f // g                            -- | x |
                                  -- | 0 |
f // gb g                         -- map(B^0, B^1, 0)      <- wrong, silently
f // gb(g, ChangeMatrix => true)  -- | x |
                                  -- | 0 |
```

### It is detectable, cheaply

A `GroebnerBasis` carries its own request:

```m2
(gb g)#"computation options"                        -- ... ChangeMatrix => false ...
(gb(g, ChangeMatrix => true))#"computation options" -- ... ChangeMatrix => true ...
```

So `quotient(Matrix, GroebnerBasis)` can test `G#"computation options"#ChangeMatrix` and either
error (which is what the report asks for) or recompute. **Which of those it should do is the one
real open question in this group** — see [Open questions](#open-questions).

### #278's premise appears to be out of date

#278 says: *"we commented out code that gave `trim M` a cheap gb obtained from `M`, because it
might have a different change matrix. Rethink and restore that optimization."* The block is still
commented out at `matrix2.m2:257-272`, carrying Dan's explanation verbatim — *"even if Syzygies is
set to false, ChangeMatrix might have been true, and we don't record that somehow."*

**Today, it is recorded.** `gbTypeCode` (`gb.m2:235`) folds `ChangeMatrix` into `SyzygyRows`, and
the three cache keys are distinct — confirmed by inspecting `m.cache` after each kind of request:

| request | cache key |
| --- | --- |
| `gb m` | `{SyzygyRows => 0, Syzygies => false}` |
| `gb(m, ChangeMatrix => true)` | `{SyzygyRows => infinity, Syzygies => false}` |
| `gb(m, Syzygies => true)` | `{SyzygyRows => infinity, Syzygies => true}` |

So the objection that stopped the optimisation in 2015 does not hold against the current cache,
and #278 is a smaller task than it reads: check the reasoning against `gbTypeCode`, restore the
block, and let the existing key do the work. **This has not been attempted here** — it wants
cause (b) settled first, since it is the same question about the same data.

### An anomaly found while checking this, worth its own issue

```m2
m = matrix{{x^2, x*y}}
gb(m, ChangeMatrix => true);
numRows getChangeMatrix gb m     -- 0
```

The matrix has exactly one cache entry, the `ChangeMatrix` one, and a plain `gb m` reuses it
rather than computing a second — yet the object it hands back reports an empty change matrix.
Reproduced on clean sources (i.e. this is not an artefact of the cause-(a) patch). It is adjacent
to #256 but it is not #256, and it is **not filed**. Parked in `parking.tsv`.

---

## Cause (c) — the PID strategy's guard misses the rings it was written for

Covers **#837**.

```m2
addHook((trim, Module), Strategy => "PID",
    (opts, M) -> (
	R := ring M;
	if instance(R,PolynomialRing) and numgens R === 1 and isField coefficientRing R
	   and not isHomogeneous M then trimPID M))
```

For `A = ZZ[i]/(i^2+1)` the guard fails twice: `A` is a `QuotientRing`, not a `PolynomialRing`,
and `coefficientRing A` is `ZZ`, not a field. So `trimPID` never runs, and:

```m2
I = ideal(5_A, i+2)
mingens I                  -- | 5 i+2 |
minimalPresentation I      -- ideal(5,5)
ideal(i+2) == I            -- true, so 5 is redundant
```

Note this one is **redundant, not wrong** — unlike cause (a), no generator is lost.

**The strategy would work if it were reached.** `smithNormalForm` — which is all `trimPID` needs —
runs fine over `A`:

```m2
smithNormalForm(presentation module I, ChangeMatrix => {true, false})
-- (| 1 0 |, | 1     0 |)
--  | 0 0 |  | -i+2  1 |
```

Rank 1 with a unit, which is the right answer. So this is a guard-widening fix, not new
mathematics. **No patch was written for it.** The guard has to be widened to the right class —
`ambient A` is `ZZ[i]`, a `PolynomialRing`, so `instance(ambient R, PolynomialRing)` reaches it,
but "is this ring a PID" is not a question M2 can answer in general, and widening it too far will
send non-PIDs into `trimPID`. That judgement is yours, and it is the second
[open question](#open-questions).

Related but **out of scope**: #44 (`minimalPresentation` over `ZZ[x,y]`) is not a PID and needs
real Groebner-bases-over-`ZZ` work.

---

## Cause (d) — `mingens` promises less than it delivers

Covers **#105**, which is documentation and signature work, not a defect.

The request is that `mingens` error on inhomogeneous input. **The triage note settles that it
should not**, and the evidence is worth keeping: erroring would break `IntegralClosure`, where
454 of 460 `mingens` calls across four `check` suites are inhomogeneous, and `isHomogeneous` is
the wrong test anyway — it is false over a `LocalRing`, where `mingens` is perfectly legitimate.

What is actually true, and undocumented, is the reverse: `mingens` *does* minimise in the
inhomogeneous case (300 random and 3 constructed cases all minimal), and `mingens-doc.m2`
promises less than that. Two smaller findings ride along:

- `Strategy => Inhomogeneous` is inert for `mingens` — it is a real branch only in `trim`
  (`matrix2.m2:245` and `:275`).
- `mingens` accepts an unrecognised `Strategy` silently, where `trim` errors.

This is the cheap row in the group and the one to do last.

---

## Regression tests

### `M2/Macaulay2/tests/normal/trim-ZZ.m2` — new, for #747 and #4095

**Verified as written:** exits 1 with `error: assertion failed` on unpatched sources, exits 0 with
the cause-(a) patch. A test that passes on unfixed sources is worth nothing, so it was run that
way round first.

```m2
-- trim over a ring whose coefficients are not a field used to return a strictly smaller
-- ideal: StopWithMinimalGenerators stopped the Groebner basis early, and the only guard
-- was "ring M === ZZ", which ZZ[x] fails.  (#747, #4095)

-- Every ideal below is constructed fresh at the point of use, which is why the first
-- three are functions rather than values.  trim caches, and any earlier ==, gb or
-- isSubset on the same ideal computes the basis that makes trim correct -- so a test
-- that reuses an ideal silently stops testing anything.

R = ZZ[a,x,y]
I = () -> ideal(a^2+5, (1+a)*x - 2*y, 3*x - (1-a)*y)
J = () -> ideal(x*y - x^2 - y^2 - 1)
K = () -> ideal(3, 1-a)
assert( trim(trim(I() + J()) + K()) == trim(trim(I() + K()) + J()) )
assert( trim(I() + J() + K()) == I() + J() + K() )

S = ZZ[x]
assert( trim ideal(x^2+x+1, x^4+x^3+x^2+x+1) == ideal 1_S )
assert( trim ideal(x^2+1, x^3+1) == ideal(2_S, x+1) )
assert( trim ideal(x^2+1, x^3+1) == ideal(x^2+1, x^3+1) )

-- the affine path must be untouched
Q = QQ[x,y]
assert( trim ideal(x^2, x*y, x^2+x*y) == ideal(x^2, x*y) )
```

### `M2/Macaulay2/tests/normal/gb-changematrix.m2` — new, for #256

**Draft, not verified**, because its shape depends on which resolution you pick for
[open question 1](#open-questions). The first two assertions hold today and pin the facts; the
third is the one the fix decides.

```m2
-- f // gb g used to return a map from the zero module, because the Groebner basis was
-- computed without a change matrix and nothing checked.  (#256)
B = QQ[x,y]
g = matrix{{x^2, x*y}}
f = matrix{{x^3}}
assert( f % g == 0 )
assert( f // gb(g, ChangeMatrix => true) == f // g )
-- if the resolution is to error:
assert( try (f // gb g; false) else true )
-- if the resolution is to recompute:
-- assert( f // gb g == f // g )
```

### #837

Add to the same `trim-ZZ.m2` once cause (c) is resolved:

```m2
A = ZZ[i]/(i^2+1)
assert( numcols mingens ideal(5_A, i+2) == 1 )
```

---

## Existing PRs

Checked three ways: PR mentions in the six triage notes, open PRs whose title or body names any
of the six, and every open PR that touches `matrix2.m2`, `matrix1.m2` or `gb.m2`.

**No open PR claims to fix any of the six, and none ever did** — no PR in any state references
them, and no commit on `development` names them. So nothing here is duplicated work.

Four open PRs touch these files, and one of them matters:

| PR | state | overlaps | verdict |
| --- | --- | --- | --- |
| [#4457](https://github.com/Macaulay2/M2/pull/4457) | open, @pzinn, 2026-07-15 | **`trimPID`, cause (c)** | coordinate |
| [#3020](https://github.com/Macaulay2/M2/pull/3020) | open, "LeftIdeal (take two)" | `mingens Ideal` at :135, the `trim` region at :170, `gb Ideal` in `gb.m2` | rebase risk only |
| [#3032](https://github.com/Macaulay2/M2/pull/3032) | open, draft | `support` at `matrix2.m2:438` | no overlap |
| [#4200](https://github.com/Macaulay2/M2/pull/4200) | open, draft | adds `applyMatrix` at :783 | no overlap |

**#4457 is the one to look at before touching cause (c).** It removes exactly one line from
`trimPID`:

```diff
     rows := select(min(rank source g,rank target g),i->isunit g_(i,i));
-    rows = rows | toList(rank target f..<rank target g); -- temporary fix for #3017
```

pzinn's own description is *"Back in #3018 I introduced workarounds due to bug #3017. Since the
latter has been (supposedly!) fixed, these are no longer needed"* — note the "supposedly". It does
not conflict with the guard-widening fix, which is in the `addHook` a few lines below, but it
changes the behaviour of the function that widening would newly expose `ZZ[i]/(i^2+1)` to. If
#4457 lands first, cause (c) applies cleanly on top; if cause (c) lands first, #4457's author is
suddenly removing a workaround from a code path with new callers. Worth one comment on #4457
either way.

**Nothing collides with cause (a).** No open PR touches either `mingb` line. #3020 is a large
refactor in the same region and would need a rebase, but there is no semantic conflict.

## Open questions

These are the two places where the fix depends on a decision rather than on a fact.

1. **#256: should `f // gb g` error, or silently recompute with a change matrix?** Erroring is
   what Dan asked for in 2015 and it is honest, but it turns working-if-lucky user code into a
   hard failure. Recomputing is friendlier and matches what `f // g` already does, but it hides a
   real performance cliff behind an innocuous operator. A third option is to error only when the
   result would be the zero map. The detection is settled either way; only the response is open.

2. **#837: how far should the PID guard widen?** `instance(ambient R, PolynomialRing)` reaches
   `ZZ[i]/(i^2+1)`, but M2 cannot decide "is this a PID" in general, and a guard that is too loose
   sends non-PIDs into `trimPID`. Narrow and explicit (Euclidean imaginary quadratic rings) is
   safe and ugly; broad is elegant and risky.

---

## Testing notes

**The trap that cost me two runs, and cost the triage sweep two before that.** `trim` caches its
result on the ideal, and `==`, `gb`, and `isSubset` all compute the Groebner basis that makes
`trim` correct. So displaying `gens gb J` immediately before `trim J` shows the *right* answer and
hides the bug. Every check needs a freshly constructed ideal, and printing anything about an ideal
before trimming it invalidates the check.

**You do not need to rebuild to iterate on this.** The local build at
`M2/BUILD/build/M2` loads Core straight from the source tree —
`Core#"source directory"` is `M2/Macaulay2/m2/` — so editing `matrix2.m2` and re-running takes
seconds. The copy under `usr-dist/common/share/Macaulay2/Core/` is *not* what gets loaded; patching
it does nothing, which is worth knowing before you spend twenty minutes wondering why your change
had no effect.

**Build currency.** That build is `1.26.06-73-gd49d057374`, 19 commits behind
`origin/development` at the time of writing, and **none of those 19 commits touches
`matrix2.m2`, `matrix1.m2`, `gb.m2` or `LLLBases.m2`** — so it is a valid platform for this group
without a rebuild.

**`check` suites worth running**, from the blast-radius grep: `Core`, then `IntegralClosure` (the
heaviest inhomogeneous `mingens` user, 454 of 460 calls), `LLLBases` (the `ring M === ZZ` branch
at :281 calls into it), and `Saturation`.

---

## Why this group

`bin/cluster` ranked it first at score 16.4, and it stayed first under six of the seven weightings
tried — it drops to third only when `severity` is turned down to 1, which is the axis this project
argues should lead. Runners-up were `packages/Complexes.m2` (15.9) and `m2/matrix1.m2` (15.0).

The group was **not** taken as the tool emitted it. `bin/cluster` also placed #3738 (`inverse of
matrix is not well defined`) here, because its note pinpoints `matrix2.m2:358-359` — within the
80-line window of #4015 at :315-320. Reading it shows the cause is in `matrix1.m2`, and it seeds
its own candidate group with #4506, #4556 and #3973. It was dropped and parked. Conversely #4095,
which the tool could not place here because its note carries no line number at all, was pulled in
by `bin/cluster`'s citation pass — #747's note names it as sharing one fix, which turned out to be
exactly right.

## Parked

Recorded in `parking.tsv`:

- **#3738 + #4506 + #4556 + #3973** — matrix inverse: non-square, singular over `RR`/`CC`, and
  tower rings. A coherent group of its own, rooted in `matrix1.m2`.
- **#2820** — `prune` is not idempotent over number fields. Same file, different cause
  (`complement`/`syz` under `minimalPresentation`), and the tool separated it correctly.
- **#4015** — `mingens` after `syz gb`. In the neighbourhood and genuinely related, but it carries
  two questions to @mikestillman unanswered since November 2025, so it scores `settled = -3` and
  would stall this group.
- **#44** — `minimalPresentation` over `ZZ[x,y]`; needs Groebner-bases-over-`ZZ` work.
- **the `getChangeMatrix` anomaly** described under cause (b) — unfiled, reproduced, no issue yet.

## Handoff

- **Nothing here is committed to `M2/`.** The cause-(a) patch was applied to
  `M2/Macaulay2/m2/matrix2.m2`, tested, and reverted; `git status` on `M2/` is clean. The diff
  above is the artefact.
- **Suggested commit split** — a suggestion only, and the split is yours:
  1. cause (a), both `mingb` lines, plus `tests/normal/trim-ZZ.m2` → closes #747 and #4095
  2. cause (b) in `gb.m2`, plus `tests/normal/gb-changematrix.m2` → closes #256, then #278 as a
     follow-up on the same reasoning
  3. cause (c), the guard → closes #837
  4. cause (d), documentation → closes #105

  (1) is independent and could go first on its own; (2) and (3) each wait on an open question.
- **No PR has been opened and nothing has been pushed.**
