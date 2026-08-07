Unmet — and rather than declining, `gcd` returns a common divisor that is **not greatest**.

### The counterexample

Over `QQ[a]/(a^2-1)`, which is `QQ × QQ`, with `e = (1+a)/2` and `f = (1-a)/2` verified idempotent and
orthogonal:

```m2
F = e*(x-1) + f*(x-2)
G = e*(x-1) + f*(x-3)
d = e*(x-1) + f            -- degree 1, divides both F and G
gcd(F, G)                  -- 1
```

A first attempt of mine, `(x^2-1)*(x-a)` against `(x^2-1)*(x+a)`, gave the right answer and was a
**coincidence** — both components share a gcd there. The idempotent construction is what makes the
components disagree.

### Cause, and the capability already exists

`gcd` calls the two-argument `rawGCD` (`factor.m2:26-28`) with no minimal polynomial, so an extension in
the coefficient ring is invisible and only the base-field part comes back. Over
`K = QQ[a]/(a^6-a^3-1)` with `R = K[t]`, `F = t^6-t^3-1`, `G = (t-a)*(t+a)`:

- the front end says `1`,
- `rawGCD(raw F, raw G, raw (ideal K)_0)` returns `t-a`.

That is Mike's 2009 diagnosis verbatim, from `bugs/mike/0-gcd-doc.m2` line 3: *"rawGCDRingElement is not
being called from the front end"*.

### The guard that was supposed to prevent this only checks the top ring

Dan's proposal in **#321** — *"just give an error if the ring is a quotient ring"* — exists at
`factor.m2:24`, but tests only the top ring. So `gcd` errors in `QQ[a]/(a^2-1)` and **proceeds** in
`(QQ[a]/(a^2-1))[x]`; likewise for pzinn's `ZZ[y]/(y^2+3)` example. This is the third guard-depth defect
of this cohort, after #4576 and #4578.

### Deliberately left separate

`bugs/mike/0-gcd-doc.m2` is a 120-line multi-ask file covering `factor` over `toField` ("NOT
CORRECT!!"), bivariate gcd and missing documentation, so it needs reading *against* this issue rather
than closing as its duplicate. **#1958** is the units-in-a-field case Mike called expected.
