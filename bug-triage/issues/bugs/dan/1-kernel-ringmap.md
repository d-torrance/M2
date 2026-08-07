Mike's 2008 recipe has three ingredients, and they have landed unevenly.

### The Hilbert-function hint: implemented, for degree-preserving maps

`ringmap.m2:320-332` sets `poincare cokernel graph` to `poincare module target f` times
`product(degrees source graph, d -> 1 - T_d)`, and asserts at `:332` that `gb` used it. Verified live
with `R = QQ[a,b,c,d, Degrees => {4,4,4,4}]` mapping to `QQ[s,t]` by `{s^4, s^3*t, s*t^3, t^4}`:
`isHomogeneous graph` is true, `canUseHilbertHint` is true, and `gb` records the
`rawGBSetHilbertFunction log` key.

### The inhomogeneous case — the one the email was written for — is unmet

`canUseHilbertHint` requires `isHomogeneous m` (`gb.m2:271`). With standard degrees the graph ideal is
inhomogeneous, because `a - s^4` mixes degrees in `QQ[p_0..p_5]`, so the block is skipped and there is no
homogenize/dehomogenize path. That is precisely Mike's recipe: homogenize a GB of `J` and the `f_i`,
form `(Jh, f1h - y1, …)`, set the hint to `(poincare Jh) * product(1 - t^deg(fih))`, then dehomogenize.

### The recipe's last step is tracked elsewhere

`forceGB` appears **0 times** in `ringmap.m2`, and **#686** asks for exactly it, quoting the same
reasoning.

### Live code

**PR #4485** (open) is adding another strategy to the same hook table, for skew-commutative targets via a
new `centerRing` — so `ringmap.m2`'s strategy block is being edited right now.
