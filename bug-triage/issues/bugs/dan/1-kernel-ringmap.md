### What the file is about

`kernel f` for a ring map is computed by forming the graph ideal of `f` and taking a Gröbner basis of
it, which is the expensive step. Knowing the ideal's Hilbert function in advance lets that computation
be given a hint and go much faster — but the hint is only usable when the ideal is homogeneous. This
file is Mike Stillman's 2008 recipe for the case where it is not: homogenize a Gröbner basis of `J`
and the `f_i`, build the hint from the homogenized pieces, then dehomogenize and `forceGB` the result.

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
