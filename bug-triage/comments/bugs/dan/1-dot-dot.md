A second note in the same pre-GitHub `bugs/` tree reaches this defect by a different spelling, and it turned up two things worth having here before anyone works on it.

`bugs/dan/1-dot-dot` (July 2009, forwarded by Mike Stillman) uses `_`-suffixed endpoints rather than `R_i`:

```m2
i1 : R = ZZ/101[x_1,x_2,x_3];

i2 : S = ZZ/101[x_1,x_2,x_3,x_4];

i3 : I = ideal(x_1_R .. x_3_R)

o3 = ideal (x , x , x )
             1   2   3

o3 : Ideal of S
```

with one line of commentary: "o4 should be an ideal of R to coincide with the expectation of the user."

That transcript is the *pre*-fix behavior. `92e9ca107e` ("fix a recent bug in `..`", 2009-10-29, three months after the file was written) rewrote `..` and added the guard that produces what this issue reports instead:

https://github.com/Macaulay2/M2/blob/68351e766d3e7991cd8bc0aa3ceecad1d49e65b8/M2/Macaulay2/m2/dotdot.m2#L12-L19

So the silently-wrong ring is gone, which is the half that mattered. What remains is the `IndexedVariable` fallback, and its consequence today is an error rather than an ideal of either ring:

```m2
i3 : ideal(x_1_R .. x_3_R)
stdio:3:9:(3):[1]: error: expected a list of numbers, matrices, ring elements or ideals
```

With no shadowing — `T = ZZ/101[y_1,y_2,y_3]` — `ideal(y_1_T .. y_3_T)` is correctly an ideal of `T`.

**The fallback is covered by a regression test**, added by that same 2009 commit:

https://github.com/Macaulay2/M2/blob/68351e766d3e7991cd8bc0aa3ceecad1d49e65b8/M2/Macaulay2/tests/normal/dot-dot.m2#L120-L129

`T` and `U` are identical rings; the assertion for the shadowed one pins the current behavior. A fix has to change that line.

**And the design question was decided once already, in #2020.** That issue is the same ask for a ring built with `monoid` and `:=`, and it was closed after the fix was written and then withdrawn: @pzinn implemented it in 6116033703 and 2340541e45, ran it on Macaulay2Web, and found that it reinterprets

```m2
R = QQ[m_(1,1)..m_(3,3)];
R = QQ[m_(1,1)..m_(2,2)];
```

so the second range expands against the *first* ring and picks up generators like `m_(1,3)`. @DanGrayson's reply was "let's be conservative here and not break code", and it was closed on that. The one suggestion there that would avoid the effect — @mahrud's, to promote to the ring of the input variables at the end rather than deciding up front — was never answered, and is probably where this should resume.

One small note on this issue's own body: the code it quotes (`v := value; bn := a -> ...`) is the implementation from *before* `92e9ca107e` and is no longer in the tree.
