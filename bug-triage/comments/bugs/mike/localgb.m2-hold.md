<!-- issue: #1985 -->
Two status updates on this issue, and a third symptom in the same ring class that looks more serious than
either of the original two.

**`R/I` still hangs.** Killed at 90 s on `R = QQ[x,MonomialOrder=>RevLex,Global=>false]` and
`I = ideal(x-1)`, exactly as reported, on 1.26.06-40-gd8e86d689d.

**`trim I` no longer does what this issue reports.** It now returns `ideal(1-x)` rather than `ideal 1`:

```m2
i1 : R = QQ[x,MonomialOrder=>RevLex,Global=>false];

i2 : I = ideal(x-1);

i3 : trim I
o3 = ideal(1 - x)
```

Worth saying that the `o3 = ideal 1` in the original report is arguably the *correct* answer, not the bug:
`x-1` is a unit in this local ring, so `I` is the unit ideal. The same point came up in #4591, where the
manual calls a correct local `%` answer "WRONG". So the change here may be a regression rather than a fix —
`trim` returning the generator unchanged is a failure to simplify, where `ideal 1` was right.

### A third symptom: `quotientRemainder` violates its documented identity

This is the part I would flag. `quotient-remainder-doc.m2:191` states, without qualification beyond a
free-module requirement:

> The equation `g*q+r == f` will hold. The source of `f` should be a free module.

In a local ring it does not hold. Three lines:

```m2
i1 : R = (ZZ/101){x,y};

i2 : (q,r) = quotientRemainder(matrix{{1_R}}, matrix{{1-x}})
o2 = (| 1 |, | 0 |)

i3 : matrix{{1-x}} * q + r == matrix{{1_R}}
o3 = false
```

So M2 asserts `1 = (1-x)·1 + 0`. The source is free of rank one, so the documented precondition is met.
The same happens in this issue's own ring:

```m2
i1 : R = QQ[x,MonomialOrder=>RevLex,Global=>false];

i2 : quotientRemainder(matrix{{1_R}}, matrix{{x-1}})
o2 = (| -1 |, | 0 |)
```

and the `//` and `%` spellings agree with `quotientRemainder`, so this is one defect and not three. A global
ring behaves correctly — `ZZ/101[x,y]` gives `q = 0`, `r = 1`, identity true.

**Which half is wrong.** The `r = 0` is defensible: `1-x` is a unit locally, so `f` really is in the image of
`g`. The quotient is the wrong part — the true value is `(1-x)^{-1} = 1+x+x^2+\cdots`, which is not a
polynomial, so there is no correct value to return. That makes the current behaviour a silent wrong answer
where an error, or a truncated quotient paired with a matching nonzero remainder, is what the identity
requires.

### Where this came from, and one more non-terminating case

`bugs/mike/localgb.m2-hold`, removed with `bugs/` in d2c8d27826 (#36), is a 2010-era page of local Gröbner
experiments over `ZZ/101` in four variables, written both as `(ZZ/101){a..d}` and with explicit
`Weights=>{-1,-1,-1,-1}`. Most of what it complains about is fixed: `syz gens I`, annotated "never
finishes", returns immediately; `matrix{{b-c^3}} // gens I`, annotated "crashes", returns; and a striking
nondeterminism where `(a*b*c*d)^2 % I` was annotated non-zero on one line and `-- now this is 0!!` on the
next now gives the same nonzero answer on five consecutive evaluations and on freshly built ideals.

What is still live there, besides the identity failure above, is one non-terminating Gröbner basis:

```m2
R = ZZ/101[a..d, MonomialOrder=>{Position=>Up, Weights=>{-1,-1,-1,-1}}, Global=>false]
I = ideal(a-b*a^2-a^3, b-a*b-c^3, a*b-b*d^4-c^5)
J = id_(source gens I) || gens I
gens gb J        -- no result in 300 s
```

which is the same shape as this issue's `R/I`: a computation over a non-global order that does not come back.

<sub>#2542 and its fix #2543 were a different local-ring quotient bug — a hook in `(quotient, Matrix, Matrix)`
permuting columns, over the `LocalRings` package's fraction-based rings rather than a negative-weight order.
#257 asks whether `remainder(Matrix,Matrix)` is as general as it could be.</sub>
