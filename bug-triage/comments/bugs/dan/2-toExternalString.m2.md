The sentence in the opening comment — *"one needs to have the ring **and its generators** as global
variables"* — has a second half that is easy to miss, and it fails even when the ring is named:

```m2
i1 : A = ZZ[a,b];

i2 : B = A/(a^2-1);

i3 : toExternalString B

o3 = A/(a^2-1)

i4 : value o3
currentString:1:1:(3):[2]: error: expected element of the same ring or promotable to it
```

`A` is a perfectly good global, and the emitted string names it. What has moved is `a`: creating `B`
rebound it to an element of the quotient, so `a^2-1` is computed in `B` rather than in `A`. The same
string evaluates without complaint if you run `use A` first — so the output is correct only relative
to a global state it does not record.

The `frac` case fails the same way through the abbreviated range form:

```m2
i1 : A = ZZ[a,b]; use A; B = A/(a^2-1,b^2-1); C = B[x,y]/(a*x+b*y);

i2 : toExternalString frac C

o2 = frac(B[x..y]/(a*x+b*y))

i3 : value o2
currentString:1:3:(3):[2]: error: no method for binary operator .. applied to objects:
            -a*b*y (of class C)
     ..     y (of class C)
```

`x..y` is only a variable list while `x` and `y` are unbound symbols; by the time the string is
evaluated they are elements of `C`.

Worth noting for whoever picks this up: the *matrix* half of this issue is in better shape than the
example above suggests, provided the ring has a name.

```m2
i1 : W = QQ[w];

i2 : toExternalString map(W^1,,{{w}})

o2 = map(W^1,W^{{-1}},{{w}})
```

That round-trips. It is the anonymous-ring case in the issue body, and the rebound-generator case
above, that do not — and they want different fixes: the first has no name to emit, the second has a
name and emits relations that are read in the wrong ring.

This ask is old. `bugs/dan/2-toExternalString.m2`, being triaged in #36, opens *"fix this to output
`(,,)` instead"* — which is done, `toExternalString (,,)` gives `(,,)` now — and then lists the
`frac`/quotient cases above, plus the matrix one, which it calls "worthless" in its 2006 form.
