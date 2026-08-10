The other half of this pair has a defect of its own, and it is the one `pivots` was written to avoid.

`leadComponent` ignores the module's `Position` direction. On a single matrix, over two rings
differing in nothing but that:

```m2
i1 : for ord in {Up, Down} do (
         R := QQ[x, MonomialOrder => Position => ord];
         m := matrix{{x, 0_R}, {x, x}, {0_R, x}};
         << "Position => " << ord << ":  leadComponent = " << leadComponent m
            << "   pivots = " << pivots m << endl)

Position => Up:    leadComponent = {1, 2}   pivots = {(1,0), (2,1)}
Position => Down:  leadComponent = {1, 2}   pivots = {(0,0), (1,1)}
```

`leadComponent` gives the same answer either way, because `Reverse => true` is hard-coded:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/m2/matrix.m2#L772-L774

`pivots` derives the direction instead, which is the first thing it does:

```m2
f := leadTerm matrix {{1_R},{1_R}};
dir := if f_(0,0) == 1 then Down else Up;
opt := Reverse => dir === Up;
```

Under `Position => Down` the leading entry of column 0 is in row 0, and `leadComponent` reports 1.

### Two things that complicate it

The documentation was rewritten in 2018 and describes the hard-coded behaviour rather than the
name: *"the largest index of a nonzero element"*, with the observation that *"**Leading** appears to
be a bit of a misnomer here, since the index/indices return are the last, not the first"*. It does
not mention `Position` at all, so a reader cannot tell from the page whether the order is meant to
matter.

And it has more consumers than it used to. The 2006 note below says *"It's used exactly once in our
packages and code: in Dmodules"*; today it is `BernsteinSato/Dresolution.m2`, `HighestWeights` and
`InvolutiveBases`.

### What is already fixed

The other complaint in that note — that `leadComponent` *"doesn't work for columns that are zero"* —
is gone. It used to fail with `array index -1 out of bounds 0 .. -1`; `nonnull` now drops zero
columns and `leadComponent Vector` returns `null`, which is the behaviour the note wanted and what
`pivots` does.
