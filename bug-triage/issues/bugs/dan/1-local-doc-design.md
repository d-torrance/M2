The passage the file objects to is still in the manual, at
[`Macaulay2Doc/M2-Singular-Book.m2:351`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/Macaulay2Doc/M2-Singular-Book.m2#L351),
in the node **"Singular Book 1.4.9"**:

```m2
TEX "The following is WRONG.  In this local ring, $y$ is in the ideal $J$.",
EXAMPLE {
     "y % J",
     },
```

### The problem

The output it calls WRONG is correct, for the reason the same sentence gives. Running the node's own
setup:

```m2
i1 : S = QQ[x,y,z]; I = ideal(y*(x-1), z*(x-1));
i2 : y % I
o2 = y                                     -- y is not in I globally, as documented

i3 : R = QQ[x,y,z,MonomialOrder=>{Weights=>{-1,-1,-1},RevLex},Global=>false];
i4 : J = substitute(I,R); gens gb J
o4 = | y-x*y z-x*z |

i5 : dim J
o5 = 1

i6 : y % J
o6 = 0
```

Localized at the origin, `x - 1` is a **unit**, so `J = (y, z)` and `y` genuinely is in `J`. The
answer `0` is right, and the sentence's second clause ("In this local ring, `y` is in the ideal
`J`") says why — while its first clause tells the reader the opposite.

### What was probably meant

Dan's note on the file reads "not wrong, or wrong in a different sense", which suggests the intended
point was about `%` in a `Global => false` ring not being a genuine local-ring remainder in general,
rather than about this particular answer. That is a substantive caveat and worth stating — but it is
a different claim from the one printed, and as written the passage teaches a reader that a correct
computation is a bug.

### Suggested shape for a fix

Replace the "WRONG" sentence with the explanation, e.g.: *"This may be surprising: in this local
ring `x - 1` is a unit, so `J = (y, z)` and `y` is in `J`."* If the intended warning was about the
limits of `Global => false` orderings, that deserves its own sentence and probably a pointer to the
`LocalRings` package.
