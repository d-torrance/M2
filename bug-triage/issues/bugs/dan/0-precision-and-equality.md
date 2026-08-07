Reproduces exactly:

```m2
i1 : 1p10 == 1.0000000000000001
o1 = true

i2 : 1p10 == 1.000000000000001
o2 = false
```

Two literals that differ only in the last digit compare differently against the same low-precision
number, and the *closer* one is the one reported unequal — `1.0000000000000001` is nearer to 1 than
`1.000000000000001` is, so whatever rule is in play is not "round to the lower precision".

### Why it looks wrong

`1p10` carries 10 bits, so it cannot distinguish either literal from 1: both should compare equal to
it, or a rule should be stated that makes one of them unequal for a reason a user can predict. As it
stands, equality between numbers of different precision is neither exact comparison nor comparison at
the lower precision.

### Notes for whoever picks this up

This sits on the `RR` arm of the promote/lift graph that MichaelABurr/M2#62 is currently formalising,
and MichaelABurr/M2#64 found two missing `lift` arms and a swapped `CC`/`CCC` pair in the same
neighbourhood — so the comparison rule and the conversion rules are probably worth settling together.
