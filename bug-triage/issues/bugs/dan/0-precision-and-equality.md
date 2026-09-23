### What the file reports

Real numbers in Macaulay2 carry a precision, and the `p` suffix sets it: `1p10` is 1 held to 10 bits,
where a plain decimal literal is held to the default 53. Comparing two such numbers therefore needs a
rule about which precision the comparison happens in. Dan's file shows that whatever the rule is, it
is not a predictable one — two literals differing only in their final digit give opposite answers
against the same `1p10`, and it is the *closer* of the two that comes back unequal.

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
