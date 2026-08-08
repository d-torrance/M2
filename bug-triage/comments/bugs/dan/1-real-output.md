A file from the pre-GitHub `bugs/` tree lands here, and the only thing it adds to the analysis
already in this thread is that the symptom is not confined to awkward magnitudes.

`bugs/dan/1-real-output` is four lines with no prose:

```m2
i2 : toExternalString .3

o2 = .29999999999999999p53e0
```

On 1.26.06-40-gd8e86d689d the trailing `e0` is gone but the digits are not:

```
toExternalString .3      ->  .29999999999999999p53
toExternalString .5      ->  .5p53
toExternalString .25     ->  .25p53
toExternalString 1.      ->  .1p53e1
toExternalString .1p100  ->  .10000000000000000000000000000002p100
```

`value ".3p53" == .3` is `true`, so one digit would round-trip where seventeen are printed. And the
contrast with `.5` and `.25` — exact in binary, so `mpfr_get_str` has a short exact answer available —
puts the closest-versus-shortest choice on a single line, using a number anyone would type. The
examples in this issue so far are near `2^53` or at precisions of 4 bits, which read as inherent
floating-point awkwardness; `.3` does not.

Nothing here about the mechanism that is not already in the 2024-11-03 comment above, including the
part that matters most — that `mpfr_get_str` gives the mathematically closest decimal rather than the
shortest that round-trips, and that Dragon4/Grisu3/Ryū have not been implemented for MPFR. The
seventeen digits observed above are consistent with that comment's note that 17, not the 16 from
#3477, is what `p53` requires.
