A smaller case of the same over-eagerness, from the pre-GitHub `bugs/` tree.
Dan Grayson filed it as `bugs/dan/0-runLengthEncoding`, with the comment
"that's a bit too aggressive":

```m2
i1 : runLengthEncode {7,2,5,6}

o1 = {7, 2, 5..6}
```

Still current on 1.26.06-8-g34d5846039. Here nothing is repeated at all, so the
duplicate-versus-range precedence this issue is about does not come into it --
the point is just that a run of **two** consecutive values becomes a range.
`5..6` and `5, 6` are the same width, so the encoding is no shorter and reads
less directly.

Both look like they come from the same place. `runLengthEncode0`
(`M2/Macaulay2/m2/indeterminates.m2:82-91`) walks the list once and decides on
the second element whether it is in a duplicate run or a successor run, setting
`dupin` to `true` or `false` and never reconsidering; then any run with `m > 1`
that is not a duplicate run is emitted as `i0 .. oi`, with no minimum length.
So `{1,2,2,2,...}` commits to a successor run before the repeats are visible,
which is the case above, and `{5,6}` becomes a range because two is enough.

Recording it here rather than opening a second issue, since revisiting that
heuristic would settle both.
