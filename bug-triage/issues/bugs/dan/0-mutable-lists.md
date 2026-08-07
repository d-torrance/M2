Still reproduces, sixteen years on, and the shape of the growth curve is the whole story:

| filling by index | time |
| --- | ---: |
| n = 1000 | 0.0013 s |
| n = 10000 | 0.3664 s |

Ten times the elements, about **280 times** the work — so this is quadratic, not a constant factor.

### Why

A `MutableList` grows one element at a time, so assigning past the end reallocates on every step, and
filling one by index is O(n²). That is the mechanism #659 records, open since 2015.

### Provenance of this comment

This was filed separately as #4501 before #659 was found — searching `mutablelist` as one word does not
match a title reading *"growth of mutable lists"*. #4501 was closed as a duplicate and the timings
moved here.

Kept separate deliberately: **#1608** is a larger redesign of the same type (balanced trees, ordered
containers), which subsumes this but is a much bigger undertaking.
