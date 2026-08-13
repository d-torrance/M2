`SortStrategy` is documented as *"Not implemented yet."* but the engine reads it, validates nothing, and
two of its bit values fail — one by crashing, one by silently returning a resolution that is not minimal.

Six variables are enough:

```m2
i1 : R = ZZ/31991[a..f];

i2 : I = ideal(a*d-b*c, a*e-b*d, b*e-c*d, a*f-c*e, b*f-d*e, c*f-e^2);

i3 : betti res(I, Strategy => 0)

            0 1  2  3 4
o3 = total: 1 6 13 12 4
         0: 1 .  .  . .
         1: . 6  4  . .
         2: . .  9 12 4

i4 : betti res(I, Strategy => 0, SortStrategy => 32768)
-- SIGSEGV
```

Sweeping the documented bit positions, each in a fresh process, against that baseline:

| `SortStrategy` | flag | `res(I, Strategy => 0, SortStrategy => ·)` |
| --- | --- | --- |
| `0` | — | minimal |
| `8192` | `1 << 13`, level-by-level | **not minimal** |
| `16384` | `1 << 14`, `auto_reduce = 1` | minimal |
| `32768` | `1 << 15`, `auto_reduce = 2` | **SIGSEGV** |
| `49152` | `3 << 14`, `auto_reduce = 3` | **SIGSEGV** |
| `65536` | `1 << 16`, heap-based reduction | minimal |
| `73728` | `1 << 16 \| 1 << 13` | **not minimal** |
| `131072` | `1 << 17`, by slanted degree | minimal |

### The crash

Whenever `auto_reduce >= 2`:

```
-* stack trace, pid: 40638
 3# res2_poly::remove(res2term*&) const at resolutions/res-a0-poly.cpp:94
 4# res2_comp::handle_pair(res2_pair*) at resolutions/res-a0.cpp:1708
 5# res2_comp::do_pairs(int, int) at resolutions/res-a0.cpp:140
 6# res2_comp::start_computation() at resolutions/res-a0.cpp:387
 7# rawStartComputation at interface/groebner.cpp:272
*-
```

`auto_reduce` is set at `res-a0.cpp:506` from `(SortStrategy & FLAGS_AUTO) >> SHIFT_AUTO`, so the values
reaching it are `0`, `1`, `2`, `3`. Only `0` and `1` survive.

### The silently non-minimal results

`res` over a field is supposed to return a minimal free resolution, and `SortStrategy` is documented as
choosing "the strategy to be used for sorting S-pairs" — sorting S-pairs cannot change the answer. But
bit 13 does, and it does so without a warning. On the 20-variable ideal in this file the Betti total goes
from 3504 to 7770:

```
SortStrategy => 0            SortStrategy => 65536 + 8192
       0  1   2   3   4    5   6   7   8  9 10          0  1   2   3    4    5    6    7   8  9 10
total: 1 35 140 301 735 1080 735 301 140 35  1   total: 1 35 220 715 1449 1951 1778 1086 427 98 10
```

Note that bit 13 is not a sorting choice at all — it selects level-by-level computation, and bit 17
selects degree-by-degree. Several algorithm switches are being carried through an option whose documented
job is sorting.

The one non-minimal result that is *not* surprising is `2^13 + 2^18`, which the author's own comment in
this file marks "not a minimal resolution... but generally fast to compute". That value sets
`do_by_level = 2` at `res-a0.cpp:507`, a deliberate strip optimization. Bit 13 alone sets
`do_by_level = 1` and carries no such caveat.

### Not confined to `OldChainComplexes`

`freeResolution` in `Complexes` takes the same option and reaches the same code:

```m2
i1 : needsPackage "Complexes";

i2 : betti freeResolution(I, Strategy => 0, SortStrategy => 8192)   -- not minimal
i3 : betti freeResolution(I, Strategy => 0, SortStrategy => 32768)  -- SIGSEGV
```

### Two smaller things in the same code

The `auto_reduce` path writes debug tracing to stdout with `gbTrace` unset:

```
b2cf<7>-b2cf<6>+ac2f<4>-abdf<5>ad2e<9>-ad2e<1>...auto reduction:
    by coeff = 1
    result =
```

And the documentation node for `[resolution, SortStrategy]` in
`packages/OldChainComplexes/docs/doc10.m2:147-152` ends with "Not implemented yet.", while
`res-a0.cpp:500-507` unpacks six fields from it. `Complexes` calls it "an internal option" instead, which
is accurate but does not say that most of its values are untested.

The narrowest fix is probably to validate the option and reject the values that do not work, rather than
to repair `auto_reduce = 2`; nothing in the tree passes a nonzero `SortStrategy`, so nothing depends on
those values today.

### Searches this rests on

Titles and bodies for `SortStrategy`, `auto_reduce`, `res2_poly`, `res-a0`, `handle_pair`, `not minimal`
and `non-minimal`; comments via `gh search issues` for `SortStrategy`, `res2_poly`, `res2_comp` and
`handle_pair`. The near misses, so they are not re-derived:
[#1578](https://github.com/Macaulay2/M2/issues/1578) and
[#4143](https://github.com/Macaulay2/M2/issues/4143) both name `auto_reduce`, but that is
`gbA::auto_reduce_by` in `gb-default.cpp` — the Gröbner engine, a different function, and #4143 is about
parallel calls; [#814](https://github.com/Macaulay2/M2/issues/814) and
[#907](https://github.com/Macaulay2/M2/issues/907) list the option name only as `help` output;
[#4394](https://github.com/Macaulay2/M2/issues/4394) touches `res-a0` but is about the slab allocator.
