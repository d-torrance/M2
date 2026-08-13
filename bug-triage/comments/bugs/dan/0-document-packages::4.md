<!-- issue: #2582 -->
`debug 2671` gave you nothing because it could not have worked, and the reason generalises.

The warning you hit fires while the symbol is being redefined, and redefining it is a state change. By the time
you are back at a prompt able to type `debug 2671`, the redefinition has happened; the warning cannot recur in
that session, so there is nothing left for the hashcode to catch. Demonstrated with the sibling warning from
#2771, which behaves the same way:

```m2
i1 : R = QQ[a,b,c];

i2 : S = QQ[a_1..c_3];
warning: clearing value of symbol a to allow access to subscripted variables based on it
       : debug with expression   debug 9539   or with command line option   --debug 9539
...

i3 : debug Core; debug 9539;

i5 : T = QQ[a_1..c_3];          -- no warning at all: a, b, c are already cleared
```

The script runs to completion with nothing raised. Whereas setting it *before* the warning fires does work:

```m2
i1 : debug Core; debug 9539;

i3 : R = QQ[a,b,c];

i4 : S = QQ[a_1..c_3];
stdio:4:1:(3): error: clearing value of symbol a to allow access to subscripted variables based on it
```

raised from `Core/debugging.m2:10`. So of the two routes the message offers, only `--debug n` on a fresh
invocation is usable for a warning of this kind, and `debug n` — the one it lists first — is not. That is
worth fixing in the message itself even if nothing else here changes.

### Two further problems with the hashcode, found while checking this

**It identifies the message text, not the warning.** The code is `(hash processErrorArgs args) % 10000` over
the *formatted* message, so any interpolated value changes it:

| message | code |
| --- | --- |
| `package XML being reloaded` | 1900 |
| `package Foo being reloaded` | 7501 |
| `package Depth being reloaded` | 160 |

and so does rewording — `clearing value of symbol x` is 7615, `clearing the value of symbol x` is 352. There
is therefore no stable number for "the reloaded-package warning", nothing that can be written in
documentation, and any edit to a warning's wording silently invalidates codes users have noted down.

**It collides.** 10000 buckets, and the message space is parameterised by package names, symbol names and
paths. Taking just the one warning above over the 299 packages installed here gives five genuine collisions:

```
"TropicalToric"        and "Resultants"       both -> 534
"ResidualIntersections" and "LatticePolytopes" both -> 7022
"Permutations"         and "FormalGroupLaws"   both -> 6930
"Visualize"            and "SymbolicPowers"    both -> 9679
"SwitchingFields"      and "InvolutiveBases"   both -> 1436
```

So `--debug 534` would break on whichever of two unrelated warnings occurred first. That is one warning site;
across all of them collisions are the expected case rather than a corner one.

### Related

#540 and #2771 both quote this same `debug with expression` line as part of what makes these warnings look
like bugs to a user — #540 asks that the message "look less like it's a bug", #2771 asks "do we really need
these warnings?". All three of you are looking at the same two lines of output, so whatever is decided here
probably wants to apply to those as well.

For what it is worth, the underlying need — a backtrace showing where a warning came from — seems real, since
warnings otherwise carry no stack at all. It is the hashcode as the handle that looks like the wrong shape; a
flag converting all warnings to errors, or `debugLevel`-gated backtraces on warnings, would serve the same
purpose without unstable colliding identifiers.

### Where this came from

`bugs/dan/0-document-packages`, one of the 857 files removed with the `bugs/` tree in d2c8d27826 and
catalogued in #36, ends with

> consider exporting function warningMessage and documenting it

`warningMessage` is the function that emits these warnings, at `Core/debugging.m2:17`, and it is not exported —
so no package can use it. 14 distributed packages hand-roll `"warning: ..."` text to `stderr` or `stdout`
instead, disagreeing on both the prefix and the stream, and `Schubert2.m2:486` reproduces Core's own
`variables.m2:21` message in reworded form because it cannot call it. None of those warnings carries a
hashcode, so none is reachable by `--debug` at all.

That request is recorded as a duplicate of this issue rather than filed, because exporting the function would
widen a mechanism whose own advice does not work.
