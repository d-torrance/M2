Unmet on all three counts — finalized, flushed, closed — and buffered data is genuinely lost, not
merely delayed. Measured rather than read:

```m2
i1 : zzf = openOut "/tmp/probe";
i2 : zzf << "hello";
i3 : zzf = null;
i4 : collectGarbage(); collectGarbage();
i5 : get "/tmp/probe"
o5 =                                    -- empty; the write never reached disk

i6 : openFiles()
o6 = {..., /tmp/probe, ...}             -- and the file is still open
```

At a normal exit the data does arrive, through `closem` (`d/stdio.d:575-583`) registered with
`atend` — but note that `closem` **flushes without closing**, so the exit path and the
garbage-collection path solve different halves of the problem.

### Two blockers, and the second is the interesting one

**There is no finalizer on the file type.** `git grep -i finaliz` over `d/stdio0.d`, `d/stdio.d` and
`d/stdiop.d` exits 1; every `GC_REGISTER_FINALIZER` in the tree is for foreign pointers, mysql,
pthreads or engine objects.

**A file can never be collected in the first place.** `openfiles` (`d/stdio.d:154-158`) is a
strong-reference list that `addfile` pushes every file onto, and only `rmfile` removes — called from
`cleanUp`, after `close`. So an unclosed file is reachable from a GC root, which is exactly what
`openFiles()` still listing it demonstrates. A finalizer added today would never run.

### The tension worth naming before anyone starts

The two halves of the request pull against each other. The flush-at-exit net that makes the common
case work depends on that same pinning: `closem` walks `openfiles`, so if files became collectable,
the exit-time flush would lose the ones the collector had already taken. A fix has to replace the
pinning with something that survives both paths, not just remove it.

**#1515** is a hard second blocker: `registerFinalizer` has been broken since 1.12 or earlier, with
**#2728** on the same GC warnings and **#1852** closed as its duplicate.
