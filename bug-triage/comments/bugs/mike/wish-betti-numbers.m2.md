<!-- issue: #4636 -->
A correction to the issue above, from another file in the same directory.

`bugs/mike/wish-betti-numbers.m2` — also removed with `bugs/` in d2c8d27826 (#36), and described by its
author as "my 'scratch' area for experimentation... checked in because I use it on several machines" — opens
with a decoded table of the very bits this issue is about:

```text
SortStrategy bits:
  2^16  -- geobuckets
  2^14  -- auto-reduce:  auto_reduce = 1
    2^14 + 2^15  -- auto_reduce = 3
    2^15 -- auto_reduce = 2
  2^13 -- do_by_level=1 (returns non-minimal res)
    2^13 + 2^18 -- do_by_level=2:  level strip
  2^17 -- do_by_degree (do by flat degree, not slanted degree).  Also non-minimal?
```

**This qualifies the non-minimal half of the issue above, and I would rather say so than leave it.** I
described bit 13 as silently returning a non-minimal resolution, and presented that as a defect alongside the
crash. The line `2^13 -- do_by_level=1 (returns non-minimal res)` shows that whoever was working on this code
knew perfectly well that bit 13 does that, and recorded it as the flag's nature rather than as a fault. So
that half is better read as an internal flag whose behaviour is undocumented publicly than as a bug in the
engine. The observation in the issue that `res` over a field is expected to return a minimal resolution still
stands as an argument about the *public* contract — `[resolution, SortStrategy]` says "Not implemented yet"
while `res-a0.cpp:500-507` unpacks six fields from it — but it is a documentation gap, not a wrong answer.

**The crash is not qualified.** The same table lists `2^15 -- auto_reduce = 2` and `2^14 + 2^15 --
auto_reduce = 3` with no suggestion that either fails, and those are exactly the two values that segfault in
`res2_poly::remove`. So the author's notes give no indication the crash was known, and nothing in this table
covers it.

**Two smaller things the table settles.** Its last line asks whether `2^17` (`do_by_degree`) is "also
non-minimal?" — measured on the six-variable example in the issue above, it is not: `SortStrategy => 131072`
returns the same minimal basis as `SortStrategy => 0`. And `2^13 + 2^18` is described here as "level strip",
which matches the comment in `5-res-options.m2` marking that value as deliberately non-minimal and confirms
it should not be counted against the engine.

For anyone reconstructing these bits, this table and the block of named constants at the top of
`bugs/mike/5-res-bench.m2` are as far as I know the only two decodings that exist outside `res-a0.hpp`
itself.
