Not done, and both halves of the file's design are still visible in the current code.

### The state today

`gb.m2` keeps a fixed `RawStrategyCodes` table and errors on an unrecognized strategy name, so there
is no way for a ring to introduce one. And `BooleanGB` does exactly what the file was written to
avoid: it exports `gbBoolean` as a separate entry point, rather than registering a strategy that
ordinary `gb` would find.

### What the file proposes

1. At polynomial-ring creation, register a strategy name in the ring where `gb` can find it.
2. Let quotient rings of that ring inherit it — but not polynomial rings *over* it.
3. Let `gb` accept the strategy name too.

The second point is the subtle one and worth preserving in any implementation: a Boolean strategy is
valid for `R/I` because the elements are the same kind of thing, and invalid for `R[y]` because they
are not.

### Notes for whoever picks this up

`Strategy` versus `Algorithm` is left open in the file. The tree has since settled towards `Strategy`
with hook-based dispatch — `kernel RingMap` (`ringmap.m2:240`) and `intersect` both work that way now
— so the machinery this asks for partly exists in a different form, and the question is whether ring-
registered strategies should be expressed as hooks rather than as a name looked up in the ring.
