This issue covers the whole `for`-bounds gap: `1-for-sequence-idea` and `1-for-by` are recorded as
duplicates of it.

### Bart Snapp's example still fails

```m2
i1 : for i from (0,0) to (5,6) do ...
     error: expected an integer

i2 : for j in (0,0)..(5,6) do ...        -- iterates all nine pairs
```

So the form the file compares against works, and the one it asks for does not.

### `evalForCode` is the whole story

`d/evaluate.d:314`. The **`in`** clause accepts a `Sequence`, a `List`, or any iterable via
`getIterator`/`getNextFunction` (`:331-341`). The **`from`** and **`to`** clauses accept only a `ZZcell`
passing `isInt` (`:348-358`), because `j` and `n` are C `int`s.

And there is **no step at all**: `forCode` (`d/parse.d:252`) has `inClause`, `fromClause`, `toClause`,
`whenClause`, `listClause` and `doClause` — no field for one — and `by` appears nowhere in the `d`
sources as a keyword. That is why `1-for-by`'s first example fails as "no method for adjacent objects:
`0` (of class ZZ) SPACE `by` (of class Symbol)" rather than as a for-loop error.

### Both needs are reachable today, through the `in` clause

```m2
for i in reverse(0..4) do ...                       -- descending
for i in select(0..10, zzk -> zzk % 2 == 0) do ...  -- stepping
```

both verified. So this is convenience over a working spelling, filed on the same basis as #4557 and
#4563.

### The file's own parenthetical deserves an answer

*"what about installing a top level method to handle that?"* — no: `from` and `to` are **parsed**, not
dispatched, so no method can reach them.

### Related

**#4271** is the live neighbour, a nested-`for` syntax whose author offers to implement it.
