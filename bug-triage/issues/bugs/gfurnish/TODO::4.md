Two threads assigning past the end of the same `MutableList` silently lose about a quarter of their
writes. No error is raised, and the list can end up shorter than the largest index that was assigned.

### The reproducer

```m2
allowableThreads = 8
L = new MutableList from {};
T = for i from 0 to 199 list schedule((j) -> (L#j = j), i);
scan(T, t -> taskResult t);
#select(0 .. 199, j -> j >= #L or L#j =!= j)
```

Twenty trials, every one of them losing something:

```
lost entries per trial: {70, 48, 62, 64, 57, 59, 71, 52, 60, 46,
                         41, 51, 47, 58, 73, 75, 46, 53, 51, 58}
trials with any loss: 20 of 20
```

Looking more closely at where the loss lands — `(final length, wrong entries within that length, indices
past the end)`:

```
(198, 44, 2)
(200, 52, 0)
(200, 48, 0)
(195, 56, 5)
(200, 42, 0)
```

So usually the list does reach length 200 and roughly a quarter of its entries are `null` where a value was
written; sometimes it also ends up shorter than the largest index assigned.

### The control: it is the growth path, not element assignment

Pre-size the list so no enlargement ever happens, and the same 200 concurrent writes are all correct:

```m2
L = new MutableList from toList(200:null);   -- long enough already
```

```
pre-sized (no enlargement), lost per trial: {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
growing,                    lost per trial: {78, 55, 63, 57, 54, 61, 66, 48, 59, 50}
```

Ten trials each. That is the whole difference between the two runs.

### Why

[`assignvector`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/d/evaluate.d#L111-L134) in
`d/evaluate.d`:

```
assignvector(m:List,i:Code,rhs:Code):Expr := (
     x := m.v;
     ...
	       val := eval(rhs);
	       when val is Error do return val else (
		    if k >= length(x) then (
			 x = new Sequence len k+1 do (
			      foreach t in x do provide t;
			      while true do provide nullE;
			      );
			 m.v = x; );
	       	    x.k = val;
```

`x := m.v` is a snapshot taken before `rhs` is even evaluated. When the index needs the list to grow, a
whole new `Sequence` is built from that snapshot and installed with `m.v = x`, and there is no lock
anywhere. Two threads growing the same list each copy from their own snapshot, so whichever installs second
discards everything the other wrote since it took its copy. The final `x.k = val` then writes into the
thread's *local* `x`, which by then may no longer be the list's sequence at all — a second way for a write
to go nowhere.

### Relationship to what is already open

- [#175](https://github.com/Macaulay2/M2/issues/175) is the umbrella, "a long term project is to make
  everything thread safe". Its running checklist names `H#k ??= v`, augmented assignment and database
  writing; this is not on it, and an umbrella is not a thing anyone can close.
- [#659](https://github.com/Macaulay2/M2/issues/659) quotes this very function, but it is about the growth
  *policy* — growing one element at a time makes filling a list quadratic. Different defect, same lines.
- [#4103](https://github.com/Macaulay2/M2/pull/4103), open, rewrites this into `changeLength`. Reading the
  diff, it is still unlocked, so the race survives it — but it doubles capacity, so reallocation happens
  O(log n) times instead of O(n), and it writes via `m.v.k` rather than into a stale local, which removes
  the second failure mode above. I would expect the numbers here to fall a long way. **I have not measured
  that**, because it needs a build; whoever picks this up should re-run the reproducer against that branch
  before deciding how much is left.

A lock around the reallocation would close it. If concurrent mutation of a shared `MutableList` is meant to
be unsupported rather than merely unimplemented, saying so in the documentation would close it too — at
present nothing says either way, and the failure is silent, which is the worst combination.

### Provenance

This is the one item listed under "not yet re-entrant" in the removed file's own inventory, written in 2010
while the thread system was being built:

> not yet re-entrant :
>
> &nbsp;&nbsp;&nbsp;&nbsp;storing an entry into a mutable list at a position greater than the size,
> causing the list to be enlarged

The rest of that inventory has held up. This one entry is unchanged fifteen years later.
