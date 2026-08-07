Two live asks, and the first is a verified documentation defect.

### The manual documents a no-op

The thread opens with Mike reporting that *"our description of how to remove a cached GB is no longer
correct"*. That description is still in the manual. `ov_groebner_bases.m2:439` is an `EXAMPLE` block
which says "we must erase the memory of the Groebner basis computed above", runs

```m2
remove(f.cache, {false,0})
```

then says "compute the Groebner basis anew" and times it. The cache key is a `GroebnerBasisOptions` hash
today, not `{false,0}`, so that `remove` is a **silent no-op** — verified: `keys (generators I).cache` is
unchanged across it and the basis is still cached.

Which means the timing the manual displays for the Hilbert-hint computation is a **cache hit**, not a
fresh run. The two `gbRemove` copies at `ov_rings.m2:1535-1536` carry the same stale key but sit inside
`-* *-` blocks, so they are dead rather than misleading.

### The design half is unadopted and still undecided

Mike proposes a `UseCache` (or `Cache`) option on `gb`, `res` and anything else that stashes results, plus
a way to ask for the key. Today `gb` and `res` have no `UseCache`, `Cache` or `CleanCache` option,
`clearCache` exists nowhere in Core (only as a `BasicDivisor` method in `WeilDivisors`), and Mike's
`getGroebnerKey` never appeared — though the key is at least inspectable now, via
`keys (generators I).cache`.

The two halves are filed together because the documentation cannot be corrected without deciding what the
supported way to clear a cache *is*.
