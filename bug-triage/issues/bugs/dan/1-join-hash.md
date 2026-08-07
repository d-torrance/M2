The guess in the file is right, and the saving is worth having.

### Every List construction folds the whole list

`basic.d:114-117` does `r.hash = hash(r)`, and `hash(x:List)` at `:82-85` is an element-by-element fold
with multiplier `1299833` over the seed `x.Class.hash + 23407`.

### Measured

| | time |
| --- | ---: |
| `join` of two 200k lists | 11.2 ms |
| the identical fold over the resulting 400k elements | 7.5–8.5 ms |
| reading a List's stored hash | 0.69 µs |

The fold is measurable separately because a `Sequence`'s hash is **not** cached — `basic.d:33-36` refolds
on every call. So roughly two-thirds of `join` is hashing.

### And the fold is combinable

```
h(L ++ M) = 1299833^(#M) * (hash L - c) + hash M   (mod 2^64),    c = hash class L + 23407
```

verified on seven cases including both empty sides, mixed element types and nested lists. That replaces
an O(n) fold with one modular exponentiation.

A caution from getting it wrong first: my initial derivation used the *Sequence* constants
`seqHashSeed`/`seqHashMult` (`basic.d:8-9`) and failed all five test cases. The List fold uses different
constants.

### The file's second half is already moot

`new List from J` and `toSequence J` on 400k elements cost 1–2 µs, because a List wraps a Sequence and
they share the array.

### Fix site

`d/actors4.d:1175-1186`, plus a `list()` variant in `basic.d` taking a precomputed hash.
