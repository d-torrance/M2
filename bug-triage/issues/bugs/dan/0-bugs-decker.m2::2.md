Neither algorithm named in this request exists, and `radical`'s implementation is still the translated classic
Macaulay script it has always been.

### What `radical` implements today

Four strategies, registered at
[`MinimalPrimes/radical.m2:129-130`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/MinimalPrimes/radical.m2#L129-L130):

| strategy | what it does |
| --- | --- |
| `Unmixed` | `unmixedradical I`, or `radical1 I` when the ideal is not assumed unmixed |
| `Decompose` | `intersect minimalPrimes I` |
| `CompleteIntersection` | an Eisenbud–Huneke–Vasconcelos method, needing a complete intersection supplied by the caller |
| `Monomial` | defers to the engine's `rawRadicalMonomialIdeal` |

The documented options — `[radical, Strategy]`, `[radical, Unmixed]`, `[radical, CompleteIntersection]` —
match that list, so there is no undocumented fifth path. And the header above the code says where it comes
from:

> Based on the Macaulay (classic) scripts written by D. Eisenbud.  Translated from Macaulay to Macaulay2 by
> M. Stillman

### The characteristic-p gap is acknowledged in the source

`radical00`, which computes the separable part variable by variable, carries this
([`radical.m2:56-60`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/MinimalPrimes/radical.m2#L56-L60)):

```m2
    scan(v, x -> (
	    -- there are THREE problems here!
	    -- (a) use linear algebra
	    -- (b) char p
	    -- (c) f might not be the smallest eqn in var v_i.
```

Item (b) is the gap Kemper's algorithm addresses. So this is not only a wishlist entry: the code itself
records that the characteristic-p case is unresolved where it takes separable parts.

### Where I looked, so nobody repeats it

Neither `Krick`, `Logar` nor `Kemper` appears anywhere in the tree in this sense — searched all file types,
not only `.m2`, across `packages/`, `packages/undistributed-packages/`, `m2/` and the engine. The only hits
for "Logar" are `NoetherNormalization` and `QuillenSuslin`, which use A. Logar's work on Noether
normalization and the Logar–Sturmfels algorithm respectively, and are unrelated to radicals. The rest are
`logarithm`, `logarg` and `dilogarithm`.

Nothing else defines a `radical` method: `MinimalPrimes/radical.m2` is the only implementation, and the
engine's contribution, `rawRadicalMonomialIdeal`, is monomial-only. No external solver supplies one either —
`Msolve` mentions radicals only in a sentence about its input being radical, and
`GeometricDecomposability` is a consumer of `radical(..., Unmixed=>true)` rather than a provider.

### What this issue does not have

A benchmark. The case above rests on the two algorithms being absent and on the char-*p* gap being
acknowledged in the source, not on a measured cost, and I have not established how slow `radical` is on
ordinary input today. If a maintainer would rather see that first, it is a reasonable thing to ask for before
anyone commits to implementing a paper.

### Provenance

This is one request from `bugs/dan/0-bugs-decker.m2`, a wishlist file removed with the `bugs/` tree in
d2c8d27826 and catalogued in #36:

> Implement faster algorithms for radical: Krick/Logar in characteristic zero and Kemper in characteristic
> p>0.  Rewrite documentation accordingly.

The references are presumably Krick and Logar, *An algorithm for the computation of the radical of an ideal
in the ring of polynomials* (AAECC 1991), and Kemper, *The calculation of radical ideals in positive
characteristic* (J. Symbolic Comput. 2002).

Two open issues touch `radical` and neither is this:
[#1017](https://github.com/Macaulay2/M2/issues/1017), that `radical` should work over Galois fields, and
[#2839](https://github.com/Macaulay2/M2/issues/2839), a correctness bug in `decompose` and `radical`. Both
are about getting an answer at all rather than getting one faster.
