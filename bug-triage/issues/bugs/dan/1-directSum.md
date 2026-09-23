### What the file asks for

`directSum` forms the direct sum of the modules, matrices, chain complexes or coherent sheaves it is
given, written either as `directSum(A, B, C)` or as `directSum L` for a list `L`. When the summands
are not already to hand but have to be computed from something else, the list has to be built first,
so the idiom is

```m2
directSum apply(L, f)
```

David's suggestion is that `directSum(L, f)` mean exactly that, with the `apply` implicit. It is a
convenience rather than a defect: nothing computes a wrong answer today, the `apply` simply has to be
written out every time. No such method is installed, so it still does.

### Why file something this small

The intended semantics are stated unambiguously in one line, there is no design question to settle, and
it is self-contained: a method installation plus a documentation node and a test. That combination suits
a first-time contributor, which is why this is on the tracker rather than parked.

### Not a duplicate of the other directSum issues

Several are open and none is about this signature: **#606** (`MutableHashTable`), **#2891** (components
and direct sums of matrices), **#1060** (degrees ignored).
