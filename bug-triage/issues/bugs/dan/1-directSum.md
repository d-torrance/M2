Not installed: `directSum(List, Function)` has no method, so David's idea — that `directSum(x, f)` mean
`directSum apply(x, f)` — still has to be written the long way.

### Why file something this small

The intended semantics are stated unambiguously in one line, there is no design question to settle, and
it is self-contained: a method installation plus a documentation node and a test. That combination suits
a first-time contributor, which is why this is on the tracker rather than parked.

### Not a duplicate of the other directSum issues

Several are open and none is about this signature: **#606** (`MutableHashTable`), **#2891** (components
and direct sums of matrices), **#1060** (degrees ignored).
