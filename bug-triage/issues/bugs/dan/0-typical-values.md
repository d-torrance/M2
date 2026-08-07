Undocumented, and the form works — which is the awkward combination, since a user has no way to learn
about a feature that is fully functional.

### What is missing

`X + X := X => (x,y) -> ...` records a typical value: after it, `typicalValues#(symbol +, X, X)` is `X`,
and the documentation machinery uses that when it renders the method. But:

- **"specifying typical values"** shows only the unary form, `prune Matrix := Matrix => f`;
- **"binary methods"** does not mention it;
- **"installing methods"** does not mention it.

So the binary spelling is nowhere in the manual.

### Related work already done

**PR #3102** (merged) used typical values when documenting *assignment* methods, so `AtomicInt += ZZ`
renders with the right type — evidence that the mechanism is actively relied on, and a natural place
from which to cross-reference the documentation being asked for here.
