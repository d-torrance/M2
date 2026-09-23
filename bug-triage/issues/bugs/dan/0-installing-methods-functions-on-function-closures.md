### What the file asks for

In Macaulay2 `f X := g` installs `g` as the method for `f` on arguments of class `X` — but that only
means anything if `f` is a *method function*, created by `method()`, which carries a table for the
method to go into. `first` is not one: it is a plain closure, `first = x -> x#0`. So assigning a
method to it has nowhere to write, and M2 accepts the line and quietly does nothing. This file asks
for an error, and then names the reason one cannot simply be added.

Still accepted silently:

```m2
i1 : first List := x -> x
o1 = {*Function[stdio:1:15-1:17]*}
o1 : FunctionClosure
```

### The complication the file names itself

Dan's own last line is the obstacle: *"Actually, this will interfere with all the code in
typicalvalues.m2!"* That file installs typical values by assigning to keys on functions that are not
method functions, so an error here would break the mechanism M2 uses to record return types. Any fix
has to distinguish "installing a typical value on a closure", which is intentional and widespread,
from "installing a method on a closure", which is the mistake being reported.

### Related, from the opposite direction

**#1979** proposes typed function closures — wanting the *capability* this file wants an error for. So
the two issues disagree about the destination, and it is worth deciding which before either is
implemented.
