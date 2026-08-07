Still accepted silently:

```m2
i1 : first List := x -> x
o1 = {*Function[stdio:1:15-1:17]*}
o1 : FunctionClosure
```

`first` is a plain function closure (`startup.m2`: `first = x -> x#0`), not a method function, so
there is no method table for the assignment to write into. Nothing is installed and nothing is said.

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
