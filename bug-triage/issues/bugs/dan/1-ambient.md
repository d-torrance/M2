Not adopted. `ambient ZZ` and `ambient QQ` still fail with "no ambient ring present"
(`Core/rings.m2:53`), so Bart Snapp's suggestion — that `ambient R` return `R` when `R` is not a
quotient ring — is unmet.

### The objection to check first, which is in the file itself

Dan's reply is the reason this never happened: internal code walks `ambient` repeatedly and relies on it
*erroring* to know when to stop. Making it return `R` turns that loop into a non-terminating one, or at
best changes where it stops. So the change is not one line in `rings.m2` — it is one line plus an audit
of the callers that use the error as a terminator.

### Why it is still worth having

From a user's point of view the current behaviour is the odd one: `ambient` of a ring that has no
quotient relations has an obvious answer, and the error forces every caller to know in advance which
kind of ring they hold. That is exactly the sort of thing that makes generic code awkward to write.
