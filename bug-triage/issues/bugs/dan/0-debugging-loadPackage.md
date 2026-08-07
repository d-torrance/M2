The documented behaviour and the actual behaviour disagree. `Core/code.m2:263` documents

> `break` -- leave the debugger, returning to top level

but `break` leaves the debugger and **resumes the file past the failing expression**, so the rest of
the file runs.

### Four lines reproduce it

Load a file containing a function that calls `error`, plus anything after it; at the debugger prompt
type `break`; the remaining expressions in the file are evaluated.

Dan's transcript in the file is the visible symptom of that: loading `Macaulay2Doc` twice errors,
`break` carries on into the next error *in the same file*, re-enters the debugger, and the session is
left sitting at `ii` — a prompt the user cannot easily escape.

### Distinct from #1928

Worth stating, since the two look alike: `continue` re-runs the failing expression and therefore
loops, while `break` skips it and continues. The two commands differ, and **neither matches its
description** — so a fix should probably settle what each is meant to do rather than adjust one of
them.
