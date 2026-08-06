There is a proposal on record for what `;;` should *mean*, which is worth having in view before it is made a no-op.

A note in the pre-GitHub `bugs/` tree (`bugs/dan/1-double-semicolon`, being triaged in #36) starts from the same syntax error this issue reports, and answers the "why is it an error" question with a use for the spelling:

> We could introduce `;;` as an operator that is like `;`, except at top level, no output values are recorded and the line number is not incremented. This would be a convenient way of inserting statements with no effect on subsequent line numbers, for convenience in comparing the output.

So the two requests point different ways. Accepting `;;` as an empty statement settles this issue, but it also spends the spelling, and the note's semantics could not then be added without changing the meaning of code already written. That may well be the right trade — a bare `;;` being an error is surprising either way — but it is a choice rather than a free win.

For what it is worth, the *need* behind the proposal is already met, and documented. `lineNumber` is assignable:

https://github.com/Macaulay2/M2/blob/68351e766d3e7991cd8bc0aa3ceecad1d49e65b8/M2/Macaulay2/packages/Macaulay2Doc/ov_repl.m2#L443-L448

so a statement can be inserted without shifting what follows:

```m2
i1 : 1+1

o1 = 2

i2 : 2+2; lineNumber = lineNumber - 2;

i2 : 3+3

o2 = 6
```

The decrement is by two rather than one because the line is counted before the next prompt is issued.

The other half of the proposal is not met: `;` suppresses *printing* but still records the value, so the output symbol is assigned either way.

```m2
i1 : 11+11;

i2 : value "o1"

o2 = 22
```

That is the part `;;` would add, and it is the smaller of the two.
