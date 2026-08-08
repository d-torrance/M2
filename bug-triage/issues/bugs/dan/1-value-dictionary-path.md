
`value` resolves symbols against the ambient `dictionaryPath` and offers no way to override that for a
single call. `methods value` has eight entries — `String`, `Nothing`, `IndexedVariableTable`,
`IndexedVariable`, `RingElement`, `Pseudocode`, `Expression`, `Symbol` — and none takes a dictionary or
a list of them.

### The only workaround is the one M2 cannot unwind

```m2
old = dictionaryPath;
dictionaryPath = {...};
value s;                    -- if this raises, the next line never runs
dictionaryPath = old;
```

M2 has no unwind-protect. `pushvar`/`popvar` (`Core.m2:67-73`) is the closest thing and does not restore
on error either — pushing a value and then raising leaves the value changed and the stack unbalanced,
and `Core.m2:65` carries `-- TODO: move to the interpreter and make thread-safe`.

At the top level this is survivable: you notice, and you fix your session.

### Where it stops being survivable

The request is worth more as a foundation than as a convenience, and the case that makes it concrete is
embedding M2 in a host program.

- **A host cannot unwind the assignment.** From C there is no `try` to wrap the call in, so an M2 error
  during `value` leaves `dictionaryPath` permanently altered with no opportunity to restore it.
- **One global path cannot serve several evaluation contexts.** A process serving multiple independent
  sessions — which is the shape of Macaulay2Web — has no way to say "evaluate this string in *that*
  session's namespace".
- **New symbols need a destination.** `value "x = 3"` puts `x` somewhere; a host wants to say where,
  and today the answer is wherever the global path happens to point.

A per-call argument is the standard answer to all three, and it is the kind of primitive that is much
cheaper to add before an embedding API is designed around its absence than after.

### The notion already exists in the language

`localDictionaries` returns the dictionaries belonging to a function's own scope, and `code.m2:20` uses
it — so "these dictionaries, for this purpose" is already expressible. What is missing is handing such a
list to `value`.

### Shape

Something like `value(String, List)`, or an option, taking the dictionaries to resolve against. The
signature is worth deciding alongside whatever an embedding API would want rather than in isolation;
the point of filing is that the gap exists and has a consumer in view.

### Related

[#1427](https://github.com/Macaulay2/M2/issues/1427) asks that `loadPackage` not scribble in the user's
dictionary — the same concern about namespace containment, from the other direction.
[#917](https://github.com/Macaulay2/M2/issues/917) and
[#1627](https://github.com/Macaulay2/M2/issues/1627) are the other open dictionary-hygiene issues.
