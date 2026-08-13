`quotient` has a `Linear` strategy that is a stub, and it is commented out of the hook table, so asking for it
is an error rather than a slow answer:

```m2
i1 : S = QQ[x,y,z];

i2 : I = ideal(x^2*y, x*z, y*z^2);

i3 : quotient(I, ideal x, Strategy => Linear)
stdio:3:1:(3): error: unrecognized Strategy => 'Linear' for (quotient,Ideal,Ideal)
```

The default path is fine — `quotient(I, ideal x)` and `I : x` agree, both giving `ideal(z, x*y)` — so nothing
is broken. What is missing is the optimisation.

### The stub already contains the requested condition

[`Saturation.m2:241-246`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/Saturation.m2#L241-L246):

```m2
    Linear => (opts, I, J) -> (
	-- assumptions: J is a single linear element, and everything is homogeneous
	if not isHomogeneous I
	or not isHomogeneous J or not isLinearForm J_0
	then return null;
	stderr << "warning: quotient strategy Linear is not yet implemented" << endl; null),
```

so the test for "the divisor is a linear form", which is what the request below asks `quotient` to check
automatically, is already written — inside the strategy that does not exist yet. That is why the two halves of
the original request are treated as one piece of work here.

### And it is unregistered, so the warning is unreachable

`Saturation.m2:261` and `:306`:

```m2
scan({Quotient, Iterate-*, Linear*-, Monomial}, strategy ->
    addHook(key := (quotient, Ideal, Ideal), algorithms#key#strategy, Strategy => strategy))
...
scan({Quotient, Iterate-*, Linear*-}, strategy ->
    addHook(key := (quotient, Module, Ideal), algorithms#key#strategy, Strategy => strategy))
```

`Linear` is inside a `-* … *-` block comment in both. A caller therefore never sees the "not yet implemented"
warning; they get `unrecognized Strategy` from the hook dispatcher instead.

### Nothing published is being broken

Worth stating, because it changes how urgent this is: `Linear` is not advertised. `hooks methods(quotient,
Ideal, Ideal)` lists only `Quotient`, `Iterate` and `Monomial`, and the `[quotient, Strategy]` node at
`Saturation/quotient-doc.m2:97` describes `Quotient` and `Iterate` and directs the reader to `hooks` for the
list. So no documented promise is unmet — this is an internal optimisation that was started and left.

It *was* advertised once. [#1317](https://github.com/Macaulay2/M2/issues/1317), from 2020, quotes the help
output of the day:

> `* Strategy => ...,  -- Possible strategies are: Iterate, Linear, and Quotient`

so the enumeration existed and was removed somewhere in the documentation restructuring since.

### What would close this

Either implement the strategy and register it, and have `quotient` select it automatically when the divisor is
a linear form — which is what the request asks for and what the stub's own precondition anticipates — or, if
nobody intends to, delete the stub and the two commented-out registrations so the next reader does not spend
the same time working out that `Linear` is unreachable.

### Provenance

Two requests from `bugs/dan/0-bugs-eisenbud.m2`, a wishlist file removed with the `bugs/` tree in d2c8d27826
and catalogued in #36, merged here because the evidence above shows they are one piece of work:

> quotient should check to see if the second arg is a variable -- in this case it should always use the
> "linear" strategy

> `time quotient(i3,S_0,Strategy=>Linear)` Claims that the strategy is not implemented.

The second still holds in substance, though the wording has drifted: the claim is no longer "not implemented"
but "unrecognized", because the strategy is not even registered.

Nothing in the tracker covers this — searched titles for `quotient` and "Linear strategy", bodies for
"quotient strategy Linear", and comments for "quotient strategy", "Strategy => Linear", `Saturation.m2` and
`isLinearForm`.
