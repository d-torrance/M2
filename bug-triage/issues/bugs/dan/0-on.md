### What the file asks for

`on` is Macaulay2's tracing facility: `on f` wraps `f` so that each call prints its arguments and its
result. A *method function* such as `basis` is not one function but a table of methods, one per
argument signature, and `on` wraps only the outer dispatcher — so tracing it tells you a call
happened but not which method took it. This file asks that `on` reach the individual methods, supplies
a draft patch doing so, and ends by noting that the draft's own classification of functions is already
incomplete.

Still unmet, and the obvious workaround does not reach the interesting case.

### Why `f = on f` is not enough

It works only for functions you defined yourself. For Core method functions — the ones actually worth
tracing — the rebinding is refused:

```m2
i1 : basis = on basis
     error: ... protected global
```

Dan's route in the file does work, and traces real calls including ones made from library code:

```m2
installMethod(basis, ZZ, Module, on(lookup m, Name => ...))
```

### Three gaps stand

1. **`on` labels by argument, not by method.** `foo(2,3)` prints `Sequence`, and an inherited call
   prints the argument's class rather than the method that actually ran — so a trace of a method
   function does not tell you which method it went to.
2. **There is no way to trace in place** without rebinding the name, which is what the protected-global
   error blocks.
3. **For a `MethodFunctionWithOptions` the trace shows the wrapper**, an `OptionTable` and a
   `FunctionClosure`, instead of the real arguments. That is the *"digging in one level deeper"* the
   file asks for.

### A gap in the file's own draft, now wider

The patch in the file classifies functions into two cases and misses `toString` and `toExternalString`,
which are in neither — and the classification has grown a third case since, because
`MethodFunctionSingle` now exists as well.
