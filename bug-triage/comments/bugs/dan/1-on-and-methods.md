A second file from the pre-GitHub `bugs/` tree makes the same request as this
issue, and it carries something the first one does not.
`bugs/dan/1-on-and-methods` opens:

> Here's code that monitors every call to any method function of `random`, which
> should be incorporated into `on`

followed by a working sketch rather than a patch. Inside it, on the line that
names the wrapper, is a comment:

```m2
setAttribute(g,ReverseDictionary,name); -- we should store the function body, not the function closure, see below
```

and the "see below" is a transcript that still reproduces exactly, on
1.26.06-40-gd8e86d689d, under `debug Core`:

```m2
i2 : f = x -> y -> z
i3 : setAttribute(f 1, ReverseDictionary, "hi there")
o3 = hi there
i4 : f 1
o4 = FunctionClosure[stdio:2:9-2:15]
i5 : setAttribute(functionBody f 1, ReverseDictionary, "hi there")
o5 = hi there
i6 : f 1
o6 = FunctionClosure[stdio:2:9-2:15]
```

Both halves fail, for different reasons, and separating them is the useful part:

- **`f 1 === f 1` is `false`.** Each application builds a fresh closure, so
  `setAttribute(f 1, …)` names an object that is immediately discarded. Naming a
  closure you have kept does work — `h = f 1; setAttribute(h, ReverseDictionary,
  "hi there"); h` prints `hi there`.
- **Naming the shared `functionBody` has no effect either**, because the printing
  path looks the attribute up on the closure and never consults its body.

That matters for the first of the three gaps listed above — `on` labelling by
argument rather than by method. Both proposed fixes route the label through
`ReverseDictionary` on a closure built at wrap time: the `Name` option in the
patch quoted in this issue, and `setAttribute(g, ReverseDictionary, name)` in the
sketch above. So a name attached that way survives only if the wrapper closure is
the same object every time the trace prints — which is what the second half of
this file was recording when it said the body, not the closure, is what should
carry it.
