Reproduces exactly. The same package option comes back as a different type depending on how the
package was loaded:

```m2
i1 : loadPackage "Foo"; class (options Foo).Configuration
o1 = OptionTable

i2 : load "Foo.m2";     class (options Foo).Configuration
o2 = List
```

### Why it matters

Package code that reads its own `Configuration` has to cope with both, or work only under one of the
two loading paths. Since `load` is what happens when a developer is iterating on a package file
directly, the difference shows up exactly when someone is debugging.

### Notes for whoever picks this up

The asymmetry is in who processes the options: `loadPackage` passes them through the option-processing
machinery that turns the list into an `OptionTable`, while a bare `load` evaluates the `newPackage`
call with whatever the author wrote — a `List`. Normalizing inside `newPackage` itself, rather than in
`loadPackage`, would make the two agree regardless of entry point.
