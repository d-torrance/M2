### What the file reports

A Macaulay2 package declares itself by calling `newPackage`, passing options such as `Configuration`.
There are two ways to get a package loaded: `loadPackage "Foo"`, which goes through M2's package
machinery, or `load "Foo.m2"`, which simply reads the file. Dan's file records that the same
`Configuration` option comes back as a different *type* depending on which route was taken. It still
does:

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
