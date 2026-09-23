### What the file asks for

Loading a Macaulay2 package puts its exported names into scope unadorned, so after loading, `graph`
means that package's `graph`. Every exported symbol also has a qualified *synonym* spelling containing
a `$` — `foo$Pkg` — which is what M2's shadowing warning tells you to use when two packages claim
the same name. This file asks `needsPackage` and `loadPackage` for an option, `Using => false`, that
loads a package with a dictionary holding only those synonyms on `dictionaryPath`: the package's names
stay reachable explicitly, but nothing is dumped into scope unadorned.

No such option exists. `needsPackage` takes `LoadDocumentation`, `Configuration`, `FileName`, `Reload`
and `DebuggingMode`, and nothing else.

### Why it is worth having

This is the missing middle between the two options that exist. `PackageImports` loads without
importing symbols at all; `PackageExports` imports them *and* re-exports them to your own users. There
is nothing that says "I want these names, but only via their qualified spellings, and I am not passing
them on" — which is exactly what a package author wants when a dependency's names collide with their
own.

**#4537** is the sweep that trips over this gap: 34 packages still use `needsPackage` because neither
existing option does what they need.

### Notes for whoever picks this up

The synonym symbols already exist — the shadowing warning M2 prints ("use the synonym `graph$0`")
depends on them — so this is about which dictionary goes on the path rather than about creating new
names.
