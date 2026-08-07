`needsPackage` still has no `Using` option. Its options are `LoadDocumentation`, `Configuration`,
`FileName`, `Reload` and `DebuggingMode`.

### What the file asks for

`needsPackage(..., Using => false)`: load the package but put a dictionary on `dictionaryPath` holding
only the **synonym** symbols — the ones containing `$` — so the package's names are reachable
explicitly (`foo$Pkg`) without being dumped into scope unadorned.

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
