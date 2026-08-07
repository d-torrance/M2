Still drops the configuration, and the path is short enough to quote.

`loadPackage Package` forces `Reload => true` (`Core/packages.m2:193`), and neither of the two callers
that reload passes `Configuration` along:

- `installPackage.m2:670`
- `testing.m2:87`

So a package loaded with `Configuration => {...}` comes back with the **defaults** when `check` or
`installPackage` reloads it.

### Why it bites quietly

Nothing errors. The package simply behaves as though the user had never configured it — which for a
package whose configuration names an external program or a directory means the failure appears later,
somewhere unrelated, as a missing executable or a wrong path.

### Notes for whoever picks this up

The configuration in force is recoverable at the point of reload: it is on the loaded package object
itself, so the fix is plausibly to read it back and pass it rather than to thread it through from the
original call. Compare **#4536**, another consequence of reloading that this catalogue filed, and
**#3852**, which added the warning about recreating instances after a reload.
