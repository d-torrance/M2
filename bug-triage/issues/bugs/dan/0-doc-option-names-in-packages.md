### What the file asks for

An optional argument in Macaulay2 is named by a symbol, so a package defining its own options defines
symbols for them. Those symbols have to be exported or the package will not install — and nothing
tells an author so. This file asks for the requirement to be documented, and for the error that
enforces it to explain itself.

Verified, and the file misplaces the error, which is worth correcting on the way in.

### The behaviour

The failure looks like this:

```
error: mutable unexported unset symbol(s) in package MyPkg: 'MyOpt'
```

Exporting `MyOpt` installs cleanly. Neither Macaulay2Doc nor the wiki style guide mentions the
requirement, which is what the file asks to fix.

### Where the error actually comes from

The file says the message "comes when the documentation is processed". It does not: it is raised from
package *closing* (`Core/packages.m2:511-513`, from `endPackage`), and it fires with no documentation
node present at all. That matters for the documentation being requested — the rule is about the
package's own symbols, not about documenting them — and it also means the error arrives before any
`beginDocumentation` work, which is why a package author sees it as a load failure rather than a doc
failure.

### Notes for whoever picks this up

Two separable pieces: document the requirement (the natural home is alongside `export` and the
optional-argument documentation template), and improve the message, which currently names the symbol
but not the reason a package author should care — that an unexported option name cannot be written to
the documentation database.
