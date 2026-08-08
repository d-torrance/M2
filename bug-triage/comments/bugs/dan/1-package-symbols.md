An earlier note of yours in the pre-GitHub `bugs/` tree asks this same question with
the opposite answer, which seems worth having on the thread.
`bugs/dan/1-package-symbols`, two lines, from 2009:

> the user needs a way to declare a symbol global but private within the package,
> even if it occurs in the core Macaulay2 system

So: **package-private globals** there, **Core-shared globals** here. Same collision,
same author, opposite directions — which is probably the most useful thing to know
about the design space before choosing.

### What the status quo actually is

Using `Gamma` from the example above, from a throwaway package, on
1.26.06-40-gd8e86d689d:

```m2
export {"Gamma"}          -- succeeds
```

```
before load:  Gamma 2 = 1
after load:   Gamma 2 = ERROR          class Gamma = Symbol

 -- warning: symbol "Gamma" in Core.Dictionary is shadowed by a symbol in ZzSymDemo.Dictionary
 --   use the synonym Core$Gamma
```

A package *can* claim a name Core owns. What it gets is not a private global but a
global that takes the name away from everyone for the rest of the session, and
`Core$Gamma` is a repair offered to whoever needed the old meaning, not isolation for
the package that wanted the new one.

That is worth stating because it is the middle case between the two proposals: neither
"private to the package" (nothing else should see it) nor "shared in Core" (everyone
agrees it is an option name), but "last loader wins, with a warning".

The two-package case is the one this issue is actually about, so here it is measured.
Two throwaway packages, `ZzSymA` and `ZzSymB`, each `export {"Gamma"}`, and `ZzSymA`
uses it as an option name:

```
needsPackage "ZzSymA"
 -- warning: symbol "Gamma" in Core.Dictionary is shadowed by a symbol in ZzSymA.Dictionary
zzaMethod(1, Gamma => 42)          -->  "A got Gamma => 42"

needsPackage "ZzSymB"
 -- warning: symbol "Gamma" in ZzSymA.Dictionary is shadowed by a symbol in ZzSymB.Dictionary
 -- warning: symbol "Gamma" in Core.Dictionary is shadowed by a symbol in ZzSymB.Dictionary
zzaMethod(1, Gamma => 42)          -->  error: encountered an unknown key or option: Gamma
```

The two symbols are distinct objects (`ZzSymA::Gamma =!= ZzSymB::Gamma`) and `symbol
Gamma` now resolves to `ZzSymB.Dictionary`, so `A`'s option table no longer has a key
matching what the caller writes. Loading an unrelated package broke a call that worked a
moment earlier — which is the concrete cost of the status quo, and the case a shared
Core symbol would fix.

It is at least warned about rather than silent, and the warning names the culprit. But
`packages.m2:88` gates it on `debuggingMode`, so under `M2 --script` all three lines are
absent and only the error survives.

### Where the warning came from

The shadowing warning is the surviving half of an older two-part request
(`bugs/dan/0-synonyms-and-collisions`): *"we should : (1) put the package symbols at
the head of the list ; (2) alert the user to the shadowing."* (2) was built —
`packages.m2:88`, gated on `debuggingMode`, so `M2 --script` does not show it — and (1)
was deliberately not, on the grounds that reordering global symbol lookup is a much
larger change. So the current behaviour is a decision rather than an accident, and
whichever direction #666 goes, it is choosing against that decision rather than filling
a gap.
