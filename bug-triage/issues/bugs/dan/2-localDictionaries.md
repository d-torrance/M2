`localDictionaries` applied to a global symbol returns a dictionary that is not local, does not
contain the symbol, and is the same one whatever symbol you pass.

```m2
i1 : localDictionaries global a

o1 = {Varieties#"private dictionary"}

i2 : localDictionaries global sin

o2 = {Varieties#"private dictionary"}

i3 : localDictionaries global zzneverused

o3 = {Varieties#"private dictionary"}

i4 : localDictionaries symbol currentPackage

o4 = {Varieties#"private dictionary"}
```

`zzneverused` is a symbol nothing has ever defined, so the answer carries no information about the
argument at all.

Three separate things are wrong with it:

- **It is not local.** `class` of the result is `GlobalDictionary`. The documented example behaves
  correctly by contrast — `localDictionaries ((f 22) 33)` gives three `LocalDictionary`s, and
  `localDictionaries()` gives one.
- **It is not the dictionary containing the symbol.** `dictionary global a` is
  `User#"private dictionary"` and `dictionary global sin` is `Core.Dictionary`.
- **It does not contain the symbol.** `(o1#0)#?"a"` and `(o2#0)#?"sin"` are both `false`.

Which package gets named is incidental. The bug file below recorded
`{Macaulay2#"private dictionary"}` in 2006 and it is `Varieties` now — it is whichever package
happened to be created last, not anything to do with the symbol.

### What the documentation says

> a list of the local dictionaries associated with the lexical scopes containing `f`

`ov_repl.m2:462-479`. A global symbol has no enclosing local scope, so `{}` would be a defensible
answer, and an error would be another. Naming an unrelated package's private dictionary is neither.

### Where it comes from

`localDictionaries` walks the frame chain outward from the symbol's frame
(`d/actors5.d:1688-1697`). A global symbol's frame is the global frame, and the dictionary closure
built from it is whatever that frame currently points at, which is why every global symbol gives the
same answer and why the answer moves as packages are added.
