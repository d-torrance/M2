Still emitted. `c/cprint.c:487` writes `__attribute__ ((constructor))` into the C that `scc1`
generates, so every build of the interpreter depends on a GCC extension rather than on anything in
the C standard.

### Why it matters, and why it has not bitten

In practice M2 is built with GCC or Clang, both of which accept the attribute, which is why this has
sat harmlessly for years. It becomes a real obstacle only for a third compiler, and it is the sort of
thing that is far cheaper to change while nobody is depending on the behaviour than during a port.

### Notes for whoever picks this up

C++11 offers no portable replacement for "run this before `main`" in C; the usual portable pattern is
an explicit initialization function called from `main`, which for generated code means `scc1` emitting
a registry the interpreter walks at startup rather than relying on the linker to run constructors.
That is a slightly larger change than swapping a spelling, which is presumably why the file records
it as a standards observation rather than a bug.
