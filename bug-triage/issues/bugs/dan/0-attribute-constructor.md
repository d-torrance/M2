### What the file is recording

The Macaulay2 interpreter is not written in C directly. Its sources are the `.d` files in
[`Macaulay2/d/`](https://github.com/Macaulay2/M2/tree/development/M2/Macaulay2/d), written in M2's own
language, and `scc1` — "the D to C translator", built in
[`Macaulay2/c/`](https://github.com/Macaulay2/M2/tree/development/M2/Macaulay2/c) — turns them into C,
which is what the build then compiles. Among the things `scc1` emits is one `<package>_prepare`
function per source file, tagged so the linker runs it before `main`. The tag it uses is
`__attribute__ ((constructor))`, a GCC extension with no ANSI C equivalent, and that is the whole of
Dan's observation: not a bug anyone can trigger, but a portability debt in generated code.

Still emitted, from `c/cprint.c:487`.

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
