Never adopted: `fsanitize` appears nowhere in the tree, and no CI workflow builds with a sanitizer.

### What has changed since the file was written

The tooling it points at is now standard. `-fsanitize=undefined` (which subsumes the integer checks
the linked post describes, via `-fsanitize=integer` for the implementation-defined cases) ships with
both GCC and Clang, so this no longer needs a research build of Clang — it needs a configure switch
and a CI job.

### Why it is likely to find things here

M2 does arithmetic on machine integers in the interpreter and the engine, and the hash functions
deliberately rely on unsigned wraparound. A sanitizer build separates the deliberate wraparound from
the accidental: `d/basic.d`'s sequence hash multiplies and overflows on purpose, while an overflow in
a monomial exponent or a degree computation is a bug. That distinction has to be made once, with
suppressions, before the output is usable — which is the main cost of adopting this.

### Notes for whoever picks this up

`-fsanitize=integer` is noisy on any hash function, so expect to start from a suppression list rather
than a clean run. A first pass over the interpreter's own startup already produces implicit-conversion
reports from `d/strings.d` and `d/expr.d` on ordinary input.
