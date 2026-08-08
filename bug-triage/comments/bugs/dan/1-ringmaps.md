The same missed predicate applies to *composing* two ring maps, not only to applying one, and a second
file from the pre-GitHub `bugs/` tree asks for it. `bugs/dan/1-ringmaps`:

> In the code that deals with the class RingMap. We could envision three type of ring maps: those
> implemented by a ring homomorphism in the kernel; those implemented by promotion; and those
> implemented by an identity function. The method for `RingMap * RingMap` could figure out what to do
> in each case. […] There should be a predicate for determining whether a ring map is an identity map.

The predicate exists — `ringmap.m2:30-32`, `f == 1` — which is the same one this issue is about.
`RingMap * RingMap` (`ringmap.m2:383-397`) does not consult it: it computes `g matrix f`, builds a fresh
`rawRingMap`, and constructs a new `RingMap`, whatever the factors are.

On `R = QQ[a..h]` with `zzid = map(R,R)` and `f = map(R,R,{b,c,d,e,f,g,h,a})`, 200 compositions each:

```
f * zzid   0.021 s
zzid * f   0.0205 s
f * f      0.035 s      (control)
```

So composing with an identity is cheaper than a real composition but not free, and the result is a new
object rather than the other factor.

### A trap when checking this

Worth recording, because the obvious probe says the opposite of the truth. `===` on `RingMap`s compares
structurally, so:

```m2
g = f * zzid
f === g                          -- true
f.RawRingMap === g.RawRingMap    -- true   (needs debug Core)
```

Neither indicates a short-circuit. What settles it is object identity via the cache, which is mutable:

```m2
f.cache#"zzmarker" = 1
g.cache#?"zzmarker"              -- false  =>  g is a different object
```

Anyone verifying that a short-circuit works will want the cache check rather than `===`.

### The third ask, for context

That file's remaining request is that Schubert2 do all pullbacks by ring maps so that composing
pullbacks on cycle classes can be optimised — which is downstream of the composition case here, not a
separate optimisation.
