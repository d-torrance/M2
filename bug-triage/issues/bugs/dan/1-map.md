Option-style destinations are already supported — but only when the source and target are the same
ring, which is why the file's example fails.

### What works today

```m2
i1 : R = QQ[a,b];

i2 : map(R, R, {a => a^2})
o2 = map(R,R,{a^2, b})          -- b is left alone, which is the useful default
```

### What does not

```m2
i3 : T = QQ[a]; U = T[b];

i4 : map(T, U, {b => a})
stdio:4:1:(3): error: destinations not specified for every generator

i5 : S = QQ[c];
i6 : map(S, R, {a => c})
stdio:6:1:(3): error: destinations not specified for every generator
```

### Why

`Core/ringmap.m2:495-501`. The option list is parsed either way, into a destination list carrying
`symbol dummy` for each generator the caller did not mention. What differs is the handling of those
placeholders:

```m2
else if R === S and S === ring commonzero then (
     -- if source==target, then the default is to leave generators alone
     for i from 0 to #m-1 do if m#i === symbol dummy then m#i = g#i;
     )
else (
     if any(m,x -> x === symbol dummy) then error "destinations not specified for every generator";
     );
```

So the notation and the "leave it alone" rule both exist; they are simply gated on the source and
target being identical.

### The case for widening the gate

In the file's example the unspecified generator is `a`, and `a` **is** a generator of the target
`T = QQ[a]` — so "leave it alone" is not merely convenient there, it is well defined. The same holds
whenever the omitted generators lie in a ring the target shares, which is the common situation for a
map out of a tower `R[b] -> R`.

Where it is genuinely undefined — `map(QQ[c], QQ[a,b], {a => c})`, with `b` having no meaning in the
target — the present error is the right answer, and should stay.

`substitute` accepts the same spelling unconditionally, since it rewrites an element rather than
defining a homomorphism:

```m2
i7 : substitute(b^2, {b => a})
o7 = a^2
```

### One thing to be careful about

The positional form fills in silently, so a caller who miscounts gets a map rather than an error:

```m2
i8 : map(T, U, {a})
o8 = map(T,U,{a, a})            -- one destination supplied, two generators mapped
```

That is an argument for the option form rather than against it, but it also means any change here
should not extend the silent filling to more cases.
