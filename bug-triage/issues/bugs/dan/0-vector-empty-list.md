### What the file reports

`vector` builds a vector — an element of a free module — out of a list of entries. Handed the empty
list it has no ring to build over and no rank to build to, so it fails; the file is the transcript of
that failure, from a 1.4 session in 2011.

There is still no way to get an empty vector, and the message has got **worse** since. The file
records `vector {}` failing with "expected nonempty list" from `matrix1.m2:276`. Today it dies further
in, inside `vector Matrix`:

```m2
i1 : vector {}
     error: expected source to be free with rank 1        -- Core/modules.m2:70
```

and the two forms that would let you name the ring or the module fail the same way:

```m2
i2 : vector(R, {})
i3 : vector(R^0, {})
```

So a user with an empty list has no way to obtain the zero vector of a module, and the diagnosis now
talks about the rank of a source rather than about the empty input.

### Notes for whoever picks this up

Two separable things: whether `vector {}` should work at all (it needs a ring, so probably not — compare
**#3328**, where defaulting a missing ring to `ZZ` was declined as a source of hard-to-find bugs), and
whether `vector(R, {})` and `vector(R^0, {})` should work (they name a ring or a module, so there is no
ambiguity to resolve). The second pair looks like the real gap.
