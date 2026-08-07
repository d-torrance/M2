Still missing. `isSurjective` has two methods and neither takes a ring map:

```m2
i1 : methods isSurjective
o1 = {(isSurjective, Matrix)  }
     {(isSurjective, SheafMap)}

i2 : R = QQ[x,y]; f = map(R, R, {x, y});
i3 : isSurjective f
stdio:3:1:(3): error: no method for adjacent objects ...
```

So the request is unchanged since the file was written.

### Notes for whoever picks this up

The companion `isInjective` case is worth checking at the same time, since the two are usually
installed together, and `kernel RingMap` already exists (with a hook-based strategy table at
`Core/ringmap.m2:240`) while there is no corresponding image computation to test surjectivity
against. That is presumably why this one never got added: the obvious implementation compares the
image subring with the target, which is `kernel`'s dual and not free.
