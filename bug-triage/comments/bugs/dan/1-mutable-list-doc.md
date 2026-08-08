One concrete item for the `HashTable` vs `MutableHashTable` part of this guide,
from the pre-GitHub `bugs/` tree. `bugs/dan/1-mutable-list-doc` is a one-line
request by Dan Grayson — *"document the use of mutable hash tables as push-only
stacks"* — over this transcript:

```m2
i35 : x = new MutableHashTable ;
i36 : x##x= a;
i37 : x##x= b;
i38 : x##x= c;
i39 : values x

o39 = {a, b, c}
```

`x##x = v` parses as `x#(#x) = v`: the key is the number of entries already
there, so each assignment appends. It still works — `peek x` gives
`0 => a, 1 => b, 2 => c` — and it is not written down anywhere in `Macaulay2Doc`
or on the wiki.

It is worth a "use X when …" entry because the obvious alternative is
significantly worse. #659 is that mutable lists grow one element at a time, so
filling one by index is O(n²); the timings posted there are about 280× the work
for 10× the elements. The hash-table idiom is the O(1) answer, and #1608 asks for
it to become a type of its own — *"a new type of MutableHashTable with integer
keys and identity hash function … which may be called a MutableVector"*.

There is already an anchor to link it to: the `"hash tables"` node
(`ov_hashtables.m2:100-102`) states the property the idiom depends on —

> One important feature of hash tables is that when the keys are consecutive
> integers starting at 0, the keys are scanned in the natural order.

— which is why `values x` comes back in insertion order rather than hash order.
The `MutableHashTable` node's own example, `scan(0 .. 30, i -> x#i = i^2)`, fills
a range whose length is known in advance, so it does not show this case.
