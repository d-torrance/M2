
Of the three requests above, the third is done and the first two are one thing: M2 acquires a
reference to every Python object it wraps and never releases it.

### The third request is met

*"make an example of a dynamically loaded C library embedded in a python module"* — `Python/doc/numpy.m2`
documents `installNumPyMethods()` with worked examples converting M2 matrices and vectors into NumPy
arrays, and there is a matplotlib tutorial with a rendered plot. Both are C extension modules loaded
into M2's embedded CPython.

### Wrapping takes a reference and nothing gives it back

`python.d:28`, in `toExpr(r:pythonObjectOrNull)`:

```
x := pythonObjectCell(po, hash_t(0));
```

No `registerFinalizer`, no `Py_DECREF`. The only three `Py_DECREF` calls in the interface —
`python-c.c:163`, `:175`, `:219` — are on local temporaries inside conversion functions, never on a
wrapped object.

Measured two ways on 1.26.06-40-gd8e86d689d.

**Refcount of a single object, watched from the Python side:**

```m2
needsPackage "Python"
runSimpleString "keep = ['zz-watched']"
rc = () -> value pythonValue "__import__('sys').getrefcount(__import__('__main__').keep)"

rc()                                    --  2   (__main__.keep, +1 held by getrefcount itself)
zzobj = pythonValue "__import__('__main__').keep"
rc()                                    --  3   <- wrapping acquired a reference
zzobj = null
collectGarbage(); collectGarbage()
pythonValue "__import__('gc').collect()"
rc()                                    --  3   <- never released
```

**Retention of many objects, with no M2 reference kept:**

```m2
zzcount = () -> value pythonValue ///len([o for o in __import__('gc').get_objects()
     if isinstance(o, list) and o == ['zz-marker']])///
zzcount()                                        --  0
scan(20, i -> pythonValue "['zz-marker']")
collectGarbage()
zzcount()                                        --  20
pythonValue "__import__('gc').collect()"
zzcount()                                        --  20
```

A caveat on what this proves, so nobody has to re-derive it: a refcount that stays put is also
consistent with the Boehm collector simply not having reclaimed the cell, and that cannot be
distinguished by observation *because there is no finalizer to watch fire*. The conclusion rests on the
three together — the refcount never returning, twenty dropped objects all surviving both collectors,
and no `Py_DECREF` on a wrapped object anywhere in the interface.

The cost is bounded: one leaked CPython allocation per distinct object M2 ever wraps, within a session.
That is unimportant for a short script and less so for a long-running process such as the one behind
Macaulay2Web.

### Why this is not a one-liner

`registerFinalizer` exists, so the mechanism is available. The work is deciding, for each site that
reaches `toExpr(r:pythonObjectOrNull)`, whether the reference being wrapped is *owned* or *borrowed* —
CPython's C API returns both — because a `Py_DECREF` on a borrowed reference is a use-after-free, which
is considerably worse than the leak. That audit is presumably why the request has stood since 2009.

### On testing it

`Python.m2` has 14 `TEST` blocks; none touches reference counts, object lifetime, or garbage collection,
which is unsurprising since a leak is not visible from M2-level behaviour. Either measurement above is
short enough to become one, which would also settle the note's second request, *"check for memory leaks"*.
