The documentation side of this, from the pre-GitHub `bugs/` tree. Dan Grayson
filed it as `bugs/dan/0-sort-doc` in May 2010, four years before this issue,
under the heading "Those options don't apply to this method!" -- with a transcript
of `sort(List)` documentation listing `DegreeOrder` and `MonomialOrder` among its
optional inputs.

Still current on 1.26.06-8-g34d5846039:

```m2
i1 : sort({3,1,2}, MonomialOrder => Descending)

o1 = {1, 2, 3}
```

`help(sort, List)` continues to advertise both options for the list method. The
mechanism is one line -- `M2/Macaulay2/m2/lists.m2:216`:

```m2
sort List :=  opts -> internalsort
```

`opts` is bound and then discarded; `internalsort` takes only the list. So both
options are accepted and silently ignored, which is why `Descending` has no
effect here while it works on matrices.

That seems to line up with the reading in this thread that `Descending` is
about column vectors rather than lists, and that `rsort` is the way to reverse a
list. If that is right, then the remaining work is on the documentation and the
signature -- not advertising options for `(sort, List)` that cannot do anything,
or rejecting them rather than ignoring them.

Recording this here rather than opening a second issue.
