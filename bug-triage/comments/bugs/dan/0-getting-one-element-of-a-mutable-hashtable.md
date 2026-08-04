> I vaguely recall this being discussed before, but couldn't find the issue

This is very likely it. The discussion predates the move to GitHub, so it was never
an issue — it was a file in the old `bugs/` tree, and here it is in full:

> There is no way to get just one element of a mutable hash table whose keys are
> unknown, because `select(1,x,i->true)` requires x to be immutable. Fix by
> relaxing that requirement, or by doing something else. We probably thought that
> requirement was important for thread safety, but now we can use mutexes or
> something.

So the restriction was deliberate rather than an oversight, and the stated reason
is thread safety — along with Dan's own view that mutexes would make it
unnecessary. That may be worth weighing against the alternatives you list.

Still reproduces on 1.26.06: `select(1, x, i -> true)` on a `MutableHashTable`
errors out.
