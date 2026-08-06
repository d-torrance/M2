This ask predates the issue by about eleven years, and the obstacle to the design in @DanGrayson's comment above turns out to be measurable.

A one-line file in the pre-GitHub `bugs/` tree (`bugs/dan/1-height-balanced-trees`, being triaged in #36) reads, in full:

> Implement height-balanced-trees in the Core.

No rationale, no API sketch, no consumer — which is why the comment here is the more useful statement of it:

> much more useful, from the point of view of applications, would be to implement red-black trees, which are a type of height balanced trees, where the keys can be any objects of Macaulay2 for which `?` provides a total ordering.

**The premise about `?` does not hold today**, in three separate ways. Measured just now:

```m2
  1 ? 2                  ->  <
  1 ? "a"                ->  NO METHOD
  symbol x ? 1           ->  NO METHOD
  set {1} ? set {2}      ->  incomparable
  y ? 1                  ->  >          -- y a ring variable
  {1} ? (1,)             ->  NO METHOD  -- List vs Sequence
  1 ? 1/2                ->  >
```

So `?` is **partial** — undefined on several ordinary pairs, including `List` against `Sequence` — and where it is defined it may return a third value, `incomparable`, rather than one of `<`, `=`, `>`. And #650 still reproduces, so it is not even stable where it does answer:

```m2
L = {set{1,3}, set{0}, set{2}};
sort L        ->  {set {2}, set {0}, set {1, 3}}
sort sort L   ->  {set {1, 3}, set {0}, set {2}}
```

That does not sink the idea, but it moves a design decision to the front of it: a balanced-tree type cannot take "any objects for which `?` provides a total ordering" as its key domain, because that set is not well defined. It would need either its own comparison function supplied at construction, or a documented restriction to key classes on which `?` is known to be total, or #650 fixed first.

For the record on the current state, since the file asks for it in the Core: there is no ordered container in Core at all. `Set`, `Tally`, `VirtualTally`, `MutableHashTable` and `MutableList` are hash- or array-backed, and ordering is obtained by calling `sort` on `keys` or `elements` afterwards. No distributed package implements a balanced tree of its own either — I checked, and every `balanced`/`avl` match under `packages/` is unrelated (balanced tropical fans, balanced ideals, minified JavaScript).
