The documentation complaint in the file has a live cause, and it is not a documentation bug: the
options really are accepted and really are ignored.

```m2
i1 : sort({3,1,2}, MonomialOrder => Descending)
o1 = {1, 2, 3}
```

`sort List := opts -> internalsort` (`Core/lists.m2:216`) binds `opts` and then drops them, so
`DegreeOrder` and `MonomialOrder` are silently discarded for lists. The synopsis the file quotes is
therefore accurate about what the method *accepts* — which is the problem.

### Provenance of this comment

Filed separately as #4529 before this issue was found (the issue-search cache was capped below it), so
#4529 was closed as a duplicate and the documentation angle moved here.
