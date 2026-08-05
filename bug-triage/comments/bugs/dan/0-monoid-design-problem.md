Further to the note above about `monoids.m2` taking `DegreeRank` from the first
argument — there is a second file in the pre-GitHub `bugs/` tree that says why
that is hard to change rather than just wrong. `bugs/dan/0-monoid-design-problem`,
by Dan Grayson:

> if `N` is `monoid[...,Join=>false,DegreeMap=>f]`, then in the expression
>
>     N ** M
>
> `f` is expected to be a map from the degree monoid of `M` to the degree monoid
> of `N`. But in general, `M` is not known in advance, so `N ** M` can't work,
> in general!

So `DegreeMap` is declared on one monoid but is a function of the *other* one,
which is not available when the option is given. That constrains any fix here:
taking the map from whichever argument carries it is not enough on its own,
because a map supplied against an unknown partner may not typecheck against the
partner it eventually meets.

The same file records what was done about associativity, and that part does not
appear to hold today:

> We arranged that the result of
>
>     monoid { ... } ** monoid { Join => false, ... }
>
> has the form
>
>     monoid { Join => false, ... }

On 1.26.06-8-g34d5846039 it does not — `Join` comes from the first argument only
(`monoids.m2:684`), so the option is dropped when the `Join => false` monoid is
on the right:

```m2
M = monoid[y];
N = monoid[x, Join => false, DegreeMap => (d -> {2*d#0})];

(options tensor(M, N)).Join   -- null,  degreeLength 2
(options tensor(N, M)).Join   -- false, degreeLength 1
```

which is the same first-argument-wins behavior as the error reported in this
issue.
