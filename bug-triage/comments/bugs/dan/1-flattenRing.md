The `newRing(S2, MonomialOrder => GRevLex, Degrees => {1,1})` workaround turns out to be doing one specific thing, and it connects this to a design question that has been open since 2006.

What distinguishes the two rings is the **degree rank**, not the monomial order:

```m2
i2 : k = QQ[x]/ideal(x^3-2); L = k[y]/ideal(y^2-2); S2 = (flattenRing L)#0;

i5 : degreeLength k, degreeLength L, degreeLength S2
o5 = (1, 2, 2)

i6 : degrees S2
o6 = {{1, 0}, {0, 1}}
```

`flattenRing` gives back a ring of degree rank 2, because it preserves the tower's joined multidegrees rather than collapsing them. Confirmed that this is what the workaround changes, and that it is sufficient on its own:

```m2
-- on S2, degree rank 2
myFun(x)   -->  error: rawFreeModule: degree rank 0, but sequence of degrees given

-- on newRing(S2, MonomialOrder => GRevLex, Degrees => {1,1}), degree rank 1
degreeLength S4  -->  1
myFun4(x)        -->  matrix {{0}, {0}, {0}, {0}, {1}, {0}}
```

That `flattenRing` behaves this way is deliberate, and Dan wrote the open question down at the time. From `bugs/dan/1-flattenRing`, a November 2006 thread with Mike Stillman now being triaged in #36:

> Actually, what should flattenRing do, generally, about setting the degrees and monomial ordering of its result? Currently, it doesn't go out of its way to reduce the degree length to 1, i.e., if it is given a quotient ring of a polynomial ring, that's good enough.

Twenty years on, that is still an exact description of the behaviour — `degreeLength` is never reduced, and the monomial order is the product order assembled by `tensor` of monoids:

https://github.com/Macaulay2/M2/blob/68351e766d3e7991cd8bc0aa3ceecad1d49e65b8/M2/Macaulay2/m2/monoids.m2#L681-L682

The same file asked a second question that *was* settled, in the opposite direction: whether the flat ring should keep the tower's multidegrees. It does, deliberately, and `matrix2.m2:181` now depends on it —

```m2
	  (R',F) := flattenRing R; -- we flatten because otherwise we might get the degree map wrong, spoiling homogeneity
```

So the two halves pull against each other, which is probably why the question stayed open: preserving multidegrees is what keeps homogeneity intact through the flattening map, and it is also what hands `pushFwd` a ring of rank 2.

That makes the fork here a real choice rather than an oversight, and it seems worth naming explicitly:

- teach `pushFwd` to handle a coefficient ring of degree rank greater than 1, leaving `flattenRing` alone; or
- give `flattenRing` an option to reduce the degree length, which is what the workaround does by hand and what the 2006 note contemplated.

No recommendation from me on which — I only have the observation that the workaround's active ingredient is the degree rank, and that the behaviour it works around was a known open question rather than a bug introduced later.
