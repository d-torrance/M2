Two engine-level suggestions for speeding up `Schubert2`, recorded when the package was being written
and still unimplemented. Filed so they are findable; neither is accompanied by a benchmark, here or in
the original, so the case for either rests on the reasoning rather than on measurement.

### 1. A dot product of two polynomials as an engine operation

The original wording:

> In case we implement intersection rings via polynomial rings of which the intersection ring is a
> quotient, it will be important, perhaps, to be able to implement the integral of a cycle class by
> means of a table of the integrals for each monomial. Such a table is conveniently represented by a
> polynomial whose coefficients are the integrals, and then to integrate an arbitrary polynomial is to
> take the dot product of the coefficient vectors. Hence, it would be useful to have the dot product of
> two polynomials as an engine operation.

There is no `rawDotProduct` or equivalent today. Note this is a different thing from the `·` operator
discussed in [#3434](https://github.com/Macaulay2/M2/issues/3434) and added by
[#3584](https://github.com/Macaulay2/M2/pull/3584), which is notation for existing top-level products;
what is wanted here is a single engine call pairing the coefficient vectors of two polynomials in the
same monomial basis.

### 2. `logg` and `expp` in the engine

> It would be much faster to implement logg and expp in the engine.

Both are still top-level methods:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/Schubert2.m2#L1447

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/packages/Schubert2.m2#L1460

They convert between Chern classes and Chern characters, so they are on the path of most Schubert2
computations, which is presumably why they were singled out.

### Caveat

Whether either would repay the work is unestablished. Nothing in the file, and nothing I have run,
shows that `logg`, `expp` or coefficient-vector pairing is a bottleneck in a real Schubert2
computation; a profile of one would be the natural first step for anyone picking this up.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/1-Schubert-speed-ups` is
thirteen lines and contains only these two items.
