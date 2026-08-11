`lift` accepts matrices; `liftable` does not. So there is no way to ask whether a matrix can be lifted
except by attempting the lift and catching the failure.

```m2
i1 : R = ZZ[x];

i2 : f = vars R

o2 = | x |

             1      1
o2 : Matrix R  <-- R

i3 : lift(f-f, ZZ)

o3 = 0

              1       1
o3 : Matrix ZZ  <-- ZZ

i4 : liftable(f-f, ZZ)
stdio:4:8:(3):[1]: error: no method found for applying liftable to:
     argument 1 :  0 (of class Matrix)
     argument 2 :  ZZ
```

`lift` has nine methods taking a matrix — `(lift,Matrix,S,S)`, `(lift,Matrix,R,R)`,
`(lift,Matrix,R,ZZ)`, `(lift,Matrix,S,QQ)`, `(lift,Matrix,S,ZZ)` and the `MutableMatrix`
counterparts. `liftable` has fourteen methods and not one of them takes a `Matrix`; every combination
raises, including `liftable(matrix{{1/1}}, ZZ)` where the corresponding `lift` succeeds.

### A top-level implementation

The obvious definition behaves correctly on the cases that motivated the request:

```m2
liftable(Matrix, Number) := (f, k) -> try (lift(f, k); true) else false
```

giving `false` for `vars R` and `true` for `vars R - vars R` over `ZZ[x]`. The element-wise
alternative agrees:

```m2
all(flatten entries f, e -> liftable(e, k))
```

The bug file asks for something better than either — engine-level predicates, its suggested names
being `rawRingElementLiftable` and `rawMatrixLiftable`, which would answer without constructing the
lifted object. Neither exists today.

### One caveat worth knowing before implementing

A try-lift implementation would inherit [#2509](https://github.com/Macaulay2/M2/issues/2509): lifting
a real matrix to `ZZ` is broken — `lift(matrix {{1.}}, ZZ)` errors although `lift(1., ZZ)` works — so
`liftable(matrix {{1.}}, ZZ)` would answer `false` for a matrix that is plainly liftable. An
engine-side predicate would need to avoid inheriting that, or #2509 would need fixing first.

### Where this came from

Cataloguing the `bugs/` directory removed in d2c8d27826 (#36). `bugs/mike/0-liftable-Matrix` is Dan
writing to Mike in November 2009:

> Mike, we need a function "IM2_RingElement_liftable", analogous to IM2_RingElement_lift, as well as
> IM2_Matrix_liftable, analogous to IM2_Matrix_lift. Better names would be rawRingElementLiftable and
> rawMatrixLiftable.

prompted by René Birkner asking for `liftable(Matrix,ZZ)` "to check if a matrix is in fact a matrix
over ZZ", and working around its absence with the element-wise definition above.

Related but distinct: [#2103](https://github.com/Macaulay2/M2/issues/2103) asks the same question for
*modules* and notes the `lift`/`liftable` documentation is thin.
