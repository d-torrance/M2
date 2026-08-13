`topCoefficients` segfaults on any matrix with two or more rows in which some component does not
involve the top variable. Two entries are enough:

```m2
i1 : R = ZZ[x,y,z];

i2 : topCoefficients matrix{{x},{y}}
-- SIGSEGV
-* stack trace, pid: 36802
 3# PolyRing::vec_coefficient_of_var(vecterm*, int, int) const at rings/poly.cpp:2366
 4# Matrix::top_coefficients(Matrix*&) const at matrices/matrix.cpp:1009
 5# rawTopCoefficients at interface/matrix.cpp:696
*-
```

Reproduced on 1.26.06. The coefficient ring is irrelevant — `ZZ`, `QQ` and `ZZ/101` all crash.

### What triggers it

| input | result |
| --- | --- |
| `topCoefficients matrix{{x},{y}}` | **SIGSEGV** |
| `topCoefficients matrix{{x},{x}}` | fine |
| `topCoefficients matrix{{x},{0}}` | fine |
| `topCoefficients matrix{{x,y}}` | fine |
| `topCoefficients matrix{{x,y},{y^2,x^2},{x^3,y^3}}` | **SIGSEGV** |

One row never crashes. `{{x},{x}}` survives because both components involve the top variable, and
`{{x},{0}}` survives because a zero entry is simply absent from the sparse vector. What is fatal is a
component that is present and whose coefficient of `x^e` is zero.

### Why

`Ring::make_vec` returns a null pointer when handed a zero coefficient:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/e/rings/ring-vecs.cpp#L20-L28

`PolyRing::vec_coefficient_of_var` appends its per-component results without allowing for that:

```c++
      vec_result->next = make_vec(t->comp, head.next);
      vec_result = vec_result->next;      // null when this component's coeff of x^e is 0
    }
```

so `vec_result` becomes null, and the next iteration of the outer loop dereferences it at
`vec_result->next = ...`. With one row the loop body runs once and there is no next iteration, which is
why one-row matrices are safe.

For `matrix{{x},{y}}`, `vec_top_coefficient` picks the smallest-index variable occurring anywhere in the
vector, `x`, with `e = 1`. The component holding `y` has no `x`, so its coefficient of `x^1` is zero,
`make_vec` returns null, and the following component dereferences it.

### This is the same defect that #4429 fixed next door

[#4429](https://github.com/Macaulay2/M2/issues/4429) fixed a SIGSEGV in `Matrix::coeffs` in June 2026,
and its commit [`d8548bd18a`](https://github.com/Macaulay2/M2/commit/d8548bd18a) is a null guard on
exactly this assumption in `coeffs_of_vec`:

```diff
-              v->next = result;
-              result = v;
+              if (v != nullptr)
+                {
+                  v->next = result;
+                  result = v;
+                }
```

`vec_coefficient_of_var` makes the same assumption and was not covered by that change, so this looks
like a missed instance rather than a new kind of problem. Whatever guard suits the style here — skipping
the append when `make_vec` yields null, rather than advancing `vec_result` onto it — should follow the
same shape.

### Not #162

[#162](https://github.com/Macaulay2/M2/issues/162), "topCoefficients crashes easily", was closed in May
2015 and reported `topCoefficients matrix {{1}}` — a constant with no variables at all. That is a
different path, guarded inside `vec_top_coefficient` by

```c++
  if (x == n_vars()) return v;
```

and it is genuinely fixed: `topCoefficients matrix {{1}}` now raises `expected polynomial ring` from
`factor.m2:218`, and `topCoefficients matrix{{1_R}}` inside a polynomial ring returns normally. The
zero-coefficient path above was never reached by that fix.

### Searches this rests on

Titles and bodies for `topCoefficients`, `top coefficient`, `coefficient`, `SIGSEGV` and `segfault`;
comments via `gh search issues` for `topCoefficients`, `vec_coefficient_of_var`, `make_vec` and
`rawTopCoefficients`. Pull requests included rather than filtered. The near misses, so they are not
re-derived: [#4429](https://github.com/Macaulay2/M2/issues/4429) as above, and
[#4452](https://github.com/Macaulay2/M2/issues/4452), a `coefficient` crash under
`equivariantHilbertSeries` in `InvariantRing`, closed and unrelated.
