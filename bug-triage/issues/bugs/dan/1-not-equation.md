
The `UnaryOperation` expression type does not exist. `git grep UnaryOperation` over the whole tree
returns two lines, both inside comments: `expressions.m2:471` and a disabled `toOpenMath` method at
`OpenMath/expr.m2:119`. `BinaryOperation` exists and is used; its unary counterpart was named but
never written.

The visible consequences are in `not`:

```m2
i1 : not (expression 1 == expression 2)
o1 = 1 != 2                                              -- fine

i2 : not (expression 1 == expression 2 == expression 3)
     error: negation of an equation with 3 parts

i3 : not expression 1
     error: no method for unary operator not applied to Holder
```

By contrast the other two logical operators are complete:

```m2
i4 : (expression 1) and (expression 2)
o4 = 1 and 2
```

### The intended implementation is in the source, commented out

`expressions.m2:471`, unchanged since the day it was written:

```m2
not Equation := e -> if #e == 2 then BinaryOperation { symbol !=, e#0, e#1 }
    else -* UnaryOperation{symbol not, e} *- error ("negation of an equation with ", toString (#e), " parts")
```

and the line that would have handled `i3`, immediately below it in the commit that added both:

```m2
-- not Expression := e -> BinaryOperation{symbol not, e}
```

(that commented body is itself wrong — a `BinaryOperation` carries `{op, left, right}`, so a
one-operand form does not fit it, which is presumably why a `UnaryOperation` was wanted.)

### The one-line request is a note to finish a placeholder

[`57f11276fe`](https://github.com/Macaulay2/M2/commit/57f11276fe) — *"start adding methods for
`not`, `and`, and `or` of expressions"*, Dan Grayson, 2009-07-18 — added all three methods in one
diff, finished `and` and `or`, and left `not` with both of its remaining cases commented out. He
created the bug file two days later, on 2009-07-20. It is a note to finish his own placeholder, and
the placeholder is still there seventeen years on.

### What would close it

A `UnaryOperation` expression type on the `BinaryOperation` template, and switching on the two
commented-out lines, so that `not` of an expression yields an expression rather than an error or a
missing method.

### Related

[#1754](https://github.com/Macaulay2/M2/issues/1754) is the same corner of `expressions.m2` from the
comparison side — `hold 4 > 5` — but its obstacle is different: it needs method lookup moved into
the `d` directory, because `x > y` always dispatches through `x ? y`. Neither fix gives the other.
