Three open issues report this same defect, one per concatenation operator, and
as far as I can tell they have never been cross-referenced:

- #607, for `|` and `||`
- #736, for `|` again
- #1060, for `++`

All three still reproduce on 1.26.06-8-g34d5846039, and one contrast localizes
the fault. Multiplication *does* carry the degree; only the concatenations drop
it:

```m2
i1 : Q = QQ[t_1,t_2];

i2 : f = map(Q^1, Q^1, {{t_1}}, Degree => {1});

i3 : degree f, isHomogeneous f

o3 = ({1}, true)

i4 : degree (f*f)

o4 = {2}

i5 : degree (f||f), degree (f|f), degree (f++f)

o5 = ({0}, {0}, {0})

i6 : isHomogeneous (f||f), isHomogeneous (f|f), isHomogeneous (f++f)

o6 = (false, false, false)
```

This was also reported in the pre-GitHub `bugs/` tree, as
`bugs/dan/0-degrees-of-maps`, where Dan Grayson put it the same way:

> For other operations, such as matrix multiplication, if the degrees all differ
> by the same number (or vector), then it will be incorporated as the degree of
> the result. It is a bug that the operation `||` doesn't incorporate the degree
> into the result if both operands have the same degree [...] same for `|` and
> `++`.

Worth noting that the transcript in that file no longer demonstrates anything —
`degree (vars Q * id_(Q^2))` is `{0}` today where it was `{1}` — so the four
lines above are a fresh reproducer rather than his.

Recording it here rather than opening a fourth issue.
