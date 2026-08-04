On "I can't figure out what was the intention here" -- there is a statement of
intent from the pre-GitHub `bugs/` tree, filed by Dan Grayson as
`bugs/dan/0-tensor-rings`:

> the tensor product below should probably have degree length 2:
>
> ```
> i3 : R = QQ[x][y]
>
> i4 : degreeLength R
>
> o4 = 2
>
> i6 : degreeLength tensor(R,R,Join=>false)
>
> o6 = 1                                   <================== ?
> ```

Still current on 1.26.06-8-g34d5846039.

That looks like the same line this issue lands on. Under `Join => false`,
`M2/Macaulay2/m2/monoids.m2:700` sets

```m2
opts.DegreeRank = Mopts.DegreeRank;
```

taking the degree rank from the first argument's monoid alone. For `R = QQ[x][y]`
that drops the coefficient ring's degree and gives 1 where `R` itself has 2; and
because that same `DegreeRank` is the `n` checked at `monoids.m2:655`, a second
argument needing more components hits `expected degree map to return a list of
length at most 1` -- the error reported here. Notably `tensor(S2, S1, Join => false)`
succeeds in the report while `tensor(S1, S2, ...)` fails, which is what taking the
rank from the first argument would predict.

So the asymmetry and the wrong `degreeLength` may be one defect rather than two.
Recording this here rather than opening a second issue.
