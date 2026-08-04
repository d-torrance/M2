The same defect shows up through `value`, and it was reported once before. From
the pre-GitHub `bugs/` tree, filed by Dan Grayson and in the repository since at
least 2010:

> This should cause an error message, because it can lead to confusing bugs:
>
> ```
> i25 : value "symbol"
>
> o25 = {*end of file*}
>
> o25 : Keyword
> ```

Still current on 1.26.06-8-g34d5846039, and not specific to `symbol` -- `value`
hands back the end-of-file token for the other quote keywords too:

```m2
i1 : value "symbol"

o1 = -*end of file*-

o1 : Keyword

i3 : value "local"

o3 = -*end of file*-

o3 : Keyword

i5 : value "symbol" === value "local"

o5 = false
```

So a caller gets a `Keyword` that prints as `-*end of file*-` rather than an
error, and the ones from different quote keywords print identically and share a
class while comparing unequal -- consistent with the CST here, where the token
is coupled with a different specifier each time (`Quote`, `LocalQuote`,
`GlobalQuote`). That is the "confusing bugs" part: the value propagates instead
of stopping at the point where the input was malformed.

Recording it here rather than opening a second issue, since it looks like one
fix settles both entry points.
