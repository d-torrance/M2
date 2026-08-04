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

Still current on 1.26.06-8-g34d5846039, and it is not specific to `symbol` --
`value` hands back the end-of-file token for each of the quote keywords, exactly
as `parse` does:

```m2
i1 : class value "symbol", class value "local", class value "global"

o1 = (Keyword, Keyword, Keyword)

i2 : value "symbol" === value "local"

o2 = true
```

So a caller gets a `Keyword` that compares equal across all of them and prints as
`-*end of file*-`, rather than an error. That is the "confusing bugs" part: the
value propagates instead of stopping at the point where the input was malformed.

Recording it here rather than opening a second issue, since it looks like one
fix settles both entry points.
