Two updates on this, from a file in the pre-GitHub `bugs/` tree that asks for the same thing more
broadly.

### The erratic behaviour reported above is gone

On 1.26.06-40-gd8e86d689d, `QQ["x_1"]` fails the same way every time rather than failing once and
then working:

```m2
i1 : QQ["x_1"]
     error: expected strings, integers, or symbols

i2 : QQ["x_1"]
     error: expected strings, integers, or symbols
```

The message has changed too — `expected strings, integers, or symbols` rather than
`baseName: no base name available`. So the surprising part of this report, that a second identical
call succeeded, no longer applies; what remains is that the call does not work at all.

### The wider ask

`bugs/dan/1-variables` wants a family of these to work, not only the indexed case:

```m2
QQ["x"]
QQ["x,y"]
i=3
QQ["x,y_i"]
QQ["x_1 .. x_4"]
```

Of those, only the first does today:

```
QQ["x"]            ->  QQ[x]
QQ["x,y"]          ->  error: expected strings, integers, or symbols
QQ["x_1"]          ->  same
QQ["x_1 .. x_4"]   ->  same
```

so a bare name is accepted while a comma-separated list, an indexed name and a range are not. That
file also proposed the means it thought were needed first — `replace(String, ZZ, String, String)` and
`replace(String, ZZ, Function, String)`, to substitute into the *n*th parenthesized subexpression, so
that a string could be preprocessed into `symbol`-qualified pieces before being handed to the ring
constructor. Neither overload exists; `lookup(replace, String, ZZ, String, String)` is `null`.

Whether the general form is worth having is a separate question from the inconsistency this issue was
opened about — but since that inconsistency appears settled, the general form is arguably what is left
of it.
