A `TABLE` does not divide the available width among its cells, so the rendered net is as wide as the
sum of the cells regardless of `printWidth`.

```m2
i1 : needsPackage "Text";

i2 : printWidth = 80;

i3 : u = concatenate(16:"asdf ");        -- 80 characters

i4 : n = net TABLE { TR { TD u, TD u }};

i5 : width n

o5 = 150

i6 : apply(unstack n, length)

o6 = {150, 80}
```

The `info` form is the same:

```m2
i7 : width info TABLE { "class" => "examples", TR { TD u, TD u }}

o7 = 151
```

Every line of that one is 151 characters wide, at `printWidth` 80.

### Why it matters more for `info` than for the terminal

Interactive output can be scrolled. The `info` form goes into the generated `.info` files, where the
reader has no horizontal scroll — and tables are used in documentation, `TABLE { "class" =>
"examples", ... }` being the standard wrapper for example blocks.

### Not the same as #1752

[#1752](https://github.com/Macaulay2/M2/issues/1752) is the other `net TABLE` report — one-line
tables rendering incorrectly, and `TR` options being printed as content — opened December 2020 and
closed February 2021. It never mentions width or `printWidth`, and the workaround discussed there,
`netList(..., Boxes => false, HorizontalSpace => 2)`, is about spacing rather than about fitting.
Recording that here so the two are not conflated.
