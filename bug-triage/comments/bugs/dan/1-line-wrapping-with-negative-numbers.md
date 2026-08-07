The same gap in the same function reaches negative numbers, and it has a reproduction from 2010.

A file in the pre-GitHub `bugs/` tree (`bugs/dan/1-line-wrapping-with-negative-numbers`, being
triaged in #36) reports that display wrapping separates a minus sign from its digits. It still
does, byte for byte:

```m2
i1 : printWidth = 79

i2 : toExternalString apply(40, i-> -2^i)

o2 = {-1,-2,-4,-8,-16,-32,-64,-128,-256,-512,-1024,-2048,-4096,-8192,-16384,-
     32768,-65536,-131072,-262144,-524288,-1048576,-2097152,-4194304,-8388608,-
     16777216,-33554432,-67108864,-134217728,-268435456,-536870912,-1073741824
     ,-2147483648,-4294967296,-8589934592,-17179869184,-34359738368,-
     68719476736,-137438953472,-274877906944,-549755813888}
```

Three lines end in a bare hyphen. The cause is the same one behind the path in this issue —
`splitcolumn` in `d/nets.d:207-221`, which decides where a break is allowed:

```
a && b                                          -- not between two alphanumerics
|| (!a && x.0 != ' ') && (!b && y.0 != ' ')      -- not between two punctuation characters
|| (x.0 == '.' && isdigit(y.0))                 -- not between . and digit
|| (y.0 == '.' && isdigit(x.0))
```

Four rules, and both cases fall through them for the same reason: one side is punctuation and the
other is alphanumeric, which no rule covers. For a file path it is `/` followed by a letter, so
`.../share/Macaulay2/` breaks before `Complexes`; for a negative number it is `-` followed by a
digit.

Dan's own reply in that file names the remedy, in the same terms this issue's title asks for:

> What's happening here is that the string created by the code on line i2 is displayed on your
> screen and wrapped to fit by code that has only the most meager understanding of M2 syntax. It
> refuses to split the parts of a utf8-encoded unicode character, to split two alphanumeric
> characters, to split two punctuation characters, and to split a period and a digit. I suppose we
> could also instruct it not to split a hyphen followed by a digit.

So if the resolution here is "teach the wrapper more rules" rather than "do not wrap paths at all",
the hyphen case is one more clause in the same expression, and the 2010 report is a second
motivating example.

One case beyond either report is visible in the output above, in case the rule set is being revised
anyway: the third line ends `-1073741824` and the fourth begins with a comma, a break between a
digit and punctuation that the rules also permit.
