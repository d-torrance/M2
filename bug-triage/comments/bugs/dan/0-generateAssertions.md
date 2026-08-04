A second input shape that breaks the same way, from the pre-GitHub `bugs/` tree.
Here it is a trailing semicolon rather than a multi-line function:

```m2
i1 : generateAssertions ///
     4;
     5
     ///

o1 =
     assert( (4;) === 4 );
     assert( (5) === 5 );
```

`assert( (4;) === 4 )` parses, but `(4;)` evaluates to `null`, so running the
generated assertion fails. Still reproduces on 1.26.06.

Same underlying cause as the multi-line case, as far as I can tell -- input is
processed a line at a time without regard for statement structure -- so a fix for
one is likely a fix for both. Recording it here rather than opening a second
issue.
