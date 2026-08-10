`pretty` has been declared experimental since 2006, its only method is `undocumented`, and no
package or Core function calls it. It is worth deciding whether to document it or drop it.

Its whole implementation is three lines (`m2/pretty.m2:6-8`):

```m2
pretty = method(Dispatch => Thing)
pretty2 = method(Dispatch => Thing)
pretty Thing := x -> stack pretty2 x
```

and its whole documentation is in `Macaulay2Doc/experimental.m2`:

```m2
undocumented (pretty, Thing)
document { Key => pretty,
     Headline => "a pretty printer", "This function is experimental and under development." }
```

So the name is exported and appears in `about`, but the one method that does anything carries no
page, and the node a reader lands on tells them only that it is unfinished.

### Why now

The bug file this comes from is a 2006 plan to invest in it — two new options, one for which types
to dig into and one for machine-readable output, prompted by Dan writing *"Its output is really
**very** pretty, and it obeys the value of printWidth strictly. That's where I'd like to spend my
time."* Twenty years later the options do not exist, the function is unchanged, and the plan is the
only record that anyone intended to develop it.

Either outcome closes this:

- **Document it.** `(pretty, Thing)` gets a page, the "experimental" sentence goes, and the
  function joins the printing tools proper.
- **Remove it.** If nothing uses it and nobody intends to, exporting a name whose documentation
  says it is unfinished costs more than it returns.

I have no view on which; the point is that "experimental and under development" has not been true
in either direction for a long time.
