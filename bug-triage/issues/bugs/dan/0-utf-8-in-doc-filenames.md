Still generated. `toFilename` maps the ASCII specials through its `tt` table but passes multi-byte
UTF-8 through unchanged, so the installed documentation holds **9 such names**, including the exact one
this file names:

```
share/doc/Macaulay2/Macaulay2Doc/html/___Gröbner_spbases.html
```

### Two closed issues are the same cause, not the same ask

Worth spelling out, because a search turns them up and they look like duplicates:

- **#47** covered this and closed in 2014 on the broken *links*, not on the filename question.
- **#211** is a second symptom of the same cause — the file appearing under two different UTF-8
  normalizations (`303 266` vs `314 210`) — closed on *"just remember how I do it and check that one
  file"*.

So neither settled the ask, which is why the file was still worth filing.

### Why it matters beyond tidiness

A non-ASCII filename is a portability and normalization hazard rather than an aesthetic one: it is what
produced #211's two spellings of the same page, and it constrains any filesystem or archive format in
the distribution chain that is not UTF-8 clean.
