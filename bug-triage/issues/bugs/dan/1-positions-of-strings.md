
The note above proposes a `LocatedString` type. This issue files the need it names rather than that
remedy, because M2 has since grown a cheaper mechanism for exactly this and one of the note's two
motivations can be demonstrated as a plain defect.

### A documentation node's recorded location points at the next construct

Same file, two adjacent blocks:

```m2
 1  newPackage("ZzLoc2", DebuggingMode => true)
 2  export {"zzThree"}
 3  zzThree = () -> 3
 4  beginDocumentation()
 5  doc ///
 6    Key
 7      zzThree
 8    Headline
 9      the third one
10  ///
11  TEST ///
12    assert(zzThree() == 3)
13  ///
```

```
locate makeDocumentTag zzThree        -->  ZzLoc2.m2:11:0        <- line 11 is the TEST line
(ZzLoc2#"test inputs")#0#"location"   -->  ZzLoc2.m2:11:5-13:3   <- exact span
```

The `doc` node occupies lines 5–10 and reports line 11 — not off by one, but pointing at whatever
happens to follow the block. The `TEST` immediately below it reports its true start and end.
A `document { ... }` node fares better but is still wrong: one on line 8 reports line 9.

### Why `TEST` gets it right

Because it is a keyword. `binding.d:315`:

```
export TestS := special("TEST", unaryop, precSpace, wide);
```

and `testing.m2:23-24` states the mechanism:

> `-- TEST is a keyword that takes an object as input and determines its location.`
> `-- It then passes the object and its location to addTest.`

`addTest(String, FilePosition)` then stores `"location" => loc`. That arrived in
[#3419](https://github.com/Macaulay2/M2/pull/3419) (2024-08-31).

The general fact underneath: `Code` in the interpreter knows its own location, while the `Expr` that
reaches a top-level function like `doc` does not. A keyword sees the former.

### The suggestion

Make `doc` a keyword, so a documentation node gets a `FilePosition` spanning its block the way a test
does. [#3599](https://github.com/Macaulay2/M2/pull/3599) ("User-defined keywords") is the
infrastructure that would make this cheap rather than bespoke — it moves keyword creation to the M2
level and gives keywords a default operation that looks up the corresponding method.

### The bookkeeping this would replace

`SimpleDoc.m2` carries 55 mentions of `linenum`/`lineNumber`, tracking line numbers by hand:
`makeTextline(line, linenum)` pairs each line with its number, and `keylinenum` is threaded through
roughly twenty section handlers. The 2009 note predicted that a located string would make this
unnecessary; a located `doc` block would do the same for the node's own position.

### What this reframing does not cover

The note's other motivation is separate and remains: `value` on a string reports a position *within*
the string against a synthetic filename, with no provenance.

```
i1 : value "1 + \n  * 2"
     currentString:2:2:(3): error: no method for prefix operator * applied to object:
```

Line and column, but nothing saying where the string came from. That part would still want something
like the original proposal, or an optional position argument to `value`.

### Related

[#3420](https://github.com/Macaulay2/M2/issues/3420) wants a `Test` keyword *inside* `document`/`doc`
for coverage tracking — same family, different purpose.
[#4409](https://github.com/Macaulay2/M2/issues/4409) is about templates for documentation nodes.
Neither proposes making `doc` itself a keyword.
