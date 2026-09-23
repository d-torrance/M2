### What the file asks for

When Macaulay2 reports an error it walks back up the call chain, and two interpreter variables decide
how much of that chain you are shown. `loadDepth` records how deeply nested the file being read is —
bootstrap code is shallow, your own input is deep. `errorDepth` is the threshold: a position is
reported, and the debugger entered, only if the code was parsed at a load depth at least as large as
`errorDepth` is now. Its documented default is 3, which shows the user's own code and nothing under
it. At startup, though, the two disagree, and still do:

```
$ M2 -q -e 'print (loadDepth,errorDepth); exit 0'
(3, 0)
```

With `errorDepth` at 0, an error raised before it catches up is reported with frames from M2's own
startup and library code — noise for a user whose mistake is at the top level. Dan is asking that the
two be brought into line sooner.

### Notes for whoever picks this up

The reason it is not simply set at startup is presumably that the internal frames *are* wanted while
Core itself is loading, when a failure there is a bug in M2 rather than in user input. So the change
is about *when* the two become equal, not about the final value: some point after Core finishes and
before the user's first expression.
