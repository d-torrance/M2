Unchanged. At startup `(loadDepth, errorDepth)` is still `(3, 0)`:

```
$ M2 -q -e 'print (loadDepth,errorDepth); exit 0'
(3, 0)
```

### What the file is after

`errorDepth` controls how much of the internal call chain an error message reports. While it lags
behind `loadDepth`, errors raised early carry frames from M2's own startup and library code, which is
noise for a user whose mistake is at the top level. Raising it sooner — to match `loadDepth` — would
trim messages to the part the user can act on.

### Notes for whoever picks this up

The reason it is not simply set at startup is presumably that the internal frames *are* wanted while
Core itself is loading, when a failure there is a bug in M2 rather than in user input. So the change
is about *when* the two become equal, not about the final value: some point after Core finishes and
before the user's first expression.
