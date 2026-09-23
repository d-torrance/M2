### What the file asks for

When an error stops Macaulay2 the debugger opens at the point of failure, and the thing you most want
there is the chain of calls that led to it. This one-line file asks for a way to produce that chain on
demand, from inside the debugger.

### Why `backtrace` is not it

M2 does have a `backtrace` variable, but it is a switch consulted *after* an error rather than a
command you can issue at the debugger prompt. So the call chain is either already printed, if
`backtrace` happened to be true when the error occurred, or gone for good: setting `backtrace = true`
once the debugger has you is too late, because the stack that would have been reported is the one you
are now standing in, and nothing re-walks it.

### Notes for whoever picks this up

The debugger already has access to the frames it would need — `listFrame` and `frame` in
`d/actors5.d` walk them for other purposes — so this is plausibly a matter of exposing a function
that formats the current chain rather than of collecting new information.

Adjacent: #4144 asks for related on-demand information at the `Exit (y/n/a/b)` interrupt prompt,
which is a different entry point to the same wish.
