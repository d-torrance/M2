`backtrace` exists but is not what the file asks for. It is a variable that toggles whether a
backtrace is printed *after* an error; there is still no way to ask for one on demand from inside the
debugger.

### The gap

When the debugger stops at an error, the information a user most wants — the chain of calls that led
there — is either already printed (if `backtrace` was true when the error occurred) or unavailable
(if it was not). Setting `backtrace = true` at the debugger prompt is too late: the stack that would
have been reported is the one being examined, and nothing re-walks it.

### Notes for whoever picks this up

The debugger already has access to the frames it would need — `listFrame` and `frame` in
`d/actors5.d` walk them for other purposes — so this is plausibly a matter of exposing a function
that formats the current chain rather than of collecting new information.

Adjacent: #4144 asks for related on-demand information at the `Exit (y/n/a/b)` interrupt prompt,
which is a different entry point to the same wish.
