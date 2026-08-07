Not automated. The ingredient the file relies on does exist — `startServer(String,String)` accepts an
interface to bind, so `startServer "127.0.0.1"` is available and gives the "only reachable from this
machine" property the file wants. What was never built is the automation around it:

- `SCSCP.m2` contains no `TEST` blocks,
- no workflow or makefile target runs an SCSCP check,
- there is no `insecure-check` target, which the file suggests as the name.

### What the file is asking for

That the SCSCP checks listed in `0-final-checks-before-distribution` — currently a manual
pre-release ritual — become something CI can run, using a loopback-bound server so that automating
them does not expose a network service.

### Notes for whoever picks this up

The security argument in the file is the load-bearing part: a test that starts a server must bind to
`lo` only, or CI machines end up briefly serving SCSCP to their network. If the checks are added,
the separate `insecure-check` target is what would opt into the non-loopback variant deliberately.
