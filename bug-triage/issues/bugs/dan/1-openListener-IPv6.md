
IPv6 works. What has no spelling is an IPv6 **address literal**, which is what the one-line
request above is about.

Measured against a server bound to `[::1]:24602`, on 1.26.06-40-gd8e86d689d:

| | by IPv6 name | by IPv6 literal |
| --- | --- | --- |
| `openInOut "$host:port"` | opens the socket | **fails** |
| `SCSCP::newConnection(host, port)` | connects, reaches the handshake | **fails** |
| `getWWW "http://…/"` | `HTTP/1.0 200 OK` | **fails** |

The resolver is already family-agnostic: `openlistener` sets `hints.ai_family = PF_UNSPEC`
(`scclib.c:733`) and `opensocket` passes `NULL` hints, so `getaddrinfo` returns AAAA records and
connects over them. Nothing there needs changing.

### The break is one line above it, and it splits on the first colon

`stdio.d:227-231`:

```
foreach c at j in filename do if c == ':' then (
     host0 = substr(filename,1,j-1);
     if j+1 < length(filename) then serv = substr(filename,j+1);
     break;
     );
```

An IPv6 literal is mostly colons, so:

- `"$::1:24601"` — the first colon is at index 1, so `host0` is empty and `serv` is `":1:24601"`.
- `"$[::1]:24601"` — `host0` is `"["` and `serv` is `":1]:24601"`.

Neither reaches `getaddrinfo` intact.

### A bare literal is not rejected, it is reinterpreted

The empty `host0` in the first case is the worse half. `stdio.d:233` reads an empty host as a
request to *listen*:

```
if length(host0) == 0 || listener then ( so = openlistener(host0,serv); ...
```

so `openInOut "$::1:24601"` does not attempt a connection at all — it attempts a bind, with
`":1:24601"` as the service, and reports `can't open listener`. The diagnostic describes an
operation the caller did not ask for.

### Suggested fix

Accept the bracketed authority form from RFC 3986 — `[::1]:2500` — by splitting on the last colon
outside brackets rather than the first colon anywhere. That is the same convention URLs, `ssh -o`,
and `getaddrinfo`'s own callers use, and it leaves the existing `host:port` and bare-`host` forms
untouched since neither contains brackets.

### Two related places, for whoever picks this up

- **`SCSCP/client.m2` already splits correctly and then loses it.** `newConnection String` at
  `:77-82` matches `^(.*)\:(.*)$`, and because `.*` is greedy that splits on the *last* colon —
  right for IPv6. But `newConnection(String,String)` then rebuilds the string and calls
  `openInOut ("$"|hostport)` at `:23`, so `stdio.d` re-splits it on the first colon and undoes the
  work.
- **`splitWWW` errors** on `http://[::1]:24602/`, so `getWWW` cannot fetch a bracketed URL even if
  the socket layer is fixed. Arguably a separate issue about URL syntax rather than about IPv6.

### Why it is worth the ten lines, and why it is low priority

Low priority: nothing in the tree passes an IP literal, and a hostname is a one-word workaround
that works at every layer.

Worth doing anyway: the literal form is the idiom M2's own documentation teaches. Every socket
example in the tree is an IPv4 literal — `newConnection("127.0.0.1", 26135)` and
`startServer "127.0.0.1"` in the `SCSCP` documentation, the `SCSCP/docinput/*.out` transcripts,
and the bind-to-loopback advice in [#4527](https://github.com/Macaulay2/M2/issues/4527) — and none
of them has an IPv6 spelling.
