
`openInOut` accepts `"-"`, a `$host:service` socket, a `!cmd` pipe and a listener. For anything
else — any ordinary filename, including a named pipe — it errors, and has since 1998:

```m2
i1 : openInOut "/tmp/p1"          -- a fifo, held open by another process
     error: can't open file /tmp/p1 for both input and output
```

`stdio.d:384-392` is the whole story: the three special prefixes are handled and the fall-through
branch is an unconditional `errmsg`.

```
export openInOut(filename:string):(file or errmsg) := (
     if readonlyfiles then return ...
     if filename === "-"                  then (file or errmsg)(stdIO)
     else if filename . 0 == '$'          then opensocket(filename,true,true,false)
     else if filename . 0 == '!'          then openpipe(filename,true,true)
     else (file or errmsg)(errmsg("can't open file "+filename+" for both input and output")));
```

That error text dates to `ceb927b193` (1998-10-17), so there has never been a version in which the
plain-filename case did anything.

### The documentation says otherwise, and names this exact case

The `openInOut` node lists four forms the argument may take, and the first is
(`ov_system.m2:496-501`):

> a string not starting with `!` or `$`: the string is taken as the name of an input output file to
> open. **For example, in Unix, it might be a named pipe.** A filename starting with `~/` will have
> the tilde replaced by the user's home directory.

So the documented behaviour and the implemented behaviour disagree, and the sentence goes out of its
way to offer a named pipe as the illustration.

### The dates say what the one-line report is

Dan Grayson added the documentation sentence above in
[`10dc2cc960`](https://github.com/Macaulay2/M2/commit/10dc2cc960) on 2008-12-30, and created the bug
file on 2009-01-12, a fortnight later. He documented the case, then noticed the code had never
supported it.

### Either half would settle it

- **Implement it.** A fifo can be opened `O_RDWR` on Linux, which is the usual way to hold one open
  without blocking on a peer, so the change is one more branch beside the three that exist.
- **Or retract the sentence.** If bidirectional plain files are not wanted, `ov_system.m2:496-501`
  should stop promising them.

The first is what the file asked for. Worth noting that
[#4526](https://github.com/Macaulay2/M2/issues/4526) — a persistent M2 compute process driven over a
pair of pipes, also from this cohort — is a consumer of the capability if it is built.
[#3666](https://github.com/Macaulay2/M2/issues/3666) is the other open `openInOut` issue and is
unrelated, about the 4096-byte buffer warning in the same node.
