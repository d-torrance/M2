### What the file proposes

`dumpdata` and `loaddata` used to let M2 dump an initialized image and start from it, so a session did
not have to redo its startup work every time. They stopped working everywhere and were removed in
`d560e8284a` (2019), and nothing replaced them: there is no fork server, no daemon mode and no
persistent-process mechanism of any kind, so every invocation of M2 pays full startup — precisely the
cost an image was there to amortize.

What the file sketches is a small protocol rather than a rewrite. One M2 process waits on a pair of
pipes, forks on a start message, and lets the child take further arguments, return output and exit,
while the parent stays warm for the next request.

### Why it still matters

Anything that runs M2 many times in short succession pays the startup cost every time: the test
suite, documentation example capture, editor integrations, and web front ends. `dumpdata` used to
address this by saving an initialized image; with it gone the cost is unamortized.

### Notes for whoever picks this up

The fork-based design in the file interacts with two things worth checking first: whether the
Boehm collector and the engine tolerate `fork` in a process that has already initialized them, and
what happens to open files and sockets in the child. There is prior discussion of sandboxing M2
processes generally that touches the same territory.
