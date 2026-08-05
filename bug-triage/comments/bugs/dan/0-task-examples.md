On "I believe we used to have something like this" — there were examples, and
there is a record of what happened to them.

`bugs/dan/0-task-examples`, from the pre-GitHub `bugs/` tree, is Dan Grayson
removing them, with the diff. The `(addCancelTask,Task,Task)` node lost this:

```m2
allowableThreads = 3
u = schedule ( () -> while true do null )
sleep 1
u
t = createTask ( () -> null )
addCancelTask(t,u)
u
schedule t
while not isReady t do sleep 1
u
taskResult t
```

His note on why:

> I removed failing task examples from the documentation. They failed, for
> example, under ubuntu 12.04. Why?
>
> Part of it is probably the garbling of printed output, since separating
> examples into lines can get confused.
>
> Sometimes tasks run forever, using cpu time. But they're too simple!

The guess looks right. #2021 is the same failure ten years later on an armhf
build, diagnosed there as a timing race in example capture and settled by
lengthening a `sleep`.

The example itself works on 1.26.06-8-g34d5846039 — `u` reaches
`<<task, canceled>>`, `t` becomes ready, `taskResult t` is `null` — but it still
emits an interleaved backtrace from the cancelled thread:

```
error: interrupted
--back trace--
```

which lands in the captured output at a point that depends on scheduling. That
is the garbling, and it is presumably why these have stayed out.

So the node at `M2/Macaulay2/packages/Macaulay2Doc/ov_threads.m2:158` still has
no `Example` block, and restoring one means either suppressing that backtrace or
accepting nondeterministic example output.
