There is no way to wait for the first of several tasks to finish. `taskResult` waits for one specific task,
and the only other introspection is `isReady`, so racing several strategies and taking whichever answers
first means busy-polling — burning a core doing nothing while you wait.

### What exists

```m2
i1 : t = schedule(() -> (2^100000; 42))

o1 = <<task, created>>

i2 : wait t
stdio:2:1:(3): error: expected an integer, or an input file or list of input files

i3 : wait {t}
stdio:3:1:(3): error: expected a list of input files or listeners, or a list of small non-negative integers

i4 : methods wait

o4 = {}
```

`wait` is defined on processes and on input files
([`d/actors4.d:815-843`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/d/actors4.d#L815-L843))
and has no `Task` case. `isReady` does:

```m2
i5 : methods isReady

o5 = {0 => (isReady, File)}
     {1 => (isReady, Task)}
```

So the available idiom for "give me the first answer" is

```m2
while not any(tasks, isReady) do nothing;
```

which spins.

### The machinery is already there, one level down

[`system/supervisorinterface.h:34`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/system/supervisorinterface.h#L34)
declares

```c
extern void* waitOnTask(struct ThreadTask* task);	// wait until done or canceled
```

and `ThreadSupervisor` already owns a condition variable for the other direction — workers blocking until
work arrives
([`system/supervisor.hpp:136-138`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/system/supervisor.hpp#L136-L138)):

```c++
  ///mutex for accessing lists
  pthreadMutex m_Mutex;
  ///new task ready to run
  pthread_cond_t m_TaskReadyToRunCondition;
```

I have read the header rather than the implementation, so I do not know how much work this is. `waitOnTask`
waits on a single task, and "wait until any of these is done" may need a second condition variable signalled
on task completion rather than only a binding for what is there. That is the first thing to check.

### Why it is worth having

This is what makes the common parallel pattern unavailable at top level: start several strategies for the
same computation, take the first answer, cancel the rest. `cancelTask` already handles the second half.
Without a wait-for-any, the first half is a spin loop that competes with the very computations it is
waiting on.

A closing condition would be some spelling of `wait` that accepts a list of tasks and returns when at least
one is ready — the same shape `wait` already has for a list of input files, which returns the ones that are
ready.

### Provenance

The removed file asks for this twice, in two separate places:

> to wait for a thread, or to wait for the first thread of a set of threads

and, thirty lines later,

> to wait for any thread in a set of threads to terminate (wait for `pthread_cond_t`?)

The parenthesis in the second is a guess at the mechanism, and it guesses right about what the supervisor
would end up using.

Neither [#3957](https://github.com/Macaulay2/M2/issues/3957), which asks for a task manager, nor
[#143](https://github.com/Macaulay2/M2/issues/143), on parallel performance, covers this.
