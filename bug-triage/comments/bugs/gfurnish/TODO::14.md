Still live on 1.26.06-40-gd8e86d689d, and the symptom has moved twice since this was filed. Two things
worth adding.

### The negative-size allocation is gone; a SIGSEGV replaced it

A task spinning on `print` still kills the session, but on the way out rather than with the
`allocate -15 bytes` message:

```m2
f = () -> while true do print "hi there"
schedule f
sleep 5
```

ends in `-- SIGSEGV` in 3 runs of 3. The control says it is the printing and not merely having a live task
at exit: the same spinning task doing `2^100` instead of `print` exits cleanly in 3 runs of 3.

### `setIOSynchronized` documents a guarantee it does not deliver

This is the part I think is actionable. `ov_threads.m2` says of `setIOSynchronized` that threads are
permitted to use `stdio` and `stderr` **"to output complete lines only"**. They are not.

With `getIOThreadMode stdio` confirmed to return `1`, sixty concurrent `print` calls of the same
33-character string:

```m2
allowableThreads = 8
setIOSynchronized()
T = for i from 1 to 60 list schedule(() -> (2^500; print "sssssssssssssssssssssssssssssssss"));
scan(T, t -> taskResult t);
```

Line lengths in the output, three runs:

```
run 1   mode=1    39 lines of 33    9 lines of 66    1 line of 99
run 2   mode=1    48 lines of 33    6 lines of 66
run 3   mode=1    34 lines of 33    9 lines of 66    2 lines of 99
```

So six to eleven of every sixty messages land merged onto a line with another. Without
`setIOSynchronized` the numbers are much the same, which is the point — the mode is not doing what its
documentation says.

The cause is visible in the mode's implementation. `M2File_GetState`
([`system/m2file.cpp:143-147`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/system/m2file.cpp#L143-L147))
takes a lock and then hands back the **shared** `unsyncState`:

```c++
    //if sync mode, acquire lock
    else if(file->currentThreadMode==1)
      {
	file->waitThreadAcquire(1);
	return file->unsyncState;
      }
```

Only mode 2 keeps per-thread states. So in mode 1 every thread appends into one buffer, and the lock is
held across the state fetch rather than across a line. `print` writes its payload and its newline
separately, so two threads can interleave between the two. Relatedly, `simpleflush`
([`d/stdio.d:405`](https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/d/stdio.d#L405)) still
carries `-- TODO: lock mutex here?` immediately above its `startFileOutput`.

Either mode 1 needs to hold its lock at line granularity, or the documented consequence should be weakened
to match what it does. #1481 was "fix printing in threads, part 1" and there was never a part 2.

### Where this came from

`bugs/gfurnish/TODO` asks, among other things, "to make printing to stdio thread-safe", and explains why in
the same terms this issue does — `stdIO` and `stderr` are thread local but the Macaulay2 variable `stdio`
is not, so printing from top level can go wrong. Its transcript shows three threads printing a 33-byte
string and `stdio.d` reporting `array index -34 out of bounds 0 .. 4095`, which is the 2010 form of the
same failure.
