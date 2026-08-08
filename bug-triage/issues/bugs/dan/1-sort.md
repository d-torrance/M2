
Two of the three requests above are unmet, and the measurements are worse than the note suggests —
the duplicate case it names is not the slowest one.

### `sort` is quadratic on ordered input

All timings on 1.26.06-40-gd8e86d689d, over lists of 50000 machine integers, with the lists built
before the clock starts (`toList(0..49999)` itself takes 0.008 s):

| input | time |
| --- | ---: |
| 50000 random distinct | **0.061 s** |
| 50000 random from `0..99` | 0.447 s |
| 50000 all equal | 19.96 s |
| 50000 pre-sorted distinct | **45.73 s** |

The first and last rows are *the same 50000 values*. Sorting them shuffled takes 61 milliseconds;
sorting them already in order takes three quarters of a minute.

Quadratic, by scaling rather than by a single point — pre-sorted input:

```
10000   1.79 s
20000   7.22 s     (4.0x for 2x the data)
40000  35.7 s      (4.9x for 2x the data)
```

Duplication scales the same way: 10000 equal elements 0.84 s, 20000 equal 3.17 s, 50000 equal 19.96 s.

### Where it lives, and what the obvious diagnosis gets wrong

`internalsort` is `sortfun` → `basicsort2` (`actors3.d:533`, installed at `:535`), over the recursive
quicksort `subsort` at `actors3.d:477-509`.

The tempting explanation is a bad pivot, and it is wrong: the pivot is already randomized,

```
a := randomint() % b;
```

at `:479`. That is presumably why random input is fast, and it is why the pre-sorted collapse is
surprising — a random pivot is supposed to make input order irrelevant.

One thing worth a look, offered as an observation rather than a diagnosis: after both recursive calls,
each invocation does a linear shift over its left partition,

```
if l+1 < j then subsort(l+1,j);
if j+1 < r then subsort(j+1,r);
for k from l+1 to j do sortlist.(k-1) = sortlist.k;
sortlist.j = pivot;
```

I have not established that this is the cause, and it should not be assumed to be.

### The note's own remedies

The file suggests making `internalsort` non-recursive, and tallying elements first to detect
duplicates. The measurements bear on both: tallying would address the 19.96 s row and would not
address the 45.73 s one, which is distinct data. Whatever the fix, the pre-sorted case is the one to
test against.

### The third request is already met

*"make sure it's interruptable!"* — it is. `alarm 2` during a 45-second sort interrupts it.

### Why this is worth more than a benchmark

Sorting an already-sorted list is not a contrived input. It happens whenever a list is sorted twice,
whenever sorted output is re-sorted under a different comparison that agrees on most elements, and
whenever data arrives in order. At 50000 elements the penalty is 750x.
