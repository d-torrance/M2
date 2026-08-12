An interrupt that arrives while the engine is inverting a ring element is reported as
`either element not invertible, or no method available to compute its inverse` — a statement about
the element, not about the interrupt. The example in the file above still produces it verbatim.

```m2
i1 : loadPackage "Schubert2";

i2 : pt = base n;

i3 : (e,n,d,m) = (3,1,6,4);

i4 : V = OO_pt^m+2;

i5 : F = flagBundle({m-n,n+2},V);

i6 : (S,Q) = F.Bundles;

i7 : B = symmetricPower_e Q;

i8 : P = projectiveBundle dual B;

i9 : R = intersectionRing P;

i10 : f = ch OO_P(1);

i11 : (alarm 3; f^-1)
stdio:11:12:(1):[0]: error: either element not invertible, or no method available to compute its inverse
```

Three runs of three. The wording has changed since 2006 — the file records
`negative power of noninvertible element requested` — but the content has not.

### The alarm is what ends it, not a genuine non-unit

This example needs a control, because `f` could simply fail to be invertible, in which case the
message would be correct and there would be nothing to report. It is not that: with the `alarm 3`
removed, `f^-1` was still running after **2 minutes**, so at 3 seconds the computation is nowhere
near finishing and the alarm is what stops it.

### Where the message comes from

`Ring::power`, handling a negative exponent, calls `invert` and then uses `is_zero` on the result as
its signal that `invert` failed:

https://github.com/Macaulay2/M2/blob/development/M2/Macaulay2/e/rings/ring.cpp#L127-L139

Any path that abandons `invert` part-way returns zero, and every such path is then reported as a
domain error about the element. An interrupt is one of them, and the `ERROR` call here displaces
whatever the real reason was.

### The interrupt is still recorded at that point

Wrapping the same expression in `try` reports the alarm instead:

```m2
i11 : r = try (alarm 3; f^-1) else "caught";
stdio:11:29:(3): error: alarm occurred
```

So the interpreter does still know an alarm fired; what differs is which of the two competing
reports wins. That `try` does not catch it is a **separate** known bug,
[#3371](https://github.com/Macaulay2/M2/issues/3371), and not what this issue is about — but it is
useful evidence that the information the message throws away is still available where it is thrown
away.

### Not these

Searched the tracker by title and body for `interrupt`, `alarm`, `invertible`, `inverse`, `unit`,
`negative power`, and by comment text (which the title search cannot reach) for `Ring::power`,
`is_zero`, `invert`, `SIGALRM`, `interrupt_exception`, `exceptionFlag`, `system_interrupted`,
`alarm occurred` and `Alarm clock`. Pull requests were included, not filtered out. The near misses,
so they are not re-derived:

* [#1392](https://github.com/Macaulay2/M2/issues/1392) and its spin-off
  [#1858](https://github.com/Macaulay2/M2/issues/1858) — an alarm *dropped entirely*, leaving a
  corrupted prompt. Here the alarm is delivered and then mis-described.
* [#3371](https://github.com/Macaulay2/M2/issues/3371) — `try` failing to catch an alarm, as above.
* [#3973](https://github.com/Macaulay2/M2/issues/3973) — the nearest neighbour: `r^-1` in a
  `toField` tower aborts with `ring element gcd computation failed`. Same negative-exponent path,
  but it dies *inside* `invert` rather than misreading what `invert` returned.
* [#4144](https://github.com/Macaulay2/M2/issues/4144),
  [#4567](https://github.com/Macaulay2/M2/issues/4567), and
  [#168](https://github.com/Macaulay2/M2/issues/168) — unrelated interrupt handling.

### Scope

The file opens with a broader claim than this issue makes:

> In many places in the engine a routine is called, it returns an illegitimate value, and the caller
> interprets it as a particular type of error and returns an error message of its own, even if the
> reason for returning was due to an interrupt. All the code should be audited.

That audit is not filed here, because an issue asking for it would have no condition under which it
could be closed. This issue is the one call site the file demonstrates. Whether the pattern is
widespread is a fair question and the sibling row
[#4616](https://github.com/Macaulay2/M2/issues/4616) plus the zero-ring case reaching the same
function from other directions suggests it is, but each needs its own reproducer.
