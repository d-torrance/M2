Context on what tightening this would have to preserve: the check in question was relaxed
deliberately, two years ago.

**#3177** (merged 2024-04-09), "frac of iterated ring", exists to let `frac` accept more rings, not
fewer:

> This is an attempt to relax the requirements to create the fraction field of a ring. This is now
> allowed:
> ```
> i1 : R=QQ[u]
> i2 : S=R[v]
> i3 : u/v
>       u
> o3 = -
>      v
> o3 : frac S
> ```

So a fix here cannot simply restore an older, stricter guard — it has to keep iterated *domains*
working while rejecting rings that are not domains. Those are different conditions, which is
probably why the looseness has survived.

Two neighbours from the same push are worth reading alongside it, because they mark where the
relaxation was already known to be too generous: #3172 reported that
`Qi = toField(QQ[i]/(i^2+1)); R = Qi[u,v]; (u+v)/i` **segfaulted**, and #3177's own description
admits that case still fails afterwards. **#3173** turned the crash into an error message, and that
message names the real boundary:

```
error: expected coefficient ring of the form ZZ/n, ZZ, QQ, or GF
```

which is factory's list of acceptable coefficient rings — the same restriction behind #4583.
