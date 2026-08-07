The audit never happened. **14 bare `sprintf` calls** remain in `c/` and `d/`, and two of them are
sized by their input rather than bounded:

- `uniquify` formats an arbitrary identifier into `char buf[1000]` — `c/dictionary.c:33`
- `system_errfmt` leaves 10 bytes of slack for three `int`s — `d/scclib.c:355`

### Why those two are the interesting ones

The remaining twelve format fixed-width things into buffers with obvious headroom. These two do not:
the first takes a name of unbounded length from the source being compiled, and the second assumes
three integers cannot need more than ten characters between them. Neither is exploitable through
ordinary use, but both are the shape of bug that becomes a CVE when someone feeds M2 generated input.

### Notes for whoever picks this up

`snprintf` with the buffer size, plus a check on the return value, is the mechanical fix for twelve of
them. The two above want a bound derived from the input instead — for `uniquify`, either a dynamic
allocation or a documented limit on identifier length that is actually enforced.
