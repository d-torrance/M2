One more item for this list, and it is the oldest: **`gb` itself takes no hooks**, which is the
reason `hermite` exists as a separate exported function at all.

A file in the pre-GitHub `bugs/` tree (`bugs/dan/2-gb-LLL-option`, being triaged in #36) opens with
*"make gb(f,Strategy=>LLL) be a hook to LLL code / maybe also res"*, and the mail thread under it
records why. Dan, in 2006:

> Since hermite and gb are supposed to do the same thing, I see no reason for LLLBases to provide a
> function called hermite, unless it's faster, in which case it should supplant "gb" (through a hook)
> instead of being called "hermite".  What do you think?

Mike, replying:

> The hermite LLL code is supposed to produce much better change of basis matrices.  I would like to
> see a hook to gb for this.

Nineteen years on, `hermite` is still exported (`LLLBases.m2:638-651`) and `gb` still has no hook
mechanism — `gb(m, Strategy => LLL)` errors, and `groebnerBasis` carries a standing
`-- TODO: hookify this` at `gb.m2:398`. The "maybe also res" half has the mechanism but no LLL hook:
`freeResolution` takes nine strategy hooks at `Complexes/FreeResolution.m2:561-569`.

Two things on this issue's existing list look like consequences of that gap rather than independent
problems:

- Item 1, the `minimalPresentation` hook disabled by `return null` (`LLLBases.m2:758-760`), is the
  one place the hook mechanism *was* used for LLL, and its comment says the routine "only partially
  minimizes; we could use this for trim and improve it for this."
- `trim` over `ZZ` reaches into LLLBases by hand instead — `needsPackage "LLLBases"` and
  `value LLLBases.Dictionary#"LLL"` at `matrix2.m2:281-285` — under a second
  `-- TODO: make into a separate hook`.

So the hookification of `gb`, and of `trim`'s `ZZ` path, may be the shared prerequisite for items 1
and 2 rather than a fourth item beside them. #4586 is the same shape of gap for `Ext`.
