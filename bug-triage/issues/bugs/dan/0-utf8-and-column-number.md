Closed as **wontfix**, and the reasoning is worth recording since the file reads as a plain bug report.

Byte columns are the convention here, not an oversight. Macaulay2Web relies on error locations being
byte offsets, so counting characters instead would be a breaking change across repositories rather than
a local fix.

### One argument that did not carry, for the record

There is a case that the counter is already a *display* column rather than a byte offset: `getc(o:PosFile)`
at `d/stdiop.d:198` advances the column to the next multiple of 8 on a tab. So the counter is not purely
a byte count today. That did not settle it, because the objection was never that strings are bytes — it
was that downstream consumers index by bytes.

Also worth recording: grepping the M2 tree for those consumers finds **none**. They live in `M2-emacs`
and `Macaulay2Web`, which is precisely why the change is cross-repository and why this stays closed.
