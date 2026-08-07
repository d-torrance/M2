Reproduces byte for byte, and the cause is one line.

`net Superscript` (`expressions.m2:774-779`) raises the exponent by `1 + depth n`, computed from the
**exponent's own** net — so the offset ignores how tall the base is. The parenthesized quotient ring in
the file measures `(height, depth) = (3,3)`: six lines with its baseline in the middle, so the exponent
lands on row 2 of 6, exactly where the file's arrow points.

### The fix, and why the obvious variant is wrong

Raising by `max(1 + depth n, height b)` against the **base** net puts the exponent above the top line,
which is what the file asks for, and is byte-identical to today's output for a one-line base and for a
matrix base — checked. The tempting `height b - 1` breaks the ordinary case by dropping the exponent onto
the baseline.

### Not a deliberate choice

`git log -S 'n^(1+depth n)'` returns only `647f7532b3`, "Initial revision" — so the formula has never
been revisited, rather than having been settled on.

### Related

**#3198** is about the parser's precedence for `_` and `^`, not about where a superscript is drawn.
