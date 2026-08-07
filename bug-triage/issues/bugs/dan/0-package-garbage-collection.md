The `Attributes` half reproduces and is measurable:

| | `Attributes` entries |
| --- | ---: |
| before | 2682 |
| after 30 × `newPackage(..., Reload => true)` | 2806 |

About **four entries leaked per reload**, never reclaimed.

### Why it is worse than a slow leak

`getAttributes` scans the whole table, so a growing `Attributes` is also a growing cost on every
lookup that consults it — which is the mechanism that would eventually make `0-slowness`'s complaint
true again. A leak that also degrades a hot path deserves attention ahead of its size.

### Scope

The file describes two leaks; this is the one that reproduces cleanly and can be counted. The other —
the 40 MB-per-cycle growth under `ulimit -v` — needs a platform where the limit is enforced, which the
file itself notes does not include macOS.

### Not covered elsewhere

**#488** and **#2562** are engine and sequence allocation respectively, so neither covers this.
