The file settles its own first half — it says `(FIXED)` for the `.installed` problem. The second half is
unmet.

### What is missing

`installPackage` touches `.installed` when a run completes (`installPackage.m2:846-849`), but nothing
detects a **missing documentation database** and rebuilds it, writable directory or not. So a prefix
whose `.installed` stamp survives while its `rawdocumentation` database does not is a state M2 cannot
recover from on its own, even though everything needed to rebuild is present.

### Why that state arises

Exactly as the file describes: starting from a partial tree — the staging area, a copied prefix, an
interrupted install — leaves the stamp and loses the database. `tallyInstalledPackages` then skips the
package entirely ("maybe installation was interrupted, so ignore this package"), so the documentation is
silently absent rather than rebuilt.

### Nearby but not the same

**#1643** wants one documentation database per prefix instead of per package, for speed; **#776** wants
the open database files bounded by an LRU cache. Neither rebuilds a missing one.
