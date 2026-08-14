> **Written by Claude** (Claude Opus 5, via Claude Code), not by @d-torrance, whose account posted it -- please weigh it accordingly.

This is done. All 857 files now carry a verdict, and the ones that are still live have been filed.

When this was closed in May, the `bugs/` tree had been removed by [`d2c8d27826`](https://github.com/Macaulay2/M2/commit/d2c8d27826) and copied to a project board, but nothing had actually been read: the question this issue asks -- *does this still reproduce, and if not, what fixed it* -- was still open for every one of them. Going through them is what happened since. 229 were reproducer scripts and 628 were prose notes; fourteen were wishlist files holding many unrelated requests each, and those were split into 191 individual asks rather than settled whole, since one issue covering ten requests is one nobody can close.

### Where they landed

| verdict | | | what it means |
| --- | ---: | ---: | --- |
| `fixed` | 414 | 48% | No longer reproduces, or the feature now exists. Where a commit or PR could be identified it is recorded -- 136 of these carry one. |
| `wontfix` | 142 | 17% | Deliberate behaviour, a need that was met another way, or a request with no closing condition anyone could act on. |
| `open` | 131 | 15% | Still broken, or the feature is still missing. 130 of these name an issue; the last is a wishlist file whose asks are filed individually. |
| `obsolete` | 82 | 10% | The premise is gone -- a dead platform, a removed function, a retired dependency. |
| `duplicate` | 77 | 9% | An existing issue already covers it, and that issue is recorded. |
| `stale-repro` | 11 | 1% | The script fails only because it uses an obsolete API, so it cannot tell you anything until someone rewrites it. |

The prediction I would have made at the start was that most of these were about cygwin, xemacs, MPIR, `dumpdata` and the old Debian packaging, and would come out `obsolete`. That was wrong by a wide margin: `obsolete` and `wontfix` together are 27%, and **the single most common outcome, by a factor of three, is that the bug was quietly fixed years ago and nobody closed the file.**

### What came out of it

- **143 issues in this repository carry the [`bugs directory`](https://github.com/Macaulay2/M2/labels/bugs%20directory) label**, which is how the cohort stays findable now that the board's drafts are settled. One more went to [Macaulay2/M2-emacs#102](https://github.com/Macaulay2/M2-emacs/issues/102), since the editor mode lives in its own repository.
- **59 comments were added to 55 existing issues** -- material a bug file contributed to something already tracked, rather than a new issue nobody needed.
- **211 distinct issue references** are recorded across the catalog in total. Most of those are pre-existing issues that a row turned out to duplicate, which is the part that would have been lost if the files had simply been deleted.

A few of the issues came not from a bug file's own complaint but from checking one. [#4556](https://github.com/Macaulay2/M2/issues/4556) is the clearest: a file asked for LU-based inverses over `RR` and `CC`, which exist and work, and testing the *failing* case as well as the succeeding one showed that `inverse` of a singular matrix returns the zero matrix instead of erroring.

### Three caveats worth stating plainly

**`fixed` is not the same as fixed on purpose.** Only 136 of the 414 carry a pointer to a commit or PR. The rest drifted into correctness with no identifiable change: an API grew a new spelling, a check stopped firing, a documentation node got written. Do not read 48% as a record of anyone responding to these files.

**The verdicts are a model's judgement, reviewed one at a time.** Every issue filed and every comment posted was brought to @d-torrance individually with the evidence behind it, and several proposed filings were stopped there -- a couple of "this is slow" reports turned out to be expression swell rather than defects, and one was already settled inside a merged pull request. But the 857 verdicts themselves were not each independently checked by a human, and some will be wrong. The reasoning behind each one is written down, which is what makes them checkable.

**Removing the files, as this issue asked, did not happen and should not now.** The convention was to delete a file once settled; instead the whole tree was removed first and the record kept in git history and in the catalog. Anything worth keeping from a reproducer has been named in its verdict rather than promoted into `tests/` -- and the record shows that when a reproducer really was worth keeping, the maintainers moved it at the time, as with `bugs/mike/1-mingens-subquotient.m2` becoming `tests/normal/mingens2.m2` in 2008.

### Where the record is

`catalog.tsv` on the [`bug-triage`](https://github.com/d-torrance/M2/tree/bug-triage/bug-triage) branch of the `d-torrance` fork: one row per file, with the verdict, the issue, the fix and a note giving the reasoning. `asks.tsv` holds the 191 asks from the fourteen wishlist files. Each row's project card carries the same block, so the board is readable without the branch. The `note` column is greppable and is the only place the reasoning lives, so before concluding that some subsystem has never been looked at, it is worth a `grep`.

One thing that column will not give you: on a `duplicate` row the `issue` column points at the *older* issue, not at anything filed here, so a sweep keyed on it will not find this work's output. The `bugs directory` label will.
