# Triaging the removed `bugs/` directory

Commit [`d2c8d27826`](https://github.com/Macaulay2/M2/commit/d2c8d278264348116ac4c1feeb95150699e3e11b)
("remove bugs directory") deleted 857 files -- 41,646 lines -- under `bugs/`. That tree was
Macaulay2's pre-GitHub bug catalog: `bugs/README` said *"one file per issue … after the bug is
fixed or the issue is resolved, we remove the file."* Going through all of them is
[#36](https://github.com/Macaulay2/M2/issues/36), open since 2013.

They were not gone through. They were bulk-copied to a private repo (see
[#4139](https://github.com/Macaulay2/M2/issues/4139)) and dropped, so this repo's git history
is now the public record of what was in them.

This directory is the tooling to work through them, answering three questions per file:

1. Does the bug still reproduce -- or, for a feature request, is the feature still missing?
2. If so, is there already an issue in Macaulay2/M2 covering it?
3. If not, which commit or PR fixed it?

Answering them is all this branch does. Nothing here modifies the Macaulay2 sources: the output
is `catalog.tsv`, the issues and comments filed from it, and the project board. Acting on a
verdict -- promoting a reproducer into `M2/Macaulay2/tests/`, or writing the fix -- is separate
work in a separate branch. See [Settling a file](#settling-a-file).

## Quick start

```sh
bin/extract         # pull the 857 files back out of git history into files/
bin/init-catalog    # build catalog.tsv
bin/fetch-issues    # cache all Macaulay2/M2 issues into cache/
bin/suggest-issues  # shortlist candidate issues per bug file
bin/run-repros      # run the 229 .m2 reproducers (slow; see below)
bin/find-fixes      # fill the "fix" column for rows marked fixed
bin/render --suggestions   # regenerate CATALOG.md
```

`files/` and `cache/` are gitignored -- both regenerate in seconds to a few minutes, and
re-committing 41k lines of a deliberately removed tree would not be welcome.

## `catalog.tsv` is the source of truth

One tab-separated row per bug file. **Columns 1-6 are machine-generated; columns 7-11 are
yours.** `bin/init-catalog` merges on `path`, so re-running it never clobbers a verdict you
typed.

| column | filled by | meaning |
| --- | --- | --- |
| `path` | `init-catalog` | `bugs/dan/0-decompose.m2` -- the join key everything else uses |
| `owner` | `init-catalog` | `dan`, `mike`, `anton`, `LAcore`, `gfurnish` |
| `prio` | `init-catalog` | leading number on the filename, Dan's own priority |
| `kind` | `init-catalog` | `repro` (a `.m2` script) or `note` (prose) |
| `lines` | `init-catalog` | line count |
| `autorun` | `run-repros` | `pass`, `pass-partial`, `fail`, `timeout`, `n/a` |
| `verdict` | **you** | see below |
| `issue` | **you** | `#4487`, if an issue already tracks it |
| `fix` | **you** | commit sha and/or `#PR` that resolved it |
| `disposition` | **you** | `test`, `quarantine`, `goals`, `issue`, `drop` |
| `note` | **you** | one line of rationale |

### Verdicts

| verdict | means |
| --- | --- |
| `todo` | not yet looked at |
| `open` | still reproduces, or the feature is still missing |
| `fixed` | no longer reproduces; put the commit or PR in `fix` |
| `duplicate` | an existing issue already covers it; put it in `issue` |
| `wontfix` | deliberate behavior, or the subsystem is gone |
| `obsolete` | the premise no longer applies -- dead platform, removed function, retired dependency |
| `stale-repro` | the script fails only because it uses an obsolete API; rewrite it before it can tell you anything |

The vocabulary comes from
[`d3ec491953`](https://github.com/Macaulay2/M2/commit/d3ec491953) ("Remove old bug files for
fixed issues"), which triaged 37 of these by hand. Its commit message is the house style for
`note`:

```
* 0.5-fix-M2-exec-location (fixed by #2163)
* 1-nextPrime (added in 2016 -- see 992b43f)
* 0-Verbosity (duplicate of #273)
* 1-constant-to-RR (promote works since #3457, toRR is compiled so wontfix)
```

## Run `git grep` from the top of the checkout

`git grep` pathspecs are relative to the **current directory**, not the repository root. Run

```sh
git grep -n pruningMap -- M2/Macaulay2/packages/
```

from inside `bug-triage/files/` -- which is where you naturally end up while reading bug files --
and it matches nothing, prints nothing, and exits 0. It looks exactly like an honest negative.

That matters here because so many verdicts rest on negatives: *no method is installed*, *the
symbol is absent*, *nothing guards this call*. A silent empty result turns "I did not look in the
right place" into "it isn't there", and the verdict reads as verified when it is not. Prefer
running a reproducer under M2 when the claim can be tested at all -- a runtime check does not care
what directory you are in.

## The ask is the mechanism; the need is what got met

These files name a specific fix as often as they name a problem, and checking only whether *that
mechanism* exists produces false `open` verdicts. `bugs/dan/0-disabling-threads` asks for the
configure option `--disable-pthreads` to be made to work again. It is still commented out with
`dnl` in `configure.ac`, so the literal ask is genuinely unmet -- but M2 grew `--no-threads`
instead, which skips `initializeThreadSupervisor` outright (`bin/main.cpp:101`), and `GC_NPROCS`
covers the collector's own threads. Both are runtime, so neither needs a rebuild, which is
strictly better for the system administrators the file is worried about.

Ask what the file wanted, not only what it proposed.

A file can also be dead in its framing and still hold something live. `bugs/dan/1-cygwin-info`
is three lines about `/usr/share/info/dir` not updating under cygwin, and cygwin has been gone
since 2021 -- but its second sentence, "check that install-info is in a prerequisite package,
too", is a separate ask that outlived the first. It is satisfied, as it happens: `configure.ac`
checks for `install-info` and errors without it when info documentation is requested. Had it not
been, `obsolete` would have buried a live request under a dead platform. Read to the end of the
file before settling it on its first line.

## A deliberate commit outranks a consistency argument

`bugs/dan/1-CC-tostring` asks that both parts of a complex number be carried to the same distance
right of the point. Its transcript shows `1e-30 + ii` printing as `1e-30+ii`; today the scalar
prints `ii`, while `matrix {{1e-30+ii}}` prints `| 1e-30+ii |`. Two code paths --
`net CC` reaches `format(...,CC)` in `gmp1.d:246`, which derives one accuracy from `exponent(z)`,
the larger part; matrix entries reach `expression CC` (`reals.m2:494`), which formats the parts
independently. One number, two renderings depending on where it sits, and `RR` shows no such
split at any magnitude from `1e-12` to `1e12`.

That reads like a defect and is not one. `530ef987f8` -- *"print complex numbers to a combined
precision for both parts"*, Dan, 2008-02-03, on the 1.1 branch, which is the release the file
asks for -- introduced the shared accuracy on purpose. A real part thirty orders below the
imaginary part is not part of a six-significant-digit rendering of that number. The file's
transcript is the *pre*-commit behavior, so it illustrates one convention while its sentence asks
for the other.

The `RR` comparison looked like a control and was not one: an `RR` scalar has one part, so the
question the commit settles never arises there. It could only ever come back "no difference".

Two habits follow. When a difference in behavior looks like an inconsistency, search for a commit
that introduced it deliberately -- `git log -S` on the line that implements it -- before recording
`open`; the author of a fifteen-year-old bug file is often the author of the commit that
superseded it. And when reaching for an analogous case as evidence, check that the analogy can
actually exhibit the thing being tested.

## Ask whether the feature also fails correctly

`bugs/dan/1-CC-inverse` asks for LU, as in LAPACK, to compute inverses over `RR` and `CC`. It is
there: `inverse Matrix` routes every `InexactField` through the engine to
`DMatLUinPlace<ARingRR>::computeLU`, which calls `dgetrf_`. Random 5×5 matrices over `RR` and
`CC` invert to `norm(A * inverse A - I)` around `1e-15`, `CC_200` to `4e-60`. `fixed`, and it
would have been reasonable to stop.

The negative case is wrong. `inverse matrix {{1.,2.},{2.,4.}}` returns the **zero matrix**
instead of erroring -- at `CC_53` and `RR_200` too -- while `rank` returns 1, `det` returns 0, and
`solve(m, id_(RR^2))` returns `null`. That last is the call `inverse` makes internally, so
`inverse` is the only consumer of the same LU decomposition that reports success. One line
explains it: `DMatLinAlg::inverse` (`dmat-lu.hpp:529`) calls `solve(id, X)`, discards the boolean,
and returns `true` unconditionally, though `solve` returns `false` for an inconsistent system at
`dmat-lu.hpp:379`.

A wording trap on the way to that, worth naming because it survived two write-ups. `solve` does
not *error* on a singular system, it returns `null` -- but the check that found it was
`try (entries solve(m, id_(RR^2))) else "errors"`, and `entries null` errors, so the `else` branch
fired and read as a clean confirmation that `solve` rejects the matrix. Wrapping the call in
something that consumes its result puts a second failure mode inside the `try`, and the two are
indistinguishable from the output. Test the return value (`=== null`, `class`) rather than a
function of it.

These files ask "does the feature exist", so the natural check is one call that should succeed.
Add one that should fail. A silent wrong answer is worse than the missing feature the file was
written about, and it is the kind of defect that survives precisely because nobody's example
exercised it. Filed as [#4556](https://github.com/Macaulay2/M2/issues/4556) -- the first issue
here that came out of *verifying a `fixed` verdict* rather than out of a bug file's own ask, which
is why it has no row in `catalog.tsv` and was filed by hand rather than by `bin/file-issues`.

### A closed issue can be fixed on only some of the paths its ask spans

The [duplicate search](#never-truncate-the-duplicate-search-and-read-it-oldest-first) on that
finding turned up [#2208](https://github.com/Macaulay2/M2/issues/2208), which is the *same ask* --
a singular square matrix should report "matrix not invertible" -- on `ZZ/5`, and closed. Reading
only the state would have made the row a duplicate of a settled issue.

What closed it is #2241 (`a032f77d13`), and it changed element-level inversion in `ZZp.cpp`,
`aring-zzp*.hpp`, `aring-gf-flint.hpp` and `ZZ.cpp`. Those are exactly the exact rings. It never
touched `dmat-lu.hpp`, so the generic `DMatLinAlg` path the inexact fields use kept the hole --
which is measurable: `ZZ/5` and `ZZ/32003` error correctly today, `RR_53` and `CC_53` do not.

The README already says to read *why* an issue closed. This is the sharper version: read *what the
fix touched*, and check the ring, precision or code path in front of you is one of them. `git show
--stat` on the closing commit answers it in one line, and a fix that lands in a ring-specific
specialization is the shape most likely to have missed its neighbours.

## Some of these were answered on the wiki

Not all Macaulay2 documentation is in the repository. The
[Package Writing Style Guide](https://github.com/Macaulay2/M2/wiki/Package-Writing-Style-Guide)
lives on the GitHub wiki, and it is where advice to package authors actually ended up -- naming
conventions, use of types, optional arguments, layout, argument order. `bugs/dan/0-doc-writing-code`
asks for exactly one of its rules to be written down, and it has been for years: *"The argument
upon which the function mainly acts should go last."*

So a documentation request that greps as unmet may simply be answered somewhere `git grep` cannot
see. Check the wiki before recording `open` on any row that asks for something to be documented.
The wiki is not in git, so those rows get no `fix` commit -- cite the page in the `note` instead.

## `autorun` is a hint, not a verdict

`M2 --script` exits 1 on an uncaught error or a failed `assert`, so `bin/run-repros` gets a
real signal. But these scripts are up to twenty years old, and plenty of them now fail on API
drift rather than on the bug they were written to demonstrate. `bugs/dan/0-decompose.m2` is
the canonical example: it dies today on

```
error: no method for binary operator == applied to objects: ... (of class Matrix) ... (of class Ideal)
```

which says nothing at all about the `decompose` bug it was reporting.

There is a second trap, and it is the bigger one. A bare `end` line halts an M2 script *and
still exits 0*. Seventy-one of the 229 reproducers park the actual demonstration after an `end`
so it can be pasted in by hand -- `bugs/anton/MISC/standardPairs.m2` runs three lines, hits
`end`, and never reaches its `assert`. Counting that as a pass would be wrong, so those are
recorded as **`pass-partial`**. Of the 85 scripts that exit 0, only 44 actually run to
completion.

(A leading `restart` needs no such treatment: under `--script` it is a no-op and execution
continues.)

So: `fail` means *a human should read this*; `pass` means *the script no longer trips, which
might be a fix or might be that the assertion stopped being checked*; `pass-partial` means
*almost nothing*. None of them writes a verdict. `bin/run-repros` puts failures whose output
looks like API drift in `cache/stale-candidates.txt`, and the full output of every run in
`cache/runlog/<path>.log`. `bin/run-repros --reclassify` redoes the `pass`/`pass-partial` split
from the files alone, without rerunning M2.

Reproducers run with a 4 GB address-space cap (`--memory`), because several of them are
memory-leak demonstrations that allocate without bound.

## Attributing a fix

Every row marked `fixed` should end up with a pointer in the `fix` column, so project 46 can
carry a link to whatever settled it. `bin/find-fixes` fills it in, preferring a PR number and
falling back to a commit sha -- most of these were fixed before Macaulay2 moved to a
pull-request workflow, so for the older ones the sha is the only pointer that exists.

It tries four sources, in descending confidence:

1. **The issue timeline**, when the row names an issue and GitHub recorded a closing commit.
2. **Local `git log`** for a commit whose message claims that fix. This is free, needs no API
   call, and finds most of the old ones. Two traps it handles: `Merge pull request #56 from …`
   means *PR* 56, not issue 56, and the commit that fixed #370 and #473 is titled `fixes to
   solution to git issues 370, 473` with no `#` anywhere -- so matching on `#N` alone misses it.
3. **The issue's comments**, for `Fixed in commit 7dd8aaa` style references.
4. **The `RESOLVED/` rename**, for the files Anton settled by moving them rather than by
   writing an issue number down. This dates the resolution accurately, but the PR it lands on
   is the one that filed the move, not necessarily the one that changed the code -- the note on
   those rows says so.

Anything it cannot pin down is left blank rather than guessed at, and rows with no issue to
anchor a search to get a shortlist in `cache/fix-candidates.tsv` instead: commits whose message
contains both the bug's slug and a fix verb. **That shortlist is a lead, not an answer, and it
is worth distrusting.** For `1-singularLocus` it offers `8cdfab7841 "fix singularLocus over
ZZ"`, which is exactly the right shape and is nevertheless wrong -- that commit's only code
change is a one-line `tensor` tweak in `newring.m2`. Read the diff before recording anything
from it.

Of the first 21 rows marked `fixed`, 16 got a pointer this way. The remainder would need a
bisect, which means building M2 at each step; that is rarely worth it, so leaving `fix` blank
is an acceptable outcome.

### Ask GitHub which PR carried a commit; do not infer it

`git log -S` is the reliable half of this. Search for the exact string the fix introduced --
`mpz_export` in `d/gmp_aux.c`, `submatrixByDegrees = method`, `rpmbuild -ba` -- and the commit
that first contains it is the fix, verifiable by reading that one diff.

Turning a sha into a PR number is where it goes wrong. The tempting move is to walk forward from
the commit to the first merge that contains it:

```sh
git log --merges --oneline --ancestry-path $sha..development | grep 'Merge pull request' | tail -1
```

**That is wrong, and it was wrong on all four commits it was tried on.** The ancestry path
contains every merge that happened to land afterwards, so `tail -1` returns whichever unrelated
branch merged next. It dated `f39e27cba3` -- whose subject is literally `fix #690` -- to PR #802,
an `emduart2/master` merge with nothing to do with it. It put the 2026 RR-hash commit on #4040,
`MichaelABurr/intervals`. Every answer looked plausible: a real PR number, a real merge, the right
era.

GitHub knows the real answer and will tell you:

```sh
gh api repos/Macaulay2/M2/commits/$sha/pulls --jq '[.[] | "#\(.number) \(.title)"] | join("; ")'
```

That returned #772 for `f39e27cba3` and #4251 for the RR hash -- both with titles that match the
commit -- and an empty string for `0a9c638129`, which predates the pull-request workflow and
correctly takes a bare sha in the `fix` column. An empty answer is information; a guessed one is
not.

The general shape of the trap: a heuristic that produces a well-formed answer for every input
gives you no signal that it failed. Prefer the source that can say "none".

## Settling a file

**This branch does not touch the Macaulay2 sources.** It is a catalog and the tooling to build
it; everything under `M2/` stays untouched. The `disposition` column records what *should*
happen to a file, and acting on it is separate work in a separate branch against `development`.
Three of its values name a destination under `M2/Macaulay2/tests/`, and they are
recommendations, not instructions to write the file now.

**Fixed?** `verdict=fixed`, the commit or PR in `fix`, and:

- `disposition=test` if the reproducer is worth keeping as a regression test in
  `M2/Macaulay2/tests/normal/`. Say so in the `note` -- which assertions, and roughly what they
  cost -- so whoever does it later does not have to re-derive it.
- `disposition=drop` if there is nothing worth keeping, which is the common case for a prose
  note or a file whose reproducer no longer runs.

Whoever eventually promotes one keeps a comment naming where it came from. That directory's
`Makefile.in` globs `*.m2`, so dropping the file in is enough, and the existing convention is:

```m2
-- used to crash (M2/bugs/dan/1-factory-bug)     -- tests/normal/factory.m2:26
-- had been in bugs/mike/0-basis r12446          -- tests/normal/basis5.m2:156
```

**Still broken?** If it should be tracked publicly, file an issue and set `verdict=open`,
`disposition=issue`, `issue=#NNNN`. A reproducer that fails but is not worth blocking CI over
would belong in `M2/Macaulay2/tests/quarantine/` (known-failing or too slow) or
`M2/Macaulay2/tests/goals/` ("we'd like to run these; some have never succeeded") -- record that
as `disposition=quarantine` or `goals` and leave the file where it is. Both directories are in
`SUBDIRS` in `M2/Macaulay2/tests/Makefile.in`, and the `--status:` comment convention from
`tests/quarantine/2-homog-bug.m2` is what a later change would follow.

**Neither?** `wontfix` or `obsolete` with a one-line `note`, `disposition=drop`.

This section used to predict that most of the 857 would land here, on the grounds that a lot of
them are about cygwin, xemacs, MPIR, `dumpdata`, and the Debian packaging that used to live in
`distributions/deb`. **That was wrong, and by a wide margin.** Of the first 216 settled:

| verdict | | |
| --- | ---: | ---: |
| `fixed` | 94 | 44% |
| `open` | 62 | 29% |
| `obsolete` | 29 | 13% |
| `duplicate` | 21 | 10% |
| `wontfix` | 10 | 5% |

So `obsolete` and `wontfix` together are 18%, not "most", and the largest single outcome by far
is that the bug was quietly fixed years ago and nobody closed the file. The shape has held
steady: it was the same to within a point at 167 and at 195 settled, so the next bucket is
unlikely to move it much either.

Grepping the *unsettled* files says the same thing rather than merely reflecting which ones got
done first: of the 704 still `todo` at 167, only 33 mentioned any retired subsystem at
all, and of 25 that looked like candidates, 14 held up. The dead-platform material is a real
seam but a thin one.

Two cautions on those numbers. They are not a random sample -- they are `dan/0`, `dan/0.1`,
`dan/0.4`–`0.9`, the start of `dan/1` and a deliberate sweep for retired subsystems, and `dan/0`
was Dan's own highest-priority bucket, which may well be where the real bugs that later got fixed
are concentrated. And `fixed` at 44% is itself a finding about the tree rather than about the files:
it means the common case is reading a fifteen-year-old report, running it, and finding it simply
works now.

A third caution the `0.4`–`0.9` bucket added: `fixed` is not the same as *fixed on purpose*. Of
its eleven `fixed` rows only five carry a pointer at all, and two of those settled the file's ask
as a side effect -- #772 reversed `Tally` and `VirtualTally` while fixing #690, and #3983 got the
source rpm by rewriting the packaging script. The other six simply drifted into correctness with
no identifiable commit: an API grew a new spelling, a check stopped firing, a doc node was
written. Do not read the 42% as a record of anyone responding to these files.

## Relationship to [project 46](https://github.com/orgs/Macaulay2/projects/46)

The org already has a project board holding roughly one draft issue per bug file, with custom
fields. This tooling is built to join to it, not to replace it -- `path` is the shared key.

The split that makes sense:

- **`catalog.tsv` here is the working surface.** Bulk edits, greppable, diffable, regenerable,
  and free to iterate on.
- **Project 46 is the public-facing board**, updated by a one-way push from the TSV once a batch
  of verdicts settles -- never hand-edited into divergence with it.
- **Real GitHub issues** get filed only for the subset that comes out `open` and is not already
  tracked. That is the step that actually gets a bug fixed, and it is what #36 asked for.

**Do not convert a draft whose verdict is `duplicate`.** Converting is only right for
`disposition=issue` -- `open`, and not already tracked -- and it has to happen while the item is
still in Backlog. A draft converted after it reaches Done becomes an issue that GitHub closes the
instant it is created, which is how
[#4492](https://github.com/Macaulay2/M2/issues/4492) came to exist: a closed issue in the public
tracker whose entire body is a dump of `bugs/anton/MISC/standardPairs.m2`. It cannot be deleted
without repo admin, so it was retitled to point at #114 instead.

Settling a `duplicate` goes: add whatever the bug file contributes to the existing issue, record
`issue=#NNNN` and `disposition=drop` in the TSV, and let the next push move the draft to Done.

**Do not archive settled drafts, and never delete one.** Archiving made sense when Status was the
only thing the board could carry, but the triage block is now the record -- archiving hides the
rationale it just published, and `ProjectV2.items` does not return archived items, so the row
becomes permanently unmatched and `--check` can never confirm it is current. Done is the resting
state. Deleting is worse still: it is irreversible, and the draft would have to be recreated from
`catalog.tsv` and `bin/extract`.

A trap when checking this from a script: because archived items are not returned, `isArchived` is
always false on what you get back, and an archived draft is indistinguishable from a deleted one
by item count alone. Three drafts archived on 2026-08-03 (`mike/git-issue-568-569.m2`,
`mike/git-issue291.m2`, `mike/git-issue604.m2`) made the board read 854 items, exactly as three
deletions would have. They have since been unarchived and read Done, so the board is back to 857
and matches the catalog -- but the count is what misled, and it would mislead again.

The reason not to make the board the only surface: draft issues are project-local. They do not
appear in issue search, cannot be referenced from a commit or PR, cannot be closed by
`Fixes #N`, and cannot be commented on by anyone not looking at the board. Parking a live bug
there is how the `bugs/` tree died the first time.

`bin/push-project` implements the sync. It defaults to `--dry-run`, needs
`gh auth refresh -s project`, and should not get `--apply` until you have read the output --
mass GraphQL mutations against a shared board cannot be reverted. Read the dry run every time,
not just the first: it now edits public issues as well as drafts. Reading it is the lower half of
the bar; see [`--apply` is not yours to give yourself](#--apply-is-not-yours-to-give-yourself)
for the other half.

The board (`PVT_kwDOAC6Xfc4BQEgX`, "bugs directory", 857 items) carries only the stock
project-template fields -- Status, Priority, Size, Estimate, Start/Target date, plus the
built-ins. There is nowhere to put a verdict, an issue number, a fix, or a note. Rather than add
five custom fields to a board other people use, the script writes:

- **Status**, the one field that fits:

  | verdict | Status |
  | --- | --- |
  | `todo` | Backlog |
  | `open`, no issue recorded | Ready -- still broken, needs an issue filed |
  | `open`, issue recorded | In progress -- tracked, somebody's to fix |
  | anything else | Done |

  When a filed issue is closed, the board's "Item closed" workflow moves it to Done on its own.
  The catalog still says `open`, so the mapping would otherwise insist on In progress and shove a
  settled bug back into the work queue on every push, with `--check` never converging. Instead the
  push defers to the tracker and reports the row:

  ```
  1 rows still say verdict=open but their issue has been closed.
  Update catalog.tsv -- only you can say whether it closed as fixed, wontfix or duplicate:
    bugs/dan/0-mutable-lists                     #4501
  ```

  Nothing updates the verdict automatically, because a closed issue does not say *why* it closed.
  That is a judgment, and it belongs in the TSV where the rest of them are.

  **A still-broken bug must never reach Done.** The board has the built-in
  "Auto-close issue" workflow enabled, so setting Status to Done on an item that is a real
  issue closes that issue, within a second. Drafts have no open/closed state and are safe,
  which is why this went unnoticed at first -- but the moment a draft is converted, Done would
  close the very bug that was just filed. That is what happened to
  [#4492](https://github.com/Macaulay2/M2/issues/4492): created 23:31:23Z, closed 23:31:24Z.
- **The draft's own body**, which needs no schema change at all. A block delimited by
  `<!-- triage:start -->` and `<!-- triage:end -->` is appended after the original bug file text,
  holding verdict, issue, fix, disposition and note. Re-running *replaces* that block rather than
  appending a second one, so revising a verdict and pushing again is safe.

The footer links to `catalog.tsv` so a reader can check the verdict against its source. That
catalog is not in Macaulay2/M2 yet, so `CATALOG_URL` in `bin/push-project` points at the branch
it currently lives on -- `bug-triage` on the `d-torrance` fork. **It is a mutable ref.** If that
branch is renamed, deleted, or merged away, the footer of every pushed draft points at a 404, so
update `CATALOG_URL` and re-push when the catalog lands somewhere permanent. Re-pushing is cheap:
changing the footer makes every already-pushed block differ from the board, so they all re-queue
on the next run without anything having to track which ones were written when.

Two details the body path depends on. Item titles carry the path with the `bugs/` prefix stripped
-- `mike/git-issue359.m2`, not `bugs/mike/git-issue359.m2` -- so `key_of` puts it back before
joining. And a draft is not in a repository, so `#114` and bare shas do not autolink there;
`linkify` rewrites them as full URLs. `/issues/N` redirects to `/pull/N`, so one form covers
issues and PRs alike.

Only drafts can be updated this way (`updateProjectV2DraftIssue`), so a converted item takes a
second path: `updateIssue`, keyed on the issue node id. Both are needed. Without the second, a
verdict revised after filing leaves the public issue contradicting `catalog.tsv` for good, which
is how [#4514](https://github.com/Macaulay2/M2/issues/4514),
[#4528](https://github.com/Macaulay2/M2/issues/4528) and
[#4529](https://github.com/Macaulay2/M2/issues/4529) came to sit closed while still publishing
`Verdict: open`.

On a real issue the block is only ever **replaced**, never introduced. An issue on the board
carrying no block was not filed from this catalog -- someone added it by hand -- and appending
our triage to a stranger's issue is not ours to do. Those rows are reported and skipped.

One asymmetry is deliberate. A row filed from here ends up naming its own issue, and
`Issue: #4526` in the body of #4526 is noise, so that field is dropped when it names nothing but
the issue you are already reading. It survives when it points elsewhere: #4529 was settled as a
duplicate, and its block still reads `Issue: #101`, which is the whole value of it.

Archived items are not returned by the API at all, so rows settled by archiving show up
permanently as unmatched -- that is expected, not a failure.

## When an existing issue is close but not the same

Run `bin/suggest-issues` and read the shortlist **before** writing a verdict, not after. It is
keyword-ranked and wrong often enough to distrust, but it found #457 for `0-polymake` and #4231
for `0-getting-one-element-of-a-mutable-hashtable`, both of which would otherwise have been filed
as new issues that already existed.

When the match is exact, the row is a `duplicate` and there is nothing to decide. When it is close
but not the same -- same function, same root cause, different symptom -- there are three ways to
settle it, and the choice belongs to whoever is triaging:

| | outcome | catalog | board |
| --- | --- | --- | --- |
| **a** | open a new issue, cross-referencing the existing one | `open`, `disposition=issue`, note names the related issue | Ready, then In progress |
| **b** | comment on the existing issue | `duplicate`, `issue=#N`, `disposition=drop`, plus a file under `comments/` | Done |
| **c** | note the existing issue, say nothing publicly | `duplicate`, `issue=#N`, `disposition=drop` | Done |

Worked examples of each: `0-dictionaryPath` took **a** against #1427, because #1427 is framed
around the user's private dictionary and the `OutputDictionary` symptom would not be found by
anyone searching for it. `0-generateAssertions` took **b** against #3413, because the semicolon
case is the same defect as the multi-line case and one fix settles both. `0-polymake` took **c**
against #457, which already says everything the bug file says.

## Never truncate the duplicate search, and read it oldest first

`bin/suggest-issues` gives three candidates per row, which is a shortlist by design. When it
comes up empty you will reach for an ad-hoc scan over `cache/issues.json`, and that is where the
trap is: cap the printed matches and you will cap away the answer.

`0-sort-doc` was filed as [#4529](https://github.com/Macaulay2/M2/issues/4529) when
[#101](https://github.com/Macaulay2/M2/issues/101) had been open since 2014 saying the same
thing. The search term `sort(list` **did** match #101. The scan printed the first fourteen hits
and #101 was the fifteenth, so it was never displayed — the negative looked as honest as a real
one. #4529 had to be closed as a duplicate and its content moved to #101 by hand.

Two rules follow, and they cost nothing:

- **Print every match.** If there are too many to read, the terms are too broad — narrow them
  rather than truncate the output.
- **Sort ascending by issue number.** Matches arrive newest-first, which is precisely backwards:
  a fifteen-year-old bug file is most likely to collide with an *old* issue, and those sort last.
- **Spell compound words both ways.** `0-mutable-lists` was filed as
  [#4501](https://github.com/Macaulay2/M2/issues/4501) when
  [#659](https://github.com/Macaulay2/M2/issues/659) had been open since 2015 with the cause,
  because `mutablelist` does not match a title reading "growth of mutable **lists**". Search
  `mutablelist` *and* `mutable list`.

A term that is also an ordinary English word is where this is thinnest. `about` matched 131
issues while checking `0-doc-Keywords`, which is a haystack, not a shortlist — narrowing found
[#3689](https://github.com/Macaulay2/M2/issues/3689), but only because someone read all 131.

The same scan also has to look past the `issue` column. Once a row is settled as a duplicate its
`issue` is repointed at the older issue — `0-sort-doc` now reads `#101` — so the issue that was
filed *from* that row is no longer named anywhere except the free text of `note`. A sweep keyed on
the column alone will not see it.

And a closed issue is not a settled ask. [#47](https://github.com/Macaulay2/M2/issues/47) and
[#211](https://github.com/Macaulay2/M2/issues/211) both name `___Gröbner_spbases.html`, and both
closed — one on "I can't reproduce this", the other on "just remember how I do it and check that
one file". Neither touched the cause, which is why `0-utf-8-in-doc-filenames` was still worth
filing as [#4531](https://github.com/Macaulay2/M2/issues/4531). Read *why* it closed before
treating a hit as a duplicate.

## Fence the bug file before you file it

These files are plain text from 2009, and GitHub renders an issue body as markdown. Single
newlines collapse, so an M2 transcript arrives as one run-on paragraph; `-*- coding: utf-8 -*-`
italicises; `___Gröbner_spbases.html` comes out bold-italic. Wrap the verbatim part in an
` ```m2 ` fence before converting the draft.

Do it by hand, per file. It looks automatable and isn't: prose and transcript alternate, and the
obvious rule of "break the fence at blank lines" shatters a transcript, because M2 puts blank
lines between `i1` and `o1` and between `o1` and its type. Prose is the real separator, and only
a reader can tell prose from a diagnostic — `stdio:12:21:(3): error: division by zero` is output,
not a sentence.

Leave short prose files alone. "audit all uses of sprintf for possible buffer overflows" does not
want a code block. Roughly a third of the files are one to six lines of English and need nothing.

Fencing is preserved once done: `bin/push-project` only ever replaces the triage block, so the
body above it survives, and a hand-adjusted fence stays adjusted.

### Unfenced HTML is not a formatting problem, it is a publishing one

Ugly rendering is the mild failure. The one that matters is that **GitHub renders raw HTML in an
issue body**, so an unfenced tag does not display -- it *acts*.

`0.8-documentation-suggestion-link` asks for a mailto link at the foot of each doc page, and to
show the URL syntax Dan pasted a sample at column zero:

```html
<a href="mailto:abbeyvet@outfront.net?CC=spooky@outfront.net
&BCC=thomasbrunt@outfront.net&Subject=Please%2C%20I%20insist
%21&Body=Hi%0DI%20would%20like%20to%20send%20you%20 ...
```

Converting that draft unaltered would have published, in the Macaulay2 tracker, a live mailto
link to three strangers' addresses, prefilled with the subject "Please, I insist" and a body
about dividing $1,000,000 among the moderators. It reads as spam, it exposes addresses nobody
consented to republish, and it would have to be edited out by hand afterwards. It is fenced now,
and #4549 carries it as a code block.

So the check before filing is not only "will this look right". Scan for a line at column zero
that starts with `<`:

```sh
awk '/^[^ \t]/ && /<[a-zA-Z\/!]/ {print FILENAME": "$0}' files/bugs/dan/0.8-*
```

Indentation is what saved the rest of that batch: a transcript indented four spaces or by a tab
is already a markdown code block, so the eleven filed alongside it needed nothing. The hazard is
specifically **unindented markup**, and it is worth one `awk` before every `bin/file-issues
--apply`.

## A consumer of M2's behavior may live in another repository

`0-utf8-and-column-number` was filed as [#4535](https://github.com/Macaulay2/M2/issues/4535) —
error messages count columns in bytes, so a line with `你好` before the error reports column 20
where the same layout in ASCII reports 16 — and closed as wontfix within the hour: byte columns
are the convention, and **Macaulay2Web relies on locations being bytes**.

The verification missed that, and not by being careless. `git grep column` over the M2 tree finds
no `.el` file using it, which reads as "nothing downstream depends on this". But the editor mode
lives in [M2-emacs](https://github.com/Macaulay2/M2-emacs) and the web front end in
[Macaulay2Web](https://github.com/Macaulay2/Macaulay2Web), neither of which is in this checkout.

So before arguing that an interface can be changed, ask who else consumes it, and remember that
the answer may not be in the repository you are grepping.

## Commenting on an existing issue

`bin/comment-issues` posts the comments for the **b** rows. The text is never generated: a
comment goes out only for a path with a hand-written file under `comments/`, mirroring the bug
path, e.g. `comments/bugs/dan/0-generateAssertions.md`. The `note` column is deliberately not used
-- those notes are internal shorthand written for the catalog, and posting them verbatim would
read as noise on a stranger's issue.

A footer names the source file and links #36 and the catalog. The **attribution is separate from
it, and goes first** -- `project.ATTRIBUTION`, one sentence saying Claude wrote the text and this
account only posted it. Someone reading a comment on their own issue is entitled to know that
before deciding how much weight to give it -- especially where a comment relays a claim rather
than something verified, as the #457 one does.

### Put the attribution above the text, not in the footer

It used to be four words at the end of a `<sub>` footer: "Drafted with AI assistance." That is too
weak twice over, and both failures are worth naming because neither is about the words being
absent.

The phrasing is wrong. "Drafted with AI assistance" describes a person writing something with help.
The truth is the reverse -- the model wrote it and a person approved it -- and the reader has no way
to tell which from that sentence.

And the placement defeats the purpose. A disclosure exists so a reader can decide how much weight
to give what follows; put it after, in small text, and it is read after that decision is already
made. This was not hypothetical: on #4556 it was missed on a first read by the person whose own
account had posted it. The fix there was to move it to the top and say plainly *Written by Claude
… not by @d-torrance, whose account posted it*, and that is now `project.ATTRIBUTION`, shared by
`bin/comment-issues` and the triage block in `bin/push-project`.

The triage block had no attribution at all, which mattered more than it looks: `push-project`
writes that block into **public issues**, not only into drafts. So a verdict and note written by a
model were appearing under a human's account on a stranger's issue with nothing saying so.

Two consequences to expect when this changes. Every already-posted comment and every already-pushed
body now differs from what the tooling would write, so all of them re-queue -- 13 comments and 215
bodies at the time of the change. That is the derived check working as designed rather than a
problem, but it means the next `--apply` **edits public text that is already live**, and that
needs asking for on its own terms. And keep it to one sentence: it sits on top of comments that are
sometimes three lines long, and a disclaimer longer than its content stops being read.

**A comment is the only thing here that notifies anyone.** Draft bodies, statuses and labels are
all silent; a comment reaches every watcher of an issue that may be a decade old. That is why the
judgment is never automated and the dry run prints the full text. Re-running edits the comment
already posted rather than adding a second, found by a trailing `<!-- bug-triage:PATH -->` marker.

## Filing the issues

`bin/file-issues` is the step [#36](https://github.com/Macaulay2/M2/issues/36) actually asked
for: a bug file that still reproduces and is not already tracked becomes an issue someone can be
assigned, reference from a commit, and close with `Fixes #N`. A draft can do none of those. It
drives `convertProjectV2DraftIssueItemToIssue`, the same mutation as the board's "Convert to
issue" button, so the item keeps its place and its body carries over.

Every issue filed this way gets the
[`bugs directory`](https://github.com/Macaulay2/M2/labels/bugs%20directory) label. That is what
keeps the cohort findable in issue search once the drafts are gone -- the one view the board
cannot give you, since project membership is not searchable from the issues page. The label is
resolved before anything is created, so if it were ever renamed the run stops rather than filing
a batch of unlabelled issues to fix up by hand.

A row is filed only when `verdict=open`, `disposition=issue`, `issue` is empty, and the path has
a title in `issue-titles.tsv`. That last requirement is not bureaucracy: draft titles are bare
paths, and converting without renaming is how #4492 landed in the tracker titled
`anton/MISC/standardPairs.m2`. The title is set on the draft first, then the draft is converted.

## Topic labels: where it came from, and who should read it

`bugs directory` answers the first question and nothing else. The cohort is a few dozen issues among
eight hundred open ones, so a bug about the engine that carries only that label is invisible to
someone filtering the tracker for engine work -- which is precisely the audience #36 wanted these
in front of when it asked for them to be filed rather than left as drafts.

So `issue-titles.tsv` carries a third column, comma-separated, holding the repository's own labels
for a row: `Engine, threads`, `Documentation, feature request`. `bin/file-issues` applies them
along with `bugs directory` as it converts, so a row's labels are chosen at the same time as its
title and go on with it.

Choosing them is a judgment, but a cheap one, and it is worth making from the `note` rather than
the title. Two rules hold it in place, both enforced rather than merely written down:

- **The name must exist in the repository.** Checked in the dry run as well as under `--apply`,
  before anything is created, so a typo stops the run instead of leaving half the batch labelled.
- **No pull-request label on an issue.** `dependencies`, `javascript`, `new package`,
  `waiting for review by package author(s)` and the rest of `project.PR_ONLY` describe a PR or
  someone's review queue. None of them says what a bug is about, and putting
  `waiting for review` on a fifteen-year-old bug report is a claim about work nobody is doing.

Match the tracker's own habits for the subsystem labels rather than importing a taxonomy: `build
issue`, `Documentation`, `Engine`, `Interpreter`, `threads` and `Core` account for most of what is
in use, and one of them usually says all there is to say about where a file belongs.

**`bug` and `feature request` are the pair worth getting right, and they are exclusive.** The
`bugs/` tree was never only bugs -- it was Dan's notebook, and the files in it divide along a line
that matters to anyone picking work off the tracker:

- **`bug`** -- M2 does the wrong thing. It returns an answer that is wrong (`symmetricPower` of a
  matrix ignoring relations), contradicts its own documentation (`break` resuming the file load it
  was supposed to leave), silently accepts something that does nothing (a method installed on a
  plain function closure), or reports an error that misdescribes what happened.
- **`feature request`** -- M2 does not do the thing yet. No `(isSurjective, RingMap)` method, no
  top-level interface to the engine's tower rings, no way to register a Gröbner strategy.

`project.EXCLUSIVE` rejects a row claiming both, because an issue carrying both has had that
judgment dodged rather than made. Plenty of rows are honestly *neither* -- a rename proposal, a
documentation gap, "generate these Makefile dependencies instead of maintaining them by hand" --
and those take a subsystem label alone. Of the first 38 filed, 17 came out `bug`, 11
`feature request`, 10 neither.

### Relabelling after the fact is by hand

Nothing here edits the labels on an issue that already exists. Labels go on at conversion and stay
whatever a maintainer makes them; revising the TSV afterwards changes what the *next* filing gets
and nothing else. If you change your mind about a filed issue, change it in the GitHub UI and in
`issue-titles.tsv`, both, or the two will quietly disagree.

That is a deliberate asymmetry with `push-project`, which does edit filed issues -- the triage
block is this catalog's own text and has to stay in step with the verdict, whereas a label is a
shared surface that maintainers, not just this tooling, write to.

The forty-three issues filed before the labels column existed were backfilled in one pass on
2026-08-05. Five of them were skipped and still carry only `bugs directory`: #4501, #4514, #4528,
#4529 and #4535, all closed. Their catalog rows are settled, and two of them are duplicates whose
`issue` column was repointed at the older issue -- `0-sort-doc` now reads #101, open since 2014
and belonging to someone else, which is not a row to label from.

**The order is push, then file.** Conversion copies the body verbatim, so a draft that has not
been pushed becomes an issue holding the bare bug file with none of the reasoning behind it --
which is exactly the useful part. `bin/file-issues` refuses to convert a draft with no triage
block rather than trusting anyone to remember. The full cycle:

```sh
$EDITOR catalog.tsv                # record verdicts
bin/render --suggestions           # refresh CATALOG.md
bin/push-project --apply           # blocks onto the drafts; open rows go to Ready
$EDITOR issue-titles.tsv           # name and label the ones to be filed
bin/file-issues --apply            # convert; issue numbers land back in catalog.tsv
bin/push-project --apply           # those rows now go to In progress
```

The second push is not redundant: `file-issues` writes the new issue number into the catalog, and
that is what moves the row from Ready to In progress.

## `--apply` is not yours to give yourself

That runbook is a description of the order, not a licence to run it. **Every `--apply` here, and
every `bin/comment-issues`, needs the maintainer's go-ahead, each time.** A clean dry run is what
you show them to get it; it is not a substitute for asking.

This matters because triaging is private and the four steps above are not. Filing an issue
notifies everyone watching the repository. A comment reaches every watcher of a thread that may
be a decade old and belongs to someone else. A push makes mass GraphQL mutations against a board
other people use, and cannot be reverted in bulk. Recording a verdict in `catalog.tsv` costs
nothing if it is wrong; the rest of it costs a stranger's attention, and an issue filed in error
has to be closed, commented, and its body corrected by hand -- see #4492, and see #4529, which
duplicated a 2014 issue because the search that would have caught it was never run.

The judgement being asked for is not "is the dry run clean". It is "should this be public at
all", and that is the maintainer's to make. Read the dry run every time; then ask.

**Nothing records that a row has been pushed, on purpose.** Every run diffs the TSV against live
board state and queues only what actually differs, so after an applied push the same command
reports nothing to do. A `pushed` column would go stale in both directions -- revise a note and
forget to clear the flag and the board silently keeps the old text; hand-edit the board and the
flag claims a sync that no longer holds. The derived check cannot lie, and it re-converges on the
next push. `./bin/push-project --check` answers the question tersely and exits 1 when the board is
behind.

## The wishlist files are parked, on purpose

Seven files open with "Place bugs that you find in here" -- `0-bugs-ataylor.m2`,
`-caviglia`, `-decker`, `-eisenbud`, `-iswanson`, `-kummini`, `-lgold`, `-popescu`,
`-stillman`, and `mike/0-bugs-with-de.m2`. Each holds several unrelated asks, so one row does not
correspond to one issue, and filing them whole would produce issues that cannot be closed until
every item in them is done. That is how these files survived twenty-five years in the first place.

They are triaged normally -- verdict, and a note recording which specific asks were confirmed
still live -- but left with `disposition` blank, so `bin/file-issues` skips them and they rest at
**Ready**. Deal with the class in one pass once the rest of the backlog is settled, rather than
re-deciding it per file. `0-document-packages` and `0-interrupts` are parked there for the same
reason: several asks, partly done.

When that pass happens, note that the `issue` column already takes several references --
`bugs/mike/git-issue-568-569.m2` carries `#568 #569` -- so a row can be settled by filing one
issue per live ask and recording them all.

## Where to start

`bugs/dan` priority `0` was the place to start -- 118 files, Dan's own highest-priority bucket,
and the same one `d3ec491953` drew from. It is done, as are `0.1` and `0.4`–`0.9`. Of the 857,
216 are settled and **641 are left**:

| | |
| --- | ---: |
| `dan`, priority `1` | 288 |
| `mike` | 207 |
| `dan`, priority `2` and beyond, plus unnumbered | 101 |
| `anton` | 34 |
| `LAcore`, `gfurnish`, root | 11 |

Take one author at a time. Their file conventions differ -- Dan's are prose notes with
transcripts, `anton` settles files by moving them into `RESOLVED/` rather than writing an issue
number down -- and switching between them means relearning the format every few rows.

The mix of kinds differs too, and it decides how a bucket feels. **390 of Dan's 410 remaining are
prose notes, against only 20 reproducers**; Mike's 207 are 139 reproducers to 68 notes. So Dan's
remainder is read-and-verify work where `autorun` says nothing at all and every verdict rests on
running the claim yourself, while Mike's will be slower per row with the `autorun` caveat above
applying to most of it. `dan/0.4`–`0.9` was 28 notes and 0 reproducers, which is what the rest of
Dan looks like.

To list a bucket:

```sh
awk -F'\t' 'NR>1 && $2=="dan" && $3=="1" && $7=="todo" {print $1}' catalog.tsv
```
