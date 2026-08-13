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

**Knowing this is not enough; it happened again while triaging `1-TAGS`.** The tell is worth
recording because it does not look like a directory mistake. Earlier greps in the same session
had worked, so `git grep -ln TAGS -- '*Makefile.in'` returning nothing read as "there is one TAGS
target and I have found it" -- a narrowing, not a failure. What exposed it was an unrelated hit
in the issue cache: #4233 is titled `Argument list too long making TAGS file` and quotes
`make -C Macaulay2/m2 TAGS`, naming a target the grep had just denied existed. Re-run from the
root, there are six.

So the practical rule is stronger than "cd first": when a `git grep` narrows a claim rather than
widening it, check `pwd` before believing it. And an outside source that contradicts a negative --
an issue, a changelog, a filename -- is worth more than the negative, because a false empty result
cannot contradict anything.

**It happened twice more in one batch, and it is not only `git grep`.** `git log -- <path>` takes
the same cwd-relative pathspec, so `git log --oneline -- M2/libraries/factory/Makefile.in` run from
`files/bugs/dan/` printed nothing and exited 0 -- a file with a dozen commits reading as untouched.

What exposed it is worth having, because it is available in the same command: **`git show
<sha>:<path>` is root-relative, and `git log -- <path>` is cwd-relative.** They were in one shell
invocation and disagreed -- `git show 33f14ffb23:M2/libraries/factory/Makefile.in` printed
`VERSION = 4.0.0+m4` while the `git log` over the same path said the file had no history. A path
that yields content but no commits is not a quiet file; it is the wrong cwd. Any pair of git
commands where one is root-relative and the other is not will catch this for free.

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

There is a second trap, and it is the bigger one. An `end` line halts an M2 script *and
still exits 0*. Of the 229 reproducers, 48 park the actual demonstration after an `end`
so it can be pasted in by hand -- `bugs/anton/MISC/standardPairs.m2` runs three lines, hits
`end`, and never reaches its `assert`. Counting that as a pass would be wrong, so those are
recorded as **`pass-partial`**, leaving 37 that exit 0 having actually run to completion.

(A leading `restart` needs no such treatment: under `--script` it is a no-op and execution
continues.)

**The predicate for that was wrong for a long time, and the shape of the error is worth having.**
`truncated()` tested `lines[i] == "end"` -- an exact match on the stripped line -- so `end--`,
`end;` and `end -- paste the rest in by hand` were all invisible, and eight reproducers were
recorded `pass` having run nothing. Three were in `LAcore` and five in `anton`, including three
memory-leak demonstrations, which is the worst place for it: a leak demo that never ran looks
exactly like one that ran clean. It is now `^end\b\s*;?\s*(--.*|-\*.*)?$`, and `bin/selftest`
carries both directions -- the variants above must match, and `endPackage`, `endl`, `ending` and
`end3` must not, since those are ordinary identifiers appearing constantly.

Two general points, both of which have now cost something here. **A predicate over text needs its
negative cases tested, not just its positive ones** -- `lines[i] == "end"` is obviously right for
the case you have in front of you and obviously incomplete the moment you write down a second one.
And **`--reclassify` is the cheap way to find out how far a classification error reached**: it
redoes the split from the files alone without rerunning M2, so after any change to `truncated()`
the honest move is to run it and read the count.

So: `fail` means *a human should read this*; `pass` means *the script no longer trips, which
might be a fix or might be that the assertion stopped being checked*; `pass-partial` means
*almost nothing*. None of them writes a verdict. `bin/run-repros` puts failures whose output
looks like API drift in `cache/stale-candidates.txt`, and the full output of every run in
`cache/runlog/<path>.log`. `bin/run-repros --reclassify` redoes the `pass`/`pass-partial` split
from the files alone, without rerunning M2.

Reproducers run with a 4 GB address-space cap (`--memory`), because several of them are
memory-leak demonstrations that allocate without bound.

### `--script` and stdin fail in opposite directions, so neither is a safe way to re-check

`bin/run-repros` uses `M2 --script`. That is the right choice, but it has one blind spot, and the
obvious workaround has a worse one. **Do not switch a row to stdin without checking for `restart`.**

- **Under `--script`, `oo` is not populated.** It is a bare `Symbol`, so `assert( degree oo == {7} )`
  dies with `no method found for applying degree to: oo (of class Symbol)` — a failure that says
  nothing about the bug. `tests/normal/overflow.m2` fails this way and passes when fed on stdin, which
  is how the test harness runs it; its own `compile-command` comment names an `overflow.out` target.
  This is narrow in practice: of the thirteen `todo` rows whose `autorun` is `fail` and which mention
  `oo` at all, exactly **one** fails under `--script` and succeeds as an input file.
- **Fed on stdin, a `restart` silently ends the session with exit 0.** Nothing after it runs — not the
  rest of the file, not an `error` placed there deliberately — and the exit status is clean, so it
  looks like a pass. **109 of the 229 reproducers carry an uncommented `restart`**, because `restart`
  is idiomatic in M2 and these files are working transcripts, so this is the common case rather than
  the exception.

Under `--script` a `restart` is a documented no-op, which is why the harness is safe as it stands and
the trap is only in the re-check. If a row needs `oo`, the choices are to run it on stdin *after*
confirming it has no `restart`, to comment the `restart` out and accept that anything depending on a
fresh session — `needsPackage` after a reload, for one — may then fail for a new reason, or to rewrite
the `oo` line. Say in the `note` which you did.

The general shape is the one this file keeps returning to: **an exit status is a measurement of the
harness as much as of the code.** A clean exit deserves the same suspicion as a suspiciously fast
benchmark, and `bugs/mike/1-yang.sing` is the extreme case — undecoded quoted-printable, so
`groebner` had nothing to compute, and the file's own timing line printed `0`.

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

**Fixed?** `verdict=fixed`, the commit or PR in `fix`, and `disposition=drop`.

In practice that is the only answer a fixed row gets. `test` names a destination under
`M2/Macaulay2/tests/normal/` and exists in the vocabulary, but across 741 settled rows it has been
used **zero times**, including on the fourteen `anton/*/RESOLVED/*.m2` reproducers where it looks
most tempting. Promoting a reproducer is writing code in the Macaulay2 sources, which is not what
this branch does, and recommending it per-row invites exactly that confusion -- the recommendation
reads as a task. If a fixed reproducer really is worth keeping, say so in the `note` and leave the
column at `drop`; deciding the fate of that class is one pass for a maintainer, not a field on 857
rows.

**It has already happened twice without this branch's help, which is the other reason not to
recommend it.** `bugs/mike/1-mingens-subquotient.m2` *is* `tests/normal/mingens2.m2`: Dan added
assertions to the bug file in `d92f2512eb` (2008-02-05, "adding assertions to a bug report file,
it's becoming a test file now that the bug is fixed") and copied them into the test suite two days
later in `f5b4e85d62`. `bugs/mike/1-joswig`'s non-terminating half went to
`tests/goals/joswig.m2` in `1b8a3b8e46`, a week after `03cd92424c` fixed its crash. So when a
reproducer was worth keeping, the maintainers moved it at the time; a `test` disposition twenty
years later is proposing work that the record shows already gets done when it is warranted. Both
of those rows are settled `fixed` with the promoting commit in `fix` and `disposition=drop`, which
is the pattern to follow.

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
`distributions/deb`. **That was wrong, and by a wide margin.** Of the 741 settled:

| verdict | | | at 327 |
| --- | ---: | ---: | ---: |
| `fixed` | 363 | 49% | 42% |
| `open` | 133 | 18% | 27% |
| `wontfix` | 103 | 14% | 9% |
| `duplicate` | 70 | 9% | 11% |
| `obsolete` | 62 | 8% | 11% |
| `stale-repro` | 10 | 1% | -- |

So `obsolete` and `wontfix` together are 22%, not "most", and the largest single outcome by far
is that the bug was quietly fixed years ago and nobody closed the file.

**The claim that used to sit here -- that the shape had held steady to within a point at every
count from 167 settled onward, so later buckets were unlikely to move it -- did not survive.** The
last column above is what it looked like at 327. `fixed` has gained 7 points and `open` has lost 9,
which is the largest movement in the table and in the direction that matters most, since `open` is
the column that turns into other people's work. Two identifiable causes, both of them about method
rather than about the files:

- **Rows that would once have been filed are now being measured first.** `bugs/mike/1-local-bug.m2`
  and `bugs/mike/1-mult-trun` were both written up as filable and both ended `wontfix` after a trace
  and a benchmark respectively -- see [the swell note](#a-slow-local-normal-form-can-be-expression-swell-not-a-defect)
  and [the parked-wishlist section](#the-wishlist-files-are-parked-on-purpose). `wontfix` gaining 5
  points is mostly this.
- **The duplicate search got wider.** Adding the comment search -- `gh search issues`, which the
  cache cannot do -- moves rows from `open` to `duplicate` that a title scan called new.

The general lesson is the one this whole file keeps relearning: a statistic that has been stable
for a while is not thereby a law, and "the next bucket is unlikely to move it" was a prediction
dressed as an observation. Quote the figure with the count it was measured at, as the table now
does, and let the next reader see the drift.

Grepping the *unsettled* files says the same thing rather than merely reflecting which ones got
done first: of the 704 still `todo` at 167, only 33 mentioned any retired subsystem at
all, and of 25 that looked like candidates, 14 held up. The dead-platform material is a real
seam but a thin one.

One caution on those numbers, weaker than it used to be. They were not a random sample while the
settled set was `dan/0`, `dan/0.1`, `dan/0.4`–`0.9`, the start of `dan/1` and a deliberate sweep for
retired subsystems -- `dan/0` being Dan's own highest-priority bucket, which may well be where the
real bugs that later got fixed are concentrated. At 741 of 857, with `dan`, `anton`, `LAcore`,
`gfurnish` and the root files complete, the remaining bias is one author: everything left is
`bugs/mike`, which is 83 reproducers to 33 notes and so likely to move `fixed` and `stale-repro`
rather than the prose-note verdicts. And `fixed` at 49% is itself a finding about the tree rather
than about the files: it means the common case is reading a fifteen-year-old report, running it, and
finding it simply works now.

A third caution the `0.4`–`0.9` bucket added: `fixed` is not the same as *fixed on purpose*. Of
its eleven `fixed` rows only five carry a pointer at all, and two of those settled the file's ask
as a side effect -- #772 reversed `Tally` and `VirtualTally` while fixing #690, and #3983 got the
source rpm by rewriting the packaging script. The other six simply drifted into correctness with
no identifiable commit: an API grew a new spelling, a check stopped firing, a doc node was
written. Do not read the 49% as a record of anyone responding to these files.

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
- **Search the bare stem, not the call form.** `1-dispatch` survived *two* searches that used
  `dispatch(` and `lookup(` before a third, using bare `dispatch`, turned up
  [#1477](https://github.com/Macaulay2/M2/issues/1477) — titled "**Dispatching** of lift and
  promote", open, and asking the same question from the implementer's side. Attaching the paren
  makes the term precise and useless: issue titles are written in English, so they inflect
  (`dispatching`, `dispatched`) and rarely quote a call. Grep for `dispatch`, then narrow.
- **Refresh the cache first.** `bin/fetch-issues` without `--refresh` is a no-op if the file
  exists, so the cache silently ages. Ours sat at #4558 while #4560 had already been filed *from
  this catalog*, which is the worst case: the issues most likely to duplicate a row you are about
  to file are the ones you filed last week. `bin/fetch-issues --refresh` takes seconds.

A term that is also an ordinary English word is where this is thinnest. `about` matched 131
issues while checking `0-doc-Keywords`, which is a haystack, not a shortlist — narrowing found
[#3689](https://github.com/Macaulay2/M2/issues/3689), but only because someone read all 131.

The same scan also has to look past the `issue` column. Once a row is settled as a duplicate its
`issue` is repointed at the older issue — `0-sort-doc` now reads `#101` — so the issue that was
filed *from* that row is no longer named anywhere except the free text of `note`. A sweep keyed on
the column alone will not see it.

### Search your own catalog too, not just the tracker

The duplicate search has a second target, and it is easier to forget because it is not public.
Two bug files can hold the same ask under unrelated names, and if one of them is already settled,
filing the other contradicts your own record.

`1-Package-dictionary` asks for `PackageDictionary` to be moved ahead of the other dictionaries on
`dictionaryPath`. It reproduces exactly, so it was written up as `open` and queued for filing. But
`0-synonyms-and-collisions` -- settled `fixed` in an earlier batch, filed under David Eisenbud's
`viewHelp Schubert2` symptom -- states the same thing as its own ask (1): *"we should : (1) put the
package symbols at the head of the list ; (2) alert the user to the shadowing."* Its note already
recorded that (2) was built instead and that reordering lookup globally is the bigger change. So
the second row had to become `wontfix`, not a new issue.

Nothing in a keyword search over `cache/issues.json` would have caught that, because the collision
was with a *bug file*, not an issue. Before filing, grep `catalog.tsv` for the subsystem as well --
`awk -F'\t' 'tolower($11) ~ /dictionarypath/'` -- and read the notes on any settled row that comes
back. The `note` column is the only place that reasoning lives.

The same check has a third leg: **rows still `todo` can hold the ask too.** `1-value-dictionary-path`,
`1-setup`, `1-setup-announcement` and `1-setup-emacs` were all unread when this batch was written
up, and any of them could have been the same request. They were not -- a `value` variant taking a
dictionary path, and three unrelated `setup()` asks -- but that was worth two minutes to establish
rather than assume.

### Search before you write the verdict, not after

The order matters more than it looks, and the tooling does not help you: `bin/suggest-issues`
shortlists only rows still `todo`, so the moment you type a verdict the row drops out of its
output and you are on your own.

Search-after fails twice over. It costs the work — three rows in one batch were written up as new
issues and two turned out to be tracked already (`1-copyFile` as
[#419](https://github.com/Macaulay2/M2/issues/419), `1-dim` as
[#3557](https://github.com/Macaulay2/M2/issues/3557)) — and worse, a note written to justify
filing is a note written to argue one side. Rereading it while holding a candidate duplicate is
not a neutral comparison.

Searching first also changes what you look for. `1-dim`'s note claimed all four of Bart Snapp's
examples failed identically; they do not, and the batched `try/else` that produced that claim
would probably have been written more carefully by someone who already knew #3557 existed and was
looking for the seam between them.

So: read the file, search the tracker and `catalog.tsv`, *then* decide. The verdict you write will
be about where the row sits among what already exists, which is what the `note` is for.

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
and those take a subsystem label alone. Of the 77 labelled so far, 33 came out `bug`, 26
`feature request`, 18 neither.

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

## Bring one row at a time, and explain the mechanism

**Ask about one potential issue per question, not a batch.** A summary of nine verdicts with a single
"shall I file these?" gets rejected, and rightly: the maintainer is being asked to underwrite nine
separate claims at once, and the one that is wrong is invisible among the eight that are right. Every
time a batch has been presented whole here, the reply has been to take them individually.

Two things make the question answerable:

- **State the mechanism before the choice.** Not "`coimage` is missing options, file it?" but: `coimage
  RingMap` is one expression over `kernel f` at `ringmap.m2:375` with no option table, so `kernel`'s
  `DegreeLimit`, `SubringLimit` and `Strategy` cannot be reached through it. A verdict is only as good
  as the reason under it, and the reason is what is being checked.
- **Say what the measurement does *not* establish.** `1-res-slow-andreic.m2` measures 2192 s against a
  reported 4 minutes in 0.9.2 — but that regression predates both surviving code paths, so nothing
  here can confirm it. Volunteering that is what lets a maintainer stop you cheaply.

The corollary is that the answer arrives per row, so **do not carry one row's answer to the next**.
"File it" for the row in front of you is not "file the rest", and see
[`--apply` is not yours to give yourself](#--apply-is-not-yours-to-give-yourself) for the separate
question of whether to publish at all.

Where a row's symptom is time or memory, the question to answer before asking anything is *is this
slow because the problem is big, or because it is a bug?* — see
[a slow local normal form can be expression swell](#a-slow-local-normal-form-can-be-expression-swell-not-a-defect).
Across three batches, every such row settled `fixed` or `wontfix`, while both genuine defects found in
the same span — [#4632](https://github.com/Macaulay2/M2/issues/4632) and
[#4633](https://github.com/Macaulay2/M2/issues/4633) — came from reading the source, in minutes. Give
the slow rows one bounded run, then decide.

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

**Three ways "each time" has been read too loosely, all of them in one session.**

- **The gate is wider than `--apply`.** It covers anything that reaches GitHub: `bin/push-project
  --apply`, `bin/file-issues --apply`, `bin/comment-issues`, a hand-run `gh issue create` for a
  finding with no row (#4556, #4574, #4575), and `git push` of this branch -- which matters because
  `project.CATALOG_URL` points every pushed triage block at it, so the branch *is* the public
  source of truth those issues cite.
- **A go-ahead does not carry to the next batch.** "Let's do the next batch" authorizes triage, not
  filing. Being told "go ahead and do all 5" once is about those five runs, on those rows.
- **Confirming a verdict is not confirming publication.** Agreeing that a row is `open` and worth
  filing settles what `catalog.tsv` should say. It does not settle whether to convert the draft
  today. Those are two questions and they get asked separately, because the first costs nothing to
  get wrong and the second costs a stranger's attention.

The failure mode is not forgetting the rule -- it is having the answer to a *different* question in
hand and treating it as this one. Reading the dry run privately and acting on it is the specific
shape to watch for: it satisfies the lower half of the bar and skips the half that matters.

**Nothing records that a row has been pushed, on purpose.** Every run diffs the TSV against live
board state and queues only what actually differs, so after an applied push the same command
reports nothing to do. A `pushed` column would go stale in both directions -- revise a note and
forget to clear the flag and the board silently keeps the old text; hand-edit the board and the
flag claims a sync that no longer holds. The derived check cannot lie, and it re-converges on the
next push. `./bin/push-project --check` answers the question tersely and exits 1 when the board is
behind.

## Four ways an existence check lies to you in M2

Every verdict here turns on "does this exist / does this still happen", and M2 has a small family
of ways to answer that question wrongly. All four of these produced a wrong reading in one batch
of thirteen files, and each looks like a clean result. The first one went on to produce two more,
in later batches, after being written up here -- see the note under it.

**A bare symbol evaluates to itself.** `try (clearCache) else "missing"` prints `clearCache` and
takes the success branch, whether or not anything is defined. `clearCache` is in fact an unbound
symbol -- the only definition in the tree is a method on `BasicDivisor` in `WeilDivisors`. Test
`class` (`Symbol` means unbound) or `# methods f`, never the bare name.

> **That advice is necessary and not sufficient, and following it still produced a wrong verdict.**
> `1-degreeLift` asks that the degree-lift function be computed automatically. `class degreeLift`
> is `Symbol` with zero methods, so the row went down as unmet — but the feature is not a global
> function of that name. It is a derivation inside `map`: `ringmap.m2:97-104` builds a matrix from
> the `DegreeMap` and solves `quotientRemainder` against it, storing the result in
> `f.cache.DegreeLift`. The ask was satisfied years ago under a different shape.
>
> An unbound symbol tells you a **name** is free. It tells you nothing about whether the
> **capability** exists, because features arrive as methods on existing functions, cache entries,
> options, or package exports at least as often as they arrive as new globals. When the name comes
> back unbound, search the source for what the file *wanted* before concluding it is missing —
> here, `git grep -n DegreeLift -- M2/Macaulay2/m2/` answers it in one line.

**`--script` turns off `debuggingMode`.** `loadPackage` warns about shadowed symbols only when it
is set (`packages.m2:88`), so a scripted check on `1-Package-dictionary` showed a clean load and
read as fixed; interactively, the file's whole transcript reproduces. Anything whose symptom is a
*warning* has to be run interactively -- pipe into `M2 --no-readline -q`.

**Inspecting a precondition is not calling the function.** `Posets#"test inputs"` is empty after a
plain `loadPackage`, which reads as `check` still being unable to find tests. But `check` loads the
documentation itself: call `check(0, "Posets")` and it reports `-- warning: reloading Posets`, runs
the test, and all 36 become visible. The ask was about what `check` does, so `check` had to be the
thing invoked.

**Your variable name may be taken.** `ch = chi L` fails with *assignment to protected global
variable* because `Schubert2` exports `ch`; the same happened with `dd` (`Complexes`) and `c` (used
as a ring variable two lines earlier). The error is clear, but it aborts the script before the test
runs, so a batch of checks can come back empty for a reason unrelated to any of them. Prefix scratch
names.

The shape they share: each yields output that is consistent with the hypothesis you are testing, so
nothing in the result signals that the test was invalid. Prefer a check whose failure mode is
visibly different from its success -- `class`, a count, an actual call -- over one that returns
something either way.

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

### Do not park a row just because it resists a verdict

There was nearly a second class here. Three single-ask files from `dan/1` looked filable but had no
state in which anyone could say they were done, so all three were written up `open` with blank
`disposition` and a note explaining why they could not be filed.

**All three dissolved when the maintainer looked at them, and none dissolved the way the note
predicted** -- which is why the class does not exist and this section is a warning instead.

- `1-caching-idea` was parked as an undecided two-design thread. But its opening sentence --
  "our description of how to remove a cached GB is no longer correct" -- is a live documentation
  defect, still in `ov_groebner_bases.m2:439`, and filable on its own. Filed with both halves as
  #4558.
- `1-configure-library-versions` was parked because "recent" has no definition. It is `fixed`:
  where a version matters, configure checks the version *or the feature that version brought*, and
  the second is the normal case -- 107 feature-test invocations against 2 version gates. Counting
  version comparisons was the wrong measurement; a low count is what a good configure script looks
  like.
- `1-cookies` was parked over its unreadable second clause. It is `wontfix`: `getWWW` already
  shells out to `openssl` for TLS, `ReflexivePolytopesDB` offers `Access => "curl"` as a documented
  escape hatch, and no consumer wants a cookie.

So the lesson is not "park the ambiguous ones". It is that a row which resists a verdict usually
has a specific reason, and naming the reason -- undecided design, wrong measurement, non-goal --
settles it. Reach for blank `disposition` after that fails, not instead of it.

**Writing that down did not stop it happening.** Nine more rows were parked over the following
batches, four of them after this section existed, until the maintainer asked why. Each carried a
plausible-sounding excuse; none survived an hour's work:

| row | the excuse | what it actually was |
| --- | --- | --- |
| `1-degreeLift` | "no such function -- the symbol is unbound" | **`fixed`**: `map` derives the lift at `ringmap.m2:97-104` |
| `1-d-translator-crash` | "confirming it would need a rebuild" | **filed as #4562**: `scc1` was already built in `BUILD/build` |
| `1-dispatch` | "no clear spec" | **duplicate of #1477**, found on the third search |
| `1-dismiss-rings` | "semantics are the whole question" | **`wontfix`**: `x = symbol x` already does it |
| `1-TAGS`, `1-directSum` | "marginal", "nobody has wanted it" | **filed as #4561, #4563** -- small and concrete |
| `1-containment`, `1-doc-startup`, `1-doc-top` | various | `wontfix`, with the reason stated |

The excuses share a shape: each describes *my* state, not the row's. "Not verified", "no clear
spec", "marginal" are all ways of saying **I stopped**, dressed as properties of the file. A row
genuinely has no closing condition only after you have looked for one; three of these needed a
single command to settle, and `1-d-translator-crash`'s excuse was simply false.

Two practical guards:

- **Before parking, name the specific next action and why it is unavailable.** "Needs the package
  installed twice, once bundled and once by the user" is a reason. "Not verified" is not — it
  restates the verdict.
- **`wontfix` is available and underused.** Four of the nine were `wontfix` all along. It carries
  a real claim — deliberate behavior, or the need met another way — and it is a *decision*, which
  blank `disposition` is not. Where the call is a judgment rather than a finding, say so in the
  `note` (`1-doc-top` does) so a maintainer can overrule it cheaply.

Parking is for [the wishlist files](#the-wishlist-files-are-parked-on-purpose) — one row holding
many unrelated asks — and nothing else has joined them.

## A fix can be taken back by a commit that was not about it

`1-document-options-error-msgs` asks, in its second half, why the html page for
`symmetricAlgebra` carries no links to its optional arguments. Searching for the commit that
answered it turns one up immediately, and it is unambiguous: `f1c1dd78f1` (2007-12-23) moved every
`[symmetricAlgebra, Opt]` key into the node's `Key` list, in direct response to this file, and the
options rendered.

Thirteen months later `9be2d70841` ("documentation; fix up `symmetricAlgebra`") restructured the
node into three `SYNOPSIS` blocks and deleted its top-level `Usage`. `processUsage` returns early
without one (`document.m2:545-548`) and `SYNOPSIS` has no `Options` slot at all (`:758-768`), so
the option list vanished — and has stayed gone for seventeen years. Filed as #4566.

Stopping at the first commit whose message matches the ask would have recorded `fixed` on a page
that has been broken since 2009. Two habits:

- **Check whether a later commit took it back.** `git log --oneline -- <the file>` after the fix
  date costs one command, and a doc node restructured by someone tidying up is the likeliest way
  for a small fix to be undone silently.
- **Verify the rendered result, not the presence of the fix in the source.** All 18 keys are still
  in `symmetricAlgebra-doc.m2:12-23` and every one resolves; `isMissingDoc` is false for all of
  them. Reading the source says fixed. Running `help symmetricAlgebra` says there is no "Optional
  inputs" section, and `help newRing` — a node that kept its `Usage` — shows what it should look
  like. The contrast is the whole finding.

## Order the transcript the way the file wrote it

`1-dot-dot-2` is eight lines: `X11 .. a` errors, then `vars(-11)` returns `X11`. Checking it meant
running two lines, and I ran them in the convenient order — `vars(-11)` first, then `X11 .. a`,
which **works**. That read as a clean `fixed`.

It works only *because* `vars(-11)` ran first. `vars ZZ` memoizes into a mutable table
(`indeterminates.m2:15-18`), which is the one thing that lets `reverseVars` invert a name it
otherwise cannot. In a fresh session, in the file's own order, it reproduces exactly. Filed as
#4570.

This is a fifth member of the family in
[four ways an existence check lies](#four-ways-an-existence-check-lies-to-you-in-m2), and the
shared shape holds: the output was consistent with "fixed" and nothing in it signalled that the
test was invalid. Where the subject is a cache, a memo table, or anything else that a previous call
populates, **run the file's transcript in the file's order, in a fresh session.** The bug file's
line numbers are evidence; `i10` before `i11` is part of the report.

## Check that the wording being asked for is not narrower than the wording that is there

`1-documentation-installPackage` asks that the Synopsis change from `installPackage PACKAGENAME`
to `installPackage "FOO"`, with the input renamed to `"FOO", a String`. That is easy to settle as
`wontfix`: `lookup(installPackage, Package)` is non-null, `installPackage FirstPackage` is a valid
call, and the node documents both `(installPackage, String)` and `(installPackage, Package)`. The
proposed wording documents one of two spellings. Ask denied, on the file's own terms.

That is the [ask-versus-need](#the-ask-is-the-mechanism-the-need-is-what-got-met) error run
backwards. Checking that Dan's exact sentence would be worse is not the same as establishing that
the page is fine. The need — a reader should be able to tell the argument is normally *quoted* —
is live, and the house style already accommodates it: of 260 `Usage` lines in
`Macaulay2Doc/functions/`, 248 are bare metavariables and the twelve containing quotes all quote a
*placeholder*, precisely where quoting matters:

```
export {"symbol1", "symbol2", ...}
exportFrom(pkg, {"symbol1", "symbol2"})
```

So `installPackage "PackageName"` is in style, conveys the quoting, and narrows nothing. Filed as
#4568. The test is cheap and worth running on any row that proposes specific words: **is the
proposal narrower than what is there, and if so, is there a version that is not?** A row can be
wrong about its own remedy and right about its complaint.

## A row can belong to another repository's tracker

`1-emacs-macro-needed` wants an `f11` variant that strips a trailing `;` before sending the line.
It is genuinely unmet — nothing in the mode does it, and the only semicolon logic *inserts* one —
but `git ls-files '*.el'` returns nothing here. `M2.el` left this tree in `78186879eb` (2020-06-24)
for [M2-emacs](https://github.com/Macaulay2/M2-emacs), which has its own active tracker.

`wontfix` would have been a lie about merit, and blank `disposition` is
[parking](#do-not-park-a-row-just-because-it-resists-a-verdict). So it was filed *there*, as
Macaulay2/M2-emacs#102, by hand — `bin/file-issues` targets Macaulay2/M2 and must not be pointed
elsewhere. The row reads `open`, `disposition=issue`, `issue=Macaulay2/M2-emacs#102`, and
deliberately has **no line in `issue-titles.tsv`**, which is what stops it being filed here later
by a bulk pass. The `note` says so.

Recording it needed a tooling fix, and the bug it exposed was worse than cosmetic. `linkify`
matched `#(\d+)` anywhere, so `M2-emacs#102` rendered as a link to *Macaulay2/M2* issue 102 — which
exists, is unrelated, and is somebody else's. It now understands GitHub's own `owner/repo#123`
form, requiring the slash rather than guessing at a bare repo name (`project.FOREIGN_REF`), and
`bin/render` and `bin/push-project` share one copy instead of two.

The same fix caught a link that had been wrong on the board for weeks: `0-toString-Vector`'s note
says the fix "applies it to `v#0`, the underlying matrix", and `v#0` was being published as a link
to issue 0. A bare `#N` now needs no word character in front of it (`project.ISSUE_REF`). **When
a reference format is generated rather than typed, test it against text that merely looks like
one** — M2 code is full of `#`.

#### The subscripted thing is an expression, so a word character was too narrow

That guard shipped covering `v#0`, and it was not enough: `(x+1)#0` and
`((value getGlobalSymbol "fourierMotzkin") A)#0` both slipped through it, and the second was live
on the board for weeks. Obvious in hindsight — M2 subscripting is `<expression>#<integer>`, and an
expression ends in a closing bracket as readily as in a name. `)`, `]` and `}` are excluded now
too, and nothing is lost by it, because a real reference never follows a closing bracket with no
space; `(#2130)` keeps its link, since what precedes the `#` there is `(`.

Two things this cost that were avoidable:

- **A regression test would have been cheaper than the second occurrence.** There is one now,
  `bin/selftest`. It carries both error modes — text that merely looks like a reference must not be
  linked, real references must not be missed — with cases taken from notes that actually exist
  rather than invented. It fails against the pre-fix regex on three of them, which is the check
  that it is guarding something.
- **The sweep is one command and finds every instance at once.** Rather than fixing the one the dry
  run showed, ask what else matches:

  ```sh
  awk -F'\t' 'NR>1 && $7!="todo" {print $1"\t"$11}' catalog.tsv | grep -oE '[^A-Za-z0-9_ ]#[0-9]+'
  ```

  That turned up the `1-packages` one, from a batch long since pushed, which reading the current
  dry run could not have shown because that row was already in sync.

One known gap left deliberately: `/` is in the exclusion class, so in a slash-separated list like
`#4576/#4578/#4583` only the first becomes a link. Three notes are spelled that way. Widening the
class would start linkifying URL fragments, and the numbers are still readable, so it stays.

### A quoted title defeats that guard, because English puts a space before the `#`

The `v#0` fix keys on the character *before* the `#`: a word character means code, so leave it
alone. That covers `v#0`, `sym#1`, `commandLine#0`, `LLLBases.Dictionary#"LLL"` — everything the
rule was written for. It does not cover the other way a `#N` arrives without being a reference,
which is **inside a quoted title**, where the preceding character is an ordinary space.

A note in `2-doc-command-line-options` cited the commit that fixed it as *"PR #3745, 'Organizing
Macaulay2Doc #5'"*. The `#5` is part of somebody's PR title — the fifth in a series — and it was
published as a link to Macaulay2/M2 `#5`, which resolves: Anton's merged pull request *"ARingRRR
tests and promote"*, unrelated to anything in that note. (`/issues/5` redirects to `/pull/5`, so a
generated issue link lands on a PR without complaining.)

It is exactly the `M2-emacs#102` failure from the opposite direction: there the guard was too loose
about what precedes a `#`, here the text genuinely looks like a reference and no guard can tell.

So this one is not fixable in `linkify`, and two habits stand in for it:

- **Read the dry run's rendered block, not the TSV.** The note in `catalog.tsv` is plain text and
  looks fine; the link only exists after `linkify` runs, so `bin/push-project` without `--apply` is
  the only place it is visible. The same batch's `#4569` title had been misquoted with an invented
  word in it, and that too was only caught by reading the rendered output next to `gh issue view`.
- **Paraphrase a title containing a `#N` rather than quoting it** — "the fifth of the Organizing
  Macaulay2Doc series" carries the same information and cannot mislink.

The general form is worth stating once, because both halves have now cost something: a reference
detector has two error modes, and testing it only against *code that resembles a reference* finds
one of them. Also test it against *prose that contains a real reference to something else*.

## Reproducible documentation is a constraint on how errors may be formatted

`1-error-file-paths` asks for absolute paths in error messages, "because the notion of current
directory is not prominent for" some users. It is unmet: `tostring(Position)` (`d/stdiop.d:113-122`)
runs the filename through `minimizeFilename`, which keeps whichever of as-given, relativized or
absolutized is *shortest*, so an absolute path handed to M2 is discarded. It reproduces, and the
by-product is worse than either option -- with cwd an ancestor the leading slash is dropped, so from
`/` even Core prints `usr/share/Macaulay2/Core/startup.m2`.

I wrote it up `open` on the strength of an argument that turned out to be backwards. `f75c83343a`
(2020-06-03) had made positions always relative *for reproducible builds*, and was reverted 13 days
later; I reasoned that the reason was obsolete because `reproduciblePaths` now scrubs example
output. It is the other way round. `reproduciblePaths` is a textual pass keyed to prefixes it knows
(srcdir, builddir, homedir); the relativizing in `minimizeFilename` is what keeps a builder's
absolute paths out of the error text **in the first place**. Make paths absolute and every doc page
that shows an error carries the path of whoever built it.

The general shape: **example output is published, so anything that decides what an error message
says is a reproducibility interface**, not only a usability one. Before proposing a change to
message formatting, ask what it does to the ~8500 `.out` files in the distribution. And when a
revert looks like collateral damage, check whether the reverted behavior was also load-bearing for
the reason it was introduced -- `9de2bbbe7a` reverted the relativizing as a side effect of an
unrelated build-directory revert, and the original reason survived the accident.

## Test the workflow, do not read it

`1-example-rerunning` asks that examples be rerun when the package source changes. I read
`installPackage.m2:561` -- `inputhash := hash inputs` over the example text alone -- and wrote it up
as unmet on that basis. The maintainer's answer was that examples *do* get rerun during development,
which sounded like a flat contradiction and was not: editing an example changes its text, so it
reruns. Two different cases, and prose about a hash cannot tell them apart.

What settled it was three installs of a six-line throwaway package:

```
installPackage #1, zzAnswer = () -> 111   ->  hash: 1332353094583   o1 = 111
edit source to 999, leave the doc alone   ->  hash: 1332353094583   o1 = 111   <- stale
add "1+1" to the Example block            ->  hash: 1731836519991153110       o1 = 999, o2 = 2
```

The third line is the part reading could not have produced: the corrected `999` had been sitting
there unpublished for a whole install cycle and surfaced only because an unrelated line was added.
It settled `wontfix` -- `RerunExamples => true` is the supported answer and a source-keyed hash
would invalidate every `.out` in the distribution -- but the *reason* is now a measurement rather
than an inference.

A throwaway package under `InstallPrefix => "scratch/"` costs about a minute. Any row about
`installPackage`, `check`, example caching or documentation building deserves one before a verdict
is written, and certainly before disagreeing with someone who runs the workflow daily.

## When a finding's owner has not been triaged yet, file for the cause

Checking `1-frac-tower` (`fixed`) showed that `frac` accepts a ring that is not a domain, giving
one where `(t^4)*(1/t)` is `0` while `t^3*(t*(1/t))` is `t^3`. Grepping `catalog.tsv` for the
subsystem -- the [search-your-own-catalog](#search-your-own-catalog-too-not-just-the-tracker) leg --
turned up `bugs/mike/0-frac-bug`, whose opening line is "Notice the nilpotent denominator in o5
below" over that very ring. Still `todo`.

The tempting move is to leave the finding in a note and let that row own it when Mike's bucket comes
round. **That is the wrong direction, and the reason is specific:** the row's own transcript is
*fixed* -- its `product l` is now `1`, not a fraction with denominator `t` -- so whoever reached it
next would have had every reason to settle it `fixed` on the symptom and the cause would have gone
with it.

So file for the cause (#4576) and record the untriaged row as its duplicate, out of turn, saying in
the note that it was out of turn and why. The general rule: **when a row's symptom is fixed but its
cause is not, the row is the least reliable place to park the cause.**

## A performance number that looks too good is a broken benchmark

`1-flattenRing-can-save-time` asks for two optimizations and quotes no cost, so it needed a
measurement. The first attempt said 200 ring-map applications took 18 microseconds -- 90 nanoseconds
each, for a call that reaches `rawRingMapEval`. That is not a fast path, it is a broken test:
`random(3, zzB)` over a multigraded tower had not produced the polynomials I assumed, and the loop
was doing nothing. Checking the inputs before believing the timing is what caught it; a second
attempt with an explicitly built 210-term polynomial gave 2.55 ms per application.

The measurement then changed the verdict rather than decorating it. `map(R,R)` satisfies `f == 1`,
and `ringmap.m2:31` already computes exactly that predicate for comparison -- so M2 knows the map is
the identity, applies it at full cost, and hands back its argument. That is a self-contained finding
needing no `flattenRing` at all, and it is what made the row worth filing.

**Before quoting a timing, assert something about the thing being timed** -- that the input is
nonzero, that the loop body ran, that the result is what you expected. A benchmark has no failure
mode of its own; it will happily report the speed of nothing.

### Build the operands outside the loop, or you will time their construction

The same trap in its commonest form, and it manufactured a finding rather than hiding one.
`1-slow-gcd` needed `gcd` timed across several rings, so each case was a closure:

```m2
zzt("gcd(t, t+1) in QQ[t]", 2000, () -> gcd(S1_0, S1_0 + 1))
```

That reported 36.7 microseconds for one variable against 17.1 for two -- `QQ[t]` apparently *slower*
than `QQ[a,b]`, which is backwards and interesting, and was about to be written up as the row's most
suspicious data point. The `S1_0 + 1` is inside the timed closure. Hoisted out, it is 19.9 against
18.7 and the anomaly is gone; the real result is a flat ~6x ratio across every ring, which is a much
duller and entirely different conclusion.

Two things make this worth its own note beyond the general rule above:

- **A comparison across cases is where it bites**, because the per-case setup differs. Timing one
  thing badly gives a number that is merely too big; timing several things badly gives a *ranking*,
  and a ranking is what gets reported as a finding.
- **The tell is an ordering that makes no sense** -- fewer variables costing more, a smaller input
  costing more than a larger one. Treat any such inversion as a bug in the harness until proven
  otherwise, because it almost always is. The same instinct caught the `sort` numbers in the same
  batch, where `toList(0..49999)` had to be hoisted before the 45-second measurement could be
  trusted; there it survived, at 0.008 s against 45.7.

## A slow local normal form can be expression swell, not a defect

`bugs/mike/1-local-bug.m2` was written up as a filable engine bug and it is not one. The measurement
looked conclusive: over `ZZ/101[t,x,y,z, MonomialOrder => Weights => 4:-1, Global => false]`, the
full Gröbner basis of its harder example is 7 elements in 8 ms, and of the 13 lead-term syzygies of
that basis -- every one an element of the ideal by construction, so every remainder must be `0` --
nine reduce in under a millisecond while four exceed 90 s, the minimal case exceeding 1500 s. The
four are `S_2`, `S_5`, `S_8`, `S_11`, **exactly the four the file marks**. Neither input size nor
ecart predicts them: `S_11` is the largest at 133 terms and slow, `S_12` at 121 is fast, and `S_8`
has the lowest ecart of all thirteen and is slow.

All of that is real and none of it is a bug. `gbTrace = 15` settles it in three ways:

- **17,639 reduction steps in 60 s, every intermediate polynomial distinct.** No cycle. A correct
  reduction never revisits a state, and this one does not.
- **The intermediate grows monotonically, 7 terms → 1,160 → 4,019 → 6,951.** Textbook intermediate
  expression swell.
- **Mora's own termination measure descends** -- `h_alpha` goes 6, 5, ... 2 -- while the chosen
  reducer is always `t*z` at ecart 2 rather than `t^7` at ecart 6, which is the correct
  minimum-ecart choice. The algorithm makes legal monotone progress throughout.

The cause is visible in the reducer: `t*z + 4*t^2*z + 3*z^3 + 6*t*z^3` has a tail of *higher* degree
than its lead term, so every cancellation against it introduces terms. The slow four are the
syzygies whose lead terms force repeated reduction by it.

Four things to carry forward, because each of them nearly produced a wrong verdict:

- **Flat resident memory reads as a cycle and is not.** RSS sat at ~300 MB for the whole run, which
  was taken as evidence of a bounded state being revisited. 7,000 terms is a few hundred KB --
  memory cannot see this swell at all. Count terms, not bytes.
- **Ruling out the obvious cost proxies is not evidence of a defect.** Size and ecart were both
  excluded, and that felt like closing in on a bug. It only meant the cost was driven by something
  else.
- **A pattern reproducing exactly across twenty years is not damning; deterministic code makes the
  same choices twice.** The 4-of-13 match with Mike's own annotation was the single most persuasive
  thing in the write-up and it carries no information about correctness.
- **Read the annotation, including its parenthetical.** `S_2 % gb1; -- this one is bad, at least for
  this choice (min alpha, min size)` names the reducer-selection strategy being *tuned*. The file is
  Mike's working notes on that heuristic, which is why it opens by pointing at `test/localgb-nn.m2`
  "for actual tests". It was never a defect report.

In a local order this is expected mathematics rather than a failure: the order is not a well-ordering
on monomials -- `1 > x > x^2 > ...` descends forever -- so reduction has no descending-chain argument
to terminate on, and Mora's ecart restriction is what recovers termination. "Reduction over a local
order did not finish" is therefore never by itself a bug report. Standing wishlist for the subsystem
is #293; the real defects in it are #568 and #569, Mora returning non-minimal generators and a
non-minimal basis.

One practical note from the same row: **`alarm` does not interrupt the engine's reduction**, so
`try (alarm 60; f % gb1) else ...` ran past 79 s and in-process timing is unusable here. That is the
#1392 and #3371 family. Use one process per case under an external `timeout` and read the exit code
-- 124 is a bound you can cite, where a live `ps` reading is not.

## A queue that reports zero cannot tell you which zero it means

The comment on #3887 was written, `bin/comment-issues` was run, and it said `0 comments to post`.
Not because the text was wrong but because `comment-issues:116` skips any row that is not a
`duplicate` naming an issue, and that row is `open` -- its comment belonged on someone else's issue,
which the vocabulary had no way to express. The file was ignored in silence.

`0 comments to post` reads identically whether there is nothing to say or something to say and no
path to say it. So the script now walks `comments/` and reports files no row will ever post, which
is the same derived-check principle as
[nothing records that a row has been pushed](#--apply-is-not-yours-to-give-yourself): compute the
answer from the two sides rather than trusting one of them. It over-reports -- it cannot tell a
hand-posted comment from an unposted one -- and that is the right direction for a reminder to fail.

## Ring shadowing bites the test, not only the subject

`promote(s, zzB)` failed inside a `flattenRing` check with `argument 1 : s (of class zzR)`. Creating
the flattened ring had rebound the global `s` to *its* variable, so the tower's coefficient ring no
longer owned the name -- and because the assignment failed, three later lines reported errors about
a `Symbol` and looked like unrelated breakage.

This is the same phenomenon as #4510, triaged an hour earlier in the same session: a second ring
reusing a symbol takes it over. Knowing it about the *subject* did not stop it happening to the
*harness*. In any check that builds more than one ring over the same variable names, reach for
`R_0`, `R_1` and `(coefficientRing R)_0` rather than the symbols, which cannot be shadowed out from
under you.

## A guard that checks the top of a ring tower is the recurring defect here

Three separate rows in three consecutive batches turned out to be the same mistake, and it is worth
naming as a pattern rather than three coincidences.

- **#4576.** `factoryAlmostGood` (`enginering.m2:335-343`) recurses `QuotientRing -> ambient` and
  `PolynomialRing -> coefficientRing` and tests only what is at the bottom, so `frac` accepts a
  quotient by any ideal and builds a "field" in which multiplication is not associative.
- **#4578.** `flattenRing Ring` tests `k === R` at `newring.m2:198`, but the overriding methods at
  `:221-225` and `:243` test only the *coefficient* ring, so `flattenRing(R, CoefficientRing => R)`
  falls through to `unable()`.
- **#4583.** The guard Dan himself proposed in #321 -- "just give an error if the ring is a quotient
  ring" -- exists at `factor.m2:24`, and checks only the top ring. So `gcd` errors in
  `QQ[a]/(a^2-1)` and proceeds in `(QQ[a]/(a^2-1))[x]`, returning a common divisor that is not
  greatest.

The shape: a ring in M2 is a chain, and a predicate written about "the ring" tends to mean the outer
one. Every such check is worth reading twice -- once for what it tests, once for how far down it
looks. **The practical tell is that the guard fires on the base case and not on one level up**, so
the cheap check is to try both: the quotient itself, and a polynomial ring over it.

## An example can pass by accident, so build one whose components disagree

For `1-gcd-over-separable-extensions` the first counterexample I tried was `(x^2-1)*(x-a)` against
`(x^2-1)*(x+a)` over `QQ[a]/(a^2-1)`, which is `QQ x QQ`. `gcd` returned `x^2-1`, the right answer,
and the row looked met.

It was luck. Under `a |-> (1,-1)` both components of that pair have the *same* gcd, so a single
Euclidean computation that ignores the product structure lands on it anyway. Rebuilding the example
from the idempotents `e = (1+a)/2`, `f = (1-a)/2` so the components disagree --
`F = e*(x-1) + f*(x-2)`, `G = e*(x-1) + f*(x-3)` -- gives `gcd` of `1` where `e*(x-1) + f` has degree
1 and divides both.

The general rule for a row about a structured object -- a product of fields, a tower, a multigrading,
a reducible ideal -- is that **an example whose components agree cannot distinguish an implementation
that respects the structure from one that ignores it.** Choose the inputs so the correct answer is
one the naive computation could not produce, and then verify the better answer really is better
(here: it divides both inputs, and its degree exceeds the one returned).

## Compare against the library, not only against the past

`1-galois-fields` asks whether PARI's finite-field moduli are better than M2's. PARI left the tree
in 2025, so the literal question is unanswerable and `obsolete` was the tempting verdict.

What made it filable was measuring M2 against a library it *already links*. `GF(3,100)` spends 6.5
seconds searching for a dense random irreducible and returns a 67-term modulus that **differs on
every call**; `rawConwayPolynomial(3,100,true)`, which routes to FLINT's `fq_nmod_ctx_init`, returns
a 3-coefficient modulus in 2.2 milliseconds. `ConwayPolynomials.m2:16` hard-codes the flag that would
reach it.

So when a file proposes adopting some external thing and that thing is gone, the question to ask is
not "is the proposal still possible" but **"is the capability it wanted available now, from
something already present"** -- often by a dependency the tree acquired for another reason. The
non-determinism found on the way was the more serious half and would not have surfaced from reading.

## Use `git -C`, because the cwd trap does not yield to vigilance

This README has warned about cwd-relative pathspecs since early on, and added the
`git show`-versus-`git log` tell later. It caught me a **fourth** time anyway:
`git log --oneline -- M2/Macaulay2/m2/hilbert.m2` printed `0`, because an earlier
`cd bug-triage` in the same shell was still in effect. Seventeen commits, reported as none.
Minutes later, writing this very section, `cd bug-triage` failed because the shell was already
there.

The pattern is structural rather than careless. Working here means alternating between
`bug-triage/` for the tooling and the repository root for the evidence, so any rule of the
form "remember which directory you are in" will fail eventually.

**So stop relying on cwd.** Set `R=/home/profzoom/src/macaulay2/M2` and write `git -C $R log
-- <path>`, `git -C $R grep ...`, and absolute paths for scripts. It costs five characters and
removes the failure mode instead of asking anyone to notice it. Three earlier lessons in this
file tried to teach noticing; this one replaces it with a habit that cannot silently fail.

**It is not only `git`.** A later batch ran `./bin/render` and got
`/bin/bash: ./bin/render: No such file or directory`, because the tool call before it had ended in the
repository root rather than here. Same cause, and the same fix extends: set `B=.../bug-triage` and
write `$B/bin/render`, `$B/catalog.tsv`. Every relative path in this directory is a latent version of
this, and unlike a bad pathspec it at least fails loudly.

## Bound anything exploratory, and sweep for strays when the batch ends

A `GF(3,582)` probe from the previous batch was still running **55 minutes** later at 99% of a
core and 261MB, having been reported as "did not finish in 540s" -- because the invocation that
was reported had a `timeout` and the one left running did not. The catalog note and #4582 both
understated the measurement by a factor of six until it was corrected.

Two habits, both cheap:

- **Put an explicit `timeout` on any command whose subject is "this might not terminate."** The
  irony of omitting it on a row about a brute-force irreducibility search is the point: the rows
  most worth bounding are exactly the ones about unbounded computation.
- **Sweep at the end of a batch**: `pgrep -a M2-binary`, and check for shells older than a minute.
  Subagents finish and report while processes they started keep running, and nothing reaps them.

The deeper error was accepting "killed by timeout 540" without checking that the process was
actually dead -- the same shape as accepting a benchmark without checking its inputs.

### Sweeping means sweeping *your own* processes

The advice above, followed literally, does damage. At the end of a later batch a `pgrep`-shaped
sweep matched three `M2-binary` processes and all three were killed; **two of them were the
maintainer's own interactive sessions.** No computation was lost, but only by luck.

`M2-binary` in the process table says nothing about who started it, and the distinguishing column
was right there unrequested:

```sh
ps -eo pid,ppid,tty,etime,args | grep [M]2-binary
```

A process from a tool call has **tty `?`** and a parent chain that reaches the agent process; an
interactive session has a real `pts/N`. The one I killed was `M2 --no-readline --print-width 114`
-- and a print width of 114 comes from somebody's terminal, not from anything here.

So: never kill by name match. Prefer not needing to -- `timeout` on the command, and capture `$!`
for anything backgrounded -- and when a sweep does find something, print `tty` and `ppid` and kill
only what is attributable to this session. A stray process of mine costs a core for an hour; a
killed session of the maintainer's costs work that cannot be recovered, so the two errors are not
symmetric and the check is one column wide.

## The record often settles a row faster than the code does

Three rows in one batch were settled by history rather than by measurement, and in each case
reading the code first would have been slower and less conclusive.

- `1-hashing-doc-examples` asked for stored example inputs to become sequences. `511504951f`
  implemented it eight days after the file was written; `7c41af8e0e` reverted it six months
  later; and `examples.m2:165-166` still carries *"don't convert `ex` on the next line to a
  sequence, because the hash code for caching example outputs will change."* The author decided
  it twice and left a note forbidding the redo.
- `1-hilbertSeries-coherent-sheaf` is an email thread dated 2008-12-10. `8b32397e67`, "disable
  hilbertFunction etc. on projective varieties", is dated **the same day**.
- `1-html-doc-directory` complains about a link Dan had added seven months earlier
  (`e67dd83a9b`) and which `0996057a46` deleted three months after the complaint.

So before instrumenting anything, run `git log -S` on the mechanism the file names. A file that
proposes a specific change is often accompanied by a commit that made it and, sometimes, another
that took it back -- and a revert plus a source comment is a stronger verdict than any
measurement, because it records a decision rather than a state.

## Grep the note column for your own path before you read the file

The duplicate check above searches the notes for a *subsystem*. Run it for the row's **own path**
too, because an earlier batch may have already settled it in passing.

`1-info-dir` and `1-info-doc` were filed and pointed at
[#4554](https://github.com/Macaulay2/M2/issues/4554) and
[#4587](https://github.com/Macaulay2/M2/issues/4587) with their still-`todo` siblings named in the
notes. So this now answers `2-info-dir` before anyone opens it:

```sh
awk -F'\t' -v p=2-info-dir 'NR>1 && index(tolower($11), p) {print $1, $7, $8}' catalog.tsv
```

Four notes currently name a `todo` sibling this way, and the pointer only lives in the *settled*
row -- a `todo` row's `note` is empty by convention, and filling one in would be a half-written
verdict that the next reader has to distrust. Which means the forward pointer is there but does
not come to you; you have to ask for it.

The reason this pays in a bucket taken alphabetically is that adjacent filenames are adjacent asks.
Four rows in one batch (`1-info`, `1-info-dir`, `1-info-doc`, `1-info-files`) were four different
questions about info files, one already tracked, one worth filing, one met by dpkg, one deferred by
its own first line -- and the row that would have produced a duplicate issue was a duplicate of an
issue **this catalog filed itself**. The tracker search finds #4554; what tells you to trust the
match is the sibling's note, stating the ask in the same words.

## When a defect survived implausibly long, look for the check that was switched off

`Macaulay2Doc.info`'s Top node has had no menu for as long as the manual has been generated, at the
one place every info reader starts. That is not obscure, so "how did nobody notice" is a real
question, and the answer was two lines of makefile:

```make
# the Info-validate function doesn't work well enough to be useful:
#	check::check-info
```

`packages/Makefile.in:121-122`, commented out by `f766cd4dfa8` on 2009-01-06 -- and `Info-validate`
is precisely the tool that reports nodes reachable only by cross-reference. Seventeen years of
nobody noticing, explained.

Worth the grep for its own sake, because a disabled check is better material than the defect. It
dates the regression window, it names a closing condition the issue can ask for, and it is a
decision someone made and wrote down, which carries more weight in an issue body than a symptom
does. Look for commented-out targets, `if false`, skipped tests and `|| true` in the subsystem the
row touches.

## The flag that makes M2 convenient to test with can be the flag that hides the bug

`-q` is in nearly every command in this directory, because an init file makes runs
irreproducible. It also changes the behaviour under test. From `last.m2:65-71`:

```m2
prefixPath = if prefixDirectory === null then {} else {prefixDirectory};
if not noinitfile and getenv "HOME" =!= "" then (
     prefixPath = prepend(applicationDirectory()|"local/", prefixPath);
```

So `-q` drops the user prefix from `prefixPath` entirely. `1-installPackage` complains that a
locally installed package is missing from the page `viewHelp` opens — and under `-q` that is
exactly what happens: `prefixPath` is `{/usr/}`, `makePackageIndex()` never scans
`~/.Macaulay2/local/`, and the package is absent. It looks like a clean reproduction of a
seventeen-year-old bug. Without `-q`: `{~/.Macaulay2/local/, /usr/}`, and the package is listed,
linked into the user prefix.

The general form: **before trusting a reproduction, ask which of your harness flags the code under
test reads.** Grep the flag's internal name — `noinitfile`, not `-q` — and see what branches on it.
A flag that suppresses output is safe; a flag that suppresses *setup* is not, and the two are
spelled the same way on the command line. Same family as "Four ways an existence check lies to you
in M2" above, and as the benchmark that measured nothing.

## A build tree in mid-build is not a test platform, and the tell is a `.tmp`

`1-installPackage-links` is about a split-layout build tree, and the only split-layout M2 here was
one in `M2/BUILD/build/`. Installing a package with it produced **11 broken links out of 77** —
apparently the reported bug, on the first machine that could show it.

It was not. Under that prefix, `Macaulay2Doc`'s documentation database exists only as
`rawdocumentation-dcba-8.db.tmp`, so the build had never finished installing it, and
`tallyInstalledPackages` says what it does about that:

```m2
if not fileExists dbfn then continue;	    -- maybe installation was interrupted, so ignore this package
```

With `Macaulay2Doc` skipped, `getPackageInfo` returns `null`, the locator falls back, and every
link into the manual comes out wrong — for reasons that have nothing to do with layouts. An
unfinished build reproduces *many* bugs it does not have.

So when a borrowed build tree is the platform, check that the thing being linked to is actually
installed in it before believing the result: `find <prefix> -name 'rawdocumentation*'` and look for
a `.tmp` suffix. And prefer measuring the direction you *can* measure soundly — here the layout-1
M2 installing into a layout-2 prefix gave 73 links and 0 broken, which is real evidence about the
cross-layout machinery, while the confounded run was evidence about nothing.

## A decision can live in a merged pull request

Two rows in one batch turned on discussions no search here could see, because
`bin/fetch-issues` dropped pull requests. Both were found only because the maintainer pointed at
them.

- `1-list-of-ideals-of-R` wants `intersect` of an empty list of ideals of `R` to answer `ideal 1_R`.
  It was written up `open` — and the question had been settled in **PR #3328** (merged 2024-06-29),
  which added the 0-argument forms for `gcd`, `lcm` and `union` and left `intersect` out on purpose.
  Mike Stillman, in a comment there: *"I would prefer not to have the ring default to ZZ for tensor
  and intersect. We have that in other cases (e.g. `ideal()`), and it causes hard to find bugs."*
- `1-loadPackage` is answered by **PR #3852** (merged 2025-06-01), which added
  `warning: reloading ...; recreate instances of types from this package`.

The cache now keeps PRs (each item carries `pull_request`), and `bin/suggest-issues` filters them
back out. **But comments are still not cached**, and in the #3328 case the decisive sentence was in
a comment. So the search gets you to the right thread, not to the decision inside it: when a hit
looks like it might be the deciding thread, open it.

### `gh search issues` reads comments; the cache cannot, so use both

Knowing comments are uncached is not enough, because the failure does not look like a gap — it looks
like a clean negative. `1-real-output` was written up with "searched issues and PRs: nothing on this",
and the maintainer replied that he was almost certain there was an open issue he had worked on,
mentioning Grisu and Ryu *in the comments*. There was: **#708**, open since 2017, whose own body ends
with the same `toExternalString` call the row is about.

Two things had to go wrong together, and both will recur:

- The decisive text was in a comment, so no scan over `cache/issues.json` could see it.
- **#708 is titled "floating point weirdness"**, which shares no word with the row, so no keyword worth
  guessing would have reached it by title or body either.

The fix is one command, and it is not a cache scan:

```sh
gh search issues --repo Macaulay2/M2 ryu --json number,title,state,isPullRequest
```

GitHub indexes comment text, so a term that only ever appeared in discussion is findable. Run it for
the row's *subject-matter jargon* — algorithm names, library function names, anything a maintainer
would type while thinking aloud — not just for the row's own vocabulary. `mpfr_get_str`, `Grisu`,
`Dragon4` would each have found #708; `toExternalString` and `shortest` did not.

And the general lesson about negatives applies here in its strongest form: this catalog's verdicts rest
on "nothing tracks this" constantly, and there are now three distinct ways that sentence can be false —
a PR the scan filtered out, a comment the cache never had, and a title that shares no vocabulary. Say
in the note which searches were run, so the next reader knows what the negative covers.

The wider point is that "no issue mentions this" is a weaker statement than it sounds. A closed PR
is where a "no" usually gets recorded, and a `wontfix` that cites the maintainer who said no is
worth ten that cite an absence of evidence.

### The ad-hoc search helper filters PRs out, and it will keep doing so

Fixing the cache did not fix the searches. Every ad-hoc scan written since has opened with

```python
items = [i for i in d if not i.get('pull_request')]
```

because the question being asked is usually "is this already an issue", where excluding PRs is
right. That line then silently answers a *different* question wrongly: "is anyone working on this".
`1-pushForward1` was written up `wontfix` on an issue-only search, and the maintainer asked whether
PRs had been checked. Three were open or freshly merged on that exact function.

The verdict survived — Joel Dodge's #4366, #4435 and #4465 widen *which finite maps* Core
`pushForward` accepts, and the row's map fails an earlier gate — but that was luck, not method, and
establishing it took a second pass over five rows.

So: **search both, and say in the note that you did.** Two habits that cost nothing:

- Print the kind alongside each hit (`PR` / `iss`) rather than filtering. A PR in the output is
  information; a PR silently absent is not.
- When a row touches a function under active development, the relevant question is not "has this been
  reported" but "is this the axis someone is already working on" — and those have different answers.
  Read the open PR's own failing example: #4435's is `unexpected degree map of ring map`, which is a
  different gate from the `no applicable strategy` the row hit, and that comparison is what settled
  it.

## An example can fail for a reason other than the one you are testing

The mirror of "an example can pass by accident", and it produced a wrong `reproduces` here.

`1-loadPackage`'s transcript ends with `flagBundle({2,2}, V)` failing after a package reload, where
`V = OO_pt^2`. Running exactly that, it failed — and I recorded the row as reproducing. It does
reproduce, but not for that reason: **rank 2 cannot carry a `{2,2}` flag**, so the call is invalid
today on mathematical grounds (`Schubert2.m2:608`, *"expected rank of bundle to be not less than the
sum of the bundle ranks"*), and it fails identically with no reload at all.

Two things hid it. `try ... else "no method"` swallowed the message and replaced it with the label
the 2010 file used, so the transcript agreed with itself. And there was no control: the whole claim
was "this fails after a reload", with nothing establishing that it succeeds without one.

So for any row of the form "X fails under condition C": run X *without* C in the same session, and
never label a caught error — print it. Here the corrected experiment needs rank 4, and then all
three cells are informative: no reload works, stale objects fail with the dispatch error the file
describes, objects created after the reload work.

## Check an identity before you put it in an issue

`1-join-hash` proposes deriving a joined list's hash from its inputs. Whether that is possible is an
arithmetic question, and my first answer was wrong: I read `seqHashSeed`/`seqHashMult`
(`d/basic.d:8-9`), derived `h(L++M) = mult^|M|·(h_L − seed) + h_M`, and it failed all five test
cases. Those constants belong to the *Sequence* fold. `hash(x:List)` at `:82-85` is a different
function — multiplier `1299833`, seed `x.Class.hash + 23407` — and with those the identity passes
seven cases including both empty sides, mixed element types and nested lists.

Cheap rule: any formula going into an issue body gets evaluated against the implementation first,
on enough cases to catch an off-by-one seed. It is three lines of M2 and it is the difference
between a filed feature request and a filed mistake.

## Prose for a stranger is not the note column

The note is one TSV line written for the catalog -- "both halves unmet, and the second is measurable
without the first" -- and for a long time the filed issues published exactly that, in a blockquote,
under the unfenced bug file. It reads as shorthand because it is shorthand.

So issue bodies now come from `issues/<path>.md`, hand-written, in the same shape as `comments/`.
The layout is provenance and attribution first, the original file **fenced**, then the prose, then the
verdict fields small in a footer. Drafts keep the old trailing-block layout.

Fencing was not cosmetic. `bugs/dan/1-dot-dot` carries email addresses at column zero that GitHub was
turning into mailto links, and every M2 transcript in the tree rendered as run-together prose. I had
been working around that by hand when writing comments.

Two things learned writing the first 85:

- **Writing the prose finds errors the note hid.** `1-map`'s note said `map` rejects option-style
  destinations. Expanding it meant reading `ringmap.m2:495-501`, which shows the form *works* for
  endomorphisms and is gated on `R === S`. The issue that got filed is narrower and truer than the note,
  and a claim I had invented about arity checking turned out to be false.
- **Say when your own earlier reasoning was wrong.** Two summaries record a first attempt that failed --
  a gcd counterexample that passed by coincidence, a hash identity that used the wrong constants. A
  reader comparing the issue with the catalog would otherwise wonder which to believe.

## A layout change can silently orphan rows from the board

`key_of` finds a row's board item by searching the body for ``Triaged from `bugs/...` ``, and
`project.py` calls that "the most durable key we have". The new issue layout moved the phrase into an
opening sentence reading "This issue was *t*riaged from" -- lowercase -- and the regex wanted a capital
T.

Four filed issues went unmatched: #4514, #4528, #4501 and #4529. Three of them are the very examples
`project.py`'s docstring cites as the reason the key exists. Nothing was lost, because matching ran
against the old bodies before they were rewritten, but a later verdict revision would have failed to
reach them and the catalog would have gone on disagreeing with the tracker.

The tell was in the dry run: `unmatched` went from 1 to 6. **Read that number after any change to what
goes in a body.** It is the only signal that the join between catalog and board still holds, and it does
not look like an error.

## Reusing one input across variants measures the cache, not the variants

`1-new-res-options` asks for the resolution strategy flags to get names, so checking it meant trying
eight spellings of `Strategy` and `SortStrategy`. Built one module and ran all eight against it. All
eight "worked", including `SortStrategy => 12345`.

`freeResolution` caches on the module -- `M.cache.ResolutionObject`, consulted at
`Complexes/FreeResolution.m2:75-95` -- so calls two through eight returned the first result and never
reached the engine. With a fresh module per call the answer is informative and half of them error,
which is the actual finding.

This is the mirror of [order the transcript the way the file
wrote it](#order-the-transcript-the-way-the-file-wrote-it): there a *previous* call populated a memo
table and made a broken thing look fixed; here the harness's own previous call made every variant
look supported. Both are the same failure, and the general form is worth stating once: **a comparison
across variants needs a fresh subject per variant, because anything M2 caches on the subject will
answer for all of them.** Rings, modules, packages and `vars` all memoize. The tell is a table with no
variation in it -- if every row of a comparison agrees, suspect the harness before believing the
result.

### `GF 4 =!= GF 4`, and other constructors that do not return the same object

In the same batch, a table of `k ** k[x,y]` across six coefficient rings reported `GF 4` as failing.
It does not. The script built the field twice -- once for `k` and once inside `k[x,y]` -- and each
`GF` call makes a new field, so it was testing two unrelated rings. Bound once, it works.

That produced a wrong claim in a message to the maintainer before it was caught, and it would have
produced a wrong sentence in a filed issue. Binding every constructed object to a name exactly once
and reusing the name costs nothing and removes the failure mode; building it inline twice looks like
the same expression and is not.

## Where to start

`bugs/dan` priority `0` was the place to start -- 118 files, Dan's own highest-priority bucket,
and the same one `d3ec491953` drew from. **`dan`, `anton`, `LAcore`, `gfurnish` and the root files
are now all settled.** Of the 857, 741 are settled and **116 are left, every one of them
`bugs/mike`**:

| | |
| --- | ---: |
| `mike`, priority `1` | 33 |
| `mike`, priority `2`–`9` | 25 |
| `mike`, unnumbered | 58 |

"Take one author at a time" was the advice while there were five; there is one now, so the axis
that remains is priority, and Mike numbered only half his files. The 58 unnumbered ones carry no
signal about his own ranking, which the numbered buckets do.

The mix of kinds is **83 reproducers to 33 notes**, the reverse of Dan's remainder, so this bucket
is slower per row and the `autorun` caveat above applies to most of it: 45 `fail`, 20
`pass-partial`, 11 `pass`, 7 `timeout`, 33 `n/a`. Read that column as a hint about where to look
first and never as a verdict -- `pass-partial` in particular means almost nothing.

To list what is left, or a slice of it:

```sh
awk -F'\t' 'NR>1 && $7=="todo" {print $1}' catalog.tsv
awk -F'\t' 'NR>1 && $2=="mike" && $3=="1" && $7=="todo" {print $1}' catalog.tsv
```
