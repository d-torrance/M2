# Triaging the open issues

Macaulay2/M2 has **897 open issues**. The oldest,
[#9](https://github.com/Macaulay2/M2/issues/9), was filed in March 2013 and has never had a
comment. **467 of them carry no label at all.** Nobody has been through the tracker as a
whole, and an issue list that large stops being a work queue and becomes an archive: the
live bugs are in there, but so is everything that was fixed years ago by someone who never
came back to say so.

This directory's first project went through the 857 files deleted with the pre-GitHub
`bugs/` tree — see [`README-bugs-directory.md`](README-bugs-directory.md), which is
finished. This one goes through the tracker, answering three questions per issue:

1. **Is it still an issue?** If not, close it.
2. **Is it a duplicate?** If so, close the newer and cross-reference the older.
3. **Does it have the right labels?** If not, fix them — and its
   [issue type](#types-not-the-bug-and-feature-request-labels), which turned out to be
   half the answer to that question.

`issues.tsv` is the output, plus the labels applied and the issues closed. As before,
**nothing here modifies the Macaulay2 sources.** Acting on a verdict — writing the fix,
promoting a reproducer into `M2/Macaulay2/tests/` — is separate work in a separate branch.

## Quick start

```sh
bin/fetch-issues --refresh   # cache every issue and PR, all states, into cache/
bin/fetch-comments           # cache all ~17k issue comments (169 requests, once)
bin/init-issues              # build issues.tsv
bin/extract-repros           # issue body -> cache/repros/<N>/{repro.m2,expected.txt}
bin/run-issue-repros         # run them against /usr/bin/M2 (slow)
bin/suggest-dups             # cache/dup-pairs.tsv and cache/dup-families.txt
bin/suggest-labels           # cache/label-suggestions.tsv
bin/render-issues            # regenerate ISSUES.md
bin/selftest                 # silent on success
```

`cache/` is gitignored and regenerates.

## The default is "still open", and it costs nothing

This is the one procedural thing that is **backwards from the first project**, and getting
it the wrong way round would make the sweep unbearable.

There, every row had to reach a verdict, and a row that came out `open` obliged a filing —
"open but unfiled" was not a disposition. Here the issues already exist. An issue that
still stands needs *nothing done to it*: no comment, no question, no notification to its
author. So:

- **If the evidence says the issue still stands, write the verdict and move on.** Do not
  ask. Do not comment. The row is settled by being left alone.
- **Ask only about closing and about duplicates.** Those are the irreversible ones, they
  land in a stranger's inbox, and they are the ones a wrong call actually costs something.
- One issue per question, with the evidence, the way
  [Bring one row at a time](README-bugs-directory.md#bring-one-row-at-a-time-and-explain-the-mechanism)
  describes. A batch of six proposed closures presented as one plan is not six decisions,
  it is one decision about a list.

### A correction goes *below* the thing it corrects

`comments/issues/<N>.md` is the first comment; `<N>.2.md`, `<N>.3.md` are follow-ups, each
posted separately and each idempotent under its own `<!-- issue-triage:<N>.<k> -->` marker.

This exists because the obvious thing is wrong.  Rewriting `<N>.md` does not add a
correction, it **edits the published comment in place** -- so a correction written that way
apologises for a claim that is no longer visible to anyone reading.  Doug caught this on
#290 before it was posted.  And quietly rewriting a published claim is the wrong instinct
regardless: the wrong claim should stay up, with the correction under it, which is what a
reader needs in order to trust either.

The marker was always meant to stop *accidental* re-posting of the same comment, not to
forbid a deliberate second one.  Numbered rather than dated so the order is visible in the
filename and a correction cannot be posted before what it corrects.

### Retitle an issue whose scope has moved

Rare, and never quietly.  #290 was the first: filed in 2015 as "bug in gb over ZZ ? (using
custom ordering)", the wrong answer it reported was fixed by *forbidding the input*, so what
it tracks now is the capability that removal took away.  The title described a symptom
nobody will ever see again, and leaving it would mean the issue reads as a stale bug report
for as long as it stays open.

The test is a change of **scope**, not of wording.  A title that could be clearer is not a
reason; a title that describes something that no longer exists is.  This comes up most on
old issues, because they have had the most time to be overtaken by their own resolutions.

`bin/retitle` is separate from `bin/apply-types` even though both go through `updateIssue`,
so a retitle cannot ride along unnoticed in a batch of eight type changes.  It refuses to
run without a hand-written comment for the issue, because a title changing under somebody
with no reason given is worse than a stale title.  Retitling needs the maintainer's
go-ahead, like a close does.

### A measurement is only evidence if the comparison is valid

Three times in this sweep I have produced a number, drawn a conclusion, and had the
conclusion turn out to rest on a comparison that did not hold.  The number was right every
time; the control was not.

- **#569** — local-order reduction takes 15s where the same element under a *global* order
  takes under a second, which reads as a regression until you remember a local order is not
  a well-ordering.  `1 > x > x^2 > ...` descends forever, so reduction there relies on
  Mora's ecart restriction to terminate at all and is expected to be dear.  The previous
  project had already settled this on `bugs/mike/1-local-bug.m2` as expression swell,
  verdict `wontfix`.  See
  [A slow local normal form can be expression swell](README-bugs-directory.md#a-slow-local-normal-form-can-be-expression-swell-not-a-defect).
- **#330** — the rank-3 Segre pushforward disagreeing with `segre F` looks like a defect
  until the rank-2 case is recognised as the coincidence, and until you notice `segre`
  carries a dual of its own.
- **#527** — `DegreeLimit=>3` returning `ideal()` looks like a broken option until the same
  map, graded so it is homogeneous, obeys the documentation exactly.

The habit that would have caught all three: before reporting a gap, state what the two sides
have in common and check that the *only* difference is the one being blamed.  A global order
and a local order do not have termination in common.  A graded map and an ungraded one do
not have a shared notion of degree.  Where a control cannot be constructed, say the
measurement is unexplained rather than calling it a finding.

### Ask of every row: would a comment materially improve it?

A fourth question, alongside the three the sweep is named for, and it applies to issues
that are staying **open** -- which is most of them.  A comment earns its notifications when
it carries something a reader of the issue cannot get from the issue:

- **a measurement**, where the thread has an unconfirmed claim.  #143 had numbers from a
  machine called "habanero" in 2014 and none since; it now has 1.78x against a 4.0x control.
- **a mechanism**, located.  #212's debug info is extracted and never linked, because
  `--add-gnu-debuglink` sits in the `else` branch of a conditional that is never taken on
  GNU binutils.
- **an answer to a question nobody answered.**  #256 had "What is the issue here exactly?"
  from 2024; #133 had a contributor offering wording in 2026 and getting no reply.
- **history that changes the reading.**  #133's message improved as a side effect of
  hookification, not because anyone acted on the issue -- which is what its asker needed
  to know.
- **work already done elsewhere.**  #248 asks to automate two files the previous project
  had already verified.

And it does not, when the only content is "still true".  #187, #241 and #247 were all
confirmed still true in the same batch and got nothing, because saying so adds a
notification and no information.  The test is not "is this issue interesting", it is
"would the next person to open this be better off".

**Read the labels an issue already has, not only the ones it lacks.**  The question is
"does it have appropriate labels", and a label that is wrong misleads harder than a
missing one: it tells a reader the issue has been classified.  #58 carried
`under discussion`, which sounds like a live conversation and turned out to have been
applied in 2020 as a swap for a now-deleted `just do` label, seven years after the last
comment on the thread.  Nothing in the body would ever have said so -- the timeline did.
An existing label that should come off goes in `rmlabels` and is **proposed for approval
like a close is**, because removing somebody else's label is a judgement about their
judgement.

The one removal that is not a judgement call is `bug` and `feature request` on an issue
being given the matching type; that is bookkeeping, and `bin/set-verdict` and
`bin/apply-types` both warn when a row is typed and keeps the label anyway.  #44 went in
as `type=Bug` still carrying `bug`, and neither `apply-types` nor `apply-labels` would
have noticed on its own: each sees only its own half.

Labels are otherwise the exception in the other direction: they notify nobody and they are
trivially reversible, so they are decided while reading and applied a batch at a time. That does not
make `bin/apply-labels --apply` self-service — see
[`--apply` is not yours to give yourself](README-bugs-directory.md#--apply-is-not-yours-to-give-yourself).

## `issues.tsv` is the source of truth

Tab-separated, one row per open issue, keyed on the issue number. Sixteen columns.

| column | filled by |
| --- | --- |
| `issue`, `created`, `updated`, `author`, `comments`, `type`, `labels` | `bin/init-issues`, from the cache |
| `repro` | `bin/extract-repros` |
| `run` | `bin/run-issue-repros` |
| `verdict`, `dup`, `settype`, `addlabels`, `rmlabels`, `action`, `note` | you |

Columns 1–9 are machine-generated and rewritten freely; columns 10–16 are yours and survive
regeneration. `bin/init-issues` merges on `issue`, so re-running it never clobbers a
verdict you typed.

Two of those need saying explicitly.

**`labels` is what GitHub has right now**, refreshed on every `init-issues` run. That is
deliberately the only record of what has been applied — there is no `applied` column.
`bin/apply-labels` derives its work by diffing `addlabels` against `labels`, so a
maintainer who relabels an issue by hand is automatically respected. A flag saying "synced"
would go on claiming a sync that stopped being true. This is the same argument
[`push-project` made](README-bugs-directory.md#a-layout-change-can-silently-orphan-rows-from-the-board)
about not recording that a row had been pushed, and it binds harder here, because these are
other people's issues and they will be edited underneath us.

**`dup` names the *older* issue**, always. `bin/selftest` asserts `int(dup) < int(issue)`
over the live TSV, because getting the direction backwards closes the surviving issue and
nothing else in the pipeline would notice.

### Verdicts

```
todo              not yet triaged
reproduces        ran the repro on clean M2; the reported failure is still there
stands            no runnable repro, and the ask is still unmet
stale-repro       the repro fails, but on API drift, not on the reported bug
not-reproducible  the repro runs clean now; the behavior is gone, cause unknown
fixed             gone, and `note` names the commit or PR that did it
duplicate         covered by an older issue; `dup` names it
not-a-bug         expected behavior; the answer is an explanation, not a change
wontfix           deliberate, or a maintainer said no
obsolete          the premise is gone -- subsystem removed, platform dead
needs-reporter    cannot be settled without the author
```

The first project used seven verdicts. Two of the splits here are deliberate.

**`not-reproducible` is not `fixed`.** "It stopped failing" and "this commit fixed it" are
different claims, which is the whole point of
[Attributing a fix](README-bugs-directory.md#attributing-a-fix), and the closing comment has
to read differently: a `fixed` row carries evidence, a `not-reproducible` row carries an
absence and must say so and invite reopening. Reach for `fixed` only when you can name the
commit or PR — [Ask GitHub which PR carried a commit; do not infer it](README-bugs-directory.md#ask-github-which-pr-carried-a-commit-do-not-infer-it).

**`reproduces` is not `stands`.** 267 of these issues have no code in the body at all.
Collapsing both into one "open" loses the fact that the second rests on reading rather than
on a run, and [saying what a measurement does not establish](README-bugs-directory.md#autorun-is-a-hint-not-a-verdict)
is the habit that keeps a triage sweep honest.

`action` is `""`, `close`, `comment`, `label-only` or `keep`. It is separate from `verdict`
on purpose: a verdict is a finding, an action is a decision, and the decision is yours.

## `run` is a hint, and a weaker one than last time

[`autorun` is a hint, not a verdict](README-bugs-directory.md#autorun-is-a-hint-not-a-verdict)
still holds, but the polarity has flipped and the hint has got worse.

A bug *file* that stopped erroring was probably fixed. A bug *report* mostly is not about
erroring at all — it is about a wrong answer. [#113](https://github.com/Macaulay2/M2/issues/113)
prints incorrect substitutions and exits 0. [#290](https://github.com/Macaulay2/M2/issues/290)
errors, but on an undefined helper the reporter never defined, which says nothing about the
Gröbner basis bug it is reporting. So `run=clean` is a *candidate*, never a finding:

```
clean            exit 0 and nothing from expected.txt matched -- the thing worth reading
reproduces       a diagnostic matched expected.txt
crash            killed by a signal, or SIGSEGV / Internal error: / Aborted in the output
different-error  exit 1, matching nothing in expected.txt -- drift, extractor damage, or a second bug
drift            different-error and STALE_PATTERNS matched -> stale-repro candidate
missing-package  the package will not load
syntax           the *extractor* failed, not M2.  Never evidence about the issue
timeout          for a performance issue this is the repro; otherwise uninformative
clean-partial    exit 0 but an `end` line cut the script short
empty            nothing to run
n/a              no runnable reproducer
n/a-platform     the issue is about a platform this machine is not
```

`n/a-platform` is not bureaucracy. `build issue` is one of the largest labels here and `arm`
has 16 open issues; a clean run on x86-64 Ubuntu is not evidence about an aarch64 build
failure, and a row carrying either label must never reach `not-reproducible` on a run alone.

Test against `/usr/bin/M2`, which is a clean development build. The in-tree build under
`M2/BUILD/build/usr-dist/` is currently a UBSan build that prints sanitizer output before it
prints anything else — see
[A build tree in mid-build is not a test platform](README-bugs-directory.md#a-build-tree-in-mid-build-is-not-a-test-platform-and-the-tell-is-a-tmp),
which is the same mistake wearing a different hat.

## Types, not the `bug` and `feature request` labels

Macaulay2 has GitHub **issue types** enabled -- `Bug`, `Feature`, `Task` -- and they
are a second taxonomy running alongside labels: single-valued, org-level, and shown
next to the title rather than in the label row.  They were quietly splitting this
corpus in half.  93 open issues carried the type `Feature`, 130 others carried the
`bug` or `feature request` **label**, and the two sets did not overlap by a single
issue.  Two vocabularies for one distinction, each holding part of the answer.

The types win.  They are single-valued, which is what `project.EXCLUSIVE` was
faking for those two labels anyway, and the labels turn out to have almost no
history to lose: `feature request` has never been on a closed issue, and `bug` on
only eleven.

| | count | what happens |
| --- | ---: | --- |
| no type, no label | 674 | set a type |
| no type, carries `bug` or `feature request` | 130 | set the type, drop the label |
| typed `Feature` already | 93 | **re-read** -- see below |
| | 897 | |

**All three types get used, Task included.**  A great deal of this tracker is
neither a defect nor a request for functionality: internal refactors, build and
packaging chores, test-suite work, documentation cleanups.  The 93 existing
assignments were all made in one pass on 2025-05-13 under a two-way reading --
anything that was not a defect became a `Feature` -- so [#9](https://github.com/Macaulay2/M2/issues/9),
a C++ template reorganisation, is currently filed as "a request, idea, or new
functionality", and so is [#62](https://github.com/Macaulay2/M2/issues/62), which
silently accepts `reverse Matrix := ...` and then errors when you use it.  Those
are a `Task` and a `Bug`.  So the 93 are re-read rather than skipped.

`bin/apply-types` sets the type; `settype` in the catalog holds the proposal and
`type` holds what GitHub has, so the work is derived by difference exactly as
`bin/apply-labels` derives its own.  Neither label is proposed any more --
`project.SUPERSEDED_BY_TYPE` names them, and `bin/apply-labels` says so if one ever
turns up in `addlabels`.

**The label definitions stay in the repository until the sweep ends.**  Deleting
`bug` now would strip it from all 88 carriers at once, including the ones nothing
has looked at yet.  It comes off one issue at a time, through `rmlabels`, and the
label itself goes only when no open issue carries it.

## Rules carried over

These were learned on the first project and are not restated here. They apply unchanged.

**Duplicate searching.** [Never truncate the duplicate search, and read it oldest first](README-bugs-directory.md#never-truncate-the-duplicate-search-and-read-it-oldest-first) ·
[Search your own catalog too](README-bugs-directory.md#search-your-own-catalog-too-not-just-the-tracker) ·
[Search before you write the verdict, not after](README-bugs-directory.md#search-before-you-write-the-verdict-not-after) ·
[`gh search issues` reads comments; the cache cannot](README-bugs-directory.md#gh-search-issues-reads-comments-the-cache-cannot-so-use-both) ·
[When an existing issue is close but not the same](README-bugs-directory.md#when-an-existing-issue-is-close-but-not-the-same)

**Publishing.** [`--apply` is not yours to give yourself](README-bugs-directory.md#--apply-is-not-yours-to-give-yourself) ·
[Bring one row at a time, and explain the mechanism](README-bugs-directory.md#bring-one-row-at-a-time-and-explain-the-mechanism) ·
[Put the attribution above the text, not in the footer](README-bugs-directory.md#put-the-attribution-above-the-text-not-in-the-footer) ·
[Commenting on an existing issue](README-bugs-directory.md#commenting-on-an-existing-issue) ·
[Prose for a stranger is not the note column](README-bugs-directory.md#prose-for-a-stranger-is-not-the-note-column) ·
[Check an identity before you put it in an issue](README-bugs-directory.md#check-an-identity-before-you-put-it-in-an-issue)

**Labels.** [Topic labels: where it came from, and who should read it](README-bugs-directory.md#topic-labels-where-it-came-from-and-who-should-read-it) ·
[Relabelling after the fact is by hand](README-bugs-directory.md#relabelling-after-the-fact-is-by-hand)

**Testing M2.** [Four ways an existence check lies to you in M2](README-bugs-directory.md#four-ways-an-existence-check-lies-to-you-in-m2) ·
[`--script` and stdin fail in opposite directions](README-bugs-directory.md#--script-and-stdin-fail-in-opposite-directions-so-neither-is-a-safe-way-to-re-check) ·
[An example can fail for a reason other than the one you are testing](README-bugs-directory.md#an-example-can-fail-for-a-reason-other-than-the-one-you-are-testing) ·
[An example can pass by accident](README-bugs-directory.md#an-example-can-pass-by-accident-so-build-one-whose-components-disagree) ·
[Ring shadowing bites the test, not only the subject](README-bugs-directory.md#ring-shadowing-bites-the-test-not-only-the-subject) ·
[`GF 4 =!= GF 4`](README-bugs-directory.md#gf-4--gf-4-and-other-constructors-that-do-not-return-the-same-object) ·
[The flag that makes M2 convenient to test with can be the flag that hides the bug](README-bugs-directory.md#the-flag-that-makes-m2-convenient-to-test-with-can-be-the-flag-that-hides-the-bug) ·
[Order the transcript the way the file wrote it](README-bugs-directory.md#order-the-transcript-the-way-the-file-wrote-it)

**Performance claims**, which almost never turn out to be defects.
[A slow local normal form can be expression swell, not a defect](README-bugs-directory.md#a-slow-local-normal-form-can-be-expression-swell-not-a-defect) ·
[A performance number that looks too good is a broken benchmark](README-bugs-directory.md#a-performance-number-that-looks-too-good-is-a-broken-benchmark) ·
[Reusing one input across variants measures the cache, not the variants](README-bugs-directory.md#reusing-one-input-across-variants-measures-the-cache-not-the-variants)

**Process.** [Run `git grep` from the top of the checkout](README-bugs-directory.md#run-git-grep-from-the-top-of-the-checkout) ·
[Use `git -C`, because the cwd trap does not yield to vigilance](README-bugs-directory.md#use-git--c-because-the-cwd-trap-does-not-yield-to-vigilance) ·
[Bound anything exploratory, and sweep for strays](README-bugs-directory.md#bound-anything-exploratory-and-sweep-for-strays-when-the-batch-ends) ·
[Sweeping means sweeping *your own* processes](README-bugs-directory.md#sweeping-means-sweeping-your-own-processes) ·
[The record often settles a row faster than the code does](README-bugs-directory.md#the-record-often-settles-a-row-faster-than-the-code-does) ·
[Grep the note column for your own path first](README-bugs-directory.md#grep-the-note-column-for-your-own-path-before-you-read-the-file) ·
[Test the workflow, do not read it](README-bugs-directory.md#test-the-workflow-do-not-read-it)

**Scope.** [A row can belong to another repository's tracker](README-bugs-directory.md#a-row-can-belong-to-another-repositorys-tracker) ·
[A consumer of M2's behavior may live in another repository](README-bugs-directory.md#a-consumer-of-m2s-behavior-may-live-in-another-repository)

## The 138 issues this directory filed are in the corpus, but not for triage

The first project filed 138 issues that are still open, all carrying the
[`bugs directory`](https://github.com/Macaulay2/M2/labels/bugs%20directory) label. They were
verified against current M2 within the last two weeks and labelled at filing time, so they
are seeded `verdict=stands, action=keep` and are not re-read.

They are still in `issues.tsv`, because the asymmetry matters and is easy to get backwards:

- **As duplicate *targets*, yes.** A 2015 issue may well duplicate one filed last week —
  though when it does, the *older* one survives, which here means the 2015 one.
- **As duplicate *queries*, no.** They were checked against the tracker at filing time.
- **Not extracted, not run.** Their bodies contain our own transcripts and our own
  attribution paragraph, and running them re-runs work already done.
- **Not used to train the label suggester**, which would otherwise learn this directory's
  labelling habits and report them back as corpus evidence.
