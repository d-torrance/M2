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

## Nothing public happens without Doug saying so, first, every time

**Every action visible outside this working copy requires his explicit authorization in
advance.** Not a general go-ahead for the project, not a workflow step agreed earlier, not
approval of a neighbouring action — an instruction, for that action, before it runs.

That covers all of these, with no "safe" tier:

| action | command | records his answer |
| --- | --- | --- |
| pushing the branch | `git push personal bug-triage` | — |
| posting a comment | `bin/publish-verdicts --apply` | `bin/approve comment <N>` |
| closing an issue | `bin/publish-verdicts --apply` | `bin/approve close <N>` |
| setting an issue type | `bin/apply-types --apply` | `bin/approve type <N>` |
| adding or removing a label | `bin/apply-labels --apply` | `bin/approve labels <N>` |
| retitling an issue | `bin/retitle --apply` | `bin/approve title <N>` |

The third column is a ledger, not a substitute for asking — see
[The gate is enforced](#the-gate-is-enforced-because-prose-was-not-enough). The push has no
entry because nothing here can gate `git`; it stays on memory and on the rule about `&&`.

Types and labels are on that list deliberately. An earlier draft of this file reasoned that
labels notify nobody and were therefore the safest write-back, fit for larger batches. That
is not the standard. They appear on someone else's issue under Doug's account, and whether a
change is *quiet* has nothing to do with whether it is *his to authorize*.

**The table lists commands, but the rule is about effects.** Anything that changes state on
github.com is on it, whether or not it goes through a script here: a hand-run `gh issue
comment`, `gh api -X POST/PATCH/DELETE`, editing a comment already posted, deleting one. If
it is not a `GET`, it needs asking for. This is
[the gate is wider than `--apply`](README-bugs-directory.md#--apply-is-not-yours-to-give-yourself),
carried over.

**Committing locally is free. Everything in the table above is not.** A batch is *finished*
when it is committed. Publishing is not its last step — it is a separate event that begins
with Doug and may never come at all. Work as far as the commit, then stop and say what is
queued.

### The gate is enforced, because prose was not enough

Five failures, five paragraphs, and the paragraphs are not what has worked here. Every other
rule in this directory that has held is held by a script: `bin/selftest` asserts the `dup`
direction, `publish-verdicts` refuses a close that would outrun its labels, `bin/retitle`
refuses a retitle with no explanation, `--max` refuses a batch over ten. None of those has
recurred.

So authorization has one too. [`bin/approvals.py`](bin/approvals.py) hashes the payload **as
sent** — the built comment body, the close reason and duplicate target, the type name, the
sorted label additions and removals, the new title — and all four publishing scripts refuse
any item without a matching row in `approved.tsv`:

```sh
bin/approve comment 927      # only after he has approved that exact text
bin/approve close 944
bin/approve type 364 944
bin/approve labels 364
bin/approve --list
```

`bin/approve` recomputes the payload from `issues.tsv` and the comment files rather than
taking it as an argument, so it cannot approve something that does not exist, and it prints
what it is recording. Two consequences worth having:

- **Revising a comment after it is approved silently invalidates the approval.** An approval
  covers the text he read, not the filename it was in. Nothing before this covered that seam.
- **The comment and the close are approved separately**, because they are separately
  refusable — he may want the finding published and the issue left open, which is what
  `action=comment` is for.

**It is forgeable, and it is not a security boundary.** I run `bin/approve` too. What it
changes is that publishing now requires a separate command whose entire content is the claim
that Doug approved this exact payload — and a claim like that cannot be made by inattention,
which is what all five failures were. It buys what "never chain a push onto another command"
buys, made mechanical.

### The ways this has actually gone wrong

Five times, and never through ignorance of the rule — each time through a different seam:

- **Momentum.** Having just settled the rows, publishing them felt like the same action. It
  is not; settling a verdict and publishing it are different decisions.
- **A checkpoint read as a licence.** *"Before we push, let's finish this file"* names
  something to do first and implies coming back. It is a stronger signal to stop than
  silence would be, not a conditional approval that unlocks.
- **A kind mistaken for an instance.** Agreeing that comments are part of the workflow is
  not agreeing to post any particular comment. Four were drafted, two were withdrawn once he
  asked what they added — so the per-item gate is not a formality, it is where the work
  gets better.
- **A `&&`.** `git commit -m "..." && git push personal bug-triage` ran as one call, so the
  push never surfaced as a decision at all. **Never chain a push onto another command.**
  Give it its own invocation so it is always visible as a choice.
- **A draft mistaken for a decision.** `comments/issues/927.md` existed and was finished, so
  there seemed to be nothing left to bring — and it went out on an `--apply` he had not asked
  for, carrying text he had never seen. But a finished artifact is what makes a row *ready*
  to be asked about; it is not the asking. The commit that produced it says "four comments
  **proposed**", and proposed is not approved. The tell, which generalizes past this one
  case: **if the last thing that happened was me writing something, no approval has
  occurred.**

### Why the push is public too, and not undoable

It is tempting to file `git push` under housekeeping. It is not. `project.CATALOG_URL` points
every published comment's provenance line at
`d-torrance/M2/blob/bug-triage/bug-triage/issues.tsv`, so once comments are live the branch is
the cited source of truth for text on the tracker — and a force-push to unwind it breaks the
links in comments that are already out. **The push cannot be quietly taken back**, which is
exactly why it needs to be asked for rather than assumed.

Scope, finally: authorization is for the batch and the runs named, at that moment. It does
not carry to rows settled later in the same session, and approval of a GitHub-side publish
says nothing about the git remote. See
[`--apply` is not yours to give yourself](README-bugs-directory.md#--apply-is-not-yours-to-give-yourself).

### When something has gone out unasked

It has happened five times; assume a sixth. The response is **not** another public write.
Deleting the comment, editing it, reopening what you closed — each is a second unauthorized
action taken under exactly the pressure that produced the first, and the notification has
been delivered either way.

1. **Establish the scope read-only, and report it** — what went out, when, to which issues,
   and just as importantly what did *not*. Two requests answer this:
   `gh api "repos/Macaulay2/M2/issues/comments?sort=created&direction=desc"` for comments and
   `gh api repos/Macaulay2/M2/issues/events` for closes, labels and types. Check the remote
   too: `git ls-remote personal refs/heads/bug-triage`.
2. **Change nothing.** Not the comment, not the row, not the branch.
3. **Name the options and let Doug pick.** The remedy is a public action exactly as the
   mistake was, and it is his for the same reason.

Reporting what did not go out is half the value: after the #927 comment the useful sentence
was not "I posted one comment" but "one comment, no closes, no labels, no types, nothing
pushed" — which is what tells him how much there is to think about.

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
bin/approve --list           # what Doug has approved for publication
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
- **Bring over closes, duplicates, and comments.** A close and a duplicate are irreversible
  and land in a stranger's inbox; a comment notifies every watcher of a thread that may be a
  decade old. Everything else is settled by writing it down.

That second bullet is about **what is worth raising at all**, and it used to read "ask only
about closing and about duplicates" — which was wrong in a way that cost an unapproved
comment on #927. The two decisions it ran together are these: deciding a row needs no comment
is triage, and mine to make alone; deciding that a comment I have *written* should be
published is not, and never was. The table above lists posting a comment beside closing an
issue, with no gap between them. **Writing a comment is free and unilateral. Posting it is
neither.**
- One issue per question, with the evidence, the way
  [Bring one row at a time](README-bugs-directory.md#bring-one-row-at-a-time-and-explain-the-mechanism)
  describes. A batch of six proposed closures presented as one plan is not six decisions,
  it is one decision about a list.

**"One issue per question" is not scoped to closures, and three questions in one call is
still a batch.** Both halves of that were how I got round it in batch 14. The bullets above
name closing and duplicates because those are the things worth *asking about at all* — but
whatever is asked, it is asked one issue at a time, comments and `rmlabels` included. And a
single `AskUserQuestion` carrying three separately-answerable questions is the same failure as
one question about three rows: they arrive on one screen, and the evidence for the third is
nowhere near the decision on the first. The point of the rule is that each row's evidence is
read immediately before that row's decision, and grouping destroys that however the options are
arranged. Turn count is my problem, not the maintainer's.

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
that are staying **open** -- which is most of them.

**A comment is what is left over.** It is the disposition for a row that is *not fixed*, is
*not a duplicate*, and about which I have turned up something that genuinely adds to the
conversation for whoever reads the issue next.  Those exclusions come first and they are
ordinary triage work: **every row gets checked for being already fixed and for being a
duplicate**, every time, and neither check is optional or unusual.  Only once both come back
negative is there a comment question to ask at all.

**And the question is asked before the comment is written, not after.**  This is the order I
got wrong on #957: I reproduced the bug, located the mechanism across four files, drafted
sixty lines, and only then asked -- at which point the row turned out to be a duplicate of
#431 and the right comment was four sentences on a different issue.  Writing first is not a
harmless head start.  It builds a case for publishing that did not exist before, and the
sunk work is exactly what makes the answer feel obvious when it is not.  Investigate as far
as the verdict; then ask; then write what was approved.

A comment earns its notifications when it carries something a reader of the issue cannot get
from the issue:

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

### Size the comment for the person it addresses, not for the finding

The test above is about content -- is it true, is it new.  It says nothing about whether the
person it is addressed to will read it, and that turns out to be a separate failure.

#133 is the case.  A contributor had come to a 2014 issue about an error message, asked which of
two wordings was wanted, and got no reply for three months.  The comment written in response ran
about 450 words: a transcript, an attributed commit for why the message had changed, a bolded
argument about why naming the coefficient ring would go stale, and a cross-reference to #1518.
Every claim in it held up -- it was rechecked afterwards and nothing needed correcting.

It still failed.  The reply said the text was not something they could follow, read the issue as
deprecated, and declined to work through it -- somebody who had arrived offering to help came away
thinking the thing was dead.  Doug then answered the same question in about sixty words -- we fall
back on this message when no hook claims the map, and it is not even true that a field is required,
since #4222 -- and closed it.

**The reply is not quoted here and the asker is not named.**  Somebody saying a wall of text lost
them is doing us a favour, and a document that preserves the moment for them to find later would
be a poor way to take it.  The number is enough to find the thread; the lesson does not need the
person.

So: **a comment addressed to a named non-maintainer is a reply to a person, and it has a budget.**
The register that is right for Mike on #985, who wrote the engine and wants the commit numbers, is
wrong for somebody passing through who asked a one-sentence question.  Two habits follow.

- **Lead with the answer to what they actually asked.**  #133's asker wanted to know which wording
  to use.  That answer was in the comment, in the fourth paragraph, under a heading about method
  tables.
- **If the finding needs 450 words, the issue is not the place for all of them.**  Put the answer
  on the thread and leave the reconstruction in `note`, which is what that column is for.  Nothing
  is lost -- the catalog is cited from every comment's provenance line.

The counter-case is real and worth keeping straight: #984's comment is long on purpose, because
its reader is whoever picks up an engine build-time project and the finding that saves them a day
is a compiler error four paragraphs in.  Length is not the fault.  Length aimed at the wrong reader
is.

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

Label *additions* are otherwise decided while reading rather than brought over one at a time,
and they accumulate in `addlabels` as a batch. **That is about how the decision is reached, not
about whether it may be published.** Applying them is
[a public action like any other](#nothing-public-happens-without-doug-saying-so-first-every-time)
and needs its own authorization; that they notify nobody and are trivially reversible does not
enter into it.

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

The one exception is spelled out longhand, `dup = "supersedes #N"`, and closes an issue into
a **newer** one. It is for the case where the old issue states a problem and a later issue
states the design the thread settled on: #776 (documentation databases stay open, one per
package) into #1643 (use a single database), where closing the newer would have thrown away
the agreed solution and kept the complaint. Requiring the word is the whole safeguard — a
bare `#N` still refuses every backwards direction, so the accident the check exists for is
caught exactly as before, and only a sentence nobody types by mistake gets through.

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

**An issue being closed is not a reason to skip its type and label.**  Setting them
is one of the three questions this sweep exists to answer, and a closed issue is
still a searchable record -- somebody looking for the build-tooling work, or for
every `Documentation` issue whether open or shut, should find it.  So "close it
without bothering to type it" is never the shortcut it looks like, and it should
not be offered as an option.  This is the reason the write-back order in
[Close last](#close-last-the-write-backs-are-order-dependent) is what it is: the
close goes after the annotations because the annotations are wanted, not merely
because `apply-types` happens to skip closed issues.

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

## The 138 issues this directory filed are triaged like any others

The first project filed 138 issues that are still open, all carrying the
[`bugs directory`](https://github.com/Macaulay2/M2/labels/bugs%20directory) label.

**They were originally seeded `verdict=stands, action=keep` and excluded from `--batch`**, on the
reasoning that they had been verified against current M2 and labelled within the fortnight before
this sweep began, so re-reading them would be redoing fresh work. That was wrong twice over, and
both faults surfaced in batch 19.

- **A duplicate is only visible from one side.** #4504 is our own filing of Dan's
  *"rename `minimalPresentation` to `minimizePresentation`"*. Triaging #1013 -- eisenbud, 2019,
  *"maybe call this something other than minimalPresentation"* -- showed the two are the same ask,
  with #1013 seven years older and therefore the survivor. Nothing about #4504 could have revealed
  that, because the older issue had not been read yet when #4504 was filed. Excluding the newer
  side means the pairing is found only by luck.
- **They were labelled at filing, but never typed.** All 138 carried no GitHub issue type at all.
  Types are one of the three questions this sweep exists to answer, so 138 rows were being skipped
  on a question that had never been asked of them.

They are now `todo` like everything else, with their filing notes preserved. Doug's call, and the
argument for it is that the cost is low -- the verification behind them is recent and holds -- while
the thing being bought is the half of duplicate detection that only works from the newer side.

What stays true from the original reasoning:

- **As duplicate *targets*, they always counted.** When one pairs with an older issue, the *older*
  one survives, which will usually mean the pre-2020 issue rather than ours.
- **Not extracted, not run.** Their bodies contain this project's own transcripts and attribution
  paragraph, and running them re-runs work already done.
- **Not used to train the label suggester**, which would otherwise learn this directory's labelling
  habits and report them back as corpus evidence.

### `--batch` has a cohort boundary, and it will run out

`bin/show --batch` selects rows with `verdict == "todo"` **and `created < --before`**, where
`--before` defaults to `2020-01-01`. That is a deliberate oldest-first strategy, but it is silent
when it runs dry: the batch simply comes back short rather than saying why. At the end of batch 19
it returned seven issues instead of ten, with 586 untriaged rows sitting on the far side of the
boundary -- and all 138 of the reinstated filings are dated 2026, so they are on that side too.

Moving the boundary (`--before 2030-01-01`) fills the batch again. Worth knowing before reading a
short batch as "nearly finished".

## Finding the commit: ask the timeline before you ask the log

A `fixed` verdict is supposed to name the commit or PR. For #603, #604 and #606 I first
wrote all three comments saying the commit could not be identified, and Doug asked how hard
I had looked. The answer was one `git log --grep` and one `git log -S` between the three of
them, and for #606 nothing at all. All three were then found in about ten minutes. The order
that worked, cheapest first:

1. **The issue's own timeline.**
   `gh api repos/Macaulay2/M2/issues/<N>/timeline --jq '.[]|select(.event=="cross-referenced")'`
   GitHub already records every PR that mentioned the issue. This alone found #603 → PR
   #3149, titled *"Attempted fix for bug in quotient for matrices over finite field"* — the
   reporter had fixed his own issue and nobody closed it. **Run this first, always.** It is
   one request and it is the only source that knows about links the commit messages don't.
2. **PR search on the symptom**, not on the issue number:
   `gh api -X GET search/issues -f q='repo:Macaulay2/M2 type:pr submatrix'`. This found #604 →
   PR #2766, *"fixed bug in submatrix with repeated rows"*, whose title is the issue's
   symptom verbatim. Fixes here are usually **not** linked with `fixes #N`, so searching the
   number finds nothing while searching the words finds it immediately.
3. **`git log -L <start>,<end>:<file>`** on the lines that implement the behaviour. This is
   the one I never reached for and it is the strongest of the four: for #606 it returned the
   entire arc of `applyUniformMethod` in one command — stash-into-the-object (2006), Dan's
   `27dc87a5f5` "stop caching direct sums" (2020, first shipped in **1.17.1**, confirming his
   in-thread "the caching ended in version 1.17" from memory), @mahrud's `1bcdc743f7`
   reinstating it into `Y.cache` (2024), and the generalisation in 2025. `--grep` and `-S`
   had found none of it.
4. **`git log -S`** last, and **when it returns a commit, read that commit's whole PR.** My
   `-S 'rawSubmatrix'` run for #604 *did* return `0ae41503cf`. I dismissed it because its
   message said "non-free sources and targets" rather than repeated rows — and it is the
   sibling commit, one day and one line apart, of the actual fix, in the same PR. The pickaxe
   points at a neighbourhood, not at a commit; a hit is a cue to open
   `gh api repos/.../commits/<sha>/pulls` and read the rest.

The failure mode this guards against is not a missing citation. It is a **false negative
written up as a finding**: "I could not identify the commit, so this closes on behaviour"
reads like diligence and is indistinguishable, to a later reader, from a genuinely
undocumented fix. Two of the three had ordinary, well-titled, test-carrying PRs. Prefer
saying nothing about attribution to asserting that none exists.

And the attribution is worth the ten minutes on its own merits: #603 closes much better as
*"@moorewf implemented @mahrud's suggested fix, with tests"* than as *"it doesn't happen any
more."*

## Close last: the write-backs are order-dependent

Once Doug has
[authorized the batch](#nothing-public-happens-without-doug-saying-so-first-every-time) — each
of these being a public action he has to ask for — the sequence is

    bin/approve type <numbers>                        # record what he approved
    bin/approve labels <numbers>
    bin/approve comment <numbers>
    bin/approve close <numbers>

    bin/apply-types  --apply
    bin/apply-labels --apply
    bin/publish-verdicts --only <numbers> --apply     # closes; must be last
    git push personal bug-triage                      # separately, and only if asked

The `bin/approve` runs are a record of a conversation that has already happened, not a step
that produces one. Running them to get past a refusal is the whole failure this exists to
stop, wearing a new hat.

and the order is not cosmetic. Both `apply-types` and `apply-labels` skip an issue that is
not `OPEN`:

```python
        if issue["state"] != "OPEN":
            print("#%d: closed since the last fetch, skipped" % number, ...)
```

That guard is right — an issue closed by somebody else between the fetch and the run is no
longer ours to annotate. But it cannot tell *their* close from *ours*, so running
`publish-verdicts` first means every issue in the batch is closed by the time the annotators
reach it, and the whole batch is skipped as if a stranger had closed it. In batch 12 that
took the type and label off #603, #604 and #606, and the skip messages scrolled past under a
`tail -4`.

`publish-verdicts` now refuses rather than relying on the operator's memory: before posting
anything it compares the row's `settype` and `addlabels` against the live issue and, if any
are still missing, declines the close with the command to run first. `pending_annotations()`
is a plain function so `bin/selftest` covers it — including the two cases that matter for
false positives, a row asking for no annotations at all, and a label somebody else added
that we never requested.

The general shape, worth remembering beyond this one script: **an idempotency guard keyed on
observable state cannot distinguish your own recent write from a third party's.** Anywhere
two scripts in a sequence both read the same state, the second one's safety check will read
the first one's effect as somebody else's edit.
