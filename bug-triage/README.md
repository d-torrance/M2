# Squashing the triaged bugs

This directory's first two projects are finished and are recorded in
[`README-bugs-directory.md`](README-bugs-directory.md) and
[`README-open-issues.md`](README-open-issues.md). Both ended at the same line: *"Acting on a
verdict — writing the fix, promoting a reproducer into `M2/Macaulay2/tests/` — is separate work in
a separate branch."* This is that work.

`issues.tsv` holds 747 open issues, every one read and verified, each with a type, a verdict and a
**note**. The notes are the asset, and they are dense: **442 name a Macaulay2 source file, 334
pinpoint a `file.ext:LINE`**, 86 cite a commit sha, 65 name a PR. Many state the cause outright.

> **#747**: `trim over ZZ[..] returns a STRICTLY SMALLER ideal (y^2+1 lost); forcing gens gb first
> fixes both this and #4095. trim uses StopWithMinimalGenerators to avoid a full gb
> (matrix2.m2:228-230), unsound over ZZ; the only ZZ guard is ring M === ZZ at :281, which ZZ[x]
> fails.`

That changes what "fix the bugs" means. Finding a bug is done. The scarce thing is deciding **which
six to open at once**, so that six fixes cost less than six times one.

## The split

**This project picks the group and writes the dossier. Doug writes the fixes.**

Nothing here commits to `M2/` and nothing here opens a PR. Candidate patches are written and *run*,
then reverted; what survives is the diff quoted in the dossier, with what actually happened when it
ran. How the fixes are split across commits and PRs is Doug's call, and a group is not promised to
be one PR.

It runs again: when a group is done — or abandoned for something more interesting — the next
dossier gets requested. So the tooling has to make dossier N+1 cheap, which is what `parking.tsv`
is for.

## The first group is an output, not an input

`bin/cluster` ranks candidate groups and the top one, once its shared cause is confirmed by
reading, is the group. It ranked
[`groups/trim-over-non-fields.md`](groups/trim-over-non-fields.md) first at 16.4, and that group
held up: six issues, three causes, one file, and a **one-line patch that fixes two silent wrong
answers in `trim`** — written, run, and measured against all 32 relevant tests with no regressions.

## How a group gets picked

    bin/index-files     notes -> cache/file-index.tsv        the missing reverse index
    bin/score           six axes -> scores.tsv               one column each, no composite
    bin/cluster         ranked candidate groups of 4-8       with the per-axis breakdown
    bin/park            parking.tsv                          what to look at next time

### The six axes

| axis | from | why |
| --- | --- | --- |
| severity | regex over note + title | leads — see below |
| type | the `type` column | Bug > Task > Feature |
| reach | labels + a curated everyday-function list | who actually hits it |
| readiness | note has `file:LINE`? repro ran? | how much of the fix is already written down |
| age | `created`, but `bugs directory` rows score 0 | they are pre-GitHub old, not new |
| settled | is the right answer disputed? | negative — see below |

**Severity leads, not type.** A feature request saying "the documented behaviour does not exist"
costs a user an afternoon; a wrong answer costs them a false theorem they will never know about,
because a strictly smaller ideal is not an error. Over the 747: **34 wrong-answer** (27 Bug),
**46 crash**, **14 hang**, 3 leak.

**`settled` is the axis triage never needed.** #4308's note says it *"built a case that separates
the three candidate behaviors"* — nobody can write that fix until a maintainer rules on which is
correct, so it scores down and gets parked however severe it is.

**No composite score is written to `scores.tsv`.** `bin/cluster` applies the weights, prints them,
and takes `--weights severity=5,age=0` to change them. A ranking you cannot argue with is a ranking
you have to take on faith, and the one contested judgement in the pipeline is the weighting.

### Clustering: the same neighbourhood of the same file

Affinity is dominated by one signal — two notes pinpointing lines within 80 of each other in the
same file. That is the strongest evidence of a shared fix site and it is *available*, on 433 of the
627 indexed mentions. Note vocabulary only forms an edge above a high cosine, because every note in
`matrix2.m2` talks about matrices and generators.

Three things were learned building it, each of which changed the output:

**Resolution has to be tiered, or the best clusters vanish.** A bare `ringmap.m2` ties with
`tests/normal/ringmap.m2`, and treating that as ambiguous dropped `ringmap.m2` from 13 issues to 6.
A triage note pinpointing `ringmap.m2:236` means the implementation; if it meant the test it would
have said so. Ties *within* a tier stay unresolved — `aring.cpp` really is two engine files.

**Transitive closure chains, and has to be bounded.** Unbounded, the 80-line window walked
`matrix2.m2` from :10 to :358 and called the whole file one group. `MAX_SPAN` refuses a merge whose
footprint would exceed 150 lines, and edges are considered closest-first so tight neighbourhoods
form before loose ones can bridge them.

**Line arithmetic alone misses the best evidence there is.** #747's note ends *"forcing gens gb
first fixes both this and #4095"* — one sentence asserting that two issues have one fix — and #4095
carries no line number at all, so no amount of proximity would ever have found it. `bin/cluster`
prints cross-references from members' notes as *offered* attachments, not merged ones: a note cites
an issue to distinguish itself from it as often as to join it, and #747's own note cites #146 only
to say it now passes.

### A candidate group is a hypothesis

File co-occurrence finds candidates; a group exists once someone confirms the rows share a cause.
That confirmation is the dossier, and it earns its keep — in group 1 the tool included #3738
(cause actually in `matrix1.m2`, and it seeds its own group) and excluded #4095 (which belonged).
Both corrections came from reading, not from arithmetic. The ranking is a **reading order**.

## What a dossier contains

Diagnosis and fix site, and where a patch suggests itself, the patch — **written and run**, with
what happened. A patch that was tried and *failed* is worth as much as one that worked; it turns
"3 days, probably" into "this is a week and it needs Mike".

Per issue: re-verified repro, diagnosis confirmed against a `development` build, blast radius,
a drafted regression test **observed to fail before the patch and pass after**, and any open
question. Then: existing PRs touching the same lines, what was considered and left out, what was
parked, and a suggested commit split.

## Reused, not rewritten

`issues.py` (validating TSV I/O), `project.py` (GraphQL, `linkify`, `ATTRIBUTION`, and the
`ISSUE_REF` lookbehind that exists because M2 subscripting writes `(x+1)#0`), `labels.py`,
`approvals.py` + `bin/approve`, `bin/show`, `bin/extract-repros` and `bin/run-issue-repros`.

`bin/selftest` gains four predicates, each of which was wrong on first writing and each of which
fails *silently*: a mis-resolved path sends someone to the wrong file, and a mis-scored axis just
reorders a ranking nobody can check by eye. Two were caught only by testing against issues whose
answer was already known — the severity guard scored *"a wrong answer that can no longer occur"* as
a wrong answer, and the `settled` axis penalised #256 for the phrase *"Answers mahrud's unanswered
2024 question"*, which is to say it penalised a row precisely for having been diagnosed thoroughly.

## The gate still binds

Unchanged from the two projects before this one: **nothing visible outside this working copy
happens without Doug saying so, for that action, in advance.** Pushing the branch, commenting on an
issue, opening a PR. Anything under `M2/` is his regardless — this branch does not modify it, and
candidate patches are reverted before the commit that describes them.
