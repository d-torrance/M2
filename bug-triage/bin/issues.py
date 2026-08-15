"""Shared helpers for reading and writing bug-triage/issues.tsv.

The catalog is a tab-separated file with a header row, one row per *open* issue on
Macaulay2/M2.  Columns 1-8 are machine-generated; columns 9-14 hold the human
verdict and must survive regeneration.

This is a sibling of catalog.py, not a generalization of it.  catalog.py is keyed on
a file path and fixes its column list at module scope, and the project it serves is
finished: its 857 rows and 191 asks all carry verdicts, and its tooling should keep
working exactly as it did.  Rewriting a settled source of truth to serve a second
schema risks the record for the sake of avoiding forty lines of duplication.  What
is genuinely shared -- the GitHub layer, the label rules, the attribution -- lives in
project.py and is imported by both.
"""

import os
import re

# Machine-generated columns, rewritten freely by the tools from cache/issues.json.
GENERATED = ["issue", "created", "updated", "author", "comments", "type",
             "labels", "repro", "run", "title"]

# Human columns, never overwritten once non-empty.
HUMAN = ["verdict", "dup", "settype", "settitle", "addlabels", "rmlabels",
         "action", "note"]

COLUMNS = GENERATED + HUMAN

VERDICTS = [
    "todo",              # not yet triaged
    "reproduces",        # ran the repro on clean M2; the reported failure is there
    "stands",            # no runnable repro, and the ask is still unmet
    "stale-repro",       # the repro fails, but on API drift, not the reported bug
    "not-reproducible",  # runs clean now; the behavior is gone, cause unknown
    "fixed",             # gone, and "note" names the commit or PR
    "duplicate",         # covered by an older issue; "dup" names it
    "not-a-bug",         # expected behavior; the answer is an explanation
    "wontfix",           # deliberate, or a maintainer said no
    "obsolete",          # the premise is gone -- subsystem removed, platform dead
    "needs-reporter",    # cannot be settled without the author
]

# What gets done about the verdict, which is a separate decision and not ours to
# infer: "fixed" is a finding, "close" is an action, and the second needs a human.
ACTIONS = ["", "close", "comment", "label-only", "keep"]

# GitHub issue types: single-valued, org-level, and a second taxonomy running
# alongside labels.  Macaulay2 has three enabled, and the split was doing real
# damage before anyone noticed it: 93 open issues carried the type "Feature" and
# 130 others carried the "bug" or "feature request" *label*, with no overlap at
# all -- two vocabularies for one distinction, each holding half the corpus.
#
# The types win, on the maintainer's call.  They are single-valued, which is what
# project.EXCLUSIVE was faking for "bug" and "feature request" anyway, and the
# labels turn out to carry almost no history to lose: "feature request" has never
# been on a closed issue and "bug" on only eleven.
#
# All three get used, including Task.  A great deal of this corpus is neither a
# defect nor a request -- #9 is a C++ template reorganisation, and there are build
# chores and test-suite cleanups by the dozen -- and a rule of "not a Bug, so a
# Feature" is exactly how the existing 93 came to describe #9 as "a request, idea,
# or new functionality".
TYPES = [
    "",         # untyped: nobody has classified it, which is honest
    "Bug",      # M2 does the wrong thing
    "Feature",  # a request for functionality M2 does not have
    "Task",     # work on the project that is neither -- refactors, chores, docs
]

# Verdicts bin/close-issues will act on at all.  A verdict outside this set can
# never produce a close, however the "action" column is filled in.
CLOSABLE = ["fixed", "not-reproducible", "duplicate", "not-a-bug", "wontfix",
            "obsolete"]

# Retitling is for an issue whose *scope* has moved, not one whose wording could
# be better.  #290 is the case that prompted it: filed in 2015 as "bug in gb over
# ZZ ? (using custom ordering)", it now tracks the capability that was removed to
# fix that bug, so the title describes a symptom nobody will see again.  Leaving
# it would mean the issue reads as a stale bug report forever.
#
# Rarely, and never quietly: it changes what everyone who has the issue bookmarked
# or in an email thread sees, so it belongs with a comment explaining the re-scope
# and needs the maintainer's go-ahead like a close does.
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CACHE = os.path.join(ROOT, "cache")
REPROS = os.path.join(CACHE, "repros")
TSV = os.path.join(ROOT, "issues.tsv")

# Comment bodies to post, one per issue, written by hand.  Kept out of the TSV for
# the reason the first project kept them out of catalog.tsv: prose for a stranger
# does not fit in a cell, and a note column that grows paragraphs stops being
# greppable.
COMMENTS = os.path.join(ROOT, "comments", "issues")


# GitHub stores whatever the browser textarea submitted, and two thirds of these
# bodies are CRLF.  Every fence and prompt regex downstream uses re.M, where "$"
# matches before the "\n" and so never matches a line still ending "\r\n" -- and it
# fails by reporting "no code block here", which reads exactly like the truth.
# Both fetchers normalize through this one function so the two caches cannot
# disagree about line endings within a single scan.
def lf(text):
    return (text or "").replace("\r\n", "\n").replace("\r", "\n")


# Raised from the 20000 the first project used.  That was enough to recognize a
# file listing; it is not enough to reach the end of a transcript, and a body cut
# off mid-transcript yields a reproducer that stops short of the line the issue is
# about -- which runs clean, for the wrong reason, and reads as "fixed".
BODY_CAP = 100000


def read(path=TSV):
    """Return a list of dicts, or [] if the catalog does not exist yet."""
    if not os.path.exists(path):
        return []
    rows = []
    with open(path, encoding="utf-8") as f:
        header = f.readline().rstrip("\n").split("\t")
        for line in f:
            if not line.strip():
                continue
            values = line.rstrip("\n").split("\t")
            # Tolerate short rows from hand-editing in an editor that trims tabs.
            values += [""] * (len(header) - len(values))
            rows.append(dict(zip(header, values)))
    return rows


def write(rows, path=TSV):
    """Write rows sorted by issue number, validating as we go.

    Validated harder than catalog.tsv is.  There, an unknown verdict was caught
    only for asks; here the columns drive irreversible actions on other people's
    issues, so a typo in "verdict" or "action" must stop the run rather than sit in
    the file until bin/close-issues reads it.
    """
    seen = set()
    for r in rows:
        n = r.get("issue", "")
        if not n.isdigit():
            raise ValueError("issue number must be a positive integer, got %r" % n)
        if n in seen:
            raise ValueError("duplicate row for issue %s" % n)
        seen.add(n)
        if r.get("verdict", "") not in VERDICTS:
            raise ValueError("unknown verdict %r on #%s" % (r.get("verdict"), n))
        if r.get("action", "") not in ACTIONS:
            raise ValueError("unknown action %r on #%s" % (r.get("action"), n))
        if r.get("settype", "") not in TYPES:
            raise ValueError("unknown issue type %r on #%s -- one of %s"
                             % (r.get("settype"), n,
                                ", ".join(repr(t) for t in TYPES if t)))
        check_dup(r)

    tmp = path + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        f.write("\t".join(COLUMNS) + "\n")
        for r in sorted(rows, key=lambda r: int(r["issue"])):
            values = []
            for c in COLUMNS:
                v = r.get(c, "")
                if "\t" in v or "\n" in v:
                    raise ValueError(
                        "tab or newline in %s of #%s: %r" % (c, r["issue"], v))
                values.append(v)
            f.write("\t".join(values) + "\n")
    os.replace(tmp, path)


DUP_RE = re.compile(r"^#?(\d+)$")
SUPERSEDES_RE = re.compile(r"^supersedes\s+#?(\d+)$")


def check_dup(row):
    """The "dup" column names the *older* issue.  Enforce it, every write.

    Direction is the one error in this project that is both easy to make and
    invisible afterwards: swap the two numbers and bin/close-issues closes the
    issue that should have survived and points it at the one that should not have.
    Nothing downstream re-derives which is which, so it has to be checked here.

    The exception is written out longhand: "supersedes #N" closes this issue into a
    *newer* one.  It comes up when the old issue states a problem and a later one
    states the design the thread settled on -- #776 (documentation databases stay
    open) into #1643 (use a single database), where closing the newer would throw
    away the agreed solution and keep the complaint.  Requiring the word means the
    bare "#N" form still refuses every backwards dup, so the accident this function
    exists to catch is caught exactly as before; only a sentence nobody types by
    mistake gets through.
    """
    dup = row.get("dup", "").strip()
    if not dup:
        return
    if SUPERSEDES_RE.match(dup):
        return
    m = DUP_RE.match(dup)
    if not m:
        raise ValueError(
            "dup must be #N, N, or \"supersedes #N\", got %r on #%s"
            % (dup, row["issue"]))
    if int(m.group(1)) >= int(row["issue"]):
        raise ValueError(
            "dup #%s is not older than #%s -- the older issue survives.  If you mean "
            "to close this one into the newer issue, write \"supersedes #%s\"."
            % (m.group(1), row["issue"], m.group(1)))


def dup_number(row):
    """The integer in the "dup" column, or None.

    Both forms resolve to the surviving issue, which is what every caller wants:
    publish-verdicts passes it to GitHub as duplicateIssueId, and render links it.
    """
    dup = row.get("dup", "").strip()
    m = SUPERSEDES_RE.match(dup) or DUP_RE.match(dup)
    return int(m.group(1)) if m else None


def labels_of(row, column="labels"):
    """A comma-separated label column as a list.  No M2 label contains a comma."""
    return [x.strip() for x in row.get(column, "").split(",") if x.strip()]


def by_number(rows):
    return {int(r["issue"]): r for r in rows}


def require(rows, what="issues.tsv"):
    if not rows:
        raise SystemExit(
            "%s is empty or missing -- run bin/fetch-issues then bin/init-issues"
            % what)
    return rows
