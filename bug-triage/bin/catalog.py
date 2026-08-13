"""Shared helpers for reading and writing bug-triage/catalog.tsv.

The catalog is a tab-separated file with a header row, one row per file from the
bugs/ tree that d2c8d27 deleted.  Columns 1-6 are machine-generated; columns
7-11 hold the human verdict and must survive regeneration.
"""

import os
import re

# Machine-generated columns, rewritten freely by the tools.
GENERATED = ["path", "owner", "prio", "kind", "lines", "autorun"]

# Human columns, never overwritten once non-empty.
HUMAN = ["verdict", "issue", "fix", "disposition", "note"]

COLUMNS = GENERATED + HUMAN

VERDICTS = [
    "todo",         # not yet triaged
    "open",         # still reproduces, or the feature is still missing
    "fixed",        # no longer reproduces; "fix" names the commit or PR
    "duplicate",    # covered by an existing issue; "issue" names it
    "wontfix",      # deliberate behavior, or the subsystem is gone
    "obsolete",     # premise no longer applies
    "stale-repro",  # fails only on outdated API usage; needs rewriting first
]

DISPOSITIONS = ["", "test", "quarantine", "goals", "issue", "drop"]

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FILES = os.path.join(ROOT, "files")
CACHE = os.path.join(ROOT, "cache")
TSV = os.path.join(ROOT, "catalog.tsv")

# What a filed issue needs that a verdict does not: a readable title, and the
# labels that put it in front of whoever works on that part of M2.  Kept out of
# catalog.tsv because they apply to the handful of rows that become issues, not
# to all 857.
#
# Keys are catalog paths, and additionally ask keys of the form "path::n" -- see
# asks.tsv below.  Nothing here needs to know the difference: a key is a string.
TITLES = os.path.join(ROOT, "issue-titles.tsv")

# One row per *ask* inside a file, for the wishlist files that hold many
# unrelated requests.  Those rows sat "parked" in catalog.tsv -- verdict=open
# with a blank disposition, so bin/file-issues skips them -- precisely because a
# file is not the thing that corresponds to an issue: bugs/dan/IDEAS is ten
# unrelated asks in 26 lines, and filing it whole would produce an issue nobody
# can close.
#
# So the file keeps its catalog row as a summary and the asks get their own rows
# here.  A file's catalog row then carries every issue its asks produced, space
# separated in the "issue" column, which the format already allowed -- see
# bugs/dan/0-degrees-of-maps, which names "#607 #1060".
#
# "n" is the ask's number within its file, 1-based and stable: renumbering would
# silently repoint an already-filed issue at a different ask.  "ask" is a short
# label for reading the TSV, not for publishing.
ASKS = os.path.join(ROOT, "asks.tsv")

ASK_COLUMNS = ["path", "n", "ask", "verdict", "issue", "fix", "disposition", "note"]


def ask_key(row):
    """The key an ask uses in issue-titles.tsv and under issues/.

    Deliberately not a path that could collide with a real file: no file in the
    bugs/ tree contains "::".
    """
    return "%s::%s" % (row["path"], row["n"])

# Priority prefix on the filename, e.g. "0-decompose.m2" or "0.5-debian-script".
PRIO_RE = re.compile(r"^([0-9]+(?:\.[0-9]+)?)-")


def owner_of(path):
    """Second path component: bugs/dan/foo -> dan.  bugs/README -> (root)."""
    parts = path.split("/")
    return parts[1] if len(parts) > 2 else "(root)"


def prio_of(path):
    m = PRIO_RE.match(os.path.basename(path))
    return m.group(1) if m else ""


def kind_of(path):
    """Only lowercase .m2 counts as a runnable reproducer.

    bugs/anton/NOTES.M2 is prose despite its extension, so match case-sensitively.
    """
    return "repro" if path.endswith(".m2") else "note"


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
    """Write rows sorted by path.  Tabs and newlines in values are not allowed."""
    rows = sorted(rows, key=lambda r: r["path"])
    tmp = path + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        f.write("\t".join(COLUMNS) + "\n")
        for r in rows:
            values = []
            for c in COLUMNS:
                v = r.get(c, "")
                if "\t" in v or "\n" in v:
                    raise ValueError(
                        "tab or newline in %s of %s: %r" % (c, r["path"], v))
                values.append(v)
            f.write("\t".join(values) + "\n")
    os.replace(tmp, path)


def read_asks(path=ASKS):
    """Rows from asks.tsv, or [] if nobody has split a file into asks yet."""
    if not os.path.exists(path):
        return []
    rows = []
    with open(path, encoding="utf-8") as f:
        header = f.readline().rstrip("\n").split("\t")
        for line in f:
            if not line.strip():
                continue
            values = line.rstrip("\n").split("\t")
            values += [""] * (len(header) - len(values))
            rows.append(dict(zip(header, values)))
    return rows


def write_asks(rows, path=ASKS):
    """Write ask rows sorted by path then ask number.

    Validated harder than catalog.tsv is, because these rows are written entirely
    by hand and a duplicate or non-numeric "n" would misfile an issue: the ask
    number is what ties a row to its title and its draft body.
    """
    def sort_key(r):
        return (r["path"], int(r["n"]))

    seen = set()
    for r in rows:
        if not r.get("n", "").isdigit() or int(r["n"]) < 1:
            raise ValueError("ask number must be a positive integer, got %r in %s"
                             % (r.get("n"), r.get("path")))
        key = (r["path"], int(r["n"]))
        if key in seen:
            raise ValueError("duplicate ask %s" % ask_key(r))
        seen.add(key)
        if r.get("verdict", "") not in VERDICTS:
            raise ValueError("unknown verdict %r in %s" % (r.get("verdict"), ask_key(r)))
        if r.get("disposition", "") not in DISPOSITIONS:
            raise ValueError("unknown disposition %r in %s"
                             % (r.get("disposition"), ask_key(r)))

    tmp = path + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        f.write("\t".join(ASK_COLUMNS) + "\n")
        for r in sorted(rows, key=sort_key):
            values = []
            for c in ASK_COLUMNS:
                v = r.get(c, "")
                if "\t" in v or "\n" in v:
                    raise ValueError(
                        "tab or newline in %s of %s: %r" % (c, ask_key(r), v))
                values.append(v)
            f.write("\t".join(values) + "\n")
    os.replace(tmp, path)


def asks_by_path(rows):
    """path -> [ask rows], each list in ask-number order."""
    out = {}
    for r in rows:
        out.setdefault(r["path"], []).append(r)
    for v in out.values():
        v.sort(key=lambda r: int(r["n"]))
    return out


def read_titles(path=TITLES):
    """path -> {"title": str, "labels": [str]} from issue-titles.tsv.

    The labels column is comma-separated and may be absent or empty; no label
    name in Macaulay2/M2 contains a comma.  Names are not validated here -- that
    needs the repository, so it happens in project.check_labels.
    """
    if not os.path.exists(path):
        return {}
    out = {}
    with open(path, encoding="utf-8") as f:
        f.readline()
        for line in f:
            if not line.strip():
                continue
            values = line.rstrip("\n").split("\t")
            values += [""] * (3 - len(values))
            out[values[0]] = {
                "title": values[1],
                "labels": [x.strip() for x in values[2].split(",") if x.strip()],
            }
    return out


def by_path(rows):
    return {r["path"]: r for r in rows}


def require(rows, what="catalog"):
    if not rows:
        raise SystemExit(
            "%s is empty or missing -- run bin/extract then bin/init-catalog first"
            % what)
    return rows
