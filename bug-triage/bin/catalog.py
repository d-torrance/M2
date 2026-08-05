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
TITLES = os.path.join(ROOT, "issue-titles.tsv")

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
