#!/usr/bin/env python3
"""Turn the per-row TSV fragments into the two tables for PR #4643.

Reads whatever .github/gfan-matrix/run.sh left in each row's artifact and
writes markdown.  Nothing here decides what the answer should be: every cell
comes from what the toolchain actually did, so a row of the PR comment's table
that does not reproduce will show up as such.
"""

import glob
import os
import sys

# The blockers listed in the PR comment, in that order, each with the probe
# that measures it.  The comment counts ten; std::erase_if and std::filesystem
# share a row there and are split here, because they fail on different
# compilers and for different reasons.  See .github/gfan-matrix/probes/ for
# what each one compiles and which gfan source it was distilled from.
BLOCKERS = [
    ("execution", "the execution header, for std::execution::par", "execution"),
    ("semaphore", "the semaphore header, for std::counting_semaphore", "semaphore"),
    ("pmr", "std::pmr, woven through the core matrix and vector types", "std::pmr"),
    ("erase_if", "std::erase_if", "erase_if"),
    ("filesystem", "the filesystem header, and -lstdc++fs on GCC 8", "filesystem"),
    ("operator_ne", "operator!= left to C++20's rewritten operator==", "operator!="),
    ("multimap_lambda", "a multimap default-constructing a lambda comparator", "multimap cmp"),
    ("std_cxx20", "the hardcoded -std=c++20", "-std=c++20"),
    ("gxx15", "the Makefile's gcc-15/g++-15 pin on macOS", "g++-15"),
    ("no_guess_bp", "-fno-guess-branch-probability on one object", "-fno-guess-bp"),
    ("march_native", "the unconditional -march=native", "-march=native"),
]

# Oldest toolchain first, so the table reads the way the argument does.
ORDER = [
    "ubuntu-18.04-gcc7", "ubuntu-18.04-gcc8", "rocky-8-gcc8",
    "ubuntu-20.04-gcc9", "ubuntu-20.04-gcc10", "debian-bullseye-gcc10",
    "ubuntu-22.04-gcc11", "ubuntu-24.04-gcc13", "ubuntu-26.04-gcc15",
    "ubuntu-22.04-clang14", "ubuntu-24.04-clang18-libc++",
    "arm64-gcc13", "arm64-clang18",
    "macos-15-arm64", "macos-15-intel",
]

ROW_FIELDS = [
    "row", "kernel", "arch", "compiler", "std",
    "unpatched", "unpatched_msg", "unpatched_clang", "patched", "tests",
]


def read_rows(root):
    rows = []
    for path in sorted(glob.glob(os.path.join(root, "*", "row.tsv"))):
        with open(path) as f:
            fields = f.read().rstrip("\n").split("\t")
        fields += [""] * (len(ROW_FIELDS) - len(fields))
        row = dict(zip(ROW_FIELDS, fields))
        row["probes"] = read_probes(os.path.join(os.path.dirname(path), "probes.tsv"))
        rows.append(row)
    rows.sort(key=lambda r: (ORDER.index(r["row"]) if r["row"] in ORDER else len(ORDER),
                             r["row"]))
    return rows


def read_probes(path):
    probes = {}
    if not os.path.exists(path):
        return probes
    with open(path) as f:
        for line in f:
            parts = line.rstrip("\n").split("\t")
            if len(parts) >= 3:
                probes[parts[0]] = (parts[1], parts[2], parts[3] if len(parts) > 3 else "")
    return probes


def cell(row, key):
    """One blocker on one system.

    `blocked` always means "this toolchain cannot do what gfan 0.8beta asks",
    whether that is a missing header, a construct the language does not have
    yet, or a compiler flag that is rejected."""
    probes = row["probes"]
    # The gcc-15 pin is inside gfan's Darwin branch; elsewhere it cannot bite.
    if key == "gxx15" and row["kernel"] != "Darwin":
        return "—"
    if key not in probes:
        return "?"
    best, cxx17, _ = probes[key]
    if best == "blocked":
        return "**✗**"
    if best == "warned":
        return "!"
    if best == "n/a":
        return "?"
    return "✓¹" if cxx17 == "blocked" else "✓"


def escape(text):
    return text.replace("|", "\\|").replace("\n", " ").strip()


def main():
    root = sys.argv[1] if len(sys.argv) > 1 else "artifacts"
    out = sys.argv[2] if len(sys.argv) > 2 else "report.md"
    rows = read_rows(root)
    if not rows:
        sys.exit("no row.tsv found under %s" % root)

    L = []
    L.append("### Which blocker each toolchain actually hits\n")
    L.append("| System | Compiler | " + " | ".join(b[2] for b in BLOCKERS) + " |")
    L.append("|---|---|" + "---|" * len(BLOCKERS))
    for r in rows:
        cells = [cell(r, b[0]) for b in BLOCKERS]
        L.append("| `%s` | %s | %s |" % (r["row"], escape(r["compiler"]), " | ".join(cells)))
    L.append("")
    L.append("**✗** the toolchain cannot do what gfan 0.8beta asks — the patch is "
             "required here &nbsp;·&nbsp; ✓ fine &nbsp;·&nbsp; ✓¹ fine at "
             "`-std=c++20`, but not at `-std=c++17` &nbsp;·&nbsp; `!` accepted, "
             "but with a diagnostic\n")
    L.append("The ✓¹ cells are the same blocker seen from the other end.  GCC 7 has "
             "no `-std=c++20` flag at all and GCC 9 spells it `c++2a`, so supporting "
             "those compilers means the tree has to compile as C++17 — and that, in "
             "turn, is what rules out the constructs those columns test, even though "
             "a newer compiler handles them happily.\n")
    L.append("Each column is a standalone probe distilled from the gfan source that "
             "uses it; the probes exist because `src/log.h` line 1 is "
             "`#include <execution>` and 110 translation units include it, so an "
             "unpatched build can only ever fail on that one blocker.\n")
    L.append("Blockers by name: " + " &nbsp;·&nbsp; ".join(
        "`%s` = %s" % (b[2], b[1]) for b in BLOCKERS) + "\n")

    L.append("### Unpatched vs. patched\n")
    L.append("| System | Arch | Compiler | Best `-std=` | gfan 0.8beta as shipped "
             "| with `patch-0.8beta` | `gfan _test` |")
    L.append("|---|---|---|---|---|---|---|")
    for r in rows:
        unpatched = r["unpatched"]
        if unpatched == "failed":
            unpatched = "fails"
        elif unpatched == "built":
            unpatched = "**builds**"
        patched = "builds" if r["patched"] == "built" else "**%s**" % r["patched"]
        L.append("| `%s` | %s | %s | `%s` | %s | %s | %s |" % (
            r["row"], r["arch"], escape(r["compiler"]), r["std"],
            unpatched, patched, r["tests"] or "—"))
    L.append("")

    L.append("<details><summary>First error from each unpatched build</summary>\n")
    for r in rows:
        if r["unpatched"] == "built":
            note = "_built unmodified; this toolchain is new enough_"
        else:
            note = escape(r["unpatched_msg"]) or "_(no error line captured)_"
        L.append("- `%s` — %s" % (r["row"], note))
        if r["unpatched_clang"] and not r["unpatched_clang"].startswith("n/a"):
            L.append("  - forcing the system compiler: %s" % escape(r["unpatched_clang"]))
    L.append("\n</details>")

    text = "\n".join(L) + "\n"
    with open(out, "w") as f:
        f.write(text)
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a") as f:
            f.write(text)
    print(text)


if __name__ == "__main__":
    main()
