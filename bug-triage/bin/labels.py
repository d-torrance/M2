"""Rules for proposing topic labels on an open issue.

Nothing here is applied automatically, and the reason is measured rather than
cautious.  Run `bin/suggest-labels --score`: against the 292 open issues somebody
else has labelled, the good rules reach 0.93 (`Documentation`), 0.86 (`build
issue`) and 0.85 (`WeylAlgebras`), and the rest fall away sharply.

Read that scoreboard carefully, because it does not mean what it looks like.  The
corpus is *under-labelled*, so a "false positive" is as often a label nobody got
round to adding as a label that is wrong.  Of the issues the `arm` rule was
scored wrong on, "building M2 on Apple silicon MacOS Sonoma" and "cmake build
fails on aarch64" are both plainly arm issues that simply do not carry the label
-- while three of the issues it was scored as *missing* are about i386, which is
not arm at all.  The measured precision is a lower bound, and the ground truth
disagrees with itself.

Which is the argument, not a caveat to it.  If the existing labels were consistent
enough to score against properly, they would be consistent enough to learn from;
they are neither.  So the rules order the reading queue and catch what reading
misses -- the opposite failure, and just as real -- and a person looking at the
issue decides.

Three tiers:

  1  structural, high precision.  Proposed by default; confirming is one keystroke.
  2  scored, but only settled by reading.  `Core` in particular means "the m2/
     scripts", which no vocabulary separates from `Interpreter`.
  3  never mechanical.  See project.JUDGMENT_ONLY.

The Tier 1 rules deliberately prefer *structure* over vocabulary wherever there is
a choice.  `Engine` by keyword scored 0.25 because every algebra issue says
"engine" somewhere; `Engine` by "the body contains a C++ backtrace or cites a path
under Macaulay2/e/" is nearly always right.  Recall drops.  Recall is not the goal:
a label that is usually wrong is worse than no label, because it sends the issue
to the wrong person and then looks settled.
"""

import re


def _rx(pattern):
    return re.compile(pattern, re.I | re.M)


# ---------------------------------------------------------------- tier 1

# The strongest single build signal is structural and lives outside this table:
# an issue whose only code block is shell.  See suggest() below.
ARM = _rx(r"\b(aarch64|arm64|armhf|armv[67]|arm-linux|apple silicon|"
          r"m1 mac|m2 mac|raspberry ?pi)\b")

BUILD = _rx(r"\./configure\b|\bautoreconf\b|\bautomake\b|\bcmake\b|\bmakefile\b|"
            r"\bmake -[jCf]\b|\bconfig\.log\b|\bldflags\b|\bcppflags\b|"
            r"\bpkg-config\b|undefined reference to|cannot find -l\w|"
            r"\bcollect2: error\b|No rule to make target")

DISTRO = _rx(r"\b(ubuntu|debian|fedora|centos|rhel|redhat|arch linux|gentoo|"
             r"opensuse|alpine|homebrew|macports|msys2?|cygwin|mingw|nixos|"
             r"conda-forge)\b")

# A path into the engine, a C++ frame, or a raw* entry point.  "engine" as a word
# is deliberately absent: it appears in issues about every part of M2.
ENGINE = _rx(r"Macaulay2/e/|\be/[a-z0-9_-]+\.(cpp|hpp|cc|hh)\b|"
             r"\b\w+\.(cpp|hpp)\b:\d+|\bstd::|\bboost::|^\s*#\d+\s+0x[0-9a-f]+|"
             r"\braw[A-Z]\w+\b|\bengine\.dd\b|\binterface\.dd\b")

DOCUMENTATION = _rx(r"\bviewHelp\b|\bbeginDocumentation\b|\bSimpleDoc\b|"
                    r"\bdocumentation node\b|\bundocumented\b|\bdoc node\b|"
                    r"\bmakeDocumentBody\b|\bhelp\s+\"|\bSYNOPSIS\b|"
                    r"\bdocumentation for\b|\bmissing documentation\b")

DOC_TITLE = _rx(r"\b(document|documents|documented|documentation|docs|typo|"
                r"manual|misspell|wording)\b")

JUPYTER = _rx(r"\bjupyter\b|\bipython\b|\bnotebook\b|\bkernel\.json\b")

EDITORS = _rx(r"\bemacs\b|\bM2\.el\b|\bvscode\b|\bvs code\b|\bvim\b|\bnvim\b|"
              r"\bsyntax highlight|\blanguage[- ]server\b|\btextmate\b|"
              r"\bM2-mode\b|\batom\b")

# "leak" only.  "out of memory" is the OOM family -- a dozen issues about `check`
# exhausting a 32-bit address space -- and is a different thing entirely.
MEMORY_LEAK = _rx(r"\bmemory leak\b|\bleaking memory\b|\bleaks memory\b|"
                  r"\bvalgrind\b|\bLeakSanitizer\b|\bdefinitely lost\b")

THREADS = _rx(r"\ballowableThreads\b|\bschedule\b\s*\(|\btaskResult\b|"
              r"\bnThreads\b|\bpthread\b|\brace condition\b|\bthread[- ]safe\b|"
              r"\bTask\b|\bparallel\b")

GROEBNER = _rx(r"\bgroebner ?bas|\bgröbner ?bas|\bmathicgb\b|\bGroebnerBasis\b")

# Package labels: the package has to be named, not merely implied.
PACKAGES = {
    "NormalToricVarieties": _rx(r"\bNormalToricVarieties\b|\bNormalToricVariety\b|"
                                r"\btoricDivisor\b|\bToricMap\b"),
    "WeylAlgebras": _rx(r"\bWeylAlgebra\b|\bDmodules\b|\bDmodule\b|\bmakeWA\b"),
    # No library names.  "flint", "eigen" and "LAPACK" name the things M2 links
    # against, so they appear in every build failure that touches them -- "Errors
    # compiling eigen.cpp" and "caching in github workflow actions" both matched,
    # and neither is about linear algebra.
    "Linear Algebra": _rx(r"\bLU ?decomposition\b|\beigenvalues?\b|"
                          r"\bsingular values?\b|\bmutable ?matrix\b|"
                          r"\brawLinAlg\w*\b|\bsolve\s*\(\s*[A-Za-z]"),
    # Not a bare "singular": it fired on "singular locus" and "singular curve"
    # in #158, which is about isNormal and has nothing to do with the computer
    # algebra system.  Require the capitalised program name, or a context word.
    "Interfaces": _rx(r"\bpolymake\b|\b4ti2\b|\btopcom\b|\bnormaliz\b|"
                      r"\bmagma\b|\bmaple\b|\bcohomCalg\b|\bphcpack\b|"
                      r"\bbertini\b|\bmsolve\b|\bnauty\b|"
                      r"\bSingular\b(?! (locus|curve|point|values?|matrix))"),
}

# ---------------------------------------------------------------- tier 2

INTERPRETER = _rx(r"\bd/\w+\.dd?\b|\bMacaulay2/d/|\bparser\b|\blexer\b|"
                  r"\bsyntax error\b|\bprecedence\b|\bkeyword\b|\bScope\b|"
                  r"\bDictionary\b|\bSymbol\b|\bstdio\b|\bnetwork\b|"
                  r"\bprintWidth\b|\btokenize")

# Not a bare "Core": check(N, "Core") appears in every test-suite report in the
# corpus, and matched 78 issues that carry no such label.
CORE = _rx(r"\bMacaulay2/m2/\b|\bm2/\w+\.m2\b|\bmethod function\b|"
           r"\btypicalValue\b|\binstallMethod\b|\bMethodFunction\b|"
           r"\bCore\.Dictionary\b|\bloadPackage \"Core\"")

HPC = _rx(r"\bhigh[- ]performance\b|\bMPI\b|\bOpenMP\b|\bGPU\b|\bcluster\b")

INFRASTRUCTURE = _rx(r"\bgithub actions?\b|\bworkflow\b|\b\.github/\b|"
                     r"\bCI\b|\bappveyor\b|\btravis\b|\bdocker\b")

# --------------------------------------------------- issue types, not labels
#
# Macaulay2 classifies Bug / Feature / Task through GitHub issue types now, not
# through the "bug" and "feature request" labels, so these three feed the
# "settype" column and the two labels are never proposed.  Same three tiers apply:
# this is a prompt for reading, and Bug in particular scored 0.14 precision as a
# label rule because every issue in the corpus contains the word "error".
FEATURE = _rx(r"\bfeature request\b|\bit would be (nice|good|useful)\b|"
              r"\bwould be nice\b|\bwish ?list\b|\bproposed feature\b|"
              r"\bplease add\b|\bshould (be able to|support|have)\b|"
              r"\bcould we\b|\bnice to have\b|\brequest for\b")

# Task: work on the project that is neither a defect nor a request for
# functionality.  Refactors, build and packaging chores, test-suite work, CI,
# documentation cleanups.  Never guessed from a body alone -- these read exactly
# like features unless you know the subject is M2's own machinery, which is why
# the existing 93 typed issues put a C++ template reorganisation under "a
# request, idea, or new functionality".
TASK = _rx(r"\breorganiz|\brefactor|\bclean ?up\b|\brewrite\b|\bmove .* to\b|"
           r"\btest ?suite\b|\bregression test\b|\bpackaging\b|"
           r"\bgithub actions?\b|\bworkflow\b|\bmaintain|\btechnical debt\b|"
           r"\bshould be (merged|split|renamed|removed|deleted)\b")

# Neither "error" nor "fails" survives here.  Both appear in almost every issue in
# the corpus, including every feature request, so a rule containing them proposes
# "bug" on everything and stops being a prompt at all -- it matched 138 issues that
# their reporters and maintainers did not call bugs.
BUG = _rx(r"\bbug\b|\bcrash(es|ed|ing)?\b|\bsegfault\b|\bwrong answer\b|"
          r"\bincorrect(ly)?\b|\bshould not (be|give|return|print)\b|"
          r"\bgives the wrong\b|\breturns the wrong\b|\bassertion failed\b|"
          r"\bis broken\b|\bregression\b")


def suggest(issue, repro):
    """[(label, tier, why)] for one cached issue record.

    "why" is the point.  A suggestion nobody can check in two seconds is a
    suggestion that gets rubber-stamped, which is worse than none at all.
    """
    title = issue["title"] or ""
    body = (issue["body"] or "")[:20000]
    text = title + "\n" + body
    out = []

    def hit(label, tier, rx, where=None, why=None):
        m = rx.search(where if where is not None else text)
        if m:
            out.append((label, tier, why or ("matched %r" % m.group(0).strip())))

    # Tier 1
    # Anchored to the title, and this is the general lesson rather than a quirk of
    # these four.  A word that names a platform, an editor or a library turns up
    # incidentally in any issue long enough to mention its environment: "emacs"
    # appeared in "Macaulay2 cheat sheet and tour", "Raspberry Pi" in "Increasing
    # memory limits", "mathicgb" in two cmake build failures.  A reporter who is
    # actually *about* one of these says so in the title.
    hit("arm", 1, ARM, where=title, why="architecture named in the title")
    hit("editors", 1, EDITORS, where=title, why="editor named in the title")
    hit("Jupyter", 1, JUPYTER, where=title, why="Jupyter named in the title")
    hit("Gröbner bases", 1, GROEBNER, where=title,
        why="Gröbner bases named in the title")

    if repro == "shell":
        out.append(("build issue", 1, "the only code in the body is shell"))
    else:
        hit("build issue", 1, BUILD)
        hit("build issue", 1, DISTRO, where=title, why="distro named in the title")
    hit("Engine", 1, ENGINE)
    hit("Documentation", 1, DOCUMENTATION)
    hit("Documentation", 1, DOC_TITLE, where=title,
        why="documentation word in the title")
    hit("Memory Leak", 1, MEMORY_LEAK)
    hit("threads", 1, THREADS)
    for name, rx in PACKAGES.items():
        hit(name, 1, rx)

    # Tier 2
    hit("Interpreter", 2, INTERPRETER)
    hit("Core", 2, CORE)
    hit("high-performance computing", 2, HPC)
    hit("Infrastructure", 2, INFRASTRUCTURE)


    # Dedupe, keeping the first (lowest-tier, most specific) reason for each.
    seen, uniq = set(), []
    for label, tier, why in out:
        if label not in seen:
            seen.add(label)
            uniq.append((label, tier, why))
    return uniq


def suggest_type(issue):
    """(type, why) for one cached issue record, or (None, None).

    Single-valued, so this picks one, and the order is the judgement.  Bug first:
    a report that M2 does the wrong thing is a Bug even when it also asks for
    something.  Task before Feature, because work on M2's own machinery describes
    what M2 does not do yet and so trips every Feature pattern -- that ordering is
    the whole difference between filing #9 as a Task and filing it, as the tracker
    currently does, as "a request, idea, or new functionality".

    Tier 2 in every case: proposed, never applied without reading.
    """
    text = (issue["title"] or "") + "\n" + (issue["body"] or "")[:20000]
    m = BUG.search(text)
    if m:
        return "Bug", "reports M2 doing the wrong thing (%r)" % m.group(0).strip()
    m = TASK.search(text)
    if m:
        return "Task", "work on M2 itself (%r)" % m.group(0).strip()
    m = FEATURE.search(text)
    if m:
        return "Feature", "asks for something M2 does not do (%r)" % m.group(0).strip()
    return None, None
