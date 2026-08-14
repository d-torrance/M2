"""Shared helpers for talking to GitHub project 46.

Both bin/push-project and bin/file-issues join catalog rows to board items, so
the query, the paging and the path <-> title mapping live here rather than being
kept in step by hand.

Everything here needs the "project" scope:

    gh auth refresh -s project
"""

import json
import re
import subprocess

ORG = "Macaulay2"
REPO = "M2"
PROJECT = 46

STATUS_FIELD = "Status"

# Delimiters for the triage block bin/push-project writes into a draft body.
# bin/file-issues checks for them before converting: an issue filed from a draft
# that has not been pushed yet would carry the bare bug file and none of the
# reasoning that justified filing it.
BEGIN = "<!-- triage:start -->"
END = "<!-- triage:end -->"
BLOCK_RE = re.compile(re.escape(BEGIN) + ".*?" + re.escape(END), re.S)


def gh(query, **variables):
    """Run one GraphQL request, sending it as a JSON body on stdin.

    The variables are *not* passed as `-f key=value` arguments.  Linux caps a
    single argument at MAX_ARG_STRLEN, 128 KiB, and a draft body carries the bug
    file verbatim -- bugs/mike/0-fgeiss-memleak.m2 is 253 KiB, most of it one
    hard-coded subquotient -- so pushing that row died with

        OSError: [Errno 7] Argument list too long: 'gh'

    after 668 rows had gone through without trouble.  A JSON body has no such
    limit, and it also carries types properly: ints stay ints and lists stay
    lists, where the argument form needed -F for one and repeated key[]=value
    for the other, and could not express an empty list at all.
    """
    body = json.dumps({"query": query, "variables": variables})
    p = subprocess.run(["gh", "api", "graphql", "--input", "-"],
                       input=body.encode("utf-8"),
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if p.returncode != 0:
        err = p.stderr.decode("utf-8", "replace")
        if "read:project" in err or "INSUFFICIENT_SCOPES" in err:
            raise SystemExit(
                "the gh token lacks project scope.  Run:\n"
                "    gh auth refresh -s project\n\n" + err)
        raise SystemExit("gh api graphql failed:\n" + err)
    return json.loads(p.stdout)


FIELDS_QUERY = """
query($org:String!, $number:Int!) {
  organization(login:$org) {
    projectV2(number:$number) {
      id title
      fields(first:50) {
        nodes {
          ... on ProjectV2FieldCommon { id name dataType }
          ... on ProjectV2SingleSelectField {
            id name dataType options { id name }
          }
        }
      }
    }
  }
}
"""

ITEMS_QUERY = """
query($org:String!, $number:Int!, $cursor:String) {
  organization(login:$org) {
    projectV2(number:$number) {
      items(first:100, after:$cursor) {
        pageInfo { hasNextPage endCursor }
        nodes {
          id type
          fieldValues(first:20) {
            nodes {
              ... on ProjectV2ItemFieldSingleSelectValue {
                name field { ... on ProjectV2FieldCommon { name } }
              }
            }
          }
          content {
            ... on DraftIssue { id title body }
            ... on Issue { id number title body url state }
          }
        }
      }
    }
  }
}
"""

REPO_ID_QUERY = """
query($owner:String!, $name:String!) { repository(owner:$owner, name:$name) { id } }
"""

LABELS_QUERY = """
query($owner:String!, $name:String!, $cursor:String) {
  repository(owner:$owner, name:$name) {
    labels(first:100, after:$cursor) {
      pageInfo { hasNextPage endCursor }
      nodes { id name description }
    }
  }
}
"""

# Applied to every issue filed from a bug file, so the whole cohort stays
# findable in issue search once the drafts are gone.
LABEL = "bugs directory"

# Labels that classify a *pull request*, not an issue.  Nothing here describes
# what a bug is about, and several of them are workflow state that belongs to
# whoever is reviewing -- putting "waiting for review by package author(s)" on a
# fifteen-year-old bug report would be a claim about someone else's queue.
# Validated against, rather than merely documented, because the labels column is
# hand-written and a plausible-looking name is easy to reach for.
PR_ONLY = {
    "contributions welcome",
    "dependencies",
    "javascript",
    "JSAG",
    "new package",
    "update to existing package(s)",
    "waiting for another PR",
    "waiting for review",
    "waiting for review by package author(s)",
}

# Label sets where at most one member may land on a single issue.  "bug" and
# "feature request" divide these files along the line that actually matters --
# M2 does the wrong thing, versus M2 does not do the thing yet -- and an issue
# carrying both has had that judgment dodged rather than made.  Several rows
# genuinely are neither, so this is not a requirement that one be present.
EXCLUSIVE = [{"bug", "feature request"}]

REPO_URL = "https://github.com/%s/%s" % (ORG, REPO)

# Where a reader can actually see the catalog.  It is not in Macaulay2/M2 yet, so
# this points at the branch it lives on.  Update it when the branch lands, or the
# footer of every pushed draft and posted comment points at a ref that is gone.
CATALOG_URL = ("https://github.com/d-torrance/M2/blob/bug-triage"
               "/bug-triage/catalog.tsv")


# Where a reader can see an original bug file: in Macaulay2/M2 at the commit just
# before d2c8d27826 removed the tree.  Catalog paths are exactly the paths those
# files had, so the URL is this prefix plus the path.
#
# NOT bug-triage/files/, which is where they sit locally: that directory is the
# first line of bug-triage/.gitignore, being extracts of blobs already in git
# history, so a link into it 404s.  Four filed issues carried such a link before
# Doug noticed.  Linking into the canonical repository is better anyway -- it is
# permanent, it does not depend on a fork or a branch surviving, and it shows the
# file where it actually lived.
REMOVAL_PARENT = "388c1ff0ce30d83751dea7bc7eac77fdc1305dd7"
FILES_URL = "%s/blob/%s" % (REPO_URL, REMOVAL_PARENT)


def file_url(path):
    """A permalink to a bug file as it was just before the tree was removed."""
    return "%s/%s" % (FILES_URL, path)

# The account these run under, named in the attribution so a reader knows the
# text is not that person's.  Change it if someone else picks the tooling up.
ACCOUNT = "@d-torrance"

# Attribution, and it goes ABOVE the text it applies to, never in the footer.
#
# It used to read "Drafted with AI assistance", tucked at the end of a <sub>
# footer after the content.  That is too weak twice over.  The phrasing reads as
# though the account holder wrote it with some help, when the truth is the
# reverse; and a reader who reaches it has already weighted the claims, which is
# exactly the decision it exists to inform.  It was missed on a first read of an
# issue by the person whose account posted it -- see #4556, where the disclosure
# was moved to the top and sharpened.
#
# Keep it to one sentence.  It sits on top of comments that are sometimes three
# lines long, and a disclaimer longer than its content stops being read.
ATTRIBUTION = (
    "> **Written by Claude** (Claude Opus 5, via Claude Code), not by %s, whose "
    "account posted it -- please weigh it accordingly." % ACCOUNT)

# GitHub's own cross-repository reference syntax, "owner/repo#123", so a row can
# name an issue that is not in this repository.  bugs/dan/1-emacs-macro-needed is
# the first: the ask is about M2.el, which left this tree in 78186879eb, so it was
# filed on Macaulay2/M2-emacs.  Substituted BEFORE the bare form -- the "#102"
# inside "Macaulay2/M2-emacs#102" would otherwise be linked into Macaulay2/M2,
# where issue 102 exists, is unrelated, and belongs to somebody else.  The slash
# is required rather than optional so a bare "M2-emacs#102" cannot be guessed at.
FOREIGN_REF = re.compile(r"\b([A-Za-z0-9._-]+/[A-Za-z0-9._-]+)#(\d+)\b")

# A bare "#123".  The lookbehind is what keeps it off M2 subscripting, which is
# "<expression>#<integer>" and not a reference to an issue.
#
# A word character covers the common spelling, "v#0" for the first slot of a
# Vector -- in bugs/dan/0-toString-Vector's note -- and that was the whole of the
# class until "(x+1)#0" and "((value getGlobalSymbol \"fourierMotzkin\") A)#0"
# turned up in two more notes, the second of them live on the board for weeks.
# The subscripted thing is an expression, so it ends in a closing bracket as
# readily as in a name; ) ] } are therefore excluded too.  Nothing is lost by it,
# because a genuine reference never follows a closing bracket with no space --
# "(#2130)" keeps its link, since what precedes the # there is "(".
# "[" is in the exclusion set so that a reference already written as a markdown
# link is left alone.  Without it, linkifying text that contains
# "[#4621](https://github.com/Macaulay2/M2/issues/4621)" matches the "#4621"
# inside the link *text* -- "[" was not excluded -- and yields
# "[[#4621](url)](url)", which renders as a stray "[#4621]" beside the real link.
# Nine of those reached four filed issues before Doug spotted it.  Hand-written
# prose under issues/ and comments/ routinely contains such links, so any caller
# that linkifies prose rather than a bare TSV column needs this.
ISSUE_REF = re.compile(r"(?<![\w/.\-)\]}\[])#(\d+)\b")

SHA_REF = re.compile(r"\b([0-9a-f]{7,40})\b")


def linkify(text, sha_width=None):
    """Make issue, cross-repository and commit references clickable.

    A draft issue lives in the project, not in a repository, so bare "#114" and
    bare shas render as plain text there.  /issues/N redirects to /pull/N, so one
    form covers issues and pull requests alike.  sha_width shortens the link
    text for a long sha without touching what it points at.
    """
    text = FOREIGN_REF.sub(
        lambda m: "[%s#%s](https://github.com/%s/issues/%s)"
        % (m.group(1), m.group(2), m.group(1), m.group(2)), text or "")
    text = ISSUE_REF.sub(
        lambda m: "[#%s](%s/issues/%s)" % (m.group(1), REPO_URL, m.group(1)), text)
    return SHA_REF.sub(
        lambda m: "[`%s`](%s/commit/%s)"
        % (m.group(1)[:sha_width] if sha_width else m.group(1), REPO_URL, m.group(1)),
        text)


def fetch_project():
    proj = gh(FIELDS_QUERY, org=ORG, number=PROJECT)["data"]["organization"]["projectV2"]
    fields = {f["name"].lower(): f for f in proj["fields"]["nodes"] if f}
    return proj, fields


def fetch_items():
    items, cursor = [], None
    while True:
        # gh refuses a null variable, so send the first page without a cursor.
        if cursor is None:
            data = gh(ITEMS_QUERY.replace(", $cursor:String", "")
                      .replace(", after:$cursor", ""), org=ORG, number=PROJECT)
        else:
            data = gh(ITEMS_QUERY, org=ORG, number=PROJECT, cursor=cursor)
        page = data["data"]["organization"]["projectV2"]["items"]
        items.extend(n for n in page["nodes"] if n.get("content"))
        if not page["pageInfo"]["hasNextPage"]:
            return items
        cursor = page["pageInfo"]["endCursor"]


def repo_id():
    return gh(REPO_ID_QUERY, owner=ORG, name=REPO)["data"]["repository"]["id"]


def all_labels():
    """name -> node id for every label in the repository."""
    out, cursor = {}, None
    while True:
        if cursor is None:
            data = gh(LABELS_QUERY.replace(", $cursor:String", "")
                      .replace(", after:$cursor", ""), owner=ORG, name=REPO)
        else:
            data = gh(LABELS_QUERY, owner=ORG, name=REPO, cursor=cursor)
        page = data["data"]["repository"]["labels"]
        out.update((n["name"], n["id"]) for n in page["nodes"])
        if not page["pageInfo"]["hasNextPage"]:
            return out
        cursor = page["pageInfo"]["endCursor"]


def check_labels(wanted, known, existing=None):
    """Validate the labels chosen for each row.  wanted maps key -> [name].

    Rejects a name that does not exist, one that only belongs on a pull request,
    and a row that claims two labels which cannot both be true.  All of it runs
    before anything is created or edited: a typo should stop the run, not leave a
    batch of half-labelled issues to go back over by hand.  GitHub errors on an
    unknown name rather than creating it, but by then the run is part-applied.

    "existing" maps the same keys to the labels the issue *already* carries, and
    the exclusion test has to include them.  Without it the test was right only
    because of an accident of the first project: it labelled issues it had just
    created, so there were never any prior labels to conflict with.  This one
    labels issues that have been open for years -- 87 already carry "bug" and 40
    already carry "feature request" -- and adding "bug" to an issue that already
    says "feature request" passed cleanly and left GitHub holding both, which is
    exactly the dodged judgment the rule exists to catch.  Drop the other one
    through rmlabels in the same operation.
    """
    names = {n for ls in wanted.values() for n in ls}
    unknown = sorted(n for n in names if n not in known)
    if unknown:
        raise SystemExit(
            "no such label in %s/%s: %s\n\nExisting labels:\n  %s"
            % (ORG, REPO, ", ".join(repr(n) for n in unknown),
               "\n  ".join(sorted(known))))
    wrong = sorted(n for n in names if n in PR_ONLY)
    if wrong:
        raise SystemExit(
            "these label(s) belong on a pull request, not on an issue: %s"
            % ", ".join(repr(n) for n in wrong))
    for group in EXCLUSIVE:
        both = sorted(k for k, ls in wanted.items()
                      if len(group & (set(ls) | set((existing or {}).get(k, [])))) > 1)
        if both:
            raise SystemExit(
                "at most one of %s may go on an issue; these rows would end up "
                "with more than one:\n  %s"
                % (", ".join(repr(n) for n in sorted(group)), "\n  ".join(both)))


# Labels that are a claim about people or process rather than about content, and
# so are never proposed mechanically.  "good beginners' project" asserts how hard
# something is; "stale" asserts nobody cares; "under discussion" asserts a
# conversation is happening.  No amount of body text supports any of those, and a
# suggester that guessed at them would be putting words in a maintainer's mouth.
# Warned about rather than rejected: a human may still mean it.
JUDGMENT_ONLY = {
    "community",
    "contributions welcome",
    "good beginners' project",
    "M2@GT26",
    "seeking a volunteer",
    "stale",
    "under discussion",
    "waiting for update",
}


# The triage block bin/push-project writes names the file it came from.  That
# survives conversion to an issue and any later retitling, which makes it the most
# durable key we have -- we wrote it, so it cannot drift.
#
# Case-insensitive on the first letter because the phrase moved.  The old layout
# put "Triaged from `path`" in a footer; the issue layout says "This issue was
# triaged from `path`" in its opening sentence, and matching a capital T silently
# orphaned four filed issues whose verdict was no longer open -- #4514, #4528,
# #4501 and #4529, three of which this docstring already cites as the reason the
# key exists.  Keep any future rewording matching this pattern.
# The optional "[" matters: issue bodies now write the path as a markdown link,
# "triaged from [`bugs/dan/IDEAS`](url)", so a pattern demanding a backtick
# immediately after "from " stops matching and every such issue falls back to
# the title-derived key -- which for a filed issue is a readable sentence, so the
# row goes unmatched and push-project can never update it again.  That happened to
# 108 issues between one --apply and the next --check; the unmatched count going
# from 2 to 7 is what showed it.
TRIAGED_FROM = re.compile(r"[Tt]riaged from \[?`(bugs/[^`]+)`")

# An issue filed from one ask inside a wishlist file, by bin/file-asks.  Written
# as a marker rather than inferred from the prose because the prose is not safe to
# rely on: an ask body naturally says "triaged from ... `bugs/dan/IDEAS`", which
# TRIAGED_FROM matches, and by_key would then hand the *file's* key to the ask's
# issue.  Whichever item came back from the API first would win that key, and if
# the ask won it, bin/push-project would rewrite its body with the file-level
# layout -- burying a targeted request under all 26 lines of IDEAS -- and set its
# status from the file row's verdict.  Nothing in the output would have looked
# wrong.  The ask key contains "::", which no catalog path does, so returning it
# keeps such an item permanently distinguishable from the file it came from.
ASK_MARKER = re.compile(r"<!--\s*bug-triage-ask:\s*(\S+?)\s*-->")


def key_of(item):
    """The catalog path an item refers to, or an ask key for a per-ask issue.

    Prefer the path recorded in the item's own triage block.  Filing an issue
    renames the item to something readable, which destroys the title-derived key,
    and the number-based fallback in match() only covers rows that are still
    open -- so a row whose verdict changed after filing would otherwise be
    orphaned from the board.

    Falling back on the title covers drafts that have not been pushed yet: those
    are titled with the path, minus the bugs/ prefix.
    """
    content = item["content"] or {}
    body = content.get("body") or ""
    # Checked first: an ask body also satisfies TRIAGED_FROM, and losing that race
    # is the failure described at ASK_MARKER.
    found = ASK_MARKER.search(body)
    if found:
        return found.group(1)
    found = TRIAGED_FROM.search(body)
    if found:
        return found.group(1)
    title = (content.get("title") or "").strip()
    if not title or title.startswith("bugs/"):
        return title or None
    return "bugs/" + title


def status_of(item):
    for fv in item.get("fieldValues", {}).get("nodes", []):
        if fv and (fv.get("field") or {}).get("name") == STATUS_FIELD:
            return fv.get("name")
    return None


def by_key(items):
    out = {}
    for item in items:
        k = key_of(item)
        if k:
            out.setdefault(k, item)
    return out


def match(rows, items):
    """Map catalog path -> board item, by title and then by issue number.

    Filing an issue renames the item to something readable, which is the whole
    point but destroys the path-from-title key.  For those rows the catalog holds
    the issue number, so fall back to that.

    The fallback is deliberately restricted to rows that bin/file-issues would
    have converted.  A "duplicate" row also names an issue, but that issue is one
    that already existed rather than this item, and matching it would attach the
    row to the wrong thing.
    """
    by_title = by_key(items)
    by_number = {}
    for item in items:
        number = item["content"].get("number")
        if number is not None:
            by_number["#%d" % number] = item

    out = {}
    for r in rows:
        item = by_title.get(r["path"])
        if (item is None and r.get("issue")
                and r["verdict"] == "open" and r["disposition"] == "issue"):
            item = by_number.get(r["issue"].split()[0])
        if item is not None:
            out[r["path"]] = item
    return out
