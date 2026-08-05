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
    cmd = ["gh", "api", "graphql", "-f", "query=" + query]
    for k, v in variables.items():
        if isinstance(v, (list, tuple)):
            # gh spells a list variable as repeated key[]=value.  An empty list
            # cannot be spelled at all, so callers must not send one.
            if not v:
                raise ValueError("empty list for GraphQL variable %r" % k)
            cmd += sum((["-f", "%s[]=%s" % (k, x)] for x in v), [])
        else:
            cmd += ["-F" if isinstance(v, int) else "-f", "%s=%s" % (k, v)]
    p = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
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


def check_labels(wanted, known):
    """Validate the labels chosen for each row.  wanted maps key -> [name].

    Rejects a name that does not exist, one that only belongs on a pull request,
    and a row that claims two labels which cannot both be true.  All of it runs
    before anything is created or edited: a typo should stop the run, not leave a
    batch of half-labelled issues to go back over by hand.  GitHub errors on an
    unknown name rather than creating it, but by then the run is part-applied.
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
        both = sorted(k for k, ls in wanted.items() if len(group & set(ls)) > 1)
        if both:
            raise SystemExit(
                "at most one of %s may go on an issue; these rows claim more "
                "than one:\n  %s"
                % (", ".join(repr(n) for n in sorted(group)), "\n  ".join(both)))


# The triage block bin/push-project writes names the file it came from.  That
# survives conversion to an issue and any later retitling, which makes it the most
# durable key we have -- we wrote it, so it cannot drift.
TRIAGED_FROM = re.compile(r"Triaged from `(bugs/[^`]+)`")


def key_of(item):
    """The catalog path an item refers to.

    Prefer the path recorded in the item's own triage block.  Filing an issue
    renames the item to something readable, which destroys the title-derived key,
    and the number-based fallback in match() only covers rows that are still
    open -- so a row whose verdict changed after filing would otherwise be
    orphaned from the board.

    Falling back on the title covers drafts that have not been pushed yet: those
    are titled with the path, minus the bugs/ prefix.
    """
    content = item["content"] or {}
    found = TRIAGED_FROM.search(content.get("body") or "")
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
