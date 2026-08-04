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
            ... on Issue { number title body url }
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


def key_of(item):
    """The catalog path an item refers to.

    Item titles carry the path with the bugs/ prefix stripped --
    "mike/git-issue359.m2" for "bugs/mike/git-issue359.m2" -- so put it back.
    Once an item has been converted to an issue and given a readable title, it no
    longer matches anything, which is why the issue number goes into the catalog.
    """
    title = (item["content"].get("title") or "").strip()
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
