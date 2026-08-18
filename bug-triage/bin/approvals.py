"""The record of what Doug has approved for publication, and the gate that reads it.

Every other rule in this directory that has held is held by a script.  The dup
direction is asserted in bin/selftest; a close that would outrun its labels is
refused by publish-verdicts; a retitle with no explanation is refused by
bin/retitle; a batch over ten is refused by --max.  None of those has recurred.

The authorization rule -- nothing public happens without Doug saying so, first,
every time -- was the one enforced by memory, and it has failed five times.  Each
failure was answered with another paragraph in the README, which is the
intervention that has *not* worked here.  So it gets a script too.

    bin/approve comment 927        # after he has approved that exact text
    bin/publish-verdicts --apply   # refuses any row without a matching row here

What is hashed is the payload as sent: the fully built comment body including the
attribution, the close reason and duplicate target, the type name, the sorted
label additions and removals, the new title.  So an approval covers *that text*,
and revising a comment after it is approved silently invalidates the approval
rather than shipping something he never read.  That seam is real and nothing
before this covered it.

**This is forgeable and it is not a security boundary.**  I run bin/approve as
well, so nothing here makes the rule unbreakable.  What it makes it is *visible*:
publishing now requires a separate command whose entire content is the claim that
Doug approved this exact payload, and a claim like that cannot be made by
inattention.  All five failures so far were inattention -- momentum, a checkpoint
read as a licence, approval of a kind read as approval of an instance, a chained
&&, and a finished draft read as a settled decision.  None of them survives a step
that has to be typed on purpose.  That is the same thing "never chain a push onto
another command" buys, made mechanical.
"""

import datetime
import hashlib
import os
import re

import issues as issuestsv

LEDGER = os.path.join(issuestsv.ROOT, "approved.tsv")

# Appended-to, never reordered: rows written before "time" and "said" existed
# have five fields and read() pads them, so the old ledger stays readable.
COLUMNS = ["date", "action", "issue", "key", "sha256", "time", "said"]

# Every completed --apply run appends here.  bin/approve reads it to tell a
# *continuation* of the authorization it is already working under from a *new*
# one: if a publish run has happened since the last approval, whatever Doug said
# to authorize that run is spent, and the next item needs him to say so again.
APPLIED = os.path.join(issuestsv.ROOT, "applied.tsv")

# One per public effect, matching the table in README.md.  "comment" and "close"
# are separate because publish-verdicts does both in one run and they are two
# decisions: a comment can be approved and the close it was written for declined.
ACTIONS = ["comment", "close", "type", "labels", "title"]


# The word Doug has to actually say.  --said used to accept any text, and the
# sixth failure went out under the quote "Please use it for this batch for now",
# which authorizes nothing and which I typed into the authorization field myself.
# A specific, rarely-typed word cannot be produced by paraphrase: either he wrote
# it or the quote is fabricated, and fabricating is a different act from inferring.
AUTHORIZING = re.compile(r"\bauthoriz|\bauthoris|\bapprov", re.I)

# ...but he also says it when asking whether he ever did.  "When did I authorize
# --apply?" is the sentence that immediately preceded this check being written.
# Anything that turns a grant into a question, a denial or a refusal, when it
# appears *before* the authorizing word and close enough to govern it.  Written
# out longhand rather than cleverly: a missed negator here publishes something.
NEGATORS = (r"did|do|does|didn'?t|don'?t|doesn'?t|never|when|whether|if|unless|"
            r"was|were|had|would|should|not|cannot|can'?t|won'?t|shan'?t|"
            r"ain'?t|couldn'?t|wouldn'?t|shouldn'?t|refus\w*|declin\w*|"
            r"reject\w*|withhold\w*|no")
NOT_AUTHORIZING = re.compile(
    r"\?|\b(%s)\b[^.]{0,40}\b(authoriz|authoris|approv)" % NEGATORS, re.I)


def authorizing(said):
    """Does this quote actually authorize, rather than ask or deny?"""
    if not AUTHORIZING.search(said or ""):
        return False, ("neither \"authorize\" nor \"approve\" appears in it")
    if NOT_AUTHORIZING.search(said or ""):
        return False, ("it reads as a question or a denial, not a grant")
    return True, ""


def digest(payload):
    """sha256 of the exact bytes that would be sent."""
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


def close_payload(reason, dup):
    return "close reason=%s dup=%s" % (reason, dup if dup else "")


def labels_payload(add, rm):
    return "labels +%s -%s" % (",".join(sorted(add)), ",".join(sorted(rm)))


def read(path=LEDGER):
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


def record(entries, said="", path=LEDGER):
    """Append (action, issue, key, payload) tuples.  Appended, never rewritten.

    The ledger is history, not state.  A superseded approval stays in it with its
    old hash, because "this text was approved on that date and then changed" is
    exactly what somebody auditing a published comment needs to be able to see.
    """
    now = datetime.datetime.now()
    today, clock = now.date().isoformat(), now.strftime("%H:%M:%S")
    new = not os.path.exists(path)
    with open(path, "a", encoding="utf-8") as f:
        if new:
            f.write("\t".join(COLUMNS) + "\n")
        for action, number, key, payload in entries:
            if action not in ACTIONS:
                raise ValueError("unknown action %r" % action)
            f.write("\t".join([today, action, str(number), key or "",
                               digest(payload), clock,
                               (said or "").replace("\t", " ")]) + "\n")


def approved(action, number, key, payload, ledger=None):
    """Is there a row approving exactly this payload?"""
    if ledger is None:
        ledger = read()
    want = digest(payload)
    return any(r["action"] == action and r["issue"] == str(number)
               and (r["key"] or "") == (key or "") and r["sha256"] == want
               for r in ledger)


def missing(wanted, ledger=None):
    """Which of [(action, number, key, payload)] have no approval on file."""
    if ledger is None:
        ledger = read()
    return [w for w in wanted if not approved(w[0], w[1], w[2], w[3], ledger)]


def refuse(unapproved):
    """The message for a batch that is missing approvals.  Never partial.

    Checked over the whole plan before anything is sent, for the reason
    bin/apply-labels validates its whole plan first: half a batch published and
    the other half refused is worse than either, and it is the half that went out
    that cannot be taken back.
    """
    lines = ["refusing to publish: %d item(s) have no approval on file.\n"
             % len(unapproved)]
    for action, number, key, _ in unapproved:
        lines.append("    %-8s #%s%s" % (action, number,
                                         "  (%s)" % key if key else ""))
    lines.append(
        "\nThese land in somebody else's inbox under Doug's account, and every one "
        "of them\nneeds his go-ahead for that item, at that moment -- not a general "
        "go-ahead for the\nproject, not a workflow agreed earlier, not approval of "
        "the row next to it.  Show him\nthe dry run, ask, and record the answer:\n")
    for action in sorted({a for a, _, _, _ in unapproved}):
        nums = sorted({n for a, n, _, _ in unapproved if a == action})
        lines.append("    bin/approve %s %s"
                     % (action, " ".join(str(n) for n in nums)))
    lines.append(
        "\nIf the payload changed after he approved it, that is why this is "
        "failing, and it is\nworking: an approval covers the text he read, not the "
        "file name it was in.")
    return "\n".join(lines)


def stamp(row):
    """'YYYY-MM-DD HH:MM:SS' for ordering.  Old rows have no time; treat them as
    the start of their day, which is right -- they all predate this mechanism."""
    return "%s %s" % (row.get("date", ""), row.get("time") or "00:00:00")


def record_applied(action, numbers, path=APPLIED):
    """Note that a publish run completed.  Called by every --apply."""
    now = datetime.datetime.now()
    new = not os.path.exists(path)
    with open(path, "a", encoding="utf-8") as f:
        if new:
            f.write("date\ttime\taction\tissues\n")
        f.write("\t".join([now.date().isoformat(), now.strftime("%H:%M:%S"),
                           action, ",".join(str(n) for n in numbers)]) + "\n")


def last_applied(path=APPLIED):
    """(stamp, action, issues) of the most recent --apply, or None."""
    if not os.path.exists(path):
        return None
    rows = read(path)
    if not rows:
        return None
    last = max(rows, key=stamp)
    return (stamp(last), last.get("action", ""), last.get("issues", ""))


def since_last_apply(ledger=None):
    """True if no publish run has happened since the newest approval on file.

    False means the authorization in force was consumed by an --apply, so the
    next approval is a *new* authorization event and needs Doug to have said so
    again.  This is the seam that produced the sixth failure: an "apply/push"
    that named a batch was reused, minutes later, for a row settled afterwards.
    """
    if ledger is None:
        ledger = read()
    applied = last_applied()
    if applied is None:
        return True
    if not ledger:
        return False
    return max(stamp(r) for r in ledger) > applied[0]
