-- Run gfanInterface's tests against one particular gfan binary.
--
-- Called by .github/gfan-matrix/run.sh with GFAN_BIN_DIR pointing at the
-- directory holding the gfan that the patched build just produced.  Nothing is
-- compiled here: the M2 running this script is a released binary, installed
-- from the same repository a user would install from, and the only part that
-- has been swapped out underneath it is gfan.
--
-- programPaths is the first place findProgram looks -- ahead of M2's own
-- programs directory and ahead of PATH -- so setting it is what makes
-- gfanInterface use this row's binary rather than the gfan the distribution
-- shipped.  run.sh deals with the other two, for the tests that check runs in
-- a subprocess instead of capturing in this one.

gfanDir = getenv "GFAN_BIN_DIR"
if gfanDir === "" then error "GFAN_BIN_DIR is not set"
programPaths#"gfan" = gfanDir

-- the same lookup gfanInterface makes, so that a binary too old to satisfy it
-- is reported here rather than from inside the first test
gfanProgram = findProgram("gfan", "gfan --help",
    MinimumVersion => ("0.8", "gfan _version | head -2 | tail -1 | sed 's/gfan//'"))
stderr << "-- M2 " << version#"VERSION" << " on " << version#"architecture" << endl
stderr << "-- gfan " << gfanProgram#"version" << endl
-- run.sh reads this line back to confirm it is this row's binary being tested
stderr << "-- GFANPATH " << gfanProgram#"path" << endl

pkg = try needsPackage("gfanInterface", LoadDocumentation => true) else (
    stderr << "-- RESULT load-failed" << endl;
    exit 1)

n = # tests pkg
failed = for k to n - 1 list (
    if (try (check(k, pkg, Verbose => true); true) else false)
    then continue else k)
stderr << "-- RESULT " << toString(n - #failed) << "/" << toString n << endl
if #failed > 0 then (
    stderr << "-- FAILED " << toString failed << endl;
    exit 1)
exit 0
