# Review of the GitHub Actions workflow

*Written by Claude (Opus 5) at Doug's request, 2026-08-22; §8 and §9 added
2026-08-23.  Not committed.*

All timings below are measured from real runs of `test_build.yml`, not estimated.
The reference run is [32561543375](https://github.com/Macaulay2/M2/actions/runs/32561543375)
("Update AdjunctionForSurfaces.m2", 2026-08-22) — a PR that changed exactly one
package file, i.e. the case this review is about.

---

## 1. The headline: rebuilding the binary is not the problem

Per-job wall time for that one-package PR:

| job | wall |
|---|---|
| `autotools-ubuntu-24.04-default` | 2:30:20 |
| `cmake-macos-15-brew-clang` | 2:30:04 |
| `cmake-ubuntu-24.04-default` | 1:26:51 |
| `autotools-macos-15-default` | 1:21:01 |
| **total runner time** | **7:48** |

And here is where the 2:30:20 in the autotools/Linux job actually went:

| phase | time | share |
|---|---|---|
| apt install | 1:29 | 1% |
| `configure` | 0:54 | 0.6% |
| `make -C libraries` | 1:54 | 1.3% |
| `make PACKAGES=` (**the M2 binary**) | **3:42** | **2.5%** |
| `make` (install 299 packages) | 1:11:46 | 48% |
| `make check` + `html-check-links` | 1:10:15 | 47% |
| `make validate-html` | <0:30 | 0.3% |

**Building the M2 binary is 3m42s — 2.5% of the job.**  Libraries add another
1m54s because almost everything comes from the PPA.  So the premise of the
request ("we rebuild Macaulay2 on 4 different runners") is, cost-wise, a red
herring: what costs 7 hours 48 minutes is *installing and checking all 299
distributed packages, four times over*.

This reframes everything below.  Skipping the binary build saves ~6 minutes.
Skipping 297 of 299 package installs and checks saves ~2 hours 20 minutes *per
job*.  And you cannot skip the binary anyway if you want to install or check
even one package — you need an `M2` to run.

The right lever is therefore **`PACKAGES=`, not the compiler.**  Reducing the
4-job matrix to 1 job for package-only PRs is still worth doing (it is a 4x
saving on top), but it is the second-order effect.

---

## 2. Three things that are silently costing you time right now

These are independent of any path-filtering work and are worth fixing first,
because each is small and each is currently a pure loss.

### 2.1 The `actions/cache` step caches nothing

```yaml
      - uses: actions/cache@v5
        if: matrix.build-system == 'cmake'
        with:
          path: |
            ~/.ccache
            ~/work/M2/M2/M2/BUILD/build/usr-host
          key: build-cache-${{ runner.os }}-...-${{ hashFiles('**/cmake/*-libraries.cmake', '.github/workflows/test_build.yml') }}
```

Querying the repo's cache list (`gh api repos/Macaulay2/M2/actions/caches`)
gives, for every single one of these entries:

```
0MB   refs/pull/4658/merge   build-cache-macOS-brew-clang-cmake-6b9c7883...
3MB   refs/pull/4658/merge   build-cache-Linux-default-cmake-6b9c7883...
0MB   refs/heads/stable      build-cache-macOS-brew-clang-cmake-1537724e...
3MB   refs/heads/stable      build-cache-Linux-default-cmake-1537724e...
```

0 MB and 3 MB. Total repo cache usage is 247 MB of the 10 GB allowance, and
98 MB + 94 MB of that is somebody's Julia depot from PR #4655.  Three separate
bugs are stacked here:

1. **Wrong ccache directory.**  ccache ≥ 4.0 uses `$XDG_CACHE_HOME/ccache`,
   i.e. `~/.cache/ccache`, and only falls back to `~/.ccache` if that directory
   already exists.  ubuntu-24.04 ships ccache 4.9.1; Homebrew ships 4.x.  On a
   fresh runner `~/.ccache` does not exist, so ccache writes to
   `~/.cache/ccache` and the cache step archives an empty directory.  The
   3 MB on Linux is `usr-host` alone; the 0 MB on macOS means even that is
   missing there.
2. **The key never rotates.**  It hashes `cmake/*-libraries.cmake` and the
   workflow file — neither of which changes when C++ sources change.
   `actions/cache` will not overwrite an existing key, so the first run to
   claim a key freezes that content forever.  You want a rolling key plus
   `restore-keys`:
   ```yaml
   key:  ccache-${{ runner.os }}-${{ matrix.build-system }}-${{ github.sha }}
   restore-keys: |
     ccache-${{ runner.os }}-${{ matrix.build-system }}-
   ```
3. **Nothing ever seeds a shared scope.**  A cache written during a
   `pull_request` run is scoped to `refs/pull/N/merge` and is invisible to every
   other PR.  A run can read caches from its own ref, its **base** branch, and
   the **default** branch.  The default branch here is `stable`; PRs target
   `development`; and `test_build.yml` never runs on a push to `development`.
   So no PR can ever restore anything useful.  Fix: add
   ```yaml
   on:
     push:
       branches: [ development ]
   ```
   so merges to `development` populate a cache that every PR can restore.
   (Fork PRs get a read-only token and cannot *write* caches — that's fine,
   restore still works.)

Also: the cache step is `if: matrix.build-system == 'cmake'`, yet
`/usr/lib/ccache` is prepended to `PATH` for *all* Linux jobs.  The autotools
jobs pay ccache's bookkeeping overhead with a guaranteed 0% hit rate.  Either
cache both or drop ccache from the autotools PATH.

Adding `ccache -s` as a step after the build would make the hit rate visible
so this cannot rot again unnoticed.

### 2.2 `make check` runs the 299 package checks strictly serially

`M2/Macaulay2/packages/Makefile.in:49-56`:

```make
$(foreach i,\
	$(sort $(ALL_PACKAGES) $(DEVEL)),\
	$(eval check::check-$i)\
	$(eval check-$i:; \
		@pre_bindir@/M2 -q --no-preload $(STOP)                 \
			-e $(call m2-need-template,$i)                  \
			-e "debug Core; argumentMode = $(ArgumentMode)" \
			-e $(call m2-check-template,$i)))
```

That generates **299 separate double-colon rules for the target `check`**, each
with an empty recipe and a single prerequisite `check-$i`.  (`DEVEL =
EngineTests` adds nothing: `EngineTests` is already listed in
`=distributed-packages`, so `$(sort)` dedupes it — and being in `ALL_PACKAGES`
means it is also *installed*, contrary to the "checked but not installed"
comment above `DEVEL`.  `DEVEL` has since been removed in `01ec37f532`.)
GNU make executes the
double-colon rules for a given target one at a time, in the order they appear,
*including* building each rule's prerequisites before moving to the next.  So
`-j4` buys nothing.

The job log confirms it directly.  Extracting the timestamp of every
`M2 ... needsPackage("...",LoadDocumentation=>true...)` invocation:

```
299 invocations, strictly sequential, spanning 1:10:11, median 5.7 s each
```

Three of the runner's four cores sit idle for seventy minutes.

#### What exactly is to blame

Double-colon rules are not the problem; having *many of them for one target*
is.  Three shapes, `4 x sleep 2` under `-j4`, GNU make 4.3:

| shape | time |
|---|---|
| (a) N double-colon rules, one prerequisite each — **M2's current shape** | 8.02 s |
| (b) single-colon target, prerequisites accumulated over N lines | 2.01 s |
| (c) *one* double-colon rule listing all N prerequisites | 2.01 s |

```make
# (a) 8.02 s
$(foreach i,$(PKGS),$(eval a::a-$i)$(eval a-$i:; @sleep 2; echo done-$i))
# (b) 2.01 s
$(foreach i,$(PKGS),$(eval b: b-$i)$(eval b-$i:; @sleep 2; echo done-$i))
# (c) 2.01 s
c:: $(foreach i,$(PKGS),c-$i)
```

Note that (b) works: GNU make lets you accumulate prerequisites for a
single-colon target across many lines as long as at most one of them carries a
recipe, and here none of them do.  So the fix is a **one-character diff**:

```diff
-	$(eval check::check-$i)\
+	$(eval check: check-$i)\
```

Combining it with the `PACKAGES=` fix from §3.3 gives:

```make
$(foreach i,\
	$(sort $(PACKAGES) $(DEVEL)),\
	$(eval check: check-$i)\
	...
```

#### Why this is safe

- Mixing `:` and `::` for the same target in one makefile is an error
  (*"target file 'check' has both : and :: entries"*), so it's worth confirming
  nothing else declares `check` with `::` in this file.  Nothing does — the
  other double-colon targets here are `check-info::`, `clean::` and
  `distclean:clean`, and the `clean check::` in `Macaulay2/e/Makefile.in` is a
  different directory's makefile.
- Subdirectory-level parallelism already works and is unaffected:
  `Macaulay2/Makefile.in` generates `check: check-in-$d` with a *single* colon,
  and the log shows `make -C e/c/d/system/bin/m2/man/packages/editors/
  html-check-links check` all starting within the same 30 ms.  It is only the
  299 inside `packages` that serialize.
- The `check-$i` rules have no prerequisites and share no state; each is an
  independent `M2` process, and the error files they leave in `/tmp/M2-*/` are
  already per-process.

#### Two things to watch

**Interleaved output.**  With `-j`, the recipes' stdout interleaves, and the M2
check failure log works by echoing the failing input line — which becomes
unreadable when four packages are talking at once.  `--output-sync=target`
(GNU make 4.0+) is not optional here; it buffers each target's output and emits
it as a block.

Use `target`, not `recurse`.  Verified against M2's recursive shape (top-level
`make check` recursing into `Macaulay2/packages`): with `-Otarget` each
package's block is still released the moment that package finishes, so the log
streams normally; with `-Orecurse` the whole sub-make's output is held until it
exits, which here would mean no output for 25-35 minutes followed by one huge
blob.  Two side effects to expect either way: blocks appear in *completion*
order rather than alphabetical, and the timestamps in the log become target
*completion* times (which is what the per-package timings in this document were
extracted from, so that parsing still works — just shifted).

**Memory, and how much this actually buys.**  A standard `ubuntu-24.04` runner
is 4 vCPU / 16 GB.  Four concurrent `M2` processes on the heavy packages
(`SpecialFanoFourfolds` 6.6 min, `Polyhedra` 4.3 min, `EngineTests` 2.7 min)
could be a lot of resident memory at once, so `-j2` is the conservative
starting point.

Measured on a real build tree, checking four small packages:

| | wall | user |
|---|---|---|
| `-j1` | 1:33 | 1:59 |
| `-j4 --output-sync=target` | 0:46 | 2:04 |

2.0x — and note `user` > `wall` even at `-j1`: each `M2 check` already uses
about 1.3 cores on its own.  So `-j4` on a 4-core runner oversubscribes, and
the honest expectation for the 70-minute CI check phase is **~25-35 minutes,
not ~20**.  Still 35-45 minutes off every full build, for a one-character
change.

### 2.2a The check phase does not depend on the install phase

This one is worth its own note, because it changes how the jobs should be laid
out and it holds *even without any filtering*.

`check-$i` has no prerequisites, and `needsPackage(..., LoadDocumentation=>true)`
resolves the package from `path`, which in a build tree includes the **source**
packages directory:

```
$ cd M2/BUILD/build/Macaulay2/packages
$ ../../usr-dist/.../bin/M2 -q --no-preload -e 'print toString path; exit 0'
{./, ../../../../Macaulay2/packages/, ../../usr-dist/common/share/Macaulay2/}
```

`../../../../Macaulay2/packages/` from that directory is the source tree.  So a
package's tests are read from source and never touch the staging area.
Demonstrated on a local build tree in which only `Core`, `Style`,
`FirstPackage` and `Macaulay2Doc` are staged — `NeuralIdeals` is *not*
installed:

```
$ time M2 -q --no-preload --stop --silent \
    -e 'needsPackage("NeuralIdeals",LoadDocumentation=>true,DebuggingMode=>true)' \
    -e 'debug Core; argumentMode = defaultMode' \
    -e 'check(NeuralIdeals,UserMode=>false,Verbose=>false); exit 0'
 -- capturing check(1, "NeuralIdeals") -- .18512s elapsed
 ...
real	0m7.037s
```

It passes.  So the 71:46 install phase and the 70:15 check phase are
**independent given the binary**, yet today they run back to back in the same
job.  Split them into two jobs that both start from the same `make PACKAGES=`
binary and the full-build wall time drops from 2:30 to roughly 1:20 with no
filtering, no caching, and no Makefile change at all.  Layer §2.2's `-j` fix on
top and the check job is ~20 minutes, making the install phase the critical
path.

(Caveat on the demonstration: `Macaulay2Doc` *was* staged in my tree, so it
proves only that the *package under test* need not be installed.  Since the
source tree is on `path` anyway, a staged `Macaulay2Doc` shouldn't be required
either, but I did not test that.)

### 2.3 The weekly regression build tests the wrong branch

```yaml
  schedule:
    - cron: '0 6 * * SUN' # runs tests on the main branch every Sunday at 06:00 UTC
```

Scheduled workflows always run on the repository's **default branch**.  For
`Macaulay2/M2` that is `stable`, whose tip is `1a37f6fe95` (2026-06-14), while
`development` is at `d49d057374` (2026-08-21).  So the Sunday run has been
re-testing the last release for two months and has never seen `development`.

Fix by adding an explicit ref, e.g. a `workflow_dispatch`-plus-`schedule` job
that does `actions/checkout` with `ref: development`, or just move the full
sweep to `push: [ development ]` (which you want anyway for §2.1.3 and as the
safety net for §3).

Related: `.github/workflows/package-review.yml` pins
`Macaulay2/M2/.github/actions/package-review@master`.  `master` is at
`00b89ec877` (2025-05-15), fifteen months stale.  The action's tree happens to
be identical on `master` and `development` today, so it works — but it is a
trap.  Pin to `development` or to a tag.

---

## 3. Change detection: the proposed design

### 3.1 A single fast filter job

Add one ~20-second job that all the others depend on:

```yaml
jobs:
  changes:
    runs-on: ubuntu-latest
    outputs:
      binary:    ${{ steps.f.outputs.binary }}    # need to rebuild + full matrix
      full:      ${{ steps.f.outputs.full }}      # need every package + every test
      packages:  ${{ steps.f.outputs.packages }}  # space-separated PACKAGES= value
    steps:
      - uses: actions/checkout@v6
        with: { fetch-depth: 0 }
      - id: f
        run: M2/BUILD/ci/classify-changes >> "$GITHUB_OUTPUT"
```

I'd put the logic in a script in the repo rather than use `dorny/paths-filter`,
because the package selection needs the dependency graph anyway (§4) and
because a script is testable outside CI.  Downstream jobs then gate on
`needs.changes.outputs.*` and pass `PACKAGES=...` through to make.

### 3.2 The classification table

| touched | rebuild binary | matrix | packages installed | checks run |
|---|---|---|---|---|
| `Macaulay2/{c,d,e,system,bin}/**` | yes | all 4 + §8 sweep | all | all + engine unit tests |
| `configure.ac`, `M2/m4/**`, `M2/cmake/**`, `**/CMakeLists.txt`, `**/Makefile.in`, `.gitmodules` | yes | all 4 + §8 sweep | all | all |
| `M2/libraries/<lib>/**`, the `build-<lib>` block of `M2/cmake/build-libraries.cmake`, `M2/submodules/<lib>` | yes, **with `<lib>` forced from source** — see §9 | all 4 + §8 sweep | all | all, plus §9.5's packages for a program |
| `Macaulay2/m2/**` (Core) | yes | all 4 | all | all |
| `Macaulay2/packages/Macaulay2Doc/**`, `Macaulay2Doc.m2`, `Style`, `FirstPackage`, `SimpleDoc`, or any **preloaded** package | yes | all 4 | all | all |
| `Macaulay2/tests/**` | yes | all 4 | all | all |
| `.github/**` | yes | all 4 | all | all *(you are testing the CI itself)* |
| `Macaulay2/packages/Foo*` only | yes, but 1 job | 1 (autotools/Linux) | `closure(Foo)` | `closure(Foo)` |
| `*.md`, `README*`, `Macaulay2/man/**`, `Macaulay2/editors/**` only | no | — | none | lint only |

Three notes on the table.

**"All 4" is the wrong ceiling for a binary change.**  The four jobs are two
newest-release platforms; the binary is shipped on considerably older ones, and
nothing on a PR compiles those.  Every row above that says "rebuild binary:
yes" should also fan out to the cheap binary-only sweep described in §8 — which
is affordable precisely *because* it is binary-only, and which is gated by the
same `binary` output of the filter job.

**Core is a superset of `m2/`.**  `Macaulay2/m2/Makefile.in` shows that
`Core/tvalues.m2` is generated from `../d/*.d`, and that `DUMPEDM2DOCFILES`
includes all of `packages/Macaulay2Doc/**`.  Macaulay2Doc is not an ordinary
package; treat it as Core.

**The preloaded packages are effectively Core too.**  `Core.m2` preloads
`Classic ConwayPolynomials Elimination IntegralClosure InverseSystems
Isomorphism LLLBases MinimalPrimes OnlineLookup PackageCitations
PrimaryDecomposition ReesAlgebra Saturation SimpleDoc TangentCone Varieties`
plus `HomologicalAlgebraPackage`.  A change to any of those is visible to every
other package regardless of what the dependency graph says, so it should
trigger the full sweep.  This is also why PR #4425 (a one-line change to
`Isomorphism`) computes a 77-package closure below — the graph is telling you
the truth, and the honest answer for a preloaded package is "rebuild
everything".

**Keep an escape hatch.**  Path filtering *will* be wrong occasionally.  Two
mitigations, both cheap:

- a `full-ci` label that forces the whole matrix
  (`if: contains(github.event.pull_request.labels.*.name, 'full-ci')`);
- the unconditional full build on `push: [ development ]` from §2.1.3, so a
  filtering mistake surfaces within the hour after merge rather than at
  release time.

With those two in place, aggressive filtering on PRs is a safe trade.

### 3.3 `PACKAGES=` already works for installs but not for checks

Good news: `M2/Macaulay2/packages/Makefile.in` already has

```make
ALL_PACKAGES = $(shell cat @srcdir@/=distributed-packages)
PACKAGES = $(ALL_PACKAGES)          # this one can be overridden by the user
...
$(foreach i, $(PACKAGES), $(eval all: all-$i) $(eval install: install-$i))
```

so `make PACKAGES="Foo Bar"` restricts the install phase today, and make
propagates command-line variables to sub-makes automatically.  `validate-html`
already honours it too — which is precedent for exactly this design.

Bad news: the `check` generator loops over `$(sort $(ALL_PACKAGES) $(DEVEL))`,
so `make check PACKAGES="Foo"` still checks all 299.  Either switch it to
`$(PACKAGES)` (defaulting to `ALL_PACKAGES`, so the default behaviour is
unchanged) or have CI call `make -C Macaulay2/packages check-Foo check-Bar`
explicitly.  The former is cleaner and combines with the single-colon fix from
§2.2 into one small patch.

### 3.4 The Macaulay2Doc floor

Every package's `.installed` target has order-only prerequisites on `Style`,
`FirstPackage`, and `Macaulay2Doc`, and make builds order-only prerequisites if
they are out of date.  From the log:

| bootstrap package | install time |
|---|---|
| `Style` | 0.5 min |
| `FirstPackage` | 0.3 min |
| `Macaulay2Doc` | **17.4 min** |

So a from-scratch one-package PR still costs roughly

```
1.5 (apt) + 1.0 (configure) + 1.9 (libraries) + 3.7 (M2)
    + 0.5 + 0.3 + 17.4 (bootstrap) + ~1 (the package) + ~0.2 (its check)
  ≈ 28 min, on one runner
```

versus 7 h 48 m of runner time today.  That is already a ~17x reduction with no
caching at all.  But 24 of those 28 minutes are the binary plus Macaulay2Doc —
which is precisely what caching can eliminate.  See §6.

For context on the install phase overall: the 299 installs total 230 CPU-min,
completing in 71:46 wall on 4 cores.  Given the 17.4-minute serial Macaulay2Doc
barrier at the front, the theoretical floor is `17.4 + 213/4 ≈ 70` min — so the
install phase is already near-optimally parallel.  Unlike the check phase, there
is nothing to win there except by not doing the work.

---

## 4. Reverse dependencies: yes, this is easy

You asked whether reverse dependencies are easy to determine.  They are, and
you are most of the way there already.

The declarations exist in the sources: of 299 distributed packages, 160 use
`PackageExports`, 144 use `PackageImports`, and 135 use `needsPackage`.  A
static parse of the 299 top-level `.m2` files builds the forward graph; invert
it and take the transitive closure.  I prototyped this in ~40 lines of Python.
No `M2` binary is needed, so it runs inside the fast filter job.

Measured closure sizes over all 299 packages:

| statistic | value |
|---|---|
| nothing depends on them (closure = 0) | 157 packages (53%) |
| closure ≤ 5 | 232 packages (78%) |
| closure > 30 | 20 packages (7%) |
| median closure | 0 |

The heavy hitters are the ones you'd expect: `LLLBases` 199, `Polyhedra` 184,
`Truncations` 175, `Complexes` 172, `Elimination` 159, `Saturation` 137,
`MinimalPrimes` 110.  Note that most of those are preloaded and so land in the
"treat as Core" bucket of §3.2 anyway.

Applied to the 18 package-only PRs among the last 60 merged PRs:

| PR | touched | would build | | PR | touched | would build |
|---|---|---|---|---|---|---|
| 4434 | 1 | 1 | | 4402 | 1 | 1 |
| 4447 | 1 | 1 | | 4401 | 1 | 2 |
| 4428 | 1 | 1 | | 4382 | 1 | 2 |
| 4379 | 1 | 1 | | 4327 | 1 | 1 |
| 4353 | 1 | 1 | | 4377 | 1 | 2 |
| 4380 | 1 | 3 | | 4410 | 1 | 7 |
| 4411 | 1 | 8 | | 4147 | 1 | 9 |
| 4427 | 6 | 6 | | 4420 | 1 | 51 |
| 4425 | 1 | 77 | | 4322 | 87 | 248 |

**Median: 2 packages of 299.**  Eleven of the eighteen build 5 or fewer.

Two implementation notes:

- Grepping `needsPackage` catches occurrences inside documentation examples as
  well as real load-time dependencies.  That **over**-approximates the
  dependency set, which is the safe direction — better to rebuild a package
  that didn't need it than to miss one.
- A more authoritative source would be to ask M2 itself
  (`Package#"required packages"` after loading), but that needs a built M2 and
  therefore can't run in the pre-flight filter job.  The static parse is the
  right tool here; if you want belt-and-braces, have the *full* build on
  `push: development` cross-check the static graph against M2's own view and
  fail loudly on a discrepancy.

Worth noting that the same graph has a second use: the generated `.installed`
rules currently declare no inter-package prerequisites at all beyond the three
bootstrap packages.  Feeding real dependencies in would let `make -jN` order
package installs correctly rather than relying on every package's dependencies
being loadable from the source tree.

### 4.1 Bugs in the existing filter

The `validate-html` step already does per-package filtering, and its logic is
where a shared script should come from — but it has some rough edges worth
carrying over as fixes rather than as-is:

```sh
PACKAGES=$(git diff --stat origin/development HEAD -- ../../Macaulay2/packages/ \
  | grep -Po "(?<=Macaulay2/packages/)[^/\.]*(?=\.m2|/)" | uniq \
  | sed 's/undistributed-packages//g' | xargs)
```

- `uniq` without a preceding `sort` only collapses *adjacent* duplicates.
- `sed 's/undistributed-packages//g'` leaves an empty field rather than
  dropping the entry; better to filter against `=distributed-packages`, which
  also guards against typos and against packages that exist on disk but aren't
  distributed.
- No reverse-dependency expansion.
- `git diff origin/development HEAD` is a two-dot diff, not a merge-base diff.
  For `pull_request` events `HEAD` is the merge commit so this is usually
  right, but if `development` has advanced since the merge ref was computed,
  other people's changes get attributed to the PR.  Over-approximation again,
  so harmless — but `git diff $(git merge-base ...)` is what you mean.
- `git diff --stat` + `test "$(...)"` works but `git diff --quiet --exit-code`
  is the idiom.

---

## 5. Making the HTML documentation viewable

GitHub Pages is **not** currently enabled on `Macaulay2/M2`
(`gh api repos/Macaulay2/M2/pages` → 404), and macaulay2.com is served
separately, so `macaulay2.github.io/M2/` is unclaimed.  Three options, in
increasing order of effort and of usefulness:

1. **Upload the built HTML as an artifact.**  It's already on disk at
   `M2/BUILD/build/usr-dist/common/share/doc/Macaulay2/<Pkg>/html/`.  This is a
   five-line change and works for fork PRs.  Downside: reviewers download and
   unzip a file; GitHub renders nothing.  Do this today regardless.

2. **A `gh-pages` preview per PR.**  Publish to `gh-pages` under `pr/<N>/` and
   post a link.  Because `pull_request` runs from forks get a read-only token,
   this has to be a separate `workflow_run`-triggered workflow that fires after
   the build completes and runs in the base-repo context with a write token.
   Add a small cleanup job on `pull_request: closed` to delete `pr/<N>/`.  This
   is the option I'd aim for.

3. **Cloudflare Pages / Netlify preview deploys.**  Best UX — unique immutable
   URL per deploy, automatic PR comment, no branch to garbage-collect — at the
   cost of a third-party account and a secret, and the same `workflow_run`
   dance for fork PRs.

Whichever you pick, note that with partial builds the docs will contain links
to packages that weren't installed.  `html-check-links` walks *everything* under
`usr-dist/common/share/doc/Macaulay2` and will report those as dangling.  So
`html-check-links` needs either its own `PACKAGES=`-style restriction or to be
confined to the full-build path.  `validate-html` is already fine, since it
validates per-package trees in isolation.

---

## 6. Reusing a cached binary — the honest answer

You asked whether the "didn't rebuild the binary" jobs can pull a binary from a
recent "did rebuild" job.  Yes, and after fixing §2.1 it is straightforward.
But the value is not where it looks:

- reusing the **binary** saves ~6 minutes;
- reusing **Macaulay2Doc + Style + FirstPackage** saves ~18 minutes.

So cache the built tree, not just the compiler output, and the prize is
Macaulay2Doc.

**Key it on content, and require an exact match.**  The thing that determines
the binary and Macaulay2Doc is a specific set of sources:

```yaml
key: tree-${{ runner.os }}-${{ matrix.build-system }}-${{ hashFiles(
       'M2/configure.ac', 'M2/cmake/**', 'M2/m4/**',
       'M2/Macaulay2/c/**', 'M2/Macaulay2/d/**', 'M2/Macaulay2/e/**',
       'M2/Macaulay2/system/**', 'M2/Macaulay2/bin/**', 'M2/Macaulay2/m2/**',
       'M2/Macaulay2/packages/Macaulay2Doc/**',
       'M2/Macaulay2/packages/Macaulay2Doc.m2',
       'M2/Macaulay2/packages/Style/**', 'M2/Macaulay2/packages/FirstPackage.m2',
       '**/Makefile.in', '.gitmodules') }}
```

Deliberately **no `restore-keys`** for this cache: a near-miss is worse than a
miss, because you'd be testing a package against a stale interpreter.  Either
the inputs hash identically — in which case the tree is exactly right — or you
build.  (Keep loose `restore-keys` for the *ccache* cache, where a partial hit
is strictly good.)

**The mtime trap.**  `git checkout` stamps every file with the checkout time, so
after restoring a cache, every `Foo.m2` looks newer than
`usr-dist/.../Foo/.installed` and make rebuilds all 299 — including the
17.4-minute Macaulay2Doc, defeating the whole exercise.  Two ways out:

- *(preferred)* pass an explicit `PACKAGES="<changed + closure>"`, so the
  unchanged packages are never in `all`'s prerequisite list at all.  The
  order-only bootstrap prerequisites still apply, though, so you additionally
  need to
- `touch` the restored `.installed` stamps for the bootstrap packages (and any
  package you're deliberately not rebuilding) after restoring the cache.  Two
  lines of shell, and worth a comment explaining why.

**Size.** A full `usr-dist` is 662 MB uncompressed locally. With 4 matrix
variants that's a few GB of the 10 GB allowance — fine today (you're using
247 MB), but expect LRU eviction pressure, so cache only the
Linux/autotools tree at first and only from `push: development`.

**Sharding, which is where this really pays.**  Once the binary plus bootstrap
packages are available as a cache or artifact, the *full* build becomes
shardable:

```yaml
strategy:
  matrix:
    shard: [1, 2, 3, 4, 5, 6, 7, 8]
```

with each shard doing `make PACKAGES="$(shard N)"` and a final job that
gathers the shards for `html-check-links` and `validate-html`.  Because
Macaulay2Doc is a hard 17.4-minute serial barrier, sharding is only worth doing
*with* the cache — and with it, the 72-minute full install could come down to
roughly 20.  Combined with §2.2 on the check side, a full development-branch
build lands in well under an hour.  That is what makes the unconditional
`push: development` safety net in §3.2 affordable.

---

## 7. What actually runs on which platform

Worth stating plainly, because it is easy to misremember and it constrains the
matrix-trimming in §3.

| | autotools Linux | cmake Linux | autotools macOS | cmake macOS |
|---|---|---|---|---|
| install 299 packages → **doc examples run** | yes | yes | yes | yes |
| `make check` → package **`TEST` blocks** | yes | — | — | — |
| `M2 --check 1/2/3` (Core basic tests) | — | — | — | yes |
| engine + memtailor/mathic/mathicgb unit tests | — | — | — | yes |
| ComputationsBook | — | — | — | yes |
| `html-check-links`, `validate-html` | yes | — | — | — |

Two observations fall out of that table.

**Packages *are* exercised on macOS — by their examples, not their tests.**
Every package's documentation examples run in the 1h15m `install-packages` step
on all four jobs.  What is Linux-only is `check`, i.e. the `TEST` blocks.  So
"we test packages on macOS" is half true, and the half that's missing is the
half written by package authors as tests.

**Two of the four jobs run no test step at all.**  `cmake-ubuntu` and
`autotools-macos` build, install, and stop — 1:26:51 and 1:21:01 respectively
with no test signal beyond examples.  For a package-only PR that is most of a
runner-day producing very little.

### 7.1 Package tests on both platforms

The machinery exists on both sides; neither workflow selects it.

**autotools/macOS** has no test step today, so adding a targeted one is a few
lines, and with §2.2/§3.3 in place the cost is bounded by `PACKAGES=`:

```yaml
      - name: Run package tests using Autotools
        if: matrix.build-system == 'autotools'
        run: make -j2 --output-sync=target -C Macaulay2/packages check PACKAGES="$PKGS"
```

**cmake** already registers per-package ctest tests —
`add_test(NAME "check-${package}-${_i}")` at `packages/CMakeLists.txt:238` —
but the workflow only runs `ctest -R "unit-tests"` and `-R "ComputationsBook"`,
so they never execute.

*`ctest -R "^check-"` will not work as-is.*  The test count comes from
`file(READ .../info-${package})` guarded by `if(EXISTS ...)`, evaluated at
**configure** time, while `info-<package>` is only written at **build** time
(`M2_INFO_TEMPLATE`, `packages/CMakeLists.txt:74`).  On a clean CI configure the
file is absent, `testcount` falls back to 0, and *zero* per-package tests get
registered.  Either re-run cmake after installing packages or move the count to
build time.  This is very likely why the path was never wired up.

### 7.2 Which packages are interface packages

Scanning for `findProgram`/`runProgram` gives 12; widening to `run "..."`,
`get "!..."`, `programPaths` and `Configuration => {"path" ...}` gives 27.
Reading what each actually invokes sorts them into three groups:

**Genuine external-program interfaces** (program in brackets):

```
CohomCalg [cohomcalg]      FourTiTwo [4ti2]        gfanInterface [gfan, fig2dev]
Msolve [msolve]            Nauty [nauty]           NautyGraphs [nauty]
Normaliz [normaliz]        Topcom [topcom]         StatePolytope [polymake]
SemidefiniteProgramming [csdp, mosek, sdpa]        CoincidentRootLoci [qepcad]
PHCpack [phc]              NumericalSchubertCalculus [phc]
RationalPoints2 [gp, sage] K3Surfaces [sage]       ConvexInterface [maple]
MonomialIntegerPrograms [scip]                     NCAlgebra [bergman]
SLPexpressions [gcc]
```

**Library / interpreter bindings** — not programs, but the most
platform-sensitive things in the list: `ForeignFunctions` (which literally
shells out to `brew --prefix`), `Python` (`python3`), `RInterface` (`R RHOME`).

**Not really interface packages:** `Visualize` and `WeierstrassSemigroups`
(`run "pwd"`, `run "rm done13"` — false positives), `Benchmark` (reads
`/usr/sbin/system_profiler`, so macOS-only by nature), `M0nbar`, and
`SpecialFanoFourfolds` + `K3Surfaces` which `curl` data files at test time —
a network dependency rather than platform coverage, and `SpecialFanoFourfolds`
is the single most expensive check in the suite at 6.6 min.  I'd leave those
out.

**Don't drive CI off that grep — and here is why it splits the way it does.**
`findProgram` is not the general "call an external program" idiom.  Its search
order (`m2/programs.m2:92-102`) is:

1. `programPaths#name` — user configuration
2. `prefixDirectory | currentLayout#"programs"` — **M2's own program directory**
3. `AdditionalPaths` — caller-supplied
4. `PATH`

Step 2 is its entire reason for existing: M2 can build some of these programs
itself and install them into its own prefix, which is *not* on `PATH`, so
something has to go looking.  That means `findProgram` marks the
**M2-buildable** programs — the ones with a recipe under `M2/libraries/` —
which is why the scan turns up 4ti2, cohomcalg, csdp, gfan, lrslib, msolve,
nauty, normaliz, palp and topcom.

Programs M2 has never built are simply invoked directly, so they never appear
in a `findProgram` grep.  That is the real reason `Polymake`, `FourierMotzkin`,
`Tropical`, `TropicalToric` and `NumericalAlgebraicGeometry` are missing from
the 27 — not randomness, but a different category:

| | provisioned by | found via |
|---|---|---|
| **A. M2-buildable** (4ti2, cohomcalg, csdp, gfan, lrslib, msolve, nauty, normaliz, palp, topcom, barvinok) | M2's own build, or distro/brew | `findProgram` |
| **B. system-only** (polymake, phc, qepcad, maple, scip, bergman, sage, gp) | distro/brew/user only | direct `run` / `get "!..."` |

`M2/libraries/polymake/` looks like a counterexample but isn't: it is an
abandoned stub pinned to polymake 2.9.8 with `SHA256SUM = # TODO: current link
is broken`, a `CONFIGURECMD` that ends in `&& false`, and a comment opening
"we haven't succeeded in making this build process work".  Polymake is
category B in practice.  Which also makes `StatePolytope`'s `findProgram
"polymake"` a miscategorisation — harmless, since step 4 falls through to
`PATH`, but there is no reason for it.

So enumerate interface packages from *both* idioms, or better, curate the list
by hand.  Treat the groups above as a starting point for a human pass, not a
manifest.

### 7.3 Which external programs each platform actually has

Extracted from the two job logs — Homebrew `Pouring` lines on macOS, apt
`Setting up` lines on Linux.  Two traps in doing this, both of which I fell into
on the first pass: tap-qualified formula names contain slashes
(`macaulay2/tap/...`), and the 4ti2 formula is named **`fourtitwo`**, so naive
greps under-report the macOS side badly.

Full set poured on the macOS runner:

```
autoconf automake bdw-gc bison blake3 boost ca-certificates ccache cddlib
cohomcalg csdp ctags eantic eigen factory fflas-ffpack flint fmt fourtitwo
fplll frobby gcc gfan givaro glpk googletest hiredis hwloc jansson libffi
libomp libtommath libtool llvm lrs m4 make mpfi mpsolve msolve nauty normaliz
ntl openblas palp python r tbb tcl-tk texinfo topcom xxhash yasm z3
```

So the picture is much better than a first pass suggests:

| program | macOS | Linux |
|---|---|---|
| 4ti2 (`fourtitwo`), cddlib, cohomcalg, csdp, factory, gfan, lrs, msolve, nauty, normaliz, palp, python, r, topcom | yes | yes |
| **polymake** | **no** | yes |
| **phc** (`phcpack`) | **no** | yes |
| **qepcad** | **no** | yes |
| maple, scip, bergman, sage, gp, mosek, sdpa | no | no |

Note that macOS gets `factory` rather than all of Singular — which is the part
M2 actually needs, so that is not a gap.  The workflow already does
`brew link factory --force`.

That leaves only three genuinely missing programs, and correspondingly few
packages that could not be checked on macOS today: `StatePolytope` and
`Polymake` (polymake), `PHCpack`, `NumericalSchubertCalculus` and
`NumericalAlgebraicGeometry` (phc), and `CoincidentRootLoci` (qepcad).
Everything else in §7.2 has its program on both platforms — including
`FourTiTwo`, which is the case I initially got wrong.

Two corollaries:

- **A pre-existing hollow spot**, unrelated to this proposal: the macOS
  `install-packages` step is already running those packages' documentation
  examples with the program absent, so macOS coverage for polymake, phc and
  qepcad is empty in both directions, not just for tests.  Worth deciding
  deliberately rather than by omission.
- **`ForeignFunctions` is the strongest single argument** for checking packages
  on macOS: it resolves libraries through `brew --prefix`, so Linux cannot
  meaningfully test it at all.

---

## 8. A platform sweep when the binary changes

You asked for this directly: when a change touches anything that goes into the
`M2` binary, build the binary — and only the binary — on a spread of platforms
reaching back to Ubuntu 18.04 and RHEL 8, so compiler problems surface on the
PR.  This is the `binary` output of the filter job in §3.1, and it is the one
branch of the classification table where the right answer is *more* jobs, not
fewer.

### 8.1 The gap is wider than "we only test new compilers"

`test_build.yml` builds on the newest release of each OS: `ubuntu-24.04` and
`macos-15`.  Everything Macaulay2 actually ships is built somewhere else.

| channel | oldest target | default compiler | runs on a PR? |
|---|---|---|---|
| Ubuntu PPA | bionic 18.04 | gcc 7.3 | no |
| Ubuntu PPA | focal 20.04 | gcc 9.3 | no |
| Ubuntu PPA | jammy 22.04 | gcc 11.2 | no |
| `build-deb.yml` | Debian bullseye | gcc 10.2 | no — `workflow_dispatch` |
| `build-rpm.yml` | AlmaLinux 8 | gcc 8.5 | no — dispatch + release tags |
| `build-dmg.yml` | macOS 14 arm64, macOS 15 x86_64 | AppleClang | no |
| `test_build.yml` | ubuntu-24.04, macos-15 arm64 | gcc 13.2, AppleClang/clang | **yes** |

(Compiler versions taken from the distributions' own indexes rather than from
memory: `gcc` 4:7.3.0 bionic, 4:9.3.0 focal, 4:11.2.0 jammy, 4:13.2.0 noble,
4:15.2.0 resolute; 4:10.2.1 bullseye, 4:12.2.0 bookworm, 4:14.2.0 trixie;
8.5.0-23 in AlmaLinux 8's AppStream.)

Bionic is a live target, not a leftover.  The PPA's bionic pocket currently
carries

```
macaulay2  1.26.06+ds~js-2~ubuntu18.04.1
```

published 2026-06-14 — i.e. the 1.26.06 sources were compiled with gcc 7 six
weeks ago, and will be again at the next release.

**And 18.04 is not merely an older compiler; it is a different Macaulay2.**
That package's `Depends` include `libflint-2.6.3` and `libflint-arb2`, where
ubuntu-24.04 has FLINT 3.  `configure.ac:880` deliberately accepts flint
≥ 2.6.3 with a separate Arb, and four sites compile differently as a result:

| site | macro |
|---|---|
| `Macaulay2/e/basic-rings/aring-ZZp-flint.hpp:21` | `HAVE_FLINT_NMOD_H` (flint ≥ 2.9) |
| `Macaulay2/e/basic-rings/aring.hpp:11` | `HAVE_FLINT_RAND_INIT` |
| `Macaulay2/d/ballarith.d:8` | `HAVE_ARB_H` vs `HAVE_FLINT_ARB_H` |
| `Macaulay2/d/ballarith.d:19` | same |

Nothing in CI compiles the flint-2 side of any of them.  A PR touching
`aring-ZZp-flint.hpp` is built against exactly one FLINT, and the other
configuration is discovered by the PPA build *after* the release is tagged.
That is the concrete failure this section exists to prevent; it isn't
hypothetical tidiness.

The same shape appears on macOS, smaller: `test_build.yml` builds arm64 only,
while `build-dmg.yml` ships an x86_64 build.  No PR compiles Macaulay2 for
Intel Macs.

### 8.2 What a binary-only build costs, per platform

"Binary only" is `make PACKAGES=`, and per §1 that is 3:42 of compiling plus
1:54 in `make -C libraries` on ubuntu-24.04 — about six minutes.  But the 1:54
is small only because the PPA supplies every library prebuilt.  Where there is
no M2 dependency repository, `make -C libraries` compiles them, and that
dominates.  Measured locally (12-core box, `make -j4` per library, download +
configure + compile + install into `usr-host`, via
`--enable-build-libraries`):

| library | from source |
|---|---|
| factory | 1:52 |
| ntl | 2:31 |
| flint | 3:25 |
| normaliz | 6:28 |

and from the macOS job of the reference run, which does build three of them
(`configure: using BUILDLIBLIST = memtailor mathic mathicgb`) on a 4-core
runner: memtailor 0:19, mathic 0:29, mathicgb 1:36.  Building the whole set is
why the `almalinux-8` job in `build-rpm.yml` takes 123 minutes — though that
number also installs all 299 packages, so it is an upper bound on the library
cost, not a measurement of it.

The saving grace for Ubuntu is that **the PPA backports the entire dependency
stack to the old series**, precisely because those releases' own archives are
too old.  Comparing the PPA's `Packages` at the two ends of the range:

| series | what the PPA carries besides M2 itself |
|---|---|
| noble 24.04 | `gfan`, `msolve` — Ubuntu supplies the rest |
| bionic 18.04 | `libflint-dev`, `libgivaro-dev`, `libmathicgb-dev`, `libmps-dev`, `libnormaliz-dev`, `fflas-ffpack`, `cohomcalg`, `gfan`, `msolve`, `nauty`, `normaliz`, `topcom` |

Still true today, verified in a container:

```
$ docker run --rm ubuntu:18.04 bash -c 'apt-get update && \
    apt-get install -y gnupg software-properties-common ca-certificates && \
    add-apt-repository -y ppa:macaulay2/macaulay2 && apt-get update && \
    apt-cache policy libflint-dev'
libflint-dev:
  Candidate: 2.6.3-3ppa1~ubuntu18.04.1
```

So a bionic job can be about as cheap as the current Linux job: add the PPA,
install the dependencies, `make PACKAGES=`.  The proof that the dependency set
resolves on bionic is that the bionic source package's `Build-Depends` — the
same `libflint-dev libgivaro-dev libmathic-dev libnormaliz-dev libsingular-dev
…` list — builds on Launchpad every release.  What I have *not* checked is
whether `test_build.yml`'s particular apt line resolves as written; several
entries in it (`clang-16`, `libomp-16-dev`, `pipx`, `qepcad`,
`w3c-markup-validator`) certainly won't, but those are compiler-variant and
test dependencies, not binary prerequisites.

RHEL 8 has no equivalent repository, so it genuinely does compile libraries
from source.  Budget 30-45 minutes for a cold run and lean on §6 afterwards: a
library tree is a pure function of `M2/libraries/**`, `M2/submodules/**` and
the platform, which makes it the easiest thing in this document to cache
correctly.

### 8.3 The matrix I would actually write

Two tiers, because "a bunch of platforms" and "on every PR" pull against each
other.

**Tier 1 — any PR with `binary == true`**, alongside the existing four jobs:

| job | how | compiler | why this one |
|---|---|---|---|
| bionic | `docker run ubuntu:18.04` + PPA | gcc 7.3 | oldest shipped; the only flint-2 build anywhere |
| almalinux 8 | `docker run almalinux:8` | gcc 8.5 | oldest RPM target; `M2/BUILD/docker/rhel` already exists |
| ubuntu-22.04 | hosted runner | gcc 11.2 | free, no container needed |
| ubuntu-24.04-arm | hosted runner | gcc 13.2 | the 32/64-bit width bugs of §10 |
| macos-15-intel | hosted runner | AppleClang | the only x86_64 macOS anywhere |
| ubuntu-26.04 | hosted runner | gcc 15.2 | new-compiler diagnostics, which in practice bite more often than old ones |

Six jobs at roughly 6-15 minutes each (AlmaLinux 8 excepted until it is
cached), against a class of change that is a minority of PRs.  Set against
4 × 2h30 on package-only PRs today, this is cheap.

**Tier 2 — `push: development` and the weekly build:** focal, jammy, bullseye,
bookworm, AlmaLinux 9 and 10, Fedora, `macos-26`, and clang on Linux.  A
failure there is "someone should look at this", not a merge blocker.

Sketch, reusing the filter job from §3.1:

```yaml
  binary-sweep:
    needs: changes
    if: needs.changes.outputs.binary == 'true'
    runs-on: ${{ matrix.runner }}
    timeout-minutes: 90
    strategy:
      fail-fast: false
      matrix:
        include:
          - { name: bionic, runner: ubuntu-latest, image: 'ubuntu:18.04' }
          - { name: alma8,  runner: ubuntu-latest, image: 'almalinux:8'  }
          - { name: jammy,  runner: ubuntu-22.04 }
          - { name: arm64,  runner: ubuntu-24.04-arm }
          - { name: intel,  runner: macos-15-intel }
          - { name: gcc15,  runner: ubuntu-26.04 }
    steps:
      - uses: actions/checkout@v6
      # native, or:  docker run --rm -v "$PWD":/src -w /src "$image" ...
      - run: make -j$(nproc) PACKAGES=
      - run: ./M2 -q --no-preload --check 1
```

### 8.4 Four mechanics that will bite

**Don't use `container:` for Ubuntu 18.04.**  Bionic is glibc 2.27
(`libc6 2.27-3ubuntu1`), and the runner injects its own Node to execute
JavaScript actions; neither the Node 20 nor the Node 24 it ships will run
against glibc that old — 2.28 is the floor.  So
`actions/checkout` — and every other JS action — fails inside an 18.04
container.  Check out on the host and `docker run -v "$PWD":/src` instead,
which is what `M2/BUILD/rpm/Makefile:20` already does for the RPM builds.
AlmaLinux 8 is glibc 2.28 and does work with `container:`, but one mechanism
for both is less to explain.

**Reuse the Dockerfiles you already have.**  `M2/BUILD/docker/{ubuntu,rhel,debian}`
already encode these dependency sets, and `M2/BUILD/docker/README.md` concedes
that "some of the above may be outdated".  Wiring them into CI is exactly what
would stop them rotting, and a bionic Dockerfile alongside them is the natural
home for the PPA lines above.

**`M2 --check 1` is free, so run it.**  Measured on a local build: 0.1 s.  It
catches a binary that compiles and links but cannot start, which is most of
what actually goes wrong on an unusual platform and is invisible to a
build-only job.  Do not reach for `--check 2` as a smoke test — it ran past ten
minutes locally before I stopped it.

**Expect old platforms to break for reasons that aren't yours.**  EOL mirrors
move, PPAs get rebuilt, upstream tarball URLs die.  A tier-1 job that fails in
`apt-get update` is a false alarm, and enough of them will train people to
ignore the sweep.  Two cheap defences: keep tier 1 small enough that a human
reads every failure, and put environment setup in its own step so a failure is
legibly not a compile error.

---

## 9. Building a library when its recipe changes

The companion rule to §8, and the second thing you asked for: if a PR changes
how a bundled library is built, CI should build that library from source
instead of installing it from the system.  Today it never does.  Every job
builds exactly three from source — `configure: using BUILDLIBLIST = memtailor
mathic mathicgb` — and it builds those three on every single run regardless of
what the PR touched, simply because `test_build.yml:179` configures with
`--with-system-gc --with-fplll` and not `--with-system-libs`, whose default is
to build them (`configure.ac:690`).  Everything else comes from the PPA or
Homebrew, always.  So most of `M2/libraries/**` and most of the
`ExternalProject_Add` blocks in `M2/cmake/build-libraries.cmake` are in effect
untested code.  The people who exercise them are people building from source,
which is also the population with no CI.

### 9.1 What counts as a recipe change

The two build systems agree on sources, so a library has one recipe expressed
in three places, and a change to any of them means the same thing.

The nine git submodules — `bdwgc`, `flint`, `frobby`, `givaro`,
`fflas_ffpack`, `googletest`, `memtailor`, `mathic`, `mathicgb` — are consumed
by cmake as `SOURCE_DIR ${CMAKE_SOURCE_DIR}/submodules/<name>` and by autotools
through `SUBMODULE = true`, which `Makefile.library.in:95-98` turns into
`cp -r @abs_top_srcdir@/submodules/$(LIBNAME)/*`.  Same checkout both ways.
Everything else is a tarball, and the pinned versions match across the two
build systems in all eighteen comparable cases: eigen 5.0.1, mpfr 4.2.2, mpfi
1.5.4, ntl 11.6.0, factory 4.4.1, cddlib 0.94n, msolve 0.10.1, mpsolve 3.2.3,
glpk 5.0, 4ti2 1.6.15, cohomCalg 0.32, gfan 0.8beta, lrslib 073, csdp 6.2.0,
nauty 2.9.3, normaliz 3.11.1, topcom 1.1.2, palp 2.21.  That agreement is
maintained by hand, so the filter script is the obvious place to *assert* it
and fail when the two drift.

So for library `foo`, "the recipe changed" is the union of

- `M2/libraries/<dir>/**`
- the `ExternalProject_Add(build-<name> …)` block in `M2/cmake/build-libraries.cmake`
- `M2/submodules/<name>` — a pointer bump touches neither of the other two

and it should force a from-source build in **both** build systems, not only in
the one whose file changed.

Attributing a hunk of `build-libraries.cmake` to a library is mechanical:
`git diff -U0` gives changed line numbers, and each maps to the nearest
preceding `ExternalProject_Add(build-X`.  Two escape hatches: anything above
the first one (lines 1-148, the `CONFIGURE`/`MAKE` variables and the
`_ADD_*_DEPENDENCY` helpers) forces every library, as do
`M2/libraries/Makefile.in`, `Makefile.library.in` and `Makefile.template`.

### 9.2 The knobs

Both build systems already have exactly the option needed; neither is used in
CI.

**autotools** — `configure.ac:655`:

```
../../configure --enable-build-libraries="flint ntl"
```

It accepts names from `LIBLIST` *and* `PROGLIST` (`configure.ac:589,592`) and
hard-errors on anything else, which is a useful safety net for a generated
argument.  The outcome is echoed at the end of configure
(`configure.ac:1851`), so CI can assert on it rather than hope:

```
configure: using BUILDLIBLIST  =  memtailor mathic mathicgb
```

**cmake** — `cmake/configure.cmake:38-39`:

```
cmake -DBUILD_LIBRARIES="FLINT NTL" -DBUILD_PROGRAMS="GFAN" …
```

`check-libraries.cmake:254-305` upper-cases the value and unsets the found
library so that it gets built instead.  Unlike configure, **cmake silently
ignores a name it does not recognise**, so here the assertion is not optional.
(While you are in that file: the usage comment at `check-libraries.cmake:4`
spells the variable `BUILD_LIBRARES`.)

### 9.3 The names don't line up, and that is the whole difficulty

`BUILD_LIBRARIES` is matched against `LIBRARY_OPTIONS`
(`check-libraries.cmake:183`) and `BUILD_PROGRAMS` against `PROGRAM_OPTIONS`
(`:245`), and neither list is the directory name:

| `M2/libraries/` | autotools name | cmake option value | cmake target |
|---|---|---|---|
| `gc` | `gc` | `BDWGC` | `build-bdwgc` |
| `gtest` | `gtest` | `GTEST` | `build-googletest` |
| `eigen` | `eigen` | `EIGEN3` | `build-eigen` |
| `nauty` | `nauty` | `NAUTY_EXECUTABLE` | `build-nauty` |
| `normaliz` | `normaliz` | `NORMALIZ` (library) and `NORMALIZ_EXECUTABLE` (program) | `build-normaliz` |
| everything else | = directory | = uppercased directory | `build-<directory>` |

`NAUTY_EXECUTABLE` is not guessable: forcing a nauty build under cmake is
`-DBUILD_PROGRAMS=NAUTY_EXECUTABLE`, because that is the variable
`FindNauty.cmake:72` sets and `check-libraries.cmake:296` tests.  Put the table
in the script as data rather than trying to derive it.

Some recipes exist on only one side, and should map to "this build system
only": `gdbm`, `readline`, `gmp`, `lapack`, `linbox`, `fplll`, `tbb` and
`barvinok` have autotools recipes but no cmake `ExternalProject`; `phcpack` and
`bertini` are the reverse.  `polymake` has both, but `M2/libraries/polymake` is
the abandoned stub described in §7.2 whose `CONFIGURECMD` ends in `&& false` —
the filter must not select it.

### 9.4 Cost, and which jobs get the flags

Two to seven minutes per library on 4 cores (§8.2), and a PR normally touches
one.  Because a recipe change *is* a portability change, run it across the
tier-1 platforms of §8.3 rather than on ubuntu-24.04 alone — a library that
builds under gcc 13 and not under gcc 7 is the whole point.

Keep the ordinary unforced jobs as well.  Forcing `flint` means that job's
binary links the built flint, so the system-flint path stops being covered by
it.

### 9.5 For a program, building it is not the test

Nine of these recipes are programs rather than libraries, and a program that
compiles but produces wrong output is exactly what a build-only job misses.
The packages that exercise each are already enumerated in §7.2, so the filter
can add them to `PACKAGES=` for nearly nothing:

| recipe | packages to check |
|---|---|
| `4ti2` | `FourTiTwo` |
| `cohomcalg` | `CohomCalg` |
| `csdp` | `SemidefiniteProgramming` |
| `gfan` | `gfanInterface`, `StatePolytope` |
| `msolve` | `Msolve` |
| `nauty` | `Nauty`, `NautyGraphs` |
| `normaliz` | `Normaliz` |
| `topcom` | `Topcom` |
| `lrslib`, `palp` | none — no distributed package invokes `lrs` or PALP |

With the `PACKAGES=`-honouring `check` target from §3.3 in place that is a
couple of minutes, and it is the difference between "the recipe compiles" and
"the program still works".

---

## 10. Other things worth changing

**Coverage gaps** get their own section — see §7.  The short version: tie test
coverage to *what changed*, not to which build system happens to run where,
otherwise trimming the matrix quietly drops whole test suites.

**No `timeout-minutes`.**  A hung job runs to the 6-hour default.  `180` on
`build` would be generous and would stop a wedged macOS runner from holding a
concurrency slot all afternoon.

**No `permissions:` block.**  `test_build.yml` and `lint.yml` run with whatever
the repo default is.  `permissions: { contents: read }` at the top of both,
raised only where a job needs more, is free hardening.  `actions/checkout` with
`persist-credentials: false` likewise.

**Comments inside a YAML block scalar are not comments.**  In the artifact step:

```yaml
          path: |
            # Autotools
            M2/BUILD/build/config.log
```

`path: |` is a literal block scalar, so `# Autotools` and `# CMake` are passed
to `upload-artifact` as path patterns and produce "no files found" warnings.
Move them above the `path:` key.

**`lint.yml` runs twice per PR.**  `on: [ push, pull_request ]` means every push
to a branch with an open PR triggers both.  Drop `push`, or restrict it to
`branches: [ development, stable ]`.  While there: it runs `sudo apt-get
install -y codespell` with no preceding `apt-get update` (works only because
the runner image's lists are warm), the trailing `env: MAKEFLAGS: -j2` is
vestigial since no `make` runs, and codespell only scans
`M2/Macaulay2/packages` — extending it to `m2`, `d`, `e`, and the docs would
cost seconds.

**arm64 coverage.**  `build-deb.yml` and `build-rpm.yml` exercise
`ubuntu-24.04-arm`, but `test_build.yml` doesn't.  Given the history of
32/64-bit width bugs surfacing only on armhf, an arm64 job on the *scheduled*
full build (not per-PR — it's free for public repos but it's another 2.5 hours)
would be cheap insurance.  A binary-only arm64 job is affordable per-PR, which
is why it appears in the tier-1 sweep of §8.3.

**Fork PRs.**  `if: github.repository == 'Macaulay2/M2' || contains(github.ref,
'global')` is fine for `pull_request` (where `github.repository` is the base
repo), but it silently disables CI on contributors' own forks before they open
a PR.  That's probably intentional; just noting it's why the `global` branch
convention exists.

**`Macaulay2/tests/engine`** is still excluded pending #1213, per the TODO in
the workflow.  Not new, but worth tracking alongside this work since the
filtering table has to decide what `Macaulay2/tests/**` changes trigger.

---

## 11. Suggested order of work

Roughly in decreasing ratio of payoff to risk:

1. **Fix the cache (§2.1)** — path, rolling key, `push: development` trigger,
   enable for autotools, add `ccache -s`.  Small, self-contained, no behaviour
   change.
2. **Parallelise `make check` (§2.2)** — a one-character change to
   `packages/Makefile.in` plus `--output-sync=target`; 35-45 minutes off
   *every* full build including the nightly and release builds.  Start at
   `-j2` and watch memory.
3. **Split install and check into two jobs (§2.2a)** — no Makefile change and
   no filtering needed; roughly halves full-build wall time, because the two
   phases are independent given the binary.
4. **Point the scheduled build at `development` (§2.3)**, and unpin
   `package-review@master`.
5. **Add the binary-only platform sweep (§8)** — bionic and AlmaLinux 8 first,
   since those are the two that are shipped and never compiled on a PR, and
   bionic is the only build of the flint-2 code paths anywhere.  This does not
   need the full classification table: a single paths-filter producing one
   `binary` boolean is enough to gate it, so it can land before step 6.
   Start with tier 1 and move anything flaky to tier 2 rather than deleting it.
6. **Add the filter job and the classification table (§3)** with a `full-ci`
   label escape hatch and an unconditional full build on `push: development`.
   Land the `PACKAGES=`-honouring `check` target and the shared changed-package
   script (folding in the §4.1 fixes) at the same time.
7. **Force from-source library builds when a recipe changes (§9)** — the
   name-mapping table plus `--enable-build-libraries=` / `-DBUILD_LIBRARIES=`,
   asserting on `BUILDLIBLIST` and on cmake's side because cmake ignores
   unknown names silently.  Naturally runs on the step-5 sweep, and it is the
   only thing that would ever test `M2/libraries/**` at all.
8. **Wire in reverse-dependency expansion (§4)** — the script from step 6, plus
   the preloaded-packages-are-Core rule.
9. **Add macOS package checks for the interface packages that have their
   programs (§7)** — cheap once §2.2/§3.3 land, and it closes the one gap the
   Linux job structurally cannot cover.  Most interface packages already have
   their program on both platforms; decide separately whether to add
   `polymake`, `phcpack` and `qepcad` to the `brew install` line, which are the
   only three genuinely missing.
10. **Upload HTML as an artifact (§5.1)**, then build out the `gh-pages`
    preview (§5.2).
11. **Cache the built tree and shard the full build (§6)** — the largest
    change, and the one that makes the always-on `push: development` sweep
    cheap.  It also removes most of the cost of AlmaLinux 8 in step 5, whose
    only real expense is compiling libraries from source.

Steps 1–4 need no design decisions and no path filtering at all, and between
them would take the current 7 h 48 m of runner time per PR down by well over
half.  Steps 6 and 8 are what take a one-package PR from 7 h 48 m to about 28
minutes on a single runner, and step 11 takes that to under 10.

Steps 5 and 7 are the only ones here that *add* work rather than remove it, and
they are worth separating in your head from the rest of the document.
Everything else is about not doing the same thing four times; those two are
about doing something that is currently not done at all, on a class of change
where the current signal is "it compiled on the newest Ubuntu and the newest
macOS".  Their cost is bounded by being binary-only — six jobs of roughly ten
minutes, on the minority of PRs that touch the binary — which is less runner
time than a single one of today's four jobs spends installing packages.
