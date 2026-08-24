# Review of the GitHub Actions workflow

*Written by Claude (Opus 5) at Doug's request, 2026-08-22; §8, §9 and §10 added
and the job architecture (§3.5-§3.7) worked out 2026-08-23.*

All timings below are measured from real runs of `test_build.yml`, not estimated.
The reference run is [32561543375](https://github.com/Macaulay2/M2/actions/runs/32561543375)
("Update AdjunctionForSurfaces.m2", 2026-08-22) — a PR that changed exactly one
package file, i.e. the case this review is about.

**Status, 2026-08-23.**  Two pull requests open:

- [#4663](https://github.com/Macaulay2/M2/pull/4663) — §2.2 and §3.3.  Single
  colon for the `check` target, `PACKAGES=` honoured by `check`, `DEVEL`
  removed.
- [#4664](https://github.com/Macaulay2/M2/pull/4664) — §2.1.  ccache switched to
  `hendrikmuhs/ccache-action` and ungated from cmake, `usr-host` cache dropped,
  `push: [ development ]` added to seed it, and `SEED_CACHE_ONLY` added so the
  seeding runs stop after building the binary.  Validated on a fork first; see
  §2.1's "Measured on a trial run" below.

§3.5-§3.7 supersede the "reduce the matrix from 4 jobs to 1" framing of the
original draft: the reduction is a shared `build` producer feeding package jobs
dimensioned by platform and harness, with the compiler axis moved to §8's sweep.

**Status, 2026-08-23 (later).**  Steps 3 and 4 of §12 are implemented on the
`ci-overhaul` branch, on top of cherry-picks of both open pull requests:

- §2.3 — the weekly sweep gets an explicit `ref` matrix axis carrying
  `development` and `stable`, ccache `save:` extended to non-pull-request
  events with only the `development` row writing, and the `push: [ development ]`
  seeding plus `SEED_CACHE_ONLY` reverted.  `package-review@master` unpinned.
- §2.2a/§3.5/§3.6 — `test_build.yml` split into `build`, `install` and `check`
  jobs, with `.github/actions/setup-build` holding the setup the three share
  and the build tree travelling between them as one zstd tarball.  §2.2's
  `-j2 --output-sync=target` now applied to `make check`.
- §11's free items: `permissions: contents: read`, `timeout-minutes`, the
  comments moved out of the `upload-artifact` block scalars, `lint.yml` no
  longer triggered by `push`.
- §13 (new) — path-based pull request labelling.

Steps 5 onward are unstarted.

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
4-job matrix down is still worth doing, but it is the second-order effect.  §3.5
and §3.6 settle what the reduction actually is: one shared `build` producer plus
package jobs dimensioned by *platform and harness* rather than by compiler.

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
`/usr/lib/ccache` is prepended to `PATH` for *all* Linux jobs — and configure
records the compiler unqualified (`checking for gcc... gcc`), so build-time
`PATH` lookup already resolves to `/usr/lib/ccache/g++`.  **Both build systems
have been routing every compile through ccache all along**; only the archived
directory was wrong.  The autotools jobs have been paying ccache's bookkeeping
for a guaranteed 0% hit rate.  So no `create-symlink` or `CC=`/`CXX=` plumbing
is needed anywhere — just cache the right place, for all four jobs.

#### The `usr-host` half of that cache does nothing — drop it

This is worth spelling out, because it looks like the more valuable half and is
not.  Splitting it into its own exactly-keyed cache (my first suggestion) does
not work either, for two independent reasons:

- **cmake** compiles memtailor, mathic and mathicgb *in-tree* as part of the
  main build (`Macaulay2/e/mathicgb/CMakeFiles/mathicgb.dir/...`, inside a
  431-target ninja build), not as external libraries.  So `usr-host` holds
  almost nothing — hence 3 MB on Linux, 0 MB on macOS — and ccache already
  covers those compiles.  `Build libraries using Ninja` is under 30 s.
- **autotools** does build the three into `usr-host`, but `install` there hangs
  off a stamp chain kept in `M2/BUILD/build/libraries`, not in `usr-host`:
  `.installed-$(VERSION) : .compiled-$(VERSION) : ... : .configured : .patched
  : .untarred`.  Restoring `usr-host` alone leaves the stamps missing, so make
  rebuilds the whole chain regardless — the same class of bug as the ccache
  path.  Making it work would mean caching the entire `libraries/` tree
  (extracted sources plus objects, per matrix variant) to save 1m54s.

And the 1m54s is better left alone rather than eliminated.  `--with-system-libs`
covers exactly gc + memtailor + mathic + mathicgb (`configure.ac:690`), so
adding it plus the three `-dev` packages would zero the phase cleanly.  But what
these jobs uniquely cover is not the submodule *sources* — cmake compiles those
too, through its own in-tree targets — it is the `M2/libraries/` machinery
wrapped around them: `git submodule update --init`, copy into `$(TARDIR)`,
`PRECONFIGURE = autoreconf -i`, `configure --disable-shared`, and install into
`usr-host`, all driven by `Makefile.library`.  `make-dist.yml:21` configures
`--with-system-libs` and skips that path entirely, and cmake never uses it, so
the autotools jobs here are the only place it runs at all.  1m54s out of a
150-minute job is a cheap price for that, and a warm ccache absorbs most of it.

#### What this is actually worth

Total compile time per job that ccache can attack:

| phase | time | what compiles |
|---|---|---|
| `make -C libraries` | 1:54 | memtailor, mathic, mathicgb only |
| `make PACKAGES=` | 3:42 | the binary: 272 TUs in `c/d/e/system/bin`, 245 of them in `e/` |
| **total** | **5:36** | |

A full hit turns that into perhaps 40-60 s (preprocess, fetch, link), so
**~4.5-5 min per job, ~18-20 min across the matrix** — about **4%** of today's
7 h 48 m.  For calibration, ccache on Doug's own machine reports 50.9% hits
over 4361 cacheable calls, though that mixes branches and compilers and so
understates the CI case, where it is one compiler and one-file deltas.

So on its own this is not where the time goes, and it should not be sold as
such.  The reasons to do it anyway:

1. It is nearly free, and it is the **only** lever for the ~22% of PRs that
   touch `c/d/e/system/bin`, where §6's tree cache misses *by construction*
   because its key hashes exactly those directories.  The two caches are
   complementary, not competing.
2. It rises to ~16% of a §3-filtered 28-minute job.
3. It makes the always-on `push: development` sweep cheaper, which is what
   funds the safety net in §3.2.

#### What was landed ([PR #4664](https://github.com/Macaulay2/M2/pull/4664))

`hendrikmuhs/ccache-action@v1.2.23` replaces the
hand-rolled step and fixes all three bugs above at once: it knows where ccache
actually lives, `append-timestamp` rotates the key with `restore-keys` prefix
matching, and the post step prints stats so this cannot rot again unnoticed.
Settings that matter for M2:

- `key:` is a *namespace*, not the whole key.  The action composes
  `"ccache-" + key + "-"` and the post step appends an ISO timestamp
  (`src/restore.ts:318-320`, `src/save.ts:122-124`), so ours saves as
  `ccache-Linux-autotools-default-2026-...Z`.  Setting it from
  `runner.os`/`build-system`/`compiler` keeps the four matrix jobs from sharing
  objects built by different compilers.
- No explicit `restore-keys`, deliberately.  §2.1 above recommends "rolling key
  plus restore-keys", but with this action `append-timestamp` *is* the rolling
  part, and the primary key prefix-matches: the action's own test suite
  (`tests.yml:112-121`) runs `key: parent` with no `restore-keys` in a job that
  `needs:` the populating job and asserts `test-cache-hit = true`, while the
  adjacent `test_cache_miss` asserts `false` for a randomized key.  Worth
  recording because GitHub's documentation is self-contradictory on whether the
  primary key is exact-match-only, and if it were, this configuration would be
  write-only.
- `max-size` left at its 500M default.  Measuring first: the build tree has 382
  objects totalling 554 MB (mean 1485 KB, `Macaulay2/e/eigen.o` alone at 145 MB
  and `d/boostmath.o` at 54 MB — and CI is comparable, compiling `-O2 -g3`), but
  ccache compresses those roughly 4x.  Doug's local stats work out to ~390 KB per
  stored entry, so **one full build is about 150 MB of ccache**, not the 800 MB
  his total suggests — that 0.8 GiB is ~2141 entries accumulated across many
  branches and compilers.  The default is therefore ~3x one build, and the peak
  during a run is only the restored generation plus whatever changed, so
  mid-build eviction (the failure mode a small cap causes, where the working set
  exceeds the cap and nothing ever hits) is not reachable.  A custom value here
  would be an unjustifiable knob, and a smaller cap also means smaller entries
  and less pressure on the shared 10 GB budget.  Raise it only if the post-step
  stats report evictions.
- `evict-old-files: job` — the default is `''`, i.e. no eviction.  Setting it
  runs `ccache --evict-older-than <job duration>s` in the post step, *after* the
  `save: false` early return and *before* the upload (`src/save.ts:100-122`), so
  it costs nothing on pull request runs and shrinks what the push runs actually
  store: each entry ends up being one build's working set (~150 MB) rather than
  whatever the cache had grown to under the 500 MB cap (~3 generations).  That
  is a ~3x reduction in the stored size, which is the 10 GB budget concern
  below.  "Not touched" relies on ccache refreshing timestamps on *hits*, not
  only on stores — it does, that is how its own LRU works — so a fully-cached
  build keeps everything it used.  The one case this is mildly wrong for is a PR
  that reverts a file to an older revision, whose object was evicted; cost is
  one recompile.
- `key:` matches the job name (`build-system`-`os`-`compiler`), so a cache entry
  reads the same as the job that wrote it.  Note `matrix.os`, not `runner.os`:
  the latter collapses to `Linux`/`macOS`, so adding e.g. `ubuntu-22.04` to the
  matrix would silently make two different toolchains share one namespace.
- `save: ${{ github.event_name == 'push' }}` — only development merges write,
  PRs read; fork PRs cannot write anyway
- `job-summary` left at its default: it needs ccache 4.10+, and the runners
  disagree — ubuntu-24.04 ships 4.9.1, the macOS runners have brew's 4.13.6 —
  so enabling it would report on two jobs and not the other two.  The post-step
  stats cover all four anyway.
- `create-symlink` left at `false`.  The existing "Prepare build environment"
  step is the better mechanism and should be kept: it puts the distro's own
  ccache symlink directory on `GITHUB_PATH`, which (a) covers versioned
  compiler names the action's eight hardcoded symlinks miss, and (b) lands
  *ahead* of `$(brew --prefix llvm)/bin` because `GITHUB_PATH` prepends and
  that step runs after "Set up compiler".  `create-symlink` writes to
  `/usr/local/bin`, which `GITHUB_PATH` entries shadow — so it would silently
  bypass ccache in the brew-clang job, the one place ordering is delicate.
  Verified working today: that job resolves to
  `/opt/homebrew/opt/ccache/libexec/clang++` and CMake reports
  `C = Clang 22.1.8 (/opt/homebrew/opt/ccache/libexec/clang)`.
- The action is still worth using for everything *other* than PATH, and not
  only for the cache directory: on darwin it sets
  `compiler_check=content`, without which ccache keys on the compiler's mtime
  and every entry misses after a Homebrew LLVM update.
- the `usr-host` cache removed entirely, per above

The number worth watching is not `max-size` but the repository total: four
entries per merge to `development` at ~150-250 MB each, with `append-timestamp`
writing a fresh key every time and old ones lingering until GitHub's LRU
reclaims them against the 10 GB budget.  Pull request runs always read the
newest, so the oldest go first, but on a busy day this becomes the dominant
consumer of a budget currently sitting at 247 MB — and it can evict other
workflows' caches.  `gh api repos/Macaulay2/M2/actions/cache/usage` after the
first few merges is the check.

#### Why the seeding runs stop after the binary

A `push: [ development ]` trigger that ran the *full* matrix would cost more
than the cache ever saves.  Measured rates: ~89 pull request runs of
`test_build.yml` per month against ~5 pushes to `development`, a ratio of ~18.
A full build costs 468 min of runner time and ccache saves ~20 min per pull
request run, so break-even needs **23.4 PR runs per push** — you have 18.  Net
about −560 min/month, i.e. paying for nothing.  Nor does it improve as the rest
of this document lands: §2.2 shaves ~40 min off the one job that runs checks,
§3's filtering deliberately does not apply to the post-merge build, and §8's
sharding cuts wall time rather than total CPU.

The way out is that **ccache only caches compiler invocations**.  Installing the
299 packages and checking them just runs `M2`; those phases compile nothing and
contribute nothing to the cache.  So a seeding run needs only: install
dependencies, configure, `make -C libraries`, `make PACKAGES=`.  That is ~33 min
across the matrix instead of 7 h 48 m, and it populates everything ccache can
hold:

```
cost:     5 ×  33 =  165 min/month
benefit: 89 ×  20 = 1780 min/month
```

Break-even drops to 1.7 PR runs per push.  Hence `SEED_CACHE_ONLY`, gating the
two `Install packages` steps, both `Run Tests` steps, `Validate HTML
documentation` and the deb upload.

One consequence to accept deliberately: `concurrency` keys on `github.ref` for
pushes, so two merges landing close together cancel the earlier run.  A
cancelled job's post step may be killed before it saves, so during a burst only
the last merge reliably refreshes the cache.

#### This whole mechanism is temporary

An earlier draft said to set `SEED_CACHE_ONLY` false once §3 lands, so that the
post-merge build became the full safety net.  That is not the plan any more:
**the weekly build should do both jobs**, and the push trigger with its env var
and six `if:` gates should then be reverted.

Two reasons.  Detection latency barely differs — roughly 5 pushes to
`development` per month against a weekly build's 4.3, so weekly is very nearly
per-merge already, while a full build on every push would roughly double the
number of full builds.  And ccache degrades gracefully: a week-old cache is a
partial hit, not a miss.  If `development` moves by a few binary PRs, a run
misses only on the changed translation units and whatever a changed header pulls
in — perhaps 5-40 objects of 272, at ~2.3 CPU-seconds each, so tens of seconds
of wall time against the 45 seconds a fully warm build already takes.  Paying
~165 min/month of seeding plus all this machinery to avoid half a minute per PR
is the wrong trade.

It stays for now because the weekly build cannot yet do the job: it runs on
`stable` (§2.3), and `save:` is keyed on `github.event_name == 'push'`, so a
scheduled run would restore a cache and never write one.  The revert is
therefore gated on:

1. §2.3 — point the schedule at `development` (as a `ref` matrix row, keeping
   `stable` too, so it does not silently stop being built);
2. `save:` extended to `schedule` and `workflow_dispatch`, with only the
   `development` row writing so the two rows do not fight over one key;
3. then revert the `push` trigger, `SEED_CACHE_ONLY` and the six gates.

Which promotes §2.3 from housekeeping to a hard prerequisite: it becomes both
the only full sweep *and* the only cache seeder.  Note also that a scheduled
run's `GITHUB_REF` is the default branch, so whatever it writes is
default-branch-scoped and readable by every PR regardless of base — the seeding
works even though the run's ref is not `development`.  What matters is what gets
checked out, not the scope.

#### Measured on a trial run

Validated on `d-torrance/M2` before the PR, which needed a temporary commit —
the job's `if` requires `Macaulay2/M2` or `global` in the ref, and the push
trigger only fires on `development`.  Worth recording that **fork pull requests
cannot exercise this at all**: for a `pull_request` event `github.ref` is
`refs/pull/N/merge`, which contains no `global`.  Only the push path is testable
on a fork.

Cold run ([32660322645](https://github.com/d-torrance/M2/actions/runs/32660322645)),
then an empty commit for a warm one
([32660996677](https://github.com/d-torrance/M2/actions/runs/32660996677)):

| job | full build | cold seed | warm seed |
|---|---|---|---|
| `autotools-ubuntu-24.04-default` | 2:30:20 | 9:19 | 3:24 |
| `cmake-ubuntu-24.04-default` | 1:26:51 | 7:33 | 3:17 |
| `autotools-macos-15-default` | 1:21:01 | 8:14 | 4:18 |
| `cmake-macos-15-brew-clang` | 2:30:04 | 8:01 | 4:56 |
| **total** | **7:48:16** | **33:07** | **15:55** |

Every estimate above held.  Seeding cost ~33 min against a predicted ~32.  The
saving is 17:12 across the matrix against a predicted 18-20 min.  Cache entries
came out at 149 MB and 142 MB on Linux against a predicted ~150 MB, and 82 MB
on both macOS jobs — clang producing smaller debug objects than gcc — for
455 MB total, comfortably inside the 10 GB budget.

The hit rate on the warm run, `autotools-ubuntu-24.04-default`:

```
Restored from cache key "ccache-autotools-ubuntu-24.04-default-2026-08-23T19:18:23.169Z".
Cacheable calls:   557 / 1042 (53.45%)
  Hits:            555 /  557 (99.64%)
  Misses:            2 /  557 ( 0.36%)
Uncacheable calls: 483 / 1042 (46.35%)
Cache size (GB):   0.2 /  0.5 (31.87%)
```

`Build Macaulay2 using Make` went 3:42 to 0:45 and `Build libraries using Make`
1:54 to 0:39.  The 483 uncacheable calls are links, preprocess-only invocations
and configure probes, which ccache cannot cache by nature.  `0.2 / 0.5 GB`
confirms the 500 MB default is right and nothing is being evicted.

Two things the trial could not show.  The pull request path — full build,
restore from the *base* branch, `save: false` — needs a cache on the base branch
and so first appears on the real PR; note the PR's own CI will report "No cache
found", because nothing has written to `Macaulay2/M2`'s `development` yet, and
the first benefit lands on the *second* PR after the merge.  And a case where
sources actually changed, where the hit rate would fall below 99.64%.

What the cache cannot touch is dependency installation, 5.6 min of the 15.9 min
warm matrix — 35%, and now the largest item in the build phase.  That is §10.

### 2.2 `make check` runs the 299 package checks strictly serially

> **Landed as [PR #4663](https://github.com/Macaulay2/M2/pull/4663)**, "A few
> autotools updates for the packages directory": *Remove DEVEL from packages
> directory Makefile*, *Use : instead of :: for check target*, *Only run checks
> for packages in PACKAGES variable*.  The rest of this section is the
> reasoning behind those three commits.

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

**And it does not need `Macaulay2Doc` either** — which matters, because
Macaulay2Doc is a 17.4-minute install and the check job would otherwise inherit
it.  An earlier draft left this open, since `Macaulay2Doc` *was* staged in the
tree used above.  Testing it directly settles it: run the same check invocation
and then ask whether it ever got loaded.

```
$ M2 -q --no-preload --stop --silent \
    -e 'needsPackage("NeuralIdeals",LoadDocumentation=>true,DebuggingMode=>true)' \
    -e 'debug Core; argumentMode = defaultMode' \
    -e 'check(NeuralIdeals,UserMode=>false,Verbose=>false)' \
    -e 'print("Macaulay2Doc loaded? " | toString(PackageDictionary#?"Macaulay2Doc")); exit 0'
...
Macaulay2Doc loaded? false
```

Never loaded, even though it was available — so it cannot matter whether it is
installed.  `LoadDocumentation=>true` reads the package's own documentation from
source; it does not pull in Macaulay2Doc.  This agrees with the Makefile, where
`check-$i` has no prerequisites at all and `make -n check PACKAGES=NeuralIdeals`
lists only that package.

So the 17.4-minute Macaulay2Doc install is an **install-job cost exclusively**:

| job | needs |
|---|---|
| install | binary + Core + Style + FirstPackage + **Macaulay2Doc** |
| check | binary + Core |

**Since revised.**  This section costs the split a duplicated binary build in
each job.  §3.5 removes that: a single `build` producer compiles once and uploads
a 232 MB runtime artifact (§6's figure, `libengine.a` excluded), and the install
and check jobs download it.  So the numbers below are pessimistic — the split
costs one build plus two downloads rather than two builds — and the conclusion is
unchanged.

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

| touched | `build` producer | package jobs (§3.5) | `binary-sweep` (§8) | packages installed | checks run |
|---|---|---|---|---|---|
| `Macaulay2/{c,d,e,system,bin}/**` | yes | autotools, both os | **yes** | all | all |
| `configure.ac`, `M2/m4/**`, `**/Makefile*.in` | yes | autotools, both os | yes | all | all |
| `M2/cmake/**`, `**/CMakeLists.txt` | yes | **cmake**, both os | yes | all | all |
| `M2/libraries/<lib>/**`, the `build-<lib>` block of `build-libraries.cmake`, `M2/submodules/<lib>` | yes, **with `<lib>` forced from source** — see §9 | both harnesses | yes | all | all, plus §9.5's packages for a program |
| `Macaulay2/m2/**` (Core) | yes | autotools, both os | no | all | all |
| `Macaulay2/packages/Macaulay2Doc/**`, `Macaulay2Doc.m2`, `Style`, `FirstPackage`, `SimpleDoc`, or any **preloaded** package | yes | autotools, both os | no | all | all |
| `Macaulay2/tests/**` | yes | autotools, both os | no | all | all |
| `.github/**` | yes | both harnesses | yes | all | all *(you are testing the CI itself)* |
| `Macaulay2/packages/Foo*` only | yes (or reused, §6) | autotools, both os | no | `closure(Foo)` | `closure(Foo)` |
| `*.md`, `README*`, `Macaulay2/man/**`, `Macaulay2/editors/**` only | no | none | no | none | lint only |

Read the "package jobs" column together with §3.7: it names *which harness*, and
each harness contributes an install job and a check job per platform.  So
"autotools, both os" is four jobs — `{install, check} x {Linux, macOS}` — all
consuming the artifact from the single `build` producer rather than compiling
their own.

Three notes on the table.

**Why the sweep column is not simply "yes" everywhere.**  The two hosted
platforms are both newest-release; the binary ships on considerably older ones
and nothing on a PR compiles those, hence §8.  But the sweep answers "does it
compile", so it is gated on changes that can affect compilation — sources, build
plumbing, library recipes — and not on Core or package `.m2` code, which cannot.

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

### 3.3 `PACKAGES=` now works for installs *and* checks

> **Landed as [PR #4663](https://github.com/Macaulay2/M2/pull/4663)** — see
> §2.2.  What follows is the before/after.

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

Bad news, since fixed: the `check` generator looped over
`$(sort $(ALL_PACKAGES) $(DEVEL))`, so `make check PACKAGES="Foo"` still checked
all 299.  PR #4663 splits it the same way `bld` / `all:` is already split — a
`check-$i` target is still generated for every package in `ALL_PACKAGES`, and
only the *prerequisite list* of `check` is restricted to `$(PACKAGES)`.  That
keeps `make check-Polyhedra` working regardless of `PACKAGES`.  Verified against
a regenerated Makefile:

| invocation | checks run |
|---|---|
| `make -n check` | 299 (unchanged) |
| `make -n check PACKAGES=NeuralIdeals` | `NeuralIdeals` |
| `make -n check PACKAGES="Truncations Complexes"` | `Complexes Truncations` |
| `make -n check-Polyhedra PACKAGES=NeuralIdeals` | `Polyhedra` |
| `make -n all PACKAGES=NeuralIdeals` | 4 installs (bootstrap chain + package) |

So the hook §3.1's filter job needs now exists on both the install and the
check side.

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

### 3.5 One job or many?  Many — split by build system

Today `test_build.yml` is a single job with **16 `if:` conditionals across 19
steps**, and `autotools-ubuntu-24.04-default` runs with 8 of its 23 steps
skipped.  A job whose main activity is deciding what not to do wants splitting,
and the numbers say where: **13 of the 16 conditionals key on
`matrix.build-system`**.  autotools and cmake share only the setup steps;
everything after diverges.

```
                            pull_request
                                 |
                                 v
                    +-------------------------------+
                    | changes          ~20 s        |
                    | binary, core, packages[],     |
                    | harness_at, harness_cm        |
                    +---------------+---------------+
                                    |
              +---------------------+--------------------+
              v                                          v
    +--------------------------+              +----------------------+
    | build   matrix: os x2    |              | binary-sweep         |
    | autotools, default cc    |              | if: binary           |
    | §6 reuse, else compile   |              | {autotools,cmake}    |
    | upload 232 MB artifact   |              |   x 8 configs (§8)   |
    +--+-------+-------+-------+              +----------------------+
       |       |       |       |
       v       v       v       v
   at-install at-check cm-install cm-check     matrix: os x2 each
   (gated per §3.7; each downloads the artifact rather than rebuilding)

The check jobs are much the cheaper half: per §2.2a they need only the binary
and Core, so they skip Macaulay2Doc's 17.4-minute install entirely, while the
install jobs must pay it.
```

Only `changes` and `build` are real dependencies; the four package jobs are
independent of each other, since §2.2a established that install and check need
nothing from one another.  `html-check-links` stays in the install jobs because
it is the one step that genuinely needs the installed doc tree.

**Why a dedicated `build` job rather than reusing a sweep cell.**  You cannot
`needs:` a single matrix cell — `needs: binary-sweep` waits for all sixteen,
including the slow macOS ones — and the sweep only runs when `binary` is true,
while the package jobs need a binary on every PR.  A separate producer also
costs almost nothing in wall time: without sharing, four jobs each build
concurrently so wall time is `4 + work`; with a producer it is `4 + 0.5 + work`.
Thirty seconds of wall time buys back three redundant builds, about twelve
minutes of runner time — which matters because concurrency, especially macOS
concurrency, is scarcer than minutes.  It is also the natural home for §6's
cross-run reuse and §6.3's smoke test: the producer pulls the tree if it can and
compiles if it cannot, and the consumers are indifferent.

**The duplication this creates wants a composite action.**  Four job definitions
each repeating dependencies, compiler, PATH, ccache, configure and
`make PACKAGES=` is ~4.5 min duplicated per job (warm) and, worse, four copies
of setup logic to keep in step.  `.github/actions/setup-build` would collapse
each job to two or three steps, and the repository already has
`.github/actions/package-review`, so the pattern is established.

**Operational gotcha:** required status checks in branch protection are matched
*by job name*.  Splitting `build` into differently-named jobs means the existing
required checks stop matching, and PRs either block on a check that will never
report or merge with nothing enforced, until an admin updates the list.  Same
applies whenever a matrix change alters a generated job name.  Worth doing
deliberately with whoever administers the repository.

### 3.6 The compiler axis does not belong in the package jobs

The current matrix conflates two orthogonal questions, and every job answers
both:

1. **Does it compile?** — varies with build-system × os × compiler
2. **Does M2 code work?** — varies with *platform*, and essentially nothing else

`installPackage` and `check` run identical interpreted `.m2` code whether gcc or
clang, cmake or autotools produced the interpreter.  Which is why
`cmake-ubuntu-24.04-default` spends 1h26m installing 299 packages and runs no
tests at all, and `autotools-macos-15-default` 1h21m likewise: they exercise the
same M2 code as the other two jobs, differing only in who built the interpreter.

| | matrix | work |
|---|---|---|
| build | build-system × os × compiler (merges with §8's sweep) | binary only, ~4 min warm |
| M2 code | os only — Linux, macOS | the expensive part, ×2 rather than ×4 |

So the `include:` for `brew-clang`, annotated *"This build tests Clang rather
than AppleClang (keep)"*, moves to the sweep alongside focal's `gcc-7` and
`gcc-8`; likewise AppleClang, which today is tested only incidentally by
`autotools-macos-15-default`.

**But move the compiler-sensitive tests with it.**  A clang-built binary *can*
behave differently running M2 code — libc++ against libstdc++, floating point,
undefined behaviour manifesting differently — and where that surfaces is
`M2 --check 1/2/3` and the engine unit tests, not package documentation
examples.  Those move into the sweep.  Dropping the compiler axis without moving
them would silently drop the coverage, which is exactly the §7 failure mode of
tying test coverage to whichever job happened to have a suite bolted on.

### 3.7 The harnesses still need exercising

The M2 code is harness-independent, but `make install-Foo` and
`cmake --build --target install-packages` are different harnesses that break
independently — so the build-system axis collapses for *coverage* while
surviving as a *trigger*.  The two file sets are disjoint and easy to filter: 96
files for autotools (`**/Makefile*.in`, `configure.ac`, `M2/m4/**`) against 50
for cmake (`**/CMakeLists.txt`, `M2/cmake/**`).

| PR touches | autotools jobs | cmake jobs |
|---|---|---|
| `packages/*.m2` (M2 code) | yes — canonical harness | no |
| `**/Makefile*.in`, `configure.ac`, `m4/**` | yes | no |
| `**/CMakeLists.txt`, `M2/cmake/**` | no | yes |
| both harnesses | yes | yes |
| `c/d/e/system/bin` (binary sources) | build both; M2-code steps via canonical only | build |
| `.github/**` | yes | yes |
| `push: development` | binary only, if seeding at all — see §2.1 | same |
| weekly | yes | yes |

The rule is **run the harness you changed, plus the canonical one for M2-code
changes** — not "always autotools".  Autotools is canonical for a stated reason
rather than taste: `make-dist.yml` uses it to build the release tarballs, and
`make check` is the only place package `TEST` blocks run at all today.

A case that looks like a hole and is not: adding a new `.cpp` under `e/` requires
updating both `Makefile.files.in` and a `CMakeLists.txt`, and a PR touching only
the cmake side would break autotools — but it necessarily also adds the `.cpp`,
which is a binary-source change, so both build anyway.

**The cmake side splits identically — and is better factored.**
`packages/CMakeLists.txt:97-103` generates an aggregate for every prefix:

```cmake
set(_target_prefixes "install;info;doc;check;all;uninstall")
foreach(_target IN LISTS _target_prefixes)
  list(TRANSFORM PACKAGES PREPEND "${_target}-" OUTPUT_VARIABLE ${_dependencies_list})
  add_custom_target(${_target}-packages DEPENDS ${${_dependencies_list}})
endforeach()
```

So `check-packages` already exists beside `install-packages`, and
`M2/CMakeLists.txt:13` even documents the intended invocation
(`--target install-packages check-packages`).  Three consequences: the split
needs no cmake plumbing change at all, only a workflow that invokes it; it fans
out through `DEPENDS` rather than double-colon rules, so Ninja parallelises it
for free and §2.2's problem was autotools-specific; and it keys on a variable
also called `PACKAGES`, so `-DPACKAGES=...` gives filtering symmetric with
`make PACKAGES=`.  Per-package targets declare `DEPENDS M2-core`
(`packages/CMakeLists.txt:215-222`) — the binary, not `install-packages` — so
§2.2a's independence result holds on this side too.

**And the harnesses have already diverged, invisibly.**  PR #4663 makes
`make check` honour `PACKAGES` and run under `-j`; the cmake equivalent registers
`add_test(NAME check-${package}-${_i})` but, per §7.1, reads the test count from
`info-${package}` at *configure* time when that file is only written during the
*build* — so it registers zero tests and `ctest -R "^check-"` matches nothing.
Two harnesses meant to do the same thing, one silently doing nothing, and
nothing in CI comparing them.  That is the argument for the weekly row rather
than treating cmake as an optional extra.

Note the gap is not that cmake lacks a check mechanism — it has the better one —
but that `check-packages` is **never invoked** by any workflow, so it has
presumably never run.  Expect the first invocation to surface real failures,
plausibly in `info-${package}` generation, since `M2_INFO_STRING` writes the
very test-count file the broken ctest path reads.  Worth running once by hand
before wiring it in, so a genuine bug is not mistaken for a bad workflow
change.  cmake package tests should go through `--target check-packages`, not
`ctest -R "^check-"`.

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

**Size — smaller than it looks.**  A full `usr-dist` is 661 MB, but 429 MB of
that is `lib/Macaulay2/lib/libengine.a`, a build intermediate neither consumer
needs.  Drop it and the shippable runtime tree is **232 MB**, perhaps 60-80 MB
compressed — a 20-30 second transfer.  (For scale, the binary itself is 217 MB
of which 95% is debug info: `strip --strip-debug` takes it to 11 MB.  Don't
actually strip it, though — you would lose the stack traces from failing checks
that `libboost-stacktrace` is a dependency for.)

### 6.1 Two layers, not one

The payload has two independently reusable parts, and keeping them separate
matters because Core changes are common:

| layer | reuse when unchanged | worth |
|---|---|---|
| compiled binary + libs | `c/d/e/system/bin`, `configure.ac`, `M2/cmake/**`, `M2/libraries/**`, submodule tree SHA | ~3:25 |
| installed Core + Macaulay2Doc + Style + FirstPackage | all of the above **plus** `m2/**`, `Macaulay2Doc/**`, `Style/**`, `FirstPackage.m2` | ~17:24 |

A package-only PR reuses both.  A Core-only PR reuses the binary layer and
overwrites Core, which is mechanically just a file copy: `m2/Makefile.in:87-89`
installs Core with `@INSTALL_DATA@`, and nothing is serialized into the binary —
the `DUMPEDM2FILES` naming and the `loaddata()` comment at `d/common.d:82` are
vestigial.  `tvalues.m2` is generated from `d/*.d`, but a `d/` change
invalidates the binary layer anyway, so it never matters for this case.

The trap is that `make` will let you get this wrong silently.
`packages/Makefile.in:78` makes `Macaulay2Doc/.installed` depend only on
`Macaulay2Doc.m2` and `find Macaulay2Doc -type f` — *not* on `m2/**` — so after
a Core change make happily keeps the prebuilt documentation.  But
`installPackage` is what runs Macaulay2Doc's examples and checks its
documentation against the loaded Core, so reusing it after a Core change skips
exactly the check that would catch a renamed function or altered signature.
Hence the second row's extra conditions, and hence §3.2 treating Macaulay2Doc as
Core.

### 6.2 Transport: GHCR, not `actions/cache`

The decisive argument is not storage, it is **recoverability**.

`actions/cache` keys are write-once.  If the tree is keyed on a content hash —
as §6 requires, since a near-miss means testing a package against the wrong
interpreter — and the entry under that hash is bad, re-running cannot replace
it.  You get the same log line this repository already produces:

```
Cache hit occurred on the primary key build-cache-Linux-default-cmake-6b9c78..., not saving cache.
```

That is the §2.1 bug in a new costume: the poisoned entry is stuck until someone
deletes it by hand (`gh api -X DELETE .../actions/caches?key=...`) or the LRU
reclaims it.  A "rebuild the tree" `workflow_dispatch` would silently do
nothing.

GHCR tags are mutable, so pushing over the tag works and consumers pick up the
new digest.  Two lesser advantages come along: GHCR storage is free and
unmetered for public repositories, so none of this competes for the 10 GB
Actions cache budget; and on Linux a job can *run in* the image, which needs no
`apt` step at all and therefore absorbs §10.

Note that the current settings would fight a manual refresh.  Both
`SEED_CACHE_ONLY` and the ccache `save:` key off `github.event_name == 'push'`,
so a dispatched run today does a *full* build and saves nothing.  Making
dispatch the refresh mechanism means including `workflow_dispatch` in both,
ideally behind a dispatch input so a cheap reseed and a full rebuild are
separately selectable.

### 6.3 Restore and verify, rather than predict

A prebuilt tree is coupled to the libraries present when it was built, so it
can in principle be invalidated by a library upgrade.  Two ways to handle that.

**The elaborate one, rejected.**  Add the library versions to the key, e.g.
`brew deps --installed macaulay2/tap/M2 | xargs brew list --versions | sha256sum`
on macOS and the equivalent `dpkg -l` digest on Linux, so a bump becomes a cache
miss rather than a broken binary.  It works, and it is only one extra key
segment rather than new infrastructure.  But it is precision aimed at the wrong
property, and the base rate does not justify it:

- On Linux, linkage is by soname — `libgmp.so.10`, `libflint.so.18`,
  `libmpfr.so.6` — and the soname *is* the ABI contract, so an ABI-breaking
  change requires a soname bump.  ubuntu-24.04 is a frozen stable release whose
  `-updates` and `-security` pockets are ABI-preserving by policy, so the only
  moving surface is the PPA.  (One genuine hole: `libsingular-factory-4.3.2.so`
  carries its version in the filename with no soname indirection, so a factory
  upgrade does break a prebuilt tree.)
- On macOS the risk is higher, because Mach-O links by absolute path to
  versioned dylib filenames (`/opt/homebrew/opt/gmp/lib/libgmp.10.dylib`) and a
  replaced formula revision removes the exact file, so dyld fails at startup.
  M2 already knows this: `build-dmg.yml` installs **dylibbundler** to rewrite
  those paths into something relocatable.  But measured drift is low — comparing
  the macOS jobs of 2026-08-22 and 2026-08-23, 53 of 54 poured formulae were
  identical and the one that changed was `ccache` (4.13.6_1 to 4.14), which
  nothing links against.  Hashing the whole install list would have produced a
  spurious miss that day for a change that could not possibly matter.

**The cheap one, recommended.**  Do not try to predict whether the tree will
work; notice when it does not.

```yaml
      - id: prebuilt
        run: |
          if usr-dist/*/bin/M2 -q --no-preload -e 'exit 0' 2>/dev/null
          then echo "ok=true"  >> "$GITHUB_OUTPUT"
          else echo "ok=false" >> "$GITHUB_OUTPUT"; fi
```

with the build steps gated on `steps.prebuilt.outputs.ok != 'true'`.  One
second, and it degrades to an ordinary build instead of a confusing failure.  It
also catches things a version hash never would: a truncated download, a missing
file, a moved prefix.

This matters more than the rarity suggests, because of blast radius.  The tree
is refreshed on pushes to `development`, roughly five a month, so a poisoned
entry would fail *every* pull request run until the next merge — a rare event
becoming days of red CI that looks like a build error rather than a stale cache.
The smoke test makes that self-healing; `workflow_dispatch` (§6.2) is then the
deliberate control rather than the only recovery path, which matters because
nobody will remember an escape hatch used twice a year.

**macOS is reachable after all.**  An earlier draft of this section claimed
otherwise on the grounds that `container:` is Linux-only.  That rules out the
image *route*, not the payload: a tarball plus restore-and-verify works on all
four jobs, and `dylibbundler` exists if a genuinely relocatable macOS tree is
ever wanted.  I would still not bundle — a bundled tree is not the tree an
ordinary build produces, so CI would start validating something subtly
different from what contributors build, which is a coverage change wearing an
optimisation's clothes.

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

**Corrected 2026-08-23**, when splitting the jobs forced a reading of what
`--check n` actually does (`m2/startup.m2.in:513-527`, `m2/testing.m2:136`):

| `--check` | is | needs |
|---|---|---|
| 1 | `runBasicTests()` — the files in `m2/basictests/` | binary only |
| 2 | `check("Core")`, whose tests are loaded from `Macaulay2/tests/normal/` | **a staged distribution**, see below |
| 3 | `checkAllPackages()` — `check(pkg)` over all 299 packages | binary + Core |

So the row below labelled "Core basic tests" was three different things, and the
third of them is the whole package test suite.

| | autotools Linux | cmake Linux | autotools macOS | cmake macOS |
|---|---|---|---|---|
| install 299 packages → **doc examples run** | yes | yes | yes | yes |
| package **`TEST` blocks** — via `make check` | yes | — | — | — |
| package **`TEST` blocks** — via `M2 --check 3` | — | — | — | **yes** |
| `M2 --check 1` (basic tests) | — | — | — | yes |
| `Macaulay2/tests/normal` — via `make check` | yes | — | — | — |
| `Macaulay2/tests/normal` — via `M2 --check 2` | — | — | — | yes |
| engine + memtailor/mathic/mathicgb unit tests | — | — | — | yes |
| ComputationsBook | — | — | — | yes |
| `html-check-links`, `validate-html` | yes | — | — | — |

**Package tests already run on both platforms**, then — which reverses what an
earlier draft of this section said, and it matters because §7.1 below and §9.5
were both written on the assumption that macOS had no package test coverage.
`checkAllPackages` runs the same `check(pkg)` the autotools `check-$i` rules do;
the two harnesses reach it by different routes and neither workflow says so.
What *is* missing on macOS is `make check`'s per-package granularity — a single
`--check 3` is one serial process with no `PACKAGES=` and no `-j`, so §7.1's
recommendation stands, but as "replace the blunt instrument", not "add the
missing coverage".

**`Macaulay2/tests/normal` is not install-independent**, unlike the per-package
checks.  `release-checklist.m2` there loads every distributed package's
documentation: a cheap gdbm lookup once the packages are staged, and a
load-each-package-in-process disaster when they are not.  Splitting install
from check without noticing this produces `out of memory trying to allocate
61625 bytes` rather than a legible failure.  So §2.2a's independence result
covers `check-$i` and *not* the `check` recursion as a whole, which also visits
`Macaulay2/tests`.  Note also that `-o` is not passed to a sub-make
(`check-in-packages` is declared one level down), so there is no way to skip
that subdirectory from a top-level `make check`: recurse into
`Macaulay2/packages` directly instead.

**Two of the four jobs run no test step at all** — the observation that
motivated §3.6.  `cmake-ubuntu` and
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

*`ctest -R "^check-"` is the wrong route anyway — see §3.7, which finds the
`check-packages` target that should be used instead.  For the record, the ctest
path is also broken:*  The test count comes from
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

**Tier 1 — any PR with `binary == true`**, replacing the compiler variation
that currently rides on the package jobs (§3.6).  Two axes, `build-system` x
`config`:

| config | runner | compiler |
|---|---|---|
| focal-gcc7 | `ubuntu-latest` + `container: ubuntu:20.04` | gcc 7.5 |
| focal-gcc8 | same container | gcc 8.4 |
| jammy | `ubuntu-22.04` | gcc 11 |
| arm64 | `ubuntu-24.04-arm` | gcc 13 |
| next | `ubuntu-26.04` | gcc 15 |
| mac-arm-apple | `macos-15` | AppleClang |
| mac-arm-brew | `macos-15` | brew clang |
| mac-intel | `macos-15-intel` | AppleClang |

Sixteen jobs, binary-only, ~4 min warm each — roughly 130 min of runner time,
and only on the ~22% of PRs touching `c/d/e/system/bin`.  Against 468 min for
today's full matrix on *every* PR, that is cheap.  Each job builds the binary and
runs `M2 --check 1..3` plus the engine unit tests: no package installs, no doc
builds.

**The build-system axis is not optional here.**  cmake's `Find*` modules and
autotools' m4 probes diverge most on the platform extremes, so a cmake-only
breakage on focal would be invisible to an autotools focal job.  It also closes
a real hole: the current matrix carries `exclude: cmake + macos-15 + default`,
so **cmake with AppleClang is tested nowhere today**.  The `include:` comment
("This build tests Clang rather than AppleClang (keep)") reads as a deliberate
choice, but its effect is that AppleClang is only ever exercised through
autotools.

**macOS is the constrained resource.**  Six of the sixteen are macOS jobs, and
macOS concurrency is far tighter than Linux, so a binary PR would queue behind
itself and behind other PRs' sweeps.  If that bites, the cut is to drop
`cmake x mac-intel` and `autotools x mac-arm-brew` — keeping every *platform* on
both build systems, but not every compiler on both.

**`ubuntu-24.04` is deliberately absent**, because §3.5's `build` job already
compiles there — but only under whichever harness the filter selected.  So the
cmake jobs need two gates with different bodies: `binary` gives build-only,
`harness_cm` gives build plus package steps.

**Prefer focal over bionic for the old-compiler slot.**  Both give gcc 7 and
flint 2, but bionic ships glibc 2.27 and Node 20 requires 2.28 or newer, so no
JavaScript action can execute in a bionic container: no `actions/checkout`, no
`actions/cache`, and a hand-rolled `git clone` instead.  Focal's glibc 2.31
clears that, making `container: ubuntu:20.04` an ordinary job.  The compilers
are the same vintage — `gcc-7 7.5.0-6ubuntu2` in focal against
`7.5.0-3ubuntu1~18.04` in bionic — and the PPA publishes `libflint-dev 2.6.3`
for both (jammy is already on flint 3.1.3).  Focal's *default* compiler is
gcc-9, so the job installs `gcc-7 g++-7` and sets `CC`/`CXX` explicitly.

Bionic remains the only place to catch a glibc-2.27-specific problem, which is
a real if narrow category; that belongs in tier 2, where a hand-rolled checkout
is tolerable.

**Tier 2 — `push: development` and the weekly build:** bionic (the only
glibc-2.27 target), bullseye, bookworm, AlmaLinux 8, 9 and 10, Fedora,
`macos-26`, and clang on Linux.  A
failure there is "someone should look at this", not a merge blocker.

Sketch, reusing the filter job from §3.1:

```yaml
  binary-sweep:
    name: sweep-${{ matrix.build-system }}-${{ matrix.config.name }}
    needs: changes
    if: needs.changes.outputs.binary == 'true'
    runs-on: ${{ matrix.config.runner }}
    timeout-minutes: 90
    strategy:
      fail-fast: false
      matrix:
        build-system: [ autotools, cmake ]
        config:
          - { name: focal-gcc7,     runner: ubuntu-latest,   image: 'ubuntu:20.04', cc: gcc-7 }
          - { name: focal-gcc8,     runner: ubuntu-latest,   image: 'ubuntu:20.04', cc: gcc-8 }
          - { name: jammy,          runner: ubuntu-22.04 }
          - { name: arm64,          runner: ubuntu-24.04-arm }
          - { name: next,           runner: ubuntu-26.04 }
          - { name: mac-arm-apple,  runner: macos-15 }
          - { name: mac-arm-brew,   runner: macos-15,        cc: brew-clang }
          - { name: mac-intel,      runner: macos-15-intel }
    steps:
      - uses: actions/checkout@v6
      - uses: ./.github/actions/setup-build      # §3.5's composite action
        with:
          build-system: ${{ matrix.build-system }}
          compiler: ${{ matrix.config.cc }}
          # native, or: docker run --rm -v "$PWD":/src -w /src ${{ matrix.config.image }}
      - run: ./M2 -q --no-preload --check 1
      - run: <build and run the engine unit tests>   # moved here per §3.6
```

`matrix.config` is an object rather than parallel lists so that `image` and `cc`
can be absent for the plain hosted runners; note `runs-on` and every reference
in the body must then say `matrix.config.runner`, not `matrix.runner`.  An
explicit `name:` is worth setting, both for legibility and because required
status checks match on it (§3.5).

### 8.4 Four mechanics that will bite

**Don't use `container:` for Ubuntu 18.04** — which is why §8.3 puts focal in
tier 1 and leaves bionic for tier 2.  Bionic is glibc 2.27
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
build-only job.  It is also the *only* one of the three that belongs in a
binary-only job: `--check 3` needs Core installed and takes as long as the
whole package suite, and `--check 2` cannot run there at all — per §7 it loads
`Macaulay2/tests/normal`, whose `release-checklist.m2` exhausts the runner's
memory without a staged distribution.  (An earlier draft of the sketch above
ran all three; it would have failed on every sweep cell.)

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

## 10. Caching the apt and Homebrew installs

Once ccache is warm this becomes the largest remaining item in the build phase,
so it is worth knowing exactly how small it is.  Measured on the warm fork run
(d-torrance/M2 run 32660996677), dependency installation is **5.6 min of a
15.9 min matrix** — 35% of it:

| job | step | time |
|---|---|---|
| `cmake-ubuntu-24.04-default` | Install requirements for Linux | 1:01 |
| `autotools-ubuntu-24.04-default` | Install requirements for Linux | 1:02 |
| `autotools-macos-15-default` | Install requirements for macOS | 1:29 |
| `cmake-macos-15-brew-clang` | Install requirements for macOS | 2:04 |

### 10.1 Caching the downloads is worthless

The obvious move — cache `/var/cache/apt/archives` or `$(brew --cache)` — buys
almost nothing, because downloading is not the cost.  Breaking the Linux minute
down by log timestamps:

```
19:22:02  add-apt-repository (PPA setup)
19:22:05  first .deb download starts
19:22:11  downloads done          <-- ~6s of downloading
19:22:13  first unpack
19:22:17  triggers
19:23:06  step ends               <-- ~49s of dpkg unpack/configure
```

Six seconds of network against forty-nine of `dpkg`.  The runners sit next to
the Azure archive mirror.  Same story for Homebrew bottles.

### 10.2 What would actually work

**A build-dependency container image, Linux only.**  Bake the apt list into an
image, publish it to GHCR, and add `container:` to the two Linux jobs; an image
pull is 10-20s against the full minute.  The publishing pattern already exists
in `docker-testbot.yml`, so this is mostly adaptation — but note that image is
*not* reusable here: it installs Macaulay2 itself from the Debian repository to
test packages against a release, not the dependencies needed to build it.

Three costs to weigh.  The image must be rebuilt whenever the apt list changes,
and a stale image silently tests against the wrong dependency versions, which
is worse than a slow build — it wants a CI check that the list and the image
agree.  It should be based on `ubuntu:24.04`, not the testbot's `debian:trixie`,
so the job keeps testing the platform it claims to.  And running in a container
changes `sudo`, paths, and the `/usr/lib/ccache` PATH entry enough to need real
verification.

**Not `awalsh128/cache-apt-pkgs-action`.**  It caches installed files rather
than packages and does not run maintainer scripts.  Fine for pure `-dev`
header-and-library packages; the list here includes `r-base`, `singular`,
`polymake`, `pipx` and `qepcad`, which have real `postinst` work.

**macOS: little to be done.**  No containers on macOS runners.  Caching the
Cellar means reproducing Homebrew's symlink farm across `opt/`, `bin/` and
`var/homebrew`, which is fragile in the way that produces confusing breakage
months later.  The one cheap thing is the explicit `brew update`, a decent slice
of the 1:29-2:04 and needed only to refresh the tap: `HOMEBREW_NO_AUTO_UPDATE=1`
plus dropping it might save 20-40s, at the risk of stale core formulae missing
bottles.  macOS is also the more variable side, 1:29 against 2:04 for the same
work.

### 10.3 Why this is last

The ceiling is ~2 min per run, Linux only, for a container image that needs
maintaining and a staleness check.  Set against §2.2's parallel `make check`
(35-45 min) or §3's filtering (hours), it is the smallest remaining item —
worth doing after §3 lands, as its own piece of work rather than folded into a
caching change.

## 11. Other things worth changing

**Coverage gaps** get their own section — see §7.  The short version: tie test
coverage to *what changed*, not to which build system happens to run where,
otherwise trimming the matrix quietly drops whole test suites.

**No `timeout-minutes`.**  A hung job runs to the 6-hour default.  `180` on
`build` would be generous and would stop a wedged macOS runner from holding a
concurrency slot all afternoon.

**Action pinning is worth a decision, repo-wide.**  All seven `uses:` lines are
tag-pinned, and git tags are mutable, so a compromised action repository can
retag and every consumer picks it up silently — which is what happened to
`tj-actions/changed-files` in March 2025 (CVE-2025-30066), where the tags were
repointed at a payload that dumped runner secrets into build logs across ~23,000
repositories.  GitHub's hardening guidance recommends full-length commit SHAs
for third-party actions.

Two coherent positions, and picking one action to pin is not either of them:
stay tag-pinned everywhere (today's state, and no worse than most repositories),
or move all seven to SHAs *and* add `.github/dependabot.yml` with
`package-ecosystem: "github-actions"` so bumps arrive as reviewable PRs.  A SHA
pin without Dependabot just rots — the repository has no dependabot config
today.  If only one line gets hardened it should be
`docker/login-action@v3`, which handles the GHCR credentials, not the build
cache.

Note that `hendrikmuhs/ccache-action`'s floating `v1` and `v1.2` tags both point
at a 2025-11-18 commit, five months *older* than the `v1.2.23` release tag, so
"use the major tag to get fixes automatically" does not hold for this one.
Adding `permissions: contents: read` (below) shrinks the blast radius of any of
this far more than pinning does.

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

## 12. Suggested order of work

Roughly in decreasing ratio of payoff to risk.  Two entries are done; §2.3 has
moved to the front because the rest now rests on it.

1. ~~**Fix the cache (§2.1)**~~ — *done, [PR #4664](https://github.com/Macaulay2/M2/pull/4664).*
   Switched to `hendrikmuhs/ccache-action`, ungated from cmake, `usr-host` cache
   dropped as ineffective, plus `push: [ development ]` and `SEED_CACHE_ONLY` so
   the seeding runs cost ~33 min rather than 7 h 48 m.  Measured 17:12 off a warm
   matrix at a 99.64% hit rate.  **The seeding half is temporary** and gets
   reverted at step 2 — see §2.1's "This whole mechanism is temporary".
2. ~~**Parallelise `make check` (§2.2)**~~ — *done, [PR #4663](https://github.com/Macaulay2/M2/pull/4663).*
   The workflow still needs `-j2 --output-sync=target` on the `make check`
   invocation to collect the benefit; the Makefile change alone only makes it
   possible.  Worth 35-45 minutes off every full build.
3. **Fix the weekly build (§2.3)** — point the schedule at `development` as a
   `ref` matrix row while keeping `stable`, extend ccache `save:` to `schedule`
   and `workflow_dispatch` with only the `development` row writing, then revert
   step 1's push-seeding.  Also unpin `package-review@master`.

   This is first among the remaining work because it is load-bearing three
   times over: it becomes the only full sweep, the only cache seeder, and the
   safety net that makes §3's filtering safe.  Today it tests `stable`, two
   months behind.  Two lines of YAML gating everything below.
4. **Split install from check, and add the `build` producer (§2.2a, §3.5)** — no
   filtering needed, and it roughly halves full-build wall time because the
   phases are independent given the binary.  Doing it as one shared producer
   plus consumers, rather than four self-sufficient jobs, saves three redundant
   builds.  Split by build system at the same time: that alone removes 13 of the
   16 conditionals, and a `.github/actions/setup-build` composite action keeps
   the four jobs from duplicating setup.  Coordinate the required-status-check
   names with whoever administers the repository *before* merging.
5. **Add the binary-only platform sweep (§8)** — needs only a single `binary`
   boolean, not the full classification table, so it can land before step 6.
   Start with the two focal jobs, since those are shipped and never compiled on
   a PR, and it is the only build of the flint-2 code paths anywhere.  Move the
   compiler axis here from the package jobs (§3.6), bringing `M2 --check 1..3`
   and the engine unit tests with it — otherwise dropping that axis silently
   drops the coverage.
6. **Add the filter job and the classification table (§3)** with a `full-ci`
   label escape hatch, now that step 3 has provided the safety net.  Land the
   harness rule (§3.7) at the same time, and invoke cmake's `check-packages`
   target — run it by hand once first, since nothing has ever invoked it.
7. **Wire in reverse-dependency expansion (§4)** — the script from step 6, plus
   the preloaded-packages-are-Core rule.
8. **Force from-source library builds when a recipe changes (§9)** — the
   name-mapping table plus `--enable-build-libraries=` / `-DBUILD_LIBRARIES=`,
   asserting on `BUILDLIBLIST` and on cmake's side because cmake ignores unknown
   names silently.  Rides on the step-5 sweep, and is the only thing that would
   ever test `M2/libraries/**`.
9. **Add macOS package checks for the interface packages that have their
   programs (§7)** — nearly free once step 4 lands, since the macOS package jobs
   already exist by then.  Decide separately whether to add `polymake`,
   `phcpack` and `qepcad` to the `brew install` line; those three are the only
   ones genuinely missing.
10. **Upload HTML as an artifact**, then build out the `gh-pages` preview —
    both in §5.
11. **Reuse the built tree across runs (§6)** — the largest change: a GHCR
    image rather than `actions/cache` (mutable tags, so a poisoned tree can
    actually be replaced), restore-and-verify in the step-4 producer, and the
    two-layer split so Core changes still reuse the binary.  Worth ~21 min on
    the ~30% of PRs that touch only packages, because the payload carries
    Macaulay2Doc's 17.4-minute install as well as the build.
12. **Cache the apt and Homebrew installs (§10)** — last, ~2 min ceiling and
    Linux only, for a container image that needs a staleness check.

Out of sequence, because it depends on nothing above and costs nothing:
**label pull requests from their paths (§13)**.  It is a separate workflow with
its own `pull_request_target` trigger, so it neither blocks nor is blocked by
any of the twelve.

Steps 1-4 need no design decisions and no path filtering at all, and between
them would take the current 7 h 48 m of runner time per PR down by well over
half.  Steps 6-7 are what take a one-package PR from 7 h 48 m to roughly 25
minutes, and step 11 takes that under 10.

Steps 5 and 8 are the only ones that *add* work rather than remove it, and are
worth separating in your head from the rest of the document.  Everything else is
about not doing the same thing four times; those two are about doing something
not currently done at all, on a class of change where today's signal is "it
compiled on the newest Ubuntu and the newest macOS".  Their cost is bounded by
being binary-only — sixteen jobs of roughly four minutes warm, on the ~22% of
PRs that touch the binary — which is still less runner time than a single one of
today's jobs spends installing packages.

---

## 13. Labelling pull requests from their paths

Doug's suggestion, and the repository is unusually well set up for it: of the 38
labels defined, nine are topical — they say *which part of Macaulay2* a change
touches — and every one of those nine is decidable from the changed paths alone.

| label | its description in the repo | paths |
|---|---|---|
| `Engine` | `Macaulay2/e` | `M2/Macaulay2/e/**` |
| `Interpreter` | — | `M2/Macaulay2/{c,d,bin}/**` |
| `threads` | `Macaulay2/system` | `M2/Macaulay2/system/**` |
| `Core` | Issues involving the Core scripts | `M2/Macaulay2/m2/**`, `Macaulay2Doc` |
| `Documentation` | — | `Macaulay2Doc`, `M2/Macaulay2/man/**`, `**/*.md` |
| `editors` | — | `M2/Macaulay2/editors/**` |
| `Infrastructure` | GitHub workflows, etc. | `.github/**`, `M2/{BUILD,cmake,m4}/**`, `configure.ac`, `**/Makefile*.in`, `**/CMakeLists.txt` |
| `dependencies` | Pull requests that update a dependency file | `M2/libraries/**`, `M2/submodules/**`, `.gitmodules` |
| `javascript` | Pull requests that update Javascript code | `**/*.js` |

**This is not the §3.2 classification wearing a different hat**, which is worth
saying because the instinct is to have the filter job emit both from one place.
The two are different functions of the same input.  §3.2 asks "how much CI does
this need", and collapses `e/`, `d/`, `c/`, `system/` and `bin/` into a single
bucket — `binary`.  The labels want those distinguished, because a reviewer
looking for engine work does not want interpreter pull requests.  Conversely
§3.2 needs the reverse-dependency closure of a package, which no label wants.
So a standalone path-to-label mapping (`.github/labeler.yml`, consumed by
`actions/labeler`) is the right factoring, and it can land now rather than
waiting on step 6.

**It has to be its own workflow, triggered by `pull_request_target`.**  A
`pull_request` run from a fork gets a read-only `GITHUB_TOKEN`, and
`permissions: pull-requests: write` cannot raise it — so labelling from inside
`test_build.yml` would work for maintainers' branches and silently do nothing
for everyone else, which is most pull requests here.  `pull_request_target`
runs in the base repository's context with a writable token.  That is the
trigger with the well-known footgun, so the mitigation has to be stated rather
than assumed: this workflow checks out nothing and runs nothing from the pull
request.  `actions/labeler` reads its configuration from the base ref and asks
the API which files changed; no code from the branch is executed.

**`sync-labels: false`**, which is the default but worth setting explicitly.
With it on, the action removes a label when the paths that earned it stop
matching — including labels a human applied deliberately, and including all the
process labels (`under discussion`, `waiting for review`, `contributions
welcome`) that share the namespace.  Additive only.

Three judgment calls left for Doug rather than guessed at:

- **`threads` for all of `Macaulay2/system/**`.**  The label's description says
  exactly that, so the mapping above follows it, but the *name* suggests a
  narrower meaning and a change to, say, `system/supervisor.cpp` may not be
  about threading at all.
- **`new package` and `update to existing package(s)`.**  Both are useful and
  `M2/Macaulay2/packages/**` is unambiguous, but telling the two apart needs a
  file's *status*, not its path, and `actions/labeler` matches paths only.
  Applying `update to existing package(s)` to every packages change would
  mislabel every new-package pull request, so both are omitted for now.  A
  small `gh api` step could do it properly by looking for `status: added`.
- **`AI-generated`.**  Not path-derivable, and self-declared by the author
  anyway.
