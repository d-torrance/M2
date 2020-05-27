Getting Macaulay2 into Debian
=============================

Visualize
---------
* Get remaining embedded Javascript libraries in Debian:
  - BootSideMenu ([#960097](https://bugs.debian.org/960097))
  - noUiSlider ([#960618](https://bugs.debian.org/960618))

mpsolve
-------
* Not in Debian yet  ([#958919](https://bugs.debian.org/958919))
  - Packaging complete, waiting for sponsor
  - https://salsa.debian.org/science-team/mpsolve
* Once it arrives in Debian, we can remove `skip-mpsolve-for-now.patch`
  and use `roots` again.

TOPCOM
------
* Not in Debian yet  ([#959826](https://bugs.debian.org/959826))
  - Packaging complete, waiting for sponsor
  - https://salsa.debian.org/science-team/topcom
* Once it arrives in Debian, we can remove `skip-topcom-for-now.patch`
  and build all the examples for `Polyhedra` and `ToricInvariants`.

flint
-------------------
* We need [flint 2.6](https://github.com/wbhart/flint2/milestone/2),
  which has not been released yet
* Once it has been released and packaged for Debian, remove
  `skip-factor-for-now.patch` so we can use `factor` in `ZZ` again.

html-check-links
----------------
* Once mpsolve/topcom/flint issues resolved, we shouldn't
  have any more broken links and we can run this check again.

reproducible builds
-------------------
* Build paths in documentation ([#1149](
  https://github.com/Macaulay2/M2/issues/1149))
  - Currently have a patch (`replace-build-path.patch`) which is applied
    only during build to take care of *some* of these problems.
  - Remaining problems:
    + Sometimes `error` returns the filename, and this code lies in the
      compiled d code, so the current patch won't work (plus I don't
      understand the d code yet!)
    + final `--loaded ...` message when loading package with `notify` set
      to `true` (possible related bug: `database not present` warning
      in docs -- is that what we want?)
    + `Core#"private dictionary"#"userpath"` isn't using the `HOME`
      environment variable I passed in the patch.  What's going on there?
      (see `path` docs)
    + `path`
    + `prefixPath`
    * `version#"configure arguments"`
    * `availableOffline` from `ReflexivePolytopesDB` (strangely, the
       build path appears in the html docs, but outputs aren't appearing
       when we run `help` -- what's going on?)
    * `runExternalM2` from `RunExternalM2` (same html-only issue as
      `availableOffline`)
    * `gfanInterface#"source file"` from `gfanInterface`
      (again, html only)
  - Brainstorming a solution:
    + Add a command line argument (`--doc-prefix`?) that tells M2 that
      it needs to swap the current path for the eventual installed path
      when generating docs?

autopkgtest
-----------
* Package testing is working except for
  - `Topcom`, et. al, while we wait for topcom
  - `StatePolytope` ([#1173](https://github.com/Macaulay2/M2/issues/1173))
  - `PHCpack`, et. al, until phcpack is packaged
  - `QuillenSuslin` until flint 2.6 is packaged
  - `SumOfSquares`, strange "protected global variable" error I can't
    reproduce
* Still need to incorporate the upstream test suite in `M2/Macaulay2/tests`

3rd-party applications
----------------------
* Not necessary for building/running Macaulay2, but used by some
  packages so would be nice to have
  - qepcad ([#951553](https://bugs.debian.org/951553))
    + CoincidentRootLoci
  - phcpack ([#820848](https://bugs.debian.org/820848))
    + MonodromySolver
    + NumericalSchubertCalculus
    + PHCpack
  - bergman (no ITP bug)
    + NCAlgebra
