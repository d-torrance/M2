Getting Macaulay2 into Debian
=============================

Visualize
---------
* Get remaining embedded Javascript libraries in Debian:
  - BootSideMenu ([#960097](https://bugs.debian.org/960097))
  - noUiSlider ([#960618](https://bugs.debian.org/960618))
* Packaging complete, waiting for sponsor
* Once they arrive in Debian, add to `Recommends`

mpsolve
-------
* In NEW queue!
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
* We need flint 2.6, which has been packaged by Julien Puydt and is waiting
  in the NEW queue.
* Once it has been released and packaged for Debian, remove
  `skip-factor-for-now.patch` so we can use `factor` in `ZZ` again.
* We should also put (>= 2.6.0~) in Build-Depends.

html-check-links
----------------
* Once mpsolve/topcom/flint issues resolved, we shouldn't
  have any more broken links and we can run this check again.

reproducible builds
-------------------
* At one point, we were down to 0 build paths in the documentation, but
  there are two big issues:
  - One of the changes (building examples inside the build directory instead
    of /tmp) was causing build failures, so that commit has been reverted.
  - We're using a few canned examples, which is not ideal.
* New idea: Replace the build paths that appear in the output file right
  after the example is run.
  - Early drafts are working pretty well.
  - We'll probably want to revert most of the relative build path commits,
    since now we're getting a bunch of "../../../usr/share/Macaulay2"'s.
  - Big issue: sometimes the build path has been split over multiple lines.

autopkgtest
-----------
* Package testing is working except for
  - `Topcom`, et. al, while we wait for topcom
  - `StatePolytope` ([#1173](https://github.com/Macaulay2/M2/issues/1173))
  - `PHCpack`, et. al, until phcpack is packaged
  - `QuillenSuslin` until flint 2.6 is packaged
  - `SumOfSquares`, strange "protected global variable" error I can't
    reproduce

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
