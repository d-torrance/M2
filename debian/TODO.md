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

normaliz
--------
* Currently broken ([#960614](https://bugs.debian.org/960614))
* Once this has been fixed, remove `skip-normaliz-for-now.patch` so
  we can use the corresponding packages.

html-check-links
----------------
* Once mpsolve/topcom/flint/normaliz issues resolved, we shouldn't
  have any more broken links and we can run this check again.

documentation
-------------
* lots of examples reference build path
  ([#1149](https://github.com/Macaulay2/M2/issues/1149))
* work has begun in my [reproducible-builds branch](
  https://github.com/d-torrance/M2/tree/reproducible-builds)

autopkgtest
-----------
* Package testing is working except for
  - `Topcom`, et. al, while we wait for topcom
  - `Normaliz`, et. al, while we wait for normaliz
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
