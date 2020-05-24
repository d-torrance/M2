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
* building examples for `TateOnProducts` causes memory errors

d/copyright
-----------
* make sure it's up-to-date
* at least one thing is wrong (M2/Macaulay2/kernel/bibasis)

autopkgtest
-----------
* Work has begun, but we're currently skipping some tests:
  - `Topcom`, et. al, while we wait for topcom
  - `Normaliz`, et. al, while we wait for normaliz
  - `StatePolytope` ([#1173](https://github.com/Macaulay2/M2/issues/1173))
  - `PHCpack`, et. al, until phcpack is packaged
  - `QuillenSuslin` until flint 2.6 is packaged
  - `SumOfSquares`, strange "protected global variable" error I can't
    reproduce
* Still need to incorporate the upstream test suites in `M2/Macaulay2/tests`
  and `M2/Macaulay2/d/basictests`

3rd-party applications
----------------------
* Do these work properly with the Debian package?
  - Maple
    + AdjointIdeal
    + ConvexInterface
    + MapleInterface
    + Parametrization
  - Bertini (can't package, non-free license)
    + Bertini
  - qepcad (not in Debian, [#951553](https://bugs.debian.org/951553))
    + CoincidentRootLoci
  - phcpack (not in Debian, [#820848](https://bugs.debian.org/820848))
    + MonodromySolver
    + NumericalSchubertCalculus
    + PHCpack
  - bergman (not in Debian, license appears to be free)
    + NCAlgebra
  - MOSEK (proprietary)
    + SemidefiniteProgramming
