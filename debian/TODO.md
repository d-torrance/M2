Getting Macaulay2 into Debian
=============================

Visualize
---------
* Get remaining embedded Javascript libraries in Debian:
  - BootSideMenu ([#960097](https://bugs.debian.org/960097))
  - noUiSlider ([#960618](https://bugs.debian.org/960618))
* Packaging complete, waiting for sponsor
* Once they arrive in Debian, remove them, add to `Files-Excluded` in
  `d/copyright`, remove stanzas from `d/copyright`, remove
  `d/missing-sources`, and add to `Recommends`

mpsolve
-------
* In NEW queue!
* Once it arrives in Debian, we can remove `skip-mpsolve-for-now.patch`
  and use `roots` again.

reproducible builds
-------------------
* Build paths in documentation have been fixed!  Mahrud has an
  outstanding pull request which also deals with building examples, so
  refactor/submit upstream once that goes through.
* My changes involved turning off wrapping of examples, so we should wrap
  them later, when they're displayed (or when generating html/info files).
* There are almost certainly more reproducibility issues which don't involve
  build paths which I'll find with reprotest.

autopkgtest
-----------
* Package testing is working except for
  - `StatePolytope` ([#1173](https://github.com/Macaulay2/M2/issues/1173))
  - `PHCpack`, et. al, until phcpack is packaged
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
