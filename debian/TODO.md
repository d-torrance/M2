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
* Build paths almost completely fixed after
  [#1337](https://github.com/Macaulay2/M2/pull/1337).
* One of the commits was reverted, causing two examples to still contain
  the build path.  Fix in [#1381](https://github.com/Macaulay2/M2/pull/1381).
* There are almost certainly more reproducibility issues which don't involve
  build paths which I'll find with reprotest.

dh_auto_test
------------
* We aren't doing html validation.
* RunExternalM2 tests sometimes fail, so we're skipping it
  ([#1330](https://github.com/Macaulay2/M2/issues/1330).

autopkgtest
-----------
* interrupt-handling.m2 is failing -- we get a return value of 35072
  ( = 256 * (128 + 9)) instead of 9.  It works find during 'make check',
  though.
* Package testing is working except for
  - `PHCpack`, et. al, until phcpack is packaged

CMake build
-----------
* Work has begun:
  https://salsa.debian.org/science-team/macaulay2/-/tree/debian-cmake

3rd-party applications
----------------------
* Try to get rid of all `use-debian-*.patch`'s by improving Macaulay2's
  support for external programs
  ([#407](https://github.com/Macaulay2/M2/issues/407)).
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
* Reverse dependencies for the future:
  - [m2r](https://cran.r-project.org/web/packages/m2r/index.html)
