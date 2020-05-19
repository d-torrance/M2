Getting Macaulay2 into Debian
=============================

Visualize
---------
* Get remaining embedded Javascript libraries in Debian:
  - BootSideMenu ([#960097](https://bugs.debian.org/960097))
  - noUiSlider ([#960618](https://bugs.debian.org/960618))
* Update versions of libraries upstream:
  - BootSideMenu
	+ 0.0.1 &rightarrow; 1.0.0
	+ `autoClose` &rightarrow; `closeOnClick`
  - three.js
	+ r55 &rightarrow; r111 (in Debian) or r116 (current upstream)
	+ drop `ShaderMaterial` for `LineBasicMaterial`/`MeshBasicMaterial`
  - clipboard.js
    + 1.5.10 &rightarrow; 2.0.6
    + `Clipboard` &rightarrow; `ClipboardJS`
  - Pull request: [#1172](https://github.com/Macaulay2/M2/pull/1172)

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
  and build all the examplse for `Polyhedra` and `ToricInvariants`.

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

d/copyright
-----------
* make sure it's up-to-date
* at least one thing is wrong (M2/Macaulay2/kernel/bibasis)

tests
-----
* Work has begun, but we're currently skipping some tests:
  - `Macaulay2Doc` ([#1157](https://github.com/Macaulay2/M2/issues/1157))
  - `Topcom`, et. al, while we wait for topcom
  - `Normaliz`, et. al, while we wait for normaliz
  - `Depth`, occassionally gives memory errors
  - `StatePolytope` ([#1173](https://github.com/Macaulay2/M2/issues/1173))

uscan
-----
* Modify script a bit and move to pages.debian.net
* One tarball is sufficient; don't need them all.
* Current versioning scheme could pose problems, e.g., let's say upstream
  releases a patch version 1.16.1, which would sort before, say,
  1.16.5.gdeadbeef.
  New proposal: 1.16+git5.deadbeef.  The following should work:
  ```
  git describe --tags | \
  sed 's/version-\([0-9.]\+\)-\([0-9]\+\)-g\([0-9a-f]\+\)/\1+git\2.\3/'
  ```

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
  - SDPA (in Debian)
    + SemidefiniteProgramming
