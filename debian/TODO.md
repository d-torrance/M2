Getting Macaulay2 into Debian
=============================

Visualize
---------
* Get remaining embedded Javascript libraries in Debian:
  - BootSideMenu.js (s/autoClose/closeOnClick/ needed after switch)
  - nouislider.js
* Repack tarball w/o embedded Javascript/font files
* When https://salsa.debian.org/js-team/node-clipboard/-/merge_requests/1 is
  merged and uploaded to Debian, we should update the path to clipboard.js
  to use /usr/share/javascript instead of /usr/share/nodejs.

mpsolve
-------
* One this is sponsored and goes through NEW, we can remove
  skip-mpsolve-for-now.patch and use `roots` again

TOPCOM
------
* Once I finish packaging it and it's sponsored and goes through NEW,
  we can remove skip-buggy-packages.patch and use `Polyhedra` and
  `ToricInvariants`.

flint
-------------------
* Once flint 2.6 is released and packaged for Debian, remove
  `skip-factor-for-now.patch` since we'll be able to factor large
  integers again.

normaliz
--------
* Restore Normaliz package once #960614 is resolved.

html-check-links
----------------
* Once mpsolve, TOPCOM, and flint issues resolved, we shouldn't
  have any more broken links and we can run this check again.

documentation
-------------
* CodepthThree and Polyhedra info's missing (Polyhedra makes sense becomes of
  TOPCOM, but what's up with CodepthThree?)
* .gitignore inside Macaulay2Doc/test is getting installed -- maybe fix
  this upstream?
* Fix for missing MonomialAlgebras images has been submitted upstream
  (#1131).
* lots of examples reference build path (#1149)
* move various package README's to /usr/share/doc

d/copyright
-----------
* make sure it's up-to-date
* at least once thing is wrong (M2/Macaulay2/kernel/bibasis)

tests
-----
* Work has begun, but we've run into upstream bugs (#1157, #1162)

upstream metadata
-----------------
* github
* citation

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
  - qepcad (not in Debian, RFP #951553)
    + CoincidentRootLoci
  - phcpack (not in Debian, ITP #820848)
    + MonodromySolver
    + NumericalSchubertCalculus
    + PHCpack
  - bergman (not in Debian, license appears to be free)
    + NCAlgebra
  - MOSEK (proprietary)
    + SemidefiniteProgramming
  - SDPA (in Debian)
    + SemidefiniteProgramming
