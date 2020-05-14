Getting Macaulay2 into Debian
=============================

Visualize
---------
* Get remaining embedded Javascript libraries in Debian:
  - BootSideMenu.js (s/autoClose/closeOnClick/ needed after switch)
  - nouislider.js
* Repack tarball w/o embedded Javascript/font files
* Fix bug in 3D visualization w/ modern three.js
  - `THREE.ShaderMaterial: attributes should now be defined in
    THREE.BufferGeometry instead.`
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
* What to do with missing images from MonomialAlgebras?  Issue has been
  sitting upstream for 4 years (#402)
* lots of examples reference build path
* move various package README's to /usr/share/doc

d/copyright
-----------
* make sure it's up-to-date
* at least once thing is wrong (M2/Macaulay2/kernel/bibasis)

tests
-----
* Figure out autopkgtests

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
  - polymake (in Debian, but upstream doesn't assume it's available
    during build like it does gfan, 4ti2, nauty, etc.)
    + StatePolytope
    + Tropical
  - MOSEK (proprietary)
    + SemidefiniteProgramming
  - SDPA (in Debian)
    + SemidefiniteProgramming
