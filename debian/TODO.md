Getting Macaulay2 into Debian
=============================

uscan
-----
* switch to using uscan directly instead of get-orig-source targets
  - this will likely work while I'm following the eigen branch, but
    after 1.16, what do we do?  just point d/watch to release-X.Y,
    where we hardcode X.Y every time?  this defeats the purpose!
  - ideally, we figure out a way to scan for all release-X.Y branches,
    find the most recent, and use that
  - my guess is that this isn't possible using just uscan?
  - maybe I could so something with Salsa Pages or something to serve
    as a fake upstream tarball hosting thing?

Visualize
---------
* Get 3 remaining embedded Javascript libraries in Debian:
  - BootSideMenu.js (s/autoClose/closeOnClick/ needed after switch)
  - clipboard.js
  - nouislider.js
* Use Debian glyphicons font
* Repack tarball w/o embedded Javascript/font files
* Fix bug in 3D visualization w/ modern three.js
  - `THREE.ShaderMaterial: attributes should now be defined in
    THREE.BufferGeometry instead.`

mpsolve
-------
* One this is sponsored and goes through NEW, we can remove
  skip-mpsolve-for-now.patch and use `roots` again

TOPCOM
------
* Once I finish packaging it and it's sponsored and goes through NEW,
  we can remove skip-buggy-packages.patch and use `Polyhedra` and
  `ToricInvariants`.

Eigen memory issues
-------------------
* Once this is resolved, remove eigen-memory-errors.patch and use `factor`
  again.  (Possible also fixed with a new upstream release of flint?)

html-check-links
----------------
* Once mpsolve, TOPCOM, and eigen memory issues resolved, we shouldn't
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
* remove empty example-output directories
* duplicate files in example-output
* install examples

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

