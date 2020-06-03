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
* After switching to relative paths at Dan's suggestion, we're down to
  33 instances.  Problems are:
  - `HOME` - previously fixed in an earlier patch; we can just set
     this during build, say `/home/m2user` to match the online interface)
	 + `Core#"private dictionary"#"userpath"` isn't using the `HOME`
       environment variable I passed in the patch.  What's going on there?
       (see `path` docs)
  - `currentDirectory` - canned example
  - `PKG#"index.html"` - currently absolute path, which seems like what
     we want in practice.  canned example?
  - `database not present` message - use relative path?
  - `prefixPath`, `realpath`, `toAbsolutePath` - canned example
  - `loadedFiles` - relative path?
* I've encountered a bug not present in 1.15.  Some examples
  exist in html form only.  No corresponding `example-output` file,
  and when viewed in M2 with `help`, we just get the input command without
  `i1`:
  - `loadedFiles`
  - `version`
  - `availableOffline` from `ReflexivePolytopesDB`
  - `runExternalM2` from `RunExternalM2`
  - `gfanInterface#"source file"` from `gfanInterface`

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
