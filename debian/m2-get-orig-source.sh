#!/bin/sh

set -e
echo -n "finding newest version ... "
VERSION=$(uscan --report-status | grep newversion | awk '{print $3}')
echo $VERSION
git fetch --quiet https://github.com/Macaulay2/M2 \
    refs/tags/stable-release-$VERSION
echo -n "generating M2 tarball ... "
git archive -o ../macaulay2-$VERSION.tar FETCH_HEAD
echo "done"
M2_EMACS_PATH=M2/Macaulay2/editors/emacs
echo -n "finding M2-emacs commit ... "
M2_EMACS_COMMIT=$(git ls-tree FETCH_HEAD -- $M2_EMACS_PATH | awk '{print $3}')
echo $M2_EMACS_COMMIT
echo -n "generating M2-emacs tarball ... "
git submodule update --quiet --init $M2_EMACS_PATH
cd $M2_EMACS_PATH
git archive -o "../../../../../M2-emacs.tar" --prefix $M2_EMACS_PATH \
    $M2_EMACS_COMMIT
echo "done"
cd ../../../..
git submodule deinit --quiet $M2_EMACS_PATH
echo -n "merging tarballs ... "
tar --concatenate --file ../macaulay2-$VERSION.tar ../M2-emacs.tar
rm ../M2-emacs.tar
echo "done"
FILES_EXCLUDED=$(awk '/^Files-Excluded:/ {print $2; flag = 1; next} \
    /^\S/ {flag = 0} flag {print $1}' debian/copyright)
echo -n "removing embedded Javascript libraries/fonts ... "
tar --delete --wildcards --file ../macaulay2-$VERSION.tar $FILES_EXCLUDED
mv ../macaulay2-$VERSION.tar ../macaulay2-$VERSION+ds.tar
echo "done"
echo -n "compressing ... "
xz -fz ../macaulay2-$VERSION+ds.tar
echo "done"
echo "orig tarball: ../macaulay2-$VERSION+ds.tar.xz"
