#!/bin/sh

# generate Macaulay2 orig tarball for creating Debian packages
# combines contents of M2 and M2-emacs git repositories and removes
# any files specified by Files-Excluded in debian/copyright
#
# options:
#   -u, --uscan
#     use uscan to determine newest stable version (default)
#
#   -d, --dev
#     use 'development' branch
#
#   -r, --ref <ref>
#     use branch or tag specificed by <ref>

set -e

if [ "x$1" = "x" -o "x$1" = "x-u" -o "x$1" = "x--uscan" ]
then
    echo -n "finding newest version using uscan ... "
    VERSION=$(uscan --report-status | grep newversion | awk '{print $3}')
    echo $VERSION
    REF="stable-release-$VERSION"
elif [ $1 = "-r" -o $1 = "--ref" ]
then
    REF=$2
elif [ $1 = "-d"  -o $1 = "--dev" ]
then
    REF="development"
else
    echo "error: unknown command line argument"
    exit 1
fi
echo "making tarball for ref '$REF'"

git fetch --tags https://github.com/Macaulay2/M2 $REF 2> /dev/null

if [ -z $VERSION ]
then
    echo -n "determining version number ... "
    VERSION=$(git describe --tags --match=version-* --abbrev=7 FETCH_HEAD | \
        sed -e 's/version-//' -e 's/-\([[:digit:]]\+\)-g/+git\1./')
    echo $VERSION
fi

echo -n "generating M2 tarball ... "
git archive -o ../macaulay2_$VERSION.orig.tar FETCH_HEAD
echo "done"

M2_EMACS_PATH=M2/Macaulay2/editors/emacs
echo -n "finding M2-emacs commit ... "
M2_EMACS_COMMIT=$(git ls-tree FETCH_HEAD -- $M2_EMACS_PATH | awk '{print $3}')
echo $M2_EMACS_COMMIT

echo -n "generating M2-emacs tarball ... "
git submodule update --quiet --init $M2_EMACS_PATH
cd $M2_EMACS_PATH
git archive -o "../../../../../M2-emacs.tar" --prefix $M2_EMACS_PATH/ \
    $M2_EMACS_COMMIT
cd ../../../..
git submodule deinit --quiet $M2_EMACS_PATH
echo "done"

echo -n "merging tarballs ... "
tar --concatenate --file ../macaulay2_$VERSION.orig.tar ../M2-emacs.tar
rm ../M2-emacs.tar
echo "done"

echo -n "removing Files-Excluded from debian/copyright ... "
FILES_EXCLUDED=$(awk '/^Files-Excluded:/ {print $2; flag = 1; next} \
    /^\S/ {flag = 0} flag {print $1}' debian/copyright)
for FILE in $FILES_EXCLUDED
do
    if tar -tf ../macaulay2_$VERSION.orig.tar | grep $FILE > /dev/null
    then
	tar --delete --wildcards --file ../macaulay2_$VERSION.orig.tar $FILE
    else
	MISSING_FILES="$MISSING_FILES $FILE"
    fi
done
mv ../macaulay2_$VERSION.orig.tar ../macaulay2_$VERSION+ds.orig.tar
echo "done"

if [ ! -z "$MISSING_FILES" ]
then
    echo "warning: in Files-Excluded but not tarball:"
    for FILE in $MISSING_FILES
    do
	echo "  $FILE"
    done
fi

echo -n "compressing ... "
xz -fz ../macaulay2_$VERSION+ds.orig.tar
echo "done"

echo "orig tarball: ../macaulay2_$VERSION+ds.orig.tar.xz"
