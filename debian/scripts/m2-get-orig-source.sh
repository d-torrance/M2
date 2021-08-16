#!/bin/sh

# generate Macaulay2 orig tarball for creating Debian packages
# combines contents of M2 and M2-emacs git repositories and removes
# any files specified by Files-Excluded in debian/copyright
#
# also updates git-description.patch and d/changelog with the corresponding
# git description/version number
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

TEMP=$(getopt -o 'udr:' -l 'uscan,dev,ref' -n "m2-get-orig-source"  -- "$@")

if [ $? -ne 0 ]; then
        echo 'Terminating...' >&2
        exit 1
fi

eval set -- "$TEMP"
unset TEMP

call_uscan() {
    echo -n "finding newest version using uscan ... "
    VERSION=$(uscan --report-status | grep newversion | \
		  awk '{print $3}')
    echo $VERSION
    REF="release-$(echo $VERSION | sed 's/~rc/-rc/')"
}

while true; do
    case "$1" in
        '-u'|'--uscan')
	    call_uscan
	    shift
	    continue
	    ;;
	'-d'|'--dev')
	    REF="development"
	    shift
	    continue
	    ;;
	'-r'|'--ref')
	    REF=$2
	    shift 2
	    continue
	    ;;
	'--')
	    shift
	    break
	    ;;
	*)
	    echo 'Internal error!' >&2
	    exit 1
	    ;;
    esac
done

if [ "x$REF" = "x" ]
then
    call_uscan
fi

echo "making tarball for ref '$REF'"

git fetch https://github.com/Macaulay2/M2 $REF 2> /dev/null

echo -n "determining version number ... "
GIT_VERSION=$(git show FETCH_HEAD:M2/VERSION)
NEW_COMMITS=$(git rev-list \
		  $(git rev-list -1 FETCH_HEAD M2/VERSION)..FETCH_HEAD --count)
GIT_COMMIT=$(git rev-parse FETCH_HEAD) # cut -c 1-7 (or 9)

if [ -z $VERSION ]
then
    VERSION=$GIT_VERSION+git$NEW_COMMITS.$(echo $GIT_COMMIT | cut -c 1-7)
fi
echo $VERSION

GIT_DESCRIPTION=version-$GIT_VERSION-$NEW_COMMITS-$(echo $GIT_COMMIT | \
							cut -c 1-9)
echo "using git description ... $GIT_DESCRIPTION"

echo -n "updating debian/patches/git-description.patch ... "
quilt push debian/patches/git-description.patch > /dev/null
sed -i "s/^then GIT_DESCRIPTION=.*/then GIT_DESCRIPTION=$GIT_DESCRIPTION/" \
    M2/configure.ac
quilt refresh > /dev/null
quilt pop -a > /dev/null
echo "done"

echo -n "updating debian/changelog ... "
dch -m -b -v $VERSION+ds-1 "" 2> /dev/null
echo "done"

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
FILES_EXCLUDED=$(gawk '/^Files-Excluded:/ {print $2; flag = 1; next} \
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
