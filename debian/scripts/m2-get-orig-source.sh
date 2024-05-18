#!/bin/sh

# generate Macaulay2 orig tarball for creating Debian packages
# combines contents of M2 and M2-emacs git repositories and removes
# any files specified by Files-Excluded in debian/copyright
#
# also updates git-description.patch and d/changelog with the corresponding
# git description/version number

set -e

TEMP=$(getopt -o 'udr:ngmhR:' \
	      -l 'uscan,dev,ref,no-tarball,git-commit,merge,help,remote' \
	      -n "m2-get-orig-source" \
	      -- "$@")

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

REMOTE=Macaulay2

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
	'-n'|'--no-tarball')
	    NOTARBALL=1
	    shift
	    continue
	    ;;
	'-g'|'--git-commit')
	    DO_GIT_COMMIT=1
	    shift
	    continue
	    ;;
	'-m'|'--merge')
	    MERGE=1
	    shift
	    continue
	    ;;
	'-R'|'--remote')
	    REMOTE=$2
	    shift 2
	    continue
	    ;;
	'-h'|'--help')
	    echo "debian/scripts/m2-get-orig-source.sh:"
	    echo " create orig tarball and update" \
		 "d/changelog (and d/p/git-description.patch) with"
	    echo " version number\n"
	    echo "options:"
	    echo "  -u, --uscan"
	    echo "    use uscan to determine newest stable version (default)"
	    echo "  -d, --dev"
	    echo "    use 'development' branch"
	    echo "  -r, --ref <ref>"
	    echo "    use branch or tag specificed by <ref>"
	    echo "  -n, --no-tarball"
	    echo "    don't generate tarball; only update d/changelog"
	    echo "  -g, --git-commit"
	    echo "    commit version bump to git"
	    echo "  -m, --merge"
	    echo "    merge branch"
	    echo "  -R, --remote"
	    echo "    set remote (default 'Macaulay2')"
	    echo "  -h, --help"
	    echo "    display this help and exit"
	    exit 0
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

if [ -z $NOTARBALL ]
then
    echo "making tarball for ref '$REF'"
else
    echo "getting version number for ref '$REF'"
fi

git fetch https://github.com/$REMOTE/M2 $REF 2> /dev/null

echo -n "determining version number ... "
GIT_DESCRIPTION=$(git describe --tags FETCH_HEAD)

if [ -z $VERSION ]
then
    VERSION=$(echo $GIT_DESCRIPTION | sed 's/^release-//')
fi
echo $VERSION

if [ $MERGE ]
then
    echo -n "merging '$REF' ... "
    if git merge-base --is-ancestor FETCH_HEAD HEAD
    then
	echo "not needed"
    else
	echo
	git merge --no-edit FETCH_HEAD

	echo -n "refreshing patches ... "

	REFRESH_PATCHES=
	quilt pop -a > /dev/null 2>&1 || true

	while true
	do
	    QUILT_PUSH=$(quilt push 2> /dev/null || true)
	    if echo $QUILT_PUSH | grep "FAILED" > /dev/null
	    then
		echo "\ncan't apply patch; refresh manually"
		exit 1
	    elif echo $QUILT_PUSH | grep offset > /dev/null
	    then
		quilt refresh > /dev/null
		REFRESH_PATCHES=1
	    elif [ -z "$QUILT_PUSH" ]
	    then
		break
	    fi
	done

	quilt pop -a > /dev/null 2>&1 || true

	if [ $REFRESH_PATCHES ]
	then
	    echo "done"
	    git add debian/patches
	    git commit -m "Refresh patches for $VERSION"
	else
	    echo "not needed"
	fi

	echo -n "regenerating examples ... "
	NUM_EXAMPLES=$(
	    M2 --stop --srcdir M2 --silent -e \
	       'loadPackage("Debian", FileName => "debian/scripts/Debian.m2");
		print generateExamples();
		exit 0' 2> /dev/null || true)
	if [ -z $NUM_EXAMPLES ]
	then
	    echo "error -- M2 binary out of date?"
	    exit 1
	else
	    echo "$NUM_EXAMPLES change(s)"
	    if [ $NUM_EXAMPLES -gt 0 ]
	    then
		git add debian/examples
		git commit -m "Regenerating examples for $VERSION"
	    fi
	fi

	echo -n "checking for new packages not in d/copyright ... "
	M2 --srcdir M2 --silent -e \
	   'loadPackage("Debian", FileName => "debian/scripts/Debian.m2");
	    pkgs = missingPackages();
	    print(#pkgs);
	    for pkg in pkgs do print("  " | toString pkg);
	    exit 0'
    fi
fi

CURRENT_VERSION=$(dpkg-parsechangelog | awk '/^Version:/ {print $2}')
REPACK_SUFFIX=+ds
DEBIAN_REVISION=-1
FULL_VERSION=$VERSION$REPACK_SUFFIX$DEBIAN_REVISION

if [ $FULL_VERSION = $CURRENT_VERSION ]
then
    echo "debian/changelog already up to date"
    DO_GIT_COMMIT=
else
    echo "using git description ... $GIT_DESCRIPTION"

    echo -n "updating debian/patches/git-description.patch ... "
    quilt push debian/patches/git-description.patch > /dev/null
    sed -i "s/GIT_DESCRIPTION=.*/GIT_DESCRIPTION=$GIT_DESCRIPTION/" \
	M2/configure.ac
    if [ $REF = "development" ]
    then
	sed -i "s/GIT_BRANCH=.*/GIT_BRANCH=development/" \
	    M2/configure.ac
    else
	sed -i "s/GIT_BRANCH=.*/GIT_BRANCH=master/" \
	    M2/configure.ac
    fi
    quilt refresh > /dev/null
    quilt pop -a > /dev/null
    echo "done"

    echo -n "updating debian/changelog ... "
    rm -f debian/changelog.dch # dch raises an error if backup file present
    dch -m -b -v "$FULL_VERSION" "" 2> /dev/null
    echo "done"
fi

if [ $DO_GIT_COMMIT ]
then
    git add debian/changelog debian/patches/git-description.patch
    git commit -m "Bump to version $VERSION$DEBIAN_SUFFIX"
fi

if [ $NOTARBALL ]
then
   exit
fi

TARBALL=macaulay2_$VERSION$REPACK_SUFFIX.orig.tar.xz

if [ -e ../$TARBALL ]
then
    echo "../$TARBALL already exists; exiting"
    exit
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
mv ../macaulay2_$VERSION.orig.tar ../macaulay2_$VERSION$REPACK_SUFFIX.orig.tar
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
xz -fz ../macaulay2_$VERSION$REPACK_SUFFIX.orig.tar
echo "done"

echo "orig tarball: ../$TARBALL"
