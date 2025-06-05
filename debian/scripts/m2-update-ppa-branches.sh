#!/bin/sh

set -e

echo -n "fetching salsa ... "
git fetch -q salsa
echo "done"

ORIGINAL_BRANCH=$(git symbolic-ref --short HEAD)

BRANCHES="debian/development"

for BRANCH in $BRANCHES
do
    echo -n "checking if $BRANCH is up to date ... "
    if git merge-base --is-ancestor salsa/$BRANCH $BRANCH
    then
	echo "yes"
    else
	echo "no"
	echo -n "checking if $BRANCH can be fast-forwarded ... "
	if git merge-base --is-ancestor $BRANCH salsa/$BRANCH
	then
	    echo -n "yes\nfast-forwarding ... "
	    git checkout -q $BRANCH
	    git merge -q --ff-only salsa/$BRANCH
	    echo "done"
	else
	    echo "no"
	    echo
	    echo "update $BRANCH and try again" | boxes -d shell
	    echo
	    exit 1
	fi
    fi
done

checkout() {
    CURRENT_BRANCH=$(git symbolic-ref --short HEAD)
    if [ $CURRENT_BRANCH != $1 ]
    then
	echo -n "checking out $1 ... "
	git checkout -q $1
	echo "done"
    fi
}

push() {
    if [ $(git rev-parse $1) != $(git rev-parse salsa/$1) ]
    then
	echo -n "pushing $1 to salsa ... "
	git push -q salsa $1:$1
	echo "done"
    fi
}

push "debian/development"

checkout $ORIGINAL_BRANCH
