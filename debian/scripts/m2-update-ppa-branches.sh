#!/bin/sh

set -e

echo -n "fetching salsa ... "
git fetch -q salsa
echo "done"

BRANCHES="debian/development ppa/bionic"

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
	    echo -n "no\nupdate $BRANCH and try again"
	    exit 1
	fi
    fi
done

CURRENT_BRANCH=$(git symbolic-ref --short HEAD)
if [ $CURRENT_BRANCH != "debian/development" ]
then
    echo -n "checking out debian/development ... "
    git checkout -q debian/development
    echo "done"
fi

echo -n "pushing debian/development to salsa ... "
git push -q salsa debian/development
echo "done"

echo -n "checking out ppa/bionic ... "
git checkout -q ppa/bionic
echo "done"

echo -n "merging debian/development into ppa/bionic ... "
git merge -q --no-edit debian/development
echo "done"

echo -n "pushing ppa/bionic to salsa ... "
git push -q salsa ppa/bionic
echo "done"

echo -n "checking out debian/development ... "
git checkout -q debian/development
echo "done"
