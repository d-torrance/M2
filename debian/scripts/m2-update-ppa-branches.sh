#!/bin/sh

set -e

echo -n "fetching salsa ... "
git fetch -q salsa
echo "done"

echo -n "checking if local branches are up to date ... "
git merge-base --is-ancestor salsa/debian/development debian/development
DEBIAN_DEVELOPMENT_UPDATED=$?
git merge-base --is-ancestor salsa/ppa/bionic ppa/bionic
PPA_BIONIC_UPDATED=$?

if [ $DEBIAN_DEVELOPMENT_UPDATED -eq 0 -a $PPA_BIONIC_UPDATED -eq 0 ]
then
    echo "yes"
else
    echo "no, update them and try again"
    exit 1
fi

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
