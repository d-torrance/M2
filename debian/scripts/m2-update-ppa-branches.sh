#!/bin/sh

set -e

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
