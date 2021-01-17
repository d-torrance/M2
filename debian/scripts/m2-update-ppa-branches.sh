#!/bin/sh

CURRENT_BRANCH=$(git symbolic-ref --short HEAD)
if [ $CURRENT_BRANCH != "debian/development" ]
then
    echo -n "checking out debian/development ... "
    git checkout debian/development 2> /dev/null
    echo "done"
fi

echo -n "pushing debian/development to salsa ... "
git push salsa debian/development 2> /dev/null
echo "done"

echo -n "checking out ppa/bionic ... "
git checkout ppa/bionic 2> /dev/null
echo "done"

echo -n "merging debian/development into ppa/bionic ... "
git merge --no-edit debian/development > /dev/null
echo "done"

echo -n "pushing ppa/bionic to salsa ... "
git push salsa ppa/bionic 2> /dev/null
echo "done"

echo -n "checking out debian/development ... "
git checkout debian/development > /dev/null 2>&1
echo "done"
