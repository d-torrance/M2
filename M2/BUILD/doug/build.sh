#!/bin/sh

git submodule init ../../Macaulay2/editors/emacs
git submodule update
make -C ../..
../../configure --prefix=/usr --disable-shared \
 --with-gtest-source-path=/home/profzoom/src/googletest/googletest/googletest
make
