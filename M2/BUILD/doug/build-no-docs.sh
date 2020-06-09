#!/bin/sh

make -C ../..
../../configure --prefix=/usr --disable-shared --disable-build-docs \
 --with-gtest-source-path=/home/profzoom/src/googletest/googletest/googletest
make
