#!/bin/sh
#
# Run one phase of run.sh where this row lives.
#
# Rows on old distributions keep a detached container alive for the whole job,
# so that each phase can be its own workflow step and still see the packages
# the previous step installed.  A top-level container: block would be simpler,
# but actions/checkout needs a node that will not run against Ubuntu 18.04's
# glibc.  Rows on a bare runner just run the script.

if docker inspect -f '{{.State.Running}}' gfan-row 2> /dev/null | grep -q true
then
    exec docker exec gfan-row bash .github/gfan-matrix/run.sh "$@"
else
    exec bash .github/gfan-matrix/run.sh "$@"
fi
