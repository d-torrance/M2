FROM opensuse/tumbleweed

RUN zypper -n install 4ti2 autoconf automake bison boost-devel cddlib-devel \
    cmake diffutils e-antic-devel eigen3-devel fflas-ffpack-devel flex      \
    flint-devel gawk gc-devel gcc gcc-c++ gcc-fortran gdbm-devel gfan git   \
    givaro-devel glpk-devel gmp-devel gtest libboost_regex-devel            \
    libboost_stacktrace-devel libfactory-devel libfactory-gftables          \
    libffi-devel libtool libxml2-devel lrslib make mpfi-devel mpfr-devel    \
    nauty ncurses-devel normaliz normaliz-devel ntl-devel openblas-devel    \
    patch pkgconf python3-devel tbb-devel which xz-devel zlib-devel

ENV PATH /home/macaulay/M2/M2/BUILD/build-docker:${PATH}

WORKDIR /home/macaulay
ENTRYPOINT M2
