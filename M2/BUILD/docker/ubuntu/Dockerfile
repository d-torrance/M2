# Time usage: <15min
# Net usage:  ~255MB
# Disk usage: <1GB docker image + 650MB Macaulay2 build

FROM ubuntu:noble

# Programs we require to build
RUN apt-get update
RUN apt-get install -y 4ti2 autoconf automake bison ca-certificates cmake cohomcalg coinor-csdp emacs fflas-ffpack flex g++ gfan gfortran git install-info libboost-dev libboost-regex-dev libboost-stacktrace-dev libcdd-dev libeigen3-dev libffi-dev libflint-dev libfrobby-dev libgc-dev libgdbm-dev libgivaro-dev libglpk-dev libgmp-dev libgtest-dev liblzma-dev libmathic-dev libmathicgb-dev libmemtailor-dev libmpfi-dev libmpfr-dev libmps-dev libnauty-dev libncurses-dev libnormaliz-dev libntl-dev libopenblas-dev libpython3-dev libreadline-dev libsingular-dev libtbb-dev libtool-bin libxml2-dev lrslib make msolve nauty ninja-build normaliz palp patch pkgconf python3 singular-data time topcom wget zlib1g-dev

ENV PATH            /home/macaulay/M2/M2/BUILD/build-docker:${PATH}

WORKDIR /home/macaulay
ENTRYPOINT M2
