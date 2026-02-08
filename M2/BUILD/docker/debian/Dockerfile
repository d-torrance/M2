# Time usage: <
# Net usage:  ~
# Disk usage: <

ARG RELEASE=latest

FROM debian:$RELEASE

# Programs we require to build
RUN apt-get update && \
    apt-get install -y bison build-essential ca-certificates curl flex       \
    gfortran git gnupg wget cmake autoconf automake ccache debhelper dh-elpa \
    dpkg-dev install-info libtool libtool-bin lintian lsb-release make       \
    ninja-build patch pkgconf python3 r-base shtool time && apt-get clean

# Libraries we require
RUN apt-get install -y libffi-dev liblzma-dev libncurses-dev libopenblas-dev  \
    libpython3-dev libxml2-dev libreadline-dev libgdbm-dev libboost-math-dev  \
    libboost-regex-dev libboost-stacktrace-dev libomp-dev libtbb-dev &&       \
    apt-get clean

# Libraries we can build
RUN apt-get install -y fflas-ffpack libeigen3-dev libflint-dev libfplll-dev \
    libgivaro-dev libglpk-dev libgmp-dev libgtest-dev libmathic-dev         \
    libmathicgb-dev libmemtailor-dev libmpfi-dev libmpfr-dev libmps-dev     \
    libntl-dev libnauty-dev libnormaliz-dev libsingular-dev libfrobby-dev   \
    libgc-dev singular-data zlib1g-dev && apt-get clean

# Programs we can build
RUN apt-get install -y libcdd-dev 4ti2 cohomcalg gfan normaliz coinor-csdp \
    nauty lrslib topcom && apt-get clean
RUN if test $(lsb_release -r | awk '{print $2}') -ge 13; \
    then apt-get install -y msolve; fi

# Add non-root user for building and running Macaulay2
RUN useradd -G sudo -g root -u 1000 -m macaulay && echo "macaulay ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
USER 1000:0

ENV PATH            /home/macaulay/M2/M2/BUILD/build-docker:${PATH}

WORKDIR /home/macaulay
ENTRYPOINT M2

# RUN apt-get install --no-install-recommends -y \
#     libgdbm6 libgomp1 libopenblas-base libreadline7 libtbb2 libtcmalloc-minimal4 libxml2
#COPY Macaulay2-*.deb /
#RUN dpkg -i /Macaulay2-*.deb
