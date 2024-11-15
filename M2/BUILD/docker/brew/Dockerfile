# Time usage: <15min
# Net usage:  ~700MB
# Disk usage: ~1GB

FROM homebrew/brew:latest

USER 1000:0

# Add the tap containing the formulae for Macaulay2 and its dependencies
RUN brew update && brew config && brew tap Macaulay2/tap

RUN brew install bash-completion && echo '\n\
    [[ -r "`brew --prefix`/etc/profile.d/bash_completion.sh" ]] && \n\
	. "`brew --prefix`/etc/profile.d/bash_completion.sh"' >> /home/linuxbrew/.bashrc

# Install dependencies of Macaulay2
RUN brew install  autoconf automake bdw-gc bison cddlib flex googletest libomp \
    make ncurses pkgconf texinfo
RUN brew install --only-dependencies Macaulay2/tap/M2
RUN brew install Macaulay2/tap/memtailor Macaulay2/tap/mathic \
    Macaulay2/tap/mathicgb

WORKDIR /home/linuxbrew
