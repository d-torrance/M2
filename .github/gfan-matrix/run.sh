#!/usr/bin/env bash
#
# One phase of one row of the gfan compiler matrix.  The same script runs on a
# bare GitHub runner and inside a docker container, so that what is tested does
# not depend on how the row happens to be hosted.
#
# Usage: run.sh <phase>, where phase is one of
#   deps       install gmp, cddlib and TBB
#   probes     compile one small probe per blocker
#   unpatched  build gfan 0.8beta as shipped        (never fails the step)
#   patched    build it with M2's patch-0.8beta applied
#   check      run gfan's own test suite
#   m2check    install a released M2 and run gfanInterface's tests against it
#   summary    write the row of the results table
# Each phase is a separate step in the workflow so that its output is easy to
# find; state is carried between them in $OUTDIR/state.sh.
#
# Inputs, all from the environment:
#   ROW        row identifier, used to name the output directory
#   CC, CXX    compiler to select; empty means "whatever gfan's Makefile picks"
#   PKGS       extra distribution packages this row needs (e.g. "g++-8")
#   CXXEXTRA   extra compiler flags for this row (e.g. "-stdlib=libc++")
#   TARBALLS   directory holding gfan0.8beta.tar.gz and cddlib-0.94n.tar.gz
#   OUTDIR     where to write results and logs

set -uo pipefail

ROW=${ROW:-local}
CC=${CC:-}
CXX=${CXX:-}
PKGS=${PKGS:-}
CXXEXTRA=${CXXEXTRA:-}
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
TOP=$(cd "$HERE/../.." && pwd)
TARBALLS=${TARBALLS:-$TOP/tarballs}
OUTDIR=${OUTDIR:-$TOP/results/$ROW}
WORK=${WORK:-$TOP/work/$ROW}
DEPS=$WORK/deps
GFANBIN=$WORK/gfan-bin
STATE=$OUTDIR/state.sh

GFAN_TARBALL=gfan0.8beta.tar.gz
GFAN_DIR=gfan0.8beta
CDDLIB_TARBALL=cddlib-0.94n.tar.gz
CDDLIB_DIR=cddlib-0.94n
PATCHFILE=$TOP/M2/libraries/gfan/patch-0.8beta

mkdir -p "$OUTDIR" "$WORK"

say() { printf '\n=== %s ===\n' "$*"; }

save() { printf '%s=%q\n' "$1" "${2:-}" >> "$STATE"; }

load() {
    # shellcheck disable=SC1090
    [ -f "$STATE" ] && . "$STATE"
    CDD_INC=${CDD_INC:-}
    CDD_LIB=${CDD_LIB:-}
    CXXCMD=${CXXCMD:-}
    CXXVERSION=${CXXVERSION:-}
    BEST_STD=${BEST_STD:-none}
    UNPATCHED_STOCK=${UNPATCHED_STOCK:-}
    UNPATCHED_STOCK_MSG=${UNPATCHED_STOCK_MSG:-}
    UNPATCHED_CLANG=${UNPATCHED_CLANG:-n/a}
    PATCHED=${PATCHED:-}
    TESTS=${TESTS:-n/a}
    M2VERSION=${M2VERSION:-n/a}
    M2TESTS=${M2TESTS:-n/a}
    # Pass the row's compiler on the make command line, where it beats the
    # assignments inside gfan's own Makefile.  Rows that leave CXX unset are
    # deliberately letting gfan choose, which on macOS means the gcc-15 pin.
    # Any row-specific flags ride along on CXX, since gfan's link rule uses
    # CCLINKER = $(CXX) and never sees CFLAGS -- which matters for -stdlib=libc++.
    SELECT=()
    [ -n "$CXX" ] && SELECT+=("CXX=$CXX${CXXEXTRA:+ $CXXEXTRA}")
    [ -n "$CC" ] && SELECT+=("CC=$CC")
    return 0
}

njobs() {
    if command -v nproc > /dev/null 2>&1; then nproc
    elif command -v sysctl > /dev/null 2>&1; then sysctl -n hw.ncpu
    else echo 2
    fi
}

# --------------------------------------------------------------------------
# deps.  gfan needs gmp, cddlib built for gmp rationals (libcddgmp, not
# libcdd), and TBB -- which the patch makes a hard requirement, since it calls
# tbb::parallel_for_each directly instead of going through libstdc++'s
# <execution> backend.  M2 never supplies TBB to gfan either way.
# --------------------------------------------------------------------------

# Docker rows run as root and have no sudo; rows on a bare runner are the other
# way round.
as_root() {
    if [ "$(id -u)" = 0 ]; then "$@"; else sudo "$@"; fi
}

# sudo does not pass the environment through, so name the frontend here rather
# than exporting it and hoping.
apt_get() { as_root env DEBIAN_FRONTEND=noninteractive apt-get "$@"; }

install_deps() {
    if [ "$(uname -s)" = Darwin ]; then
        brew install gmp cddlib tbb || true
        return
    fi
    . /etc/os-release
    case $ID in
        ubuntu | debian)
            export DEBIAN_FRONTEND=noninteractive
            apt_get update
            apt_get install -y --no-install-recommends \
                ca-certificates make patch g++ libgmp-dev libcdd-dev libtbb-dev \
                $PKGS
            ;;
        rocky | rhel | almalinux | centos)
            dnf -y install dnf-plugins-core
            # TBB lives in PowerTools/CRB on RHEL 8 derivatives.
            dnf config-manager --set-enabled powertools 2> /dev/null ||
                dnf config-manager --set-enabled crb 2> /dev/null || true
            dnf -y install gcc-c++ make patch gmp-devel tbb-devel \
                autoconf automake libtool diffutils findutils file which $PKGS
            # There is no cddlib package for RHEL 8, in EPEL or anywhere else,
            # so build it the way M2/libraries/cddlib/Makefile.in does.
            build_cddlib
            ;;
        *)
            echo "unknown distribution $ID" >&2
            exit 1
            ;;
    esac
}

build_cddlib() {
    say "building cddlib from source"
    mkdir -p "$WORK/cddlib" "$DEPS"
    tar xzf "$TARBALLS/$CDDLIB_TARBALL" -C "$WORK/cddlib" || return 1
    (
        cd "$WORK/cddlib/$CDDLIB_DIR" &&
            autoreconf -vif &&
            ./configure --prefix="$DEPS" --disable-shared &&
            make -C lib-src &&
            make -C lib-src install
    ) 2>&1 | tee "$OUTDIR/cddlib.log" | tail -20
}

# cddlib's headers live in .../include/cdd on older Debian and .../include/cddlib
# on newer ones and on Homebrew.  gfan is told about the directory out of band,
# which is what cddnoprefix=yes exists for, so find it rather than guess.
find_cddlib() {
    local h prefix
    for h in "$DEPS/include/cddlib/cdd.h" "$DEPS/include/cdd/cdd.h"; do
        [ -f "$h" ] && { CDD_INC=$(dirname "$h"); CDD_LIB=$DEPS/lib; return; }
    done
    for prefix in /usr /usr/local $(command -v brew > /dev/null 2>&1 && brew --prefix); do
        for h in "$prefix"/include/cddlib/cdd.h "$prefix"/include/cdd/cdd.h; do
            [ -f "$h" ] && { CDD_INC=$(dirname "$h"); CDD_LIB=$prefix/lib; return; }
        done
    done
    echo "could not find cdd.h" >&2
    exit 1
}

phase_deps() {
    : > "$STATE"
    say "installing dependencies for $ROW"
    install_deps
    find_cddlib
    CXXCMD=${CXX:-$(command -v g++ || command -v clang++)}
    CXXVERSION=$($CXXCMD --version 2>&1 | head -1)
    save CDD_INC "$CDD_INC"
    save CDD_LIB "$CDD_LIB"
    save CXXCMD "$CXXCMD"
    save CXXVERSION "$CXXVERSION"
    say "$ROW: $CXXVERSION on $(uname -s) $(uname -m)"
    echo "cddlib headers: $CDD_INC"
    echo "cddlib libraries: $CDD_LIB"
}

# --------------------------------------------------------------------------
# probes.  gfan 0.8beta asks for -std=c++20; a toolchain that cannot give it
# that gets the newest standard it does have, which is what every downstream
# packager tried before giving up and requiring gfan 0.7.  Each probe is also
# run at -std=c++17, the standard the patched tree compiles as -- not by
# preference but because GCC 7 has no -std=c++20 flag and GCC 9 spells it
# c++2a, so supporting those compilers forces the whole tree down to C++17.
# --------------------------------------------------------------------------

pick_std() {
    local s
    for s in c++20 c++2a c++17; do
        if $CXXCMD "-std=$s" $CXXEXTRA -x c++ -c /dev/null -o /dev/null > /dev/null 2>&1; then
            BEST_STD=$s
            return
        fi
    done
    BEST_STD=none
}

# probe_source <id> <file> <compile|link>
probe_source() {
    local id=$1 file=$2 link=$3 s status detail
    detail=
    local out=()
    for s in "$BEST_STD" c++17; do
        local log=$OUTDIR/probe-$id-$s.log
        if [ "$s" = none ]; then
            status=n/a
        elif [ "$link" = link ]; then
            $CXXCMD "-std=$s" $CXXEXTRA "$file" -o "$WORK/probe.out" > "$log" 2>&1 &&
                status=ok || status=blocked
        else
            $CXXCMD "-std=$s" $CXXEXTRA -c "$file" -o "$WORK/probe.o" > "$log" 2>&1 &&
                status=ok || status=blocked
        fi
        out+=("$status")
        [ "$status" = blocked ] && [ -z "$detail" ] &&
            detail=$(grep -m1 -E 'error|fatal' "$log" | cut -c1-160)
    done
    printf '%s\t%s\t%s\t%s\n' "$id" "${out[0]}" "${out[1]}" "$detail" >> "$OUTDIR/probes.tsv"
}

# probe_flag <id> <flag> -- distinguishes hard rejection from a mere warning,
# since clang only warns about some of the options gfan hardcodes.
probe_flag() {
    local id=$1 flag=$2 status
    local log=$OUTDIR/probe-$id.log
    if ! $CXXCMD "$flag" -x c++ -c /dev/null -o /dev/null > "$log" 2>&1; then
        status=blocked
    elif grep -qiE 'warning|ignor|unsupported|unknown' "$log"; then
        status=warned
    else
        status=ok
    fi
    printf '%s\t%s\t%s\t%s\n' "$id" "$status" "$status" \
        "$(head -1 "$log" | cut -c1-160)" >> "$OUTDIR/probes.tsv"
}

phase_probes() {
    : > "$OUTDIR/probes.tsv"
    say "probing $CXXCMD"
    pick_std
    save BEST_STD "$BEST_STD"
    echo "newest standard this compiler accepts: $BEST_STD"
    probe_source execution        "$HERE/probes/execution.cpp"       compile
    probe_source semaphore        "$HERE/probes/semaphore.cpp"       compile
    probe_source pmr              "$HERE/probes/pmr.cpp"             compile
    probe_source erase_if         "$HERE/probes/erase_if.cpp"        compile
    probe_source filesystem       "$HERE/probes/filesystem.cpp"      link
    probe_source operator_ne      "$HERE/probes/operator_ne.cpp"     compile
    probe_source multimap_lambda  "$HERE/probes/multimap_lambda.cpp" compile
    probe_flag   std_cxx20        -std=c++20
    probe_flag   march_native     -march=native
    probe_flag   no_guess_bp      -fno-guess-branch-probability
    # gfan 0.8beta's Makefile pins CC/CXX to gcc-15/g++-15 on Darwin.
    if command -v g++-15 > /dev/null 2>&1; then
        printf 'gxx15\tok\tok\t%s\n' "$(command -v g++-15)" >> "$OUTDIR/probes.tsv"
    else
        printf 'gxx15\tblocked\tblocked\tno g++-15 on PATH\n' >> "$OUTDIR/probes.tsv"
    fi
    say "probe results"
    cat "$OUTDIR/probes.tsv"
}

# --------------------------------------------------------------------------
# The builds.  Options match what M2 passes in M2/libraries/gfan/Makefile.in
# and M2/cmake/build-libraries.cmake: cddnoprefix=yes plus an explicit include
# directory, and CDD_LINKOPTIONS overridden wholesale to defeat the hardcoded
# -L/usr/local.
#
# Replacing OPTFLAGS wholesale is also what M2 does, and it is why -march=native
# never reaches an M2 build of gfan -- so that blocker shows up in the probes
# rather than in these logs.  -std=c++20 lives in CFLAGS, not OPTFLAGS, and
# -fno-guess-branch-probability lives in its own rule, so both survive.
# gmp is left to gfan's own Makefile, which already knows where Homebrew puts it.
# --------------------------------------------------------------------------

make_gfan() {
    local dir=$1 log=$2
    shift 2
    (
        cd "$dir" &&
            make -j"$(njobs)" \
                cddnoprefix=yes \
                "OPTFLAGS=-w -O2 -ffast-math -DGMPRATIONAL -I$CDD_INC" \
                "CDD_LINKOPTIONS=-L$CDD_LIB -lcddgmp" \
                "$@"
    ) 2>&1 | tee "$log"
}

unpack() {
    local dest=$1
    rm -rf "$dest"
    mkdir -p "$dest"
    tar xzf "$TARBALLS/$GFAN_TARBALL" -C "$dest"
}

# One build attempt against the pristine tree.  Expected to fail on anything
# but a recent GCC; what we want out of it is the first thing it says.
try_unpatched() {
    local label=$1
    shift
    say "unpatched build ($label)"
    unpack "$WORK/unpatched-$label"
    local log=$OUTDIR/unpatched-$label.log
    if make_gfan "$WORK/unpatched-$label/$GFAN_DIR" "$log" "$@"; then
        UNPATCHED_RESULT=built
    else
        UNPATCHED_RESULT=failed
    fi
    UNPATCHED_FIRST=$(grep -m1 -E 'error:|Error |fatal error|No such file|command not found|unrecognized' \
        "$log" | sed 's/^[[:space:]]*//' | cut -c1-200)
    say "unpatched ($label): $UNPATCHED_RESULT"
    [ -n "$UNPATCHED_FIRST" ] && echo "first error: $UNPATCHED_FIRST"
}

phase_unpatched() {
    try_unpatched stock ${SELECT+"${SELECT[@]}"}
    save UNPATCHED_STOCK "$UNPATCHED_RESULT"
    save UNPATCHED_STOCK_MSG "$UNPATCHED_FIRST"

    # On macOS the stock run only ever demonstrates the gcc-15 pin, so force
    # the system compiler as well to see what the sources themselves do.
    if [ "$(uname -s)" = Darwin ] && [ -z "$CXX" ]; then
        try_unpatched clang CXX=clang++ CC=clang
        save UNPATCHED_CLANG "$UNPATCHED_RESULT${UNPATCHED_FIRST:+: $UNPATCHED_FIRST}"
    else
        save UNPATCHED_CLANG "n/a"
    fi
    # A failure here is the expected outcome, not an error in the row.
    return 0
}

phase_patched() {
    say "applying patch-0.8beta"
    unpack "$WORK/patched"
    # -p0 from the directory holding gfan0.8beta/ is what M2's autotools path
    # does; see PATCHCMD in M2/libraries/Makefile.library.in.
    ( cd "$WORK/patched" && patch --batch -p0 < "$PATCHFILE" ) 2>&1 |
        tee "$OUTDIR/patch.log"
    if grep -qiE 'fail|fuzz|rej' "$OUTDIR/patch.log"; then
        say "patch did not apply cleanly"
        save PATCHED patch-failed
        return 1
    fi
    echo "files patched: $(grep -ci '^patching file' "$OUTDIR/patch.log")"

    say "patched build"
    if make_gfan "$WORK/patched/$GFAN_DIR" "$OUTDIR/patched.log" \
        ${SELECT+"${SELECT[@]}"}; then
        save PATCHED built
        say "patched build succeeded"
        return 0
    fi
    save PATCHED failed
    say "patched build FAILED"
    return 1
}

# gfan's test harness (src/app_test.cpp) always returns 0 from main, so the
# exit status says nothing; the counts it prints are the only signal.  No
# ulimit is applied, which is what VLIMIT = unlimited in M2's Makefile.in
# amounts to.
phase_check() {
    if [ "$PATCHED" != built ]; then
        say "skipping tests: no binary"
        save TESTS "n/a"
        return 1
    fi
    say "gfan _test"
    ( cd "$WORK/patched/$GFAN_DIR" && ./gfan _test ) 2>&1 | tee "$OUTDIR/check.log"
    local passed failed
    passed=$(sed -n 's/^Number of succesful tests \([0-9]*\)$/\1/p' "$OUTDIR/check.log" | tail -1)
    failed=$(sed -n 's/^Number of failed tests \([0-9]*\)$/\1/p' "$OUTDIR/check.log" | tail -1)
    if [ -z "$passed" ] || [ -z "$failed" ]; then
        save TESTS "no result"
        say "the test harness printed no counts"
        return 1
    fi
    if [ "$failed" = 0 ]; then
        save TESTS "$passed/$((passed + failed))"
        say "$passed/$((passed + failed)) tests passed"
        return 0
    fi
    save TESTS "$passed/$((passed + failed)) FAILED"
    say "$failed of $((passed + failed)) tests FAILED"
    return 1
}

# --------------------------------------------------------------------------
# m2check.  A gfan that builds and passes its own tests is only useful to M2 if
# M2 can drive it, so install a released Macaulay2 the way a user of this system
# would -- the PPA on Ubuntu, macaulay2.com's repositories on Debian and RHEL,
# the Homebrew tap on macOS -- point its gfanInterface at the binary the patched
# build just produced, and run that package's tests.
#
# Nothing is compiled in this phase.  M2 comes from a binary package, so what is
# under test is the one thing that changed: gfan.  That also means the M2 here is
# the last release rather than this branch, which is what we want -- the patch
# has to work with the M2 people are running, not only with the tree it sits in.
# --------------------------------------------------------------------------

install_m2() {
    if [ "$(uname -s)" = Darwin ]; then
        brew tap Macaulay2/tap || return 1
        # newer Homebrew will not install from a third-party tap without this;
        # older ones have no such command, and neither may ask us anything
        brew trust Macaulay2/tap < /dev/null || true
        brew install Macaulay2/tap/M2
        return
    fi
    . /etc/os-release
    case $ID in
        ubuntu)
            apt_get install -y --no-install-recommends \
                ca-certificates gnupg dirmngr software-properties-common &&
                as_root add-apt-repository -y ppa:macaulay2/macaulay2 &&
                apt_get update &&
                apt_get install -y --no-install-recommends macaulay2
            ;;
        debian)
            # a flat repository per suite, described by a deb822 .sources file
            apt_get install -y --no-install-recommends ca-certificates curl &&
                as_root curl -fsSLo \
                    /usr/share/keyrings/macaulay2-archive-key.asc \
                    https://macaulay2.com/Repositories/Debian/macaulay2-archive-key.asc &&
                as_root curl -fsSLo \
                    /etc/apt/sources.list.d/macaulay2.sources \
                    "https://macaulay2.com/Repositories/Debian/$VERSION_CODENAME/macaulay2.sources" &&
                apt_get update &&
                apt_get install -y --no-install-recommends macaulay2
            ;;
        rocky | rhel | almalinux | centos)
            # $releasever in the .repo file selects the el8 packages here
            as_root curl -fsSLo /etc/yum.repos.d/Macaulay2.repo \
                https://macaulay2.com/Repositories/Scientific/Macaulay2.repo &&
                as_root dnf -y install Macaulay2
            ;;
        *)
            echo "no Macaulay2 packages known for $ID" >&2
            return 1
            ;;
    esac
}

# findProgram looks in programPaths, then in M2's own programs directory, then
# along PATH.  m2check.m2 sets the first; move anything sitting in the second
# aside, so that the tests check runs in a subprocess -- where programPaths does
# not survive, but the prepended PATH does -- cannot reach past our directory to
# the gfan the distribution shipped.
shadow_installed_gfan() {
    local dir
    dir=$(M2 -q --no-readline -e \
        'print("PROGRAMSDIR " | prefixDirectory | currentLayout#"programs"); exit 0' \
        2> /dev/null | sed -n 's/^PROGRAMSDIR //p' | tail -1)
    [ -n "$dir" ] && [ -e "$dir/gfan" ] || return 0
    echo "moving $dir/gfan aside"
    as_root mv "$dir/gfan" "$dir/gfan.shipped"
}

phase_m2check() {
    if [ "$PATCHED" != built ]; then
        say "skipping the M2 tests: no gfan binary"
        save M2VERSION "n/a"
        save M2TESTS "n/a"
        return 1
    fi

    say "installing Macaulay2"
    if ! install_m2 > "$OUTDIR/m2-install.log" 2>&1; then
        tail -30 "$OUTDIR/m2-install.log"
        say "could not install Macaulay2"
        save M2VERSION "install failed"
        save M2TESTS "n/a"
        return 1
    fi
    M2VERSION=$(M2 --version 2>&1 | tail -1)
    save M2VERSION "$M2VERSION"
    echo "Macaulay2 $M2VERSION at $(command -v M2)"

    mkdir -p "$GFANBIN"
    cp "$WORK/patched/$GFAN_DIR/gfan" "$GFANBIN/gfan"
    shadow_installed_gfan

    say "check gfanInterface"
    local status used result
    GFAN_BIN_DIR=$GFANBIN PATH=$GFANBIN:$PATH \
        M2 --script "$HERE/m2check.m2" > "$OUTDIR/m2check.log" 2>&1
    status=$?
    cat "$OUTDIR/m2check.log"

    # If M2 found some other gfan, or would not accept ours, then whatever the
    # tests did says nothing about this row.
    used=$(sed -n 's/^-- GFANPATH //p' "$OUTDIR/m2check.log" | tail -1)
    if [ -z "$used" ]; then
        say "M2 would not accept the gfan in $GFANBIN"
        save M2TESTS "gfan rejected"
        return 1
    fi
    if [ "${used%/}" != "$GFANBIN" ]; then
        say "M2 resolved gfan to $used, not $GFANBIN"
        save M2TESTS "wrong gfan"
        return 1
    fi

    result=$(sed -n 's/^-- RESULT //p' "$OUTDIR/m2check.log" | tail -1)
    if [ -z "$result" ]; then
        say "m2check.m2 printed no result"
        save M2TESTS "no result"
        return 1
    fi
    if [ "$result" = load-failed ]; then
        say "gfanInterface would not load"
        save M2TESTS "load failed"
        return 1
    fi
    if [ "$status" -eq 0 ]; then
        save M2TESTS "$result"
        say "gfanInterface: $result tests passed"
        return 0
    fi
    save M2TESTS "$result FAILED"
    say "gfanInterface: $result -- some tests FAILED"
    return 1
}

phase_summary() {
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$ROW" "$(uname -s)" "$(uname -m)" "$CXXVERSION" "$BEST_STD" \
        "$UNPATCHED_STOCK" "$UNPATCHED_STOCK_MSG" "$UNPATCHED_CLANG" \
        "$PATCHED" "$TESTS" "$M2VERSION" "$M2TESTS" > "$OUTDIR/row.tsv"
    say "row"
    cat "$OUTDIR/row.tsv"
}

case ${1:-all} in
    deps) phase_deps ;;
    probes) load && phase_probes ;;
    unpatched) load && phase_unpatched ;;
    patched) load && phase_patched ;;
    check) load && phase_check ;;
    m2check) load && phase_m2check ;;
    summary) load && phase_summary ;;
    all)
        phase_deps &&
            load && phase_probes &&
            load && phase_unpatched
        load && phase_patched
        load && phase_check
        load && phase_m2check
        load && phase_summary
        ;;
    *)
        echo "usage: $0 {deps|probes|unpatched|patched|check|m2check|summary|all}" >&2
        exit 2
        ;;
esac
