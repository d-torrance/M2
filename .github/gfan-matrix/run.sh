#!/usr/bin/env bash
#
# One row of the gfan compiler matrix.  The same script runs on a bare GitHub
# runner and inside a docker image, so that what is tested does not depend on
# how the row happens to be hosted.
#
# Inputs, all from the environment:
#   ROW        row identifier, used to name the output directory
#   CC, CXX    compiler to select; empty means "whatever gfan's Makefile picks"
#   PKGS       extra distribution packages this row needs (e.g. "g++-8")
#   CXXEXTRA   extra compiler flags for this row (e.g. "-stdlib=libc++")
#   TARBALLS   directory holding gfan0.8beta.tar.gz and cddlib-0.94n.tar.gz
#   OUTDIR     where to write results and logs
#
# Everything it learns goes into $OUTDIR/probes.tsv and $OUTDIR/row.tsv, which
# .github/gfan-matrix/report.py turns into the two tables.

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

GFAN_TARBALL=gfan0.8beta.tar.gz
GFAN_DIR=gfan0.8beta
CDDLIB_TARBALL=cddlib-0.94n.tar.gz
CDDLIB_DIR=cddlib-0.94n
PATCHFILE=$TOP/M2/libraries/gfan/patch-0.8beta

mkdir -p "$OUTDIR" "$WORK"
: > "$OUTDIR/probes.tsv"

say() { printf '\n=== %s ===\n' "$*"; }

njobs() {
    if command -v nproc > /dev/null 2>&1; then nproc
    elif command -v sysctl > /dev/null 2>&1; then sysctl -n hw.ncpu
    else echo 2
    fi
}

# --------------------------------------------------------------------------
# Dependencies.  gfan needs gmp, cddlib built for gmp rationals (libcddgmp,
# not libcdd), and TBB -- which the patch makes a hard requirement, since it
# calls tbb::parallel_for_each directly instead of going through libstdc++'s
# <execution> backend.  M2 never supplies TBB to gfan either way.
# --------------------------------------------------------------------------

install_deps() {
    say "installing dependencies for $ROW"
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

# apt-get and sudo, whichever this host wants.
apt_get() {
    if [ "$(id -u)" = 0 ]; then apt-get "$@"; else sudo apt-get "$@"; fi
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
    ) > "$OUTDIR/cddlib.log" 2>&1
}

# cddlib's headers live in .../include/cdd on older Debian and .../include/cddlib
# on newer ones and on Homebrew.  gfan is told about the directory out of band,
# which is what cddnoprefix=yes exists for, so find it rather than guess.
find_cddlib() {
    local h
    for h in "$DEPS/include/cddlib/cdd.h" "$DEPS/include/cdd/cdd.h"; do
        [ -f "$h" ] && { CDD_INC=$(dirname "$h"); CDD_LIB=$DEPS/lib; return; }
    done
    local prefix
    for prefix in /usr /usr/local $(command -v brew > /dev/null 2>&1 && brew --prefix); do
        for h in "$prefix"/include/cddlib/cdd.h "$prefix"/include/cdd/cdd.h; do
            [ -f "$h" ] && { CDD_INC=$(dirname "$h"); CDD_LIB=$prefix/lib; return; }
        done
    done
    echo "could not find cdd.h" >&2
    exit 1
}

# --------------------------------------------------------------------------
# Probes.  gfan 0.8beta asks for -std=c++20; a toolchain that cannot give it
# that gets the newest standard it does have, which is what every downstream
# packager tried before giving up and requiring gfan 0.7.  Each probe is also
# run at -std=c++17, the standard the patched tree compiles as, because a few
# of the constructs are C++20-only regardless of how new the compiler is.
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

# probe_source <id> <file> <link?>
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

run_probes() {
    say "probing $CXXCMD"
    pick_std
    echo "best standard: $BEST_STD"
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
    ) > "$log" 2>&1
}

unpack() {
    local dest=$1
    rm -rf "$dest"
    mkdir -p "$dest"
    tar xzf "$TARBALLS/$GFAN_TARBALL" -C "$dest"
}

# One unpacked-and-built attempt against the pristine tree.  Expected to fail;
# what we want is the first thing it says.
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
    echo "$label: $UNPATCHED_RESULT: $UNPATCHED_FIRST"
    tail -20 "$log"
}

# --------------------------------------------------------------------------

main() {
    install_deps
    find_cddlib
    echo "cddlib headers: $CDD_INC   libraries: $CDD_LIB"

    CXXCMD=${CXX:-$(command -v g++ || command -v clang++)}
    CXXVERSION=$($CXXCMD --version 2>&1 | head -1)
    say "$ROW: $CXXVERSION on $(uname -s) $(uname -m)"

    run_probes

    # Pass the row's compiler on the make command line, where it beats the
    # assignments inside gfan's own Makefile.  Rows that leave CXX unset are
    # deliberately letting gfan choose, which on macOS means the gcc-15 pin.
    # Any row-specific flags ride along on CXX, since gfan's link rule uses
    # CCLINKER = $(CXX) and never sees CFLAGS -- which matters for -stdlib=libc++.
    SELECT=()
    [ -n "$CXX" ] && SELECT+=("CXX=$CXX${CXXEXTRA:+ $CXXEXTRA}")
    [ -n "$CC" ] && SELECT+=("CC=$CC")

    try_unpatched stock ${SELECT+"${SELECT[@]}"}
    UNPATCHED_STOCK=$UNPATCHED_RESULT
    UNPATCHED_STOCK_MSG=$UNPATCHED_FIRST

    # On macOS the stock run only ever demonstrates the gcc-15 pin, so force
    # the system compiler as well to see what the sources themselves do.
    UNPATCHED_CLANG=n/a
    UNPATCHED_CLANG_MSG=
    if [ "$(uname -s)" = Darwin ] && [ -z "$CXX" ]; then
        try_unpatched clang CXX=clang++ CC=clang
        UNPATCHED_CLANG=$UNPATCHED_RESULT
        UNPATCHED_CLANG_MSG=$UNPATCHED_FIRST
    fi

    say "patched build"
    unpack "$WORK/patched"
    ( cd "$WORK/patched" && patch --batch -p0 < "$PATCHFILE" ) \
        > "$OUTDIR/patch.log" 2>&1
    PATCH_STATUS=$?
    grep -ci '^patching file' "$OUTDIR/patch.log"
    if [ $PATCH_STATUS -ne 0 ]; then
        PATCHED=patch-failed
    elif make_gfan "$WORK/patched/$GFAN_DIR" "$OUTDIR/patched.log" \
        ${SELECT+"${SELECT[@]}"}; then
        PATCHED=built
    else
        PATCHED=failed
        tail -30 "$OUTDIR/patched.log"
    fi
    echo "patched: $PATCHED"

    # gfan's test harness (src/app_test.cpp) always returns 0 from main, so the
    # exit status says nothing; the counts it prints are the only signal.  No
    # ulimit is applied, which is what VLIMIT = unlimited amounts to.
    say "gfan _test"
    TESTS=n/a
    if [ "$PATCHED" = built ]; then
        ( cd "$WORK/patched/$GFAN_DIR" && ./gfan _test ) \
            > "$OUTDIR/check.log" 2>&1
        local passed failed
        passed=$(sed -n 's/^Number of succesful tests \([0-9]*\)$/\1/p' "$OUTDIR/check.log" | tail -1)
        failed=$(sed -n 's/^Number of failed tests \([0-9]*\)$/\1/p' "$OUTDIR/check.log" | tail -1)
        if [ -n "$passed" ] && [ "$failed" = 0 ]; then
            TESTS="$passed/$((passed + failed))"
        elif [ -n "$passed" ]; then
            TESTS="$passed/$((passed + failed)) FAILED"
        else
            TESTS="no result"
        fi
        tail -5 "$OUTDIR/check.log"
    fi
    echo "tests: $TESTS"

    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$ROW" "$(uname -s)" "$(uname -m)" "$CXXVERSION" "$BEST_STD" \
        "$UNPATCHED_STOCK" "$UNPATCHED_STOCK_MSG" "$UNPATCHED_CLANG$( [ -n "$UNPATCHED_CLANG_MSG" ] && echo ": $UNPATCHED_CLANG_MSG")" \
        "$PATCHED" "$TESTS" > "$OUTDIR/row.tsv"
    cat "$OUTDIR/row.tsv"

    # The row fails only if the patched tree does not build and pass; an
    # unpatched build that fails is the expected outcome, not an error.
    [ "$PATCHED" = built ] || return 1
    case $TESTS in *FAILED | "no result") return 1 ;; esac
    return 0
}

main "$@"
