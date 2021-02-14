#!/bin/sh

# Aeson Benchmark Runner
#######################################################################

# Configuration
#######################################################################

HC=ghc
SAVENAME="auto-$(git rev-parse --short HEAD)"
CONFIDENCE=0.99
RUNS=""
PATTERN=""

AESON_BENCH_DATADIR="benchmarks/json-data"
export AESON_BENCH_DATADIR

# usage
#######################################################################

usage() {
cat <<EOF
./bench.sh - Aeson benchmark runner

Usage: ./bench.sh [help|run|compare] [options]

help - print this message
-------------------------

    ./bench.sh help

run - run a benchmark suite
---------------------------

    ./bench.sh run [options]

Options:

    -w HC, --with-compiler HC      Compiler to use
    -n NAME, --name NAME           Name to save benchmark results as (current: $SAVENAME)
    -p PATTERN, --pattern PATTERN  Which benchmarks to run
    --confidence CI                Confidence interval (default: $CONFIDENCE)

build - only build a benchmark suite
-----------------------------------

    ./bench.sh build [options]

Options:

    -w HC, --with-compiler HC   Compiler to use
EOF
}

# "library"
#######################################################################

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
CYAN='\033[0;96m'
RESET='\033[0m' # No Color

putError() {
    echo "${RED}ERROR:${RESET} $*"
}

putInfo() {
    echo "${CYAN}INFO:${RESET} $*"
}

command() {
    echo "${BLUE}RUN:${RESET} $*"
    "$@"
    exitcode=$?
    if [ $exitcode -ne 0 ]; then
        echo "${RED}FAIL:${RESET} $*"
        exit 1
    fi
}

# build
#######################################################################

cmdBuild() {
    # Argument parsing

    while [ $# -gt 0 ]; do
        arg=$1
        case $arg in
            -w|--with-compiler)
                HC=$2
                shift
                shift
                ;;
            *)
                putError "Unknown argument '$1'"
                usage
                exit 1
                ;;
        esac
    done

    # Checking existence of tools
    putInfo "Checking tools"
    command cabal --version

    # Building
    putInfo "Building benchmarks"

    command cabal build --project-file cabal.bench.project -w "$HC"  aeson-benchmark-suite
}

# run
#######################################################################

cmdRun() {
    # Argument parsing

    while [ $# -gt 0 ]; do
        arg=$1
        case $arg in
            -w|--with-compiler)
                HC="$2"
                shift
                shift
                ;;
            -n|--name)
                SAVENAME="$2"
                shift
                shift
                ;;
            -p|--pattern)
                PATTERN="$2"
                shift
                shift
                ;;
            --confidence)
                CONFIDENCE="$2"
                shift
                shift
                ;;
            *)
                putError "Unknown argument '$1'"
                usage
                exit 1
                ;;
        esac
    done

	CSV=".bench-results/${SAVENAME}.csv"
	if [ -f "$CSV" ]; then
		putError "$CSV already exists, aborting..."
		exit 1
	fi

	# Checking if the target file exits

    # Checking existence of tools
    putInfo "Checking tools"
    command cabal --version
    command cabal-plan --version

    # Building
    putInfo "Building benchmarks"
    command cabal build --project-file cabal.bench.project -w "$HC" aeson-benchmark-suite

    # Running
    putInfo "Running benchmarks: $SAVENAME"
    command mkdir -p .bench-results
    command "$(cabal-plan list-bin aeson-benchmark-suite)" --csv "$CSV" --ci "$CONFIDENCE" -m pattern "$PATTERN"
}

# compare
#######################################################################

cmdCompare() {
    # Argument parsing

    while [ $# -gt 0 ]; do
        arg=$1
        case $arg in
            -w|--with-compiler)
                HC=$2
                shift
                shift
                ;;
            -n|--name)
                SAVENAME=$2
                shift
                shift
                ;;
            *)
                RUNS="$RUNS $1"
                shift
                ;;
        esac
    done

	# Comparing th eresults
    putInfo "Comparing runs:$RUNS"

    # Map runs to CVS files
    CSV=""
    for run in $RUNS; do
        CSV="$CSV .bench-results/${run}.csv"
    done

    # shellcheck disable=SC2086
    command "criterion-cmp" $CSV
}

# main
#######################################################################

if [ $# -le 0 ]; then
    usage
    exit 1
fi

case $1 in
    run)
        shift;
        cmdRun "$@"
        ;;
    compare)
        shift;
        cmdCompare "$@"
        ;;
	build)
		shift;
		cmdBuild "$@"
		;;
    help)
        usage
        ;;
    *)
        putError "Unknown command '$1'"
        usage
        exit 1
        ;;
esac
