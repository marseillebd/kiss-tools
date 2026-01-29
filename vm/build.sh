#!/usr/bin/env bash
set -euo pipefail

# Check for program dependencies.
# This is done my just calling the `type` command, and letting `set -e` cause the failure.
# We otherwise allow the output to spew, so that a builder can inspect what commands get found (or not).
type gcc
type haddock

# Check for library dependencies.
# This is done with `ghc-pkg`, so we also need to check that command exists
# (though it's likely given `ghc`).
type ghc-pkg
ghc-pkg list array

# Allow configuration via environment variables.
# - `PROJDIR`: folder where the vm source code resides; default `.`.
#   Note that this build script should be at `$PROJDIR/build.sh`
# - `TMPDIR`: folder where intermediate files will be kept; default `$PROJDIR/.build`
# - `OUTDIR`: folder where build artifacts will be placed; default `$PROJDIR/out`
# - `BINDIR`: folder to put the documentation; default `$OUTDIR/bin`
# - `BINDIR`: folder to put the documentation; default `$OUTDIR/docs`
printf >&2 "PROJDIR=%s\n" "${PROJDIR:=.}"
printf >&2 "TMPDIR=%s\n" "${TMPDIR:=$PROJDIR/.build}"
printf >&2 "OUTDIR=%s\n" "${OUTDIR:=$PROJDIR/out}"
printf >&2 "BINDIR=%s\n" "${BINDIR:=$OUTDIR/bin}"
printf >&2 "DOCDIR=%s\n" "${DOCDIR:=$OUTDIR/docs}"

ghcflags=""
haddock_flags=""

ghc_flags+=" -outputdir $TMPDIR"

# set flags so that haddock need not recompile
# ghc_flags+=" -fwrite-interface -haddock"
# haddock_flags+=" --no-compilation --optghc="\'"-outputdir=$TMPDIR"\'
# generate documentation with hyperlinked source
ghc_flags+=" -fwrite-ide-info"
haddock_flags+=" --hyperlinked-source"

# Build VM binary with ghc.
mkdir -p "$BINDIR"
ghc $ghc_flags -o "$BINDIR/vm" "$PROJDIR/Vm.hs"

# Build documentation with haddock.
mkdir -p "$DOCDIR"
haddock $haddock_flags --html -o "$DOCDIR" "$PROJDIR/Vm.hs"

