#!/bin/sh
set -ex
cd "$(dirname "$0")"

mkdir -p .build

# build virtual machine
ghc Vm.hs -hidir .build/ -odir .build/ -o .build/vm-hs

# build stage-0 example hex files into vm executables
# this would be by-hand, but we're on linux, so it's fine
for prog in hello hex; do
  ./hex.sh <"$prog.hex" >".build/$prog.bin0"
done

# build stage-1 hextool from hex
# using the stage-0 hextool
.build/vm-hs .build/hex.bin0 <"hex.hex" >".build/hex.bin1"

# build final-stage example hex files into vm executables
# using the stage-1 hextool
for prog in hello hex; do
  .build/vm-hs .build/hex.bin1 <"$prog.hex" >".build/$prog.bin"
done



