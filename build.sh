#!/bin/sh
set -ex
cd "$(dirname "$0")"

mkdir -p .build

# build virtual machine
ghc-9.12 vm/Vm.hs -hidir .build/ -odir .build/ -o .build/vm-hs

# build the hexasm:
# - first a stage-0 with a shell script
# - then a stage-1 with stage-0
# - then a final hexasm from stage-1 (which I'm not sure is strictly necessary
# this would be by-hand, but we're on linux, so it's fine
hexasm/hexasm.sh <hexasm/hexasm.hex >.build/hexasm.bin0
.build/vm-hs .build/hexasm.bin0 <"hexasm/hexasm.hex" >".build/hexasm.bin1"
.build/vm-hs .build/hexasm.bin1 <"hexasm/hexasm.hex" >".build/hexasm.bin"

# build example hex files into vm executables
# using the bootstrapped hextool
for prog in hexasm/examples/*.hex; do
  .build/vm-hs .build/hexasm.bin <"$prog" >".build/$(basename "$prog").bin"
done



