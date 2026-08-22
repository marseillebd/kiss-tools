#!/bin/sh
set -ex
cd "$(dirname "$0")"

if [ -d dist ]; then
  mv dist dist.bak
  [ -d dist.bak ] && rm -r dist.bak
fi

mkdir -p dist/src
cp vm/Vm.hs hexasm/hexasm.hex dist/src

./build.sh

mkdir -p dist/bin
cp .build/vm-hs dist/bin/kiss-vm-hs
ln -s kiss-vm-hs dist/bin/kiss-vm
cp .build/hexasm.bin dist/bin/kiss-hexasm
chmod +x dist/bin/kiss-hexasm

mkdir -p dist/doc/kiss-tools # TODO
mkdir -p dist/man/man1 # TODO for program docs
mkdir -p dist/man/man7 # TODO for the vm spec and cheatsheet
