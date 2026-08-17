#!/bin/sh
set -ex
cd "$(dirname "$0")"

if [ -d dist ]; then
  mv dist dst.bak
  [ -d dist.bak ] && rm -r dist.bak
fi

mkdir -p dist/src
cp Vm.hs hex.hex dist/src

./build.sh

mkdir -p dist/bin
cp .build/vm-hs dist/bin/kiss-vm-hs
ln -s kiss-vm-hs dist/bin/kiss-vm
cp .build/hex.bin dist/bin/kiss-hex
chmod +x dist/bin/kiss-hex

mkdir -p dist/doc/kiss-tools # TODO
mkdir -p dist/man/man1 # TODO for program docs
mkdir -p dist/man/man7 # TODO for the vm spec and cheatsheet
