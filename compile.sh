#!/bin/bash
rm deploy/*html
pushd src
ghc Main.hs || rm src/Main
popd
./src/Main
