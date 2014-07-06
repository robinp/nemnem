#!/bin/bash
rm deploy/*html
pushd nemnem-lib
cabal install
popd

pushd nemnem-genstatic
cabal clean
cabal build
popd
