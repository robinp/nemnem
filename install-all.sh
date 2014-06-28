#!/bin/bash
for pkg in nemnem-lib nemnem-genstatic
do
  pushd $pkg
  cabal install || die "Error"
  popd
done
