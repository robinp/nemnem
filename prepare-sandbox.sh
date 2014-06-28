#!/bin/bash
cabal sandbox init
for pkg in nemnem-genstatic nemnem-lib
do
  ln -s $(readlink -m cabal.sandbox.config) $pkg/
  ln -s $(readlink -m .cabal.sandbox) $pkg/
done
