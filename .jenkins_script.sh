#!/bin/bash

set -x 
set -e

cabal --version
cabal sandbox init

PKGS=" ./accelerate-backend-kit/backend-kit \
       ./accelerate-backend-kit/icc-opencl "
# ./accelerate-multidev/
# ./accelerate-cuda/

# First, let's make sure everything installs:
cabal install --disable-documentation ./  $PKGS $*

TOP=`pwd`
for dir in accelerate-backend-kit/icc-opencl/ ; 
do
  cd $dir
  cabal sandbox init --sandbox=$TOP/.cabal-sandbox/
  cabal test --show-details=always
done
