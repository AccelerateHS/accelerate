#!/bin/bash

set -x 
set -e

cabal --version
cabal sandbox init

# First, let's make sure everything installs:
cabal install  ./ ./accelerate-multidev/ \
   ./accelerate-backend-kit/backend-kit \
   ./accelerate-backend-kit/icc-opencl $*
# ./accelerate-cuda/

TOP=`pwd`
for dir in accelerate-backend-kit/icc-opencl/ ; 
do
  cd $dir
  cabal sandbox init --sandobx=$TOP/.cabal-sandbox/
  cabal test
done
