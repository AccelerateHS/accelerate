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
cabal install --only-dependencies --enable-tests --disable-library-profiling  --disable-documentation ./  $PKGS $*
cabal install --disable-library-profiling --disable-documentation ./  $PKGS $*

TOP=`pwd`

function test_dir() {
  dir=$1
  cd $dir
  cabal sandbox init --sandbox=$TOP/.cabal-sandbox/
  cabal test --show-details=always
  cd $TOP
}

test_dir $TOP/accelerate-backend-kit/icc-opencl/
test_dir $TOP/accelerate-multidev/ || echo "acclerate-multidev failed tests!  But that's allowed for now."
