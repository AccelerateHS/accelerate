#!/bin/bash

set -x 
set -e

cabal --version
cabal sandbox init

# First, let's make sure everything installs:
cabal install --enable-tests \ 
   ./ ./accelerate-multidev/ \
   ./accelerate-backend-kit/backend-kit \
   ./accelerate-backend-kit/icc-opencl $*
# ./accelerate-cuda/

