#!/bin/bash

set -x 
set -e

cabal install ./ ./accelerate-multidev/ \
   ./accelerate-backend-kit/backend-kit \
   ./accelerate-backend-kit/icc-opencl
# ./accelerate-cuda/
