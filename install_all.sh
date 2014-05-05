#!/bin/bash

# A simple script to install all the packages under this directory.
# Make sure git submodules are updated!

# 
PKGS="./ ./accelerate-backend-kit/backend-kit ./accelerate-backend-kit/icc-opencl ./accelerate-multidev ./accelerate-cuda "

ARGS="--disable-library-profiling --disable-documentation --constraint=cuda<0.6"

cabal install $ARGS $PKGS $*

