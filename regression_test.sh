#!/bin/bash

# After "cabal install" run this script as a quick regression check.
#   Also try "./regression_test.sh --cuda"

OPTIONS=$*

set -e
# set -x 

echo; echo "================================================================================"
echo "First the main battery of tests:"
echo "================================================================================"
./dist/build/accelerate-examples/accelerate-examples -k --size=100

echo; echo "================================================================================"
echo "Next, additional application tests, beginning with mandelbrot:"
echo "================================================================================"

./dist/build/accelerate-mandelbrot/accelerate-mandelbrot --size=64 --limit=10 --benchmark --samples=1 $OPTIONS


