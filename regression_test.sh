#!/bin/bash

# After "cabal install" run this script as a quick regression check.
#   Also try "./regression_test.sh --cuda"

OPTIONS=$*

set -e
# set -x 

echo; echo "================================================================================"
echo "First the main battery of tests:"
echo "================================================================================"
./dist/build/accelerate-examples/accelerate-examples -k --size=100 $OPTIONS

echo; echo "================================================================================"
echo "Next, additional application tests, beginning with mandelbrot:"
echo "================================================================================"

./dist/build/accelerate-mandelbrot/accelerate-mandelbrot --size=64 --limit=10 --benchmark --samples=1 $OPTIONS


echo; echo "================================================================================"
echo "accelerate-fluid:"
echo "================================================================================"

 ./dist/build/accelerate-fluid/accelerate-fluid --benchmark $OPTIONS

echo; echo "================================================================================"
echo "accelerate-crystal:"
echo "================================================================================"

 ./dist/build/accelerate-crystal/accelerate-crystal --benchmark $OPTIONS


# echo; echo "================================================================================"
# echo "accelerate-quickcheck:"
# echo "================================================================================"

# Apparently there isn't a flag to switch which backend to use here?
#  ./dist/build/accelerate-quickcheck/accelerate-quickcheck 
