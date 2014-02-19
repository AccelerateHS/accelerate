#!/bin/bash

# A script used to launch a benchmark suite, used by our benchmarking servers.
# Assumes all submodules are updated.

set -x 
set -e

cd `dirname $0`

TOP=`pwd`
HSBENCHER_SANDBOX=$TOP/.cabal-sandbox/

which cabal
cabal --version


# Now we link the sandbox so that the benchmarks happen in the same environment.
# cd array-dsl-benchmarks/
# ln -s -f ../.cabal-sandbox 
#ln -s -f ../cabal.sandbox.config

DIRS="$TOP/array-dsl-benchmarks/ $TOP/array-dsl-benchmarks/accelerate/nbody/seq_c $TOP/array-dsl-benchmarks/accelerate/nbody/cilk $TOP/array-dsl-benchmarks/accelerate/nbody/cuda  "

for dir in $DIRS; do 
  cd $dir
  cabal sandbox init --sandbox=$HSBENCHER_SANDBOX
  cd $TOP
done

# (0) Build benchmark runner:
cd $TOP/array-dsl-benchmarks/
make

# (1) First we depend on the jenkins script to install the basic packages
# and run tests:
cd $TOP/
./.jenkins_script.sh

# (2) Then we run the actual benchmarks
cd $TOP/array-dsl-benchmarks/
./run_array_dsl_benchmarks
