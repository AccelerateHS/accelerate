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

DIRS="$TOP/array-dsl-benchmarks/ \
   $TOP/array-dsl-benchmarks/accelerate/nbody/seq_c \
   $TOP/array-dsl-benchmarks/accelerate/nbody/cilk  \
   $TOP/array-dsl-benchmarks/accelerate/nbody/cuda  \
   $TOP/array-dsl-benchmarks/accelerate/nbody/fission1  \
   $TOP/array-dsl-benchmarks/accelerate/nbody/spmd1  \
   $TOP/array-dsl-benchmarks/accelerate/nbody/spmd2  \
  "

# Disabling:  This first scale_flops benchmark didn't do the trick:
   # $TOP/array-dsl-benchmarks/accelerate/scale_flops/seq_c \
   # $TOP/array-dsl-benchmarks/accelerate/scale_flops/cilk \
   # $TOP/array-dsl-benchmarks/accelerate/scale_flops/cuda \


# $TOP/array-dsl-benchmarks/accelerate/scale_flops/cilk $TOP/array-dsl-benchmarks/accelerate/scale_flops/cuda

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
ACCELERATE_INSTALL_ONLY=1 ./.jenkins_script.sh

# (2) Then we run the actual benchmarks
# ----------------------------------------

# Parfunc account, registered app in api console:
CID=905767673358.apps.googleusercontent.com
SEC=2a2H57dBggubW1_rqglC7jtK

# Accelerate/multidev table docID:
# TABID=1E17ssTkVafPYjzPjO9m1uOqlq8Cz2T9D48PQo7s
# https://www.google.com/fusiontables/DataSource?docid=1E17ssTkVafPYjzPjO9m1uOqlq8Cz2T9D48PQo7s
TABLENAME=Array-DSL-bench-results

TRIALS=7

cd $TOP/array-dsl-benchmarks/

# Enable upload of benchmarking data to a Google Fusion Table:
./run_array_dsl_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=$TABLENAME --clientid=$CID --clientsecret=$SEC $*
