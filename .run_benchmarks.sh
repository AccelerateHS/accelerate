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
   $TOP/array-dsl-benchmarks/accelerate/nbody_temp/cpugpu \
   $TOP/array-dsl-benchmarks/accelerate/blackscholes/seq_c \
   $TOP/array-dsl-benchmarks/accelerate/blackscholes/cilk  \
   $TOP/array-dsl-benchmarks/accelerate/blackscholes/cuda  \
   $TOP/array-dsl-benchmarks/accelerate/blackscholes/fission1  \
   $TOP/array-dsl-benchmarks/accelerate/blackscholes/spmd1  \
   $TOP/array-dsl-benchmarks/accelerate/blackscholes/spmd2  \
   $TOP/array-dsl-benchmarks/accelerate/blackscholes_temp/cpugpu \
   $TOP/array-dsl-benchmarks/accelerate/scale_flops/seq_c \
   $TOP/array-dsl-benchmarks/accelerate/scale_flops/cilk \
   $TOP/array-dsl-benchmarks/accelerate/scale_flops/cuda \
   $TOP/array-dsl-benchmarks/accelerate/scale_flops2/seq_c \
   $TOP/array-dsl-benchmarks/accelerate/scale_flops2/cilk \
   $TOP/array-dsl-benchmarks/accelerate/scale_flops2/cuda \
  "

   # $TOP/array-dsl-benchmarks/accelerate/smvm/seq_c \
   # $TOP/array-dsl-benchmarks/accelerate/smvm/cilk  \
   # $TOP/array-dsl-benchmarks/accelerate/smvm/cuda  \
   # $TOP/array-dsl-benchmarks/accelerate/smvm/fission1  \
   # $TOP/array-dsl-benchmarks/accelerate/smvm/spmd1  \
   # $TOP/array-dsl-benchmarks/accelerate/smvm/spmd2  \


#   $TOP/array-dsl-benchmarks/accelerate/nbody_temp/2gpu \

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

TRIALS=3

cd $TOP/array-dsl-benchmarks/

# Enable upload of benchmarking data to a Google Fusion Table:
./run_array_dsl_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=$TABLENAME --clientid=$CID --clientsecret=$SEC $*
