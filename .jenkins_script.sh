#!/bin/bash
# -------------------------------------------------------------
# Regression testing script.
# -------------------------------------------------------------
#
# This is used for continuous integration.  It may contain bits
# that are specific to our Jenkins installation.  However, we 
# try to keep it so you can run this by hand.  For example, with
# a command like this:
#
#    ./.jenkins_script.sh
#
# Or perhaps like this:
#
#    CABAL=cabal NOSANDBOX=1 JENKINS_GHC=7.8.2 ./.jenkins_script.sh
# 
# ------------------------------------------------------------- 

set -x 
set -e

PKGS=" ./ ./accelerate-backend-kit/backend-kit \
          ./accelerate-backend-kit/icc-opencl \
          "
if [ "$USECUDA" == "1" ]; then 
  PKGS=" $PKGS ./accelerate-backend-kit/simple-cuda ./accelerate-cuda/ "
fi
PKGNAMES=" accelerate accelerate-cuda accelerate-backend-kit accelerate-icc-opencl simple-cuda "

# Temporarily removing these. 
# multidev is a bit outdated (Iterate) 
#       ./accelerate-multidev/ " 
#       ./accelerate-cuda/ "
# 

#------------------------------------------------------------
# Choose commands 

if [ "$CABAL" == "" ]; then 
  CABAL=cabal
fi 
if [ "$JENKINS_GHC" == "" ]; then 
  GHC=ghc
  GHCPKG=ghc-pkg
else
  ENVSCRIPT=$HOME/rn_jenkins_scripts/acquire_ghc.sh
  # This is specific to our testing setup at IU:
  if [ -f "$ENVSCRIPT" ]; then 
    source "$ENVSCRIPT"
  fi
  GHC=ghc-$JENKINS_GHC
  GHCPKG=ghc-pkg-$JENKINS_GHC
fi

CBLARGS="--disable-library-profiling  --disable-documentation --constraint=cuda<0.6 --with-ghc=$GHC $*"

#------------------------------------------------------------
# Init sandbox and link it

#temporarily added by js

# cabal update
if [ "$NOSANDBOX" == "" ] || [ "$NOSANDBOX" == "0" ]; then
  $CABAL --version
  $CABAL sandbox init
  $CABAL sandbox hc-pkg list
  $GHCPKG list -v || echo ok
else 
  $CABAL sandbox delete || echo ok 
  # rm -rf .cabal-sandbox cabal.sandbox.config
  # Very aggressively delete sandboxes:
  find . -name cabal.sandbox.config | xargs rm -f 
  find . -name .cabal-sandbox       | xargs rm -rf
  for $name in $PKGNAMES; do
    $GHCPKG unregister $name --force || echo ok
  done
fi

TOP=`pwd`
for dir in $PKGS; do
  cd $dir
  if [ "$NOSANDBOX" == "" ] || [ "$NOSANDBOX" == "0" ]; then
    $CABAL sandbox init --sandbox=$TOP/.cabal-sandbox/
  fi
  cd $TOP
done

#------------------------------------------------------------
# Begin installation

# I'm having various problems, so this first one is just at test:
# $CABAL install $CBLARGS test-framework test-framework-hunit HUnit -j
$CABAL install $CBLARGS $PKGS --force-reinstalls -j 
$CABAL install $CBLARGS $PKGS --only-dependencies --enable-tests -j

#------------------------------------------------------------
# Begin testing

function test_dir() {
  dir=$1
  shift
  args=$*
  cd $dir
  time $CABAL test --show-details=always $args
  cd $TOP
}

# Provide a way to run this script for installs only without actually running the tests.
if [ "$ACCELERATE_INSTALL_ONLY" != "1" ] && [ "$NOTEST" != "1" ]; then 

  # (1) Test the interpreters:
  for TRGT in " test-accelerate-backend-kit-unittests test-accelerate-backend-kit-simple-interp  test-accelerate-main-interp"; do
    test_dir $TOP/accelerate-backend-kit/backend-kit $TRGT --test-option="--threads=8"
  done

  # (2) Test the C/Cilk backends
  # Split these out to run with specific arguments:
  test_dir $TOP/accelerate-backend-kit/icc-opencl/   test-accelerate-cpu-sequential --test-option="--threads=8" 

  # Currently [2014.02.13] running Cilk from multiple dynamic libs causes errors (backend-kit issue #4)
  (test_dir $TOP/accelerate-backend-kit/icc-opencl/   test-accelerate-cpu-cilk  --test-option="--threads=1" 2>1 | tee /tmp/out)

#  test_dir $TOP/accelerate-multidev/ || echo "acclerate-multidev failed tests!  But that's allowed for now."

  # test_dir $TOP/accelerate-cuda/ || echo "acclerate-cuda failed tests!  But that's allowed for now."

  if [ "$USECUDA" == "1" ]; then 
    (test_dir $TOP/accelerate-cuda/ test-accelerate-cuda  --test-option="--threads=1" 2>1 | tee /tmp/out)
  fi

fi
