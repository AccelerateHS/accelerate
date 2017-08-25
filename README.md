Examples programs using Accelerate
==================================

[![Build Status](https://travis-ci.org/AccelerateHS/accelerate-examples.svg?branch=master)](https://travis-ci.org/AccelerateHS/accelerate-examples)
[![Hackage](https://img.shields.io/hackage/v/accelerate-examples.svg)](https://hackage.haskell.org/package/accelerate-examples)
[![Docker Automated build](https://img.shields.io/docker/automated/tmcdonell/accelerate-examples.svg)](https://hub.docker.com/r/tmcdonell/accelerate-examples/)
[![Docker status](https://images.microbadger.com/badges/image/tmcdonell/accelerate-examples.svg)](https://microbadger.com/images/tmcdonell/accelerate-examples)

Example programs using the Accelerate library. If you add new features to the
base library or find a bug, please add a test case. The aim is for this program
to evolve and be useful for both performance and regression testing.

If you have found a bug, please report it to:
https://github.com/AccelerateHS/accelerate/issues


Installation
------------

### External dependencies

Installation of `accelerate-examples` and its dependencies requires several
external packages. You may need to adjust the package names or versions slightly
for your system.

  * Ubuntu/Debian (apt-get):
    - llvm-4.0-dev
    - freeglut3-dev
    - libfftw3-dev

  * Mac OS ([homebrew](http://brew.sh/index.html))
    - fftw
    - libffi
    - llvm-hs/homebrew-llvm/llvm-4.0

If you want to use the CUDA GPU enabled backend
[`accelerate-llvm-ptx`](https://github.com/AccelerateHS/accelerate-llvm), you
will also need to install the CUDA toolkit for your system. You can find an
installer on NVIDIA's website here:

  * https://developer.nvidia.com/cuda-downloads


### stack

For development, the recommend build method is via the
[`stack`](http://haskellstack.org) tool. This will simplify pulling in
dependencies not yet on Hackage. For example, to build using ghc-8.0:

```bash
ln -s stack-8.0.yaml stack.yaml     # only once
stack build                         # or, 'stack install' to install the executables globally
```

Before building, you may want to edit the `stack.yaml` file to change the build
configuration. In particular, the `flags` section at the bottom can be used to
enable or disable individual example programs and accelerate backends, as well
as features such as monitoring and debug output.


Adding new backends
-------------------

Adding support for new Accelerate backends should require only a few minor
additions to the cabal file and the module
'Data.Array.Accelerate.Examples.Internal.Backend'. 

