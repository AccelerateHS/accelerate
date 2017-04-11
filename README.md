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
    - llvm-3.5-dev
    - libedit3-dev
    - libglut3-dev
    - libfftw3-dev

  * Mac OS ([homebrew](http://brew.sh/index.html))
    - fftw
    - libffi
    - homebrew/versions/llvm35

If you want to use the CUDA GPU enabled backends
[`accelerate-cuda`](https://github.com/AccelerateHS/accelerate-cuda) or
[`accelerate-llvm-ptx`](https://github.com/AccelerateHS/accelerate-llvm), you
will also need to install the CUDA toolkit for your system. You can find an
installer on NVIDIA's website here:

  * https://developer.nvidia.com/cuda-downloads


### stack

The recommend installation method is via [`stack`](http://haskellstack.org). For
example, to build using ghc-7.10:

```bash
ln -s stack-7.10.yaml stack.yaml        # only once
stack setup --upgrade-cabal             # only once, if using CUDA
stack install                           # or, 'stack build' to not install the executables globally
```

Before building, you may want to edit the `stack.yaml` file to change the build
configuration. In particular, the `flags` section at the bottom can be used to
enable or disable individual example programs and accelerate backends, as well
as features such as monitoring and debug output.


### cabal

Installing the development version via `cabal` is not recommended. However, if
you must, here are some tips:

  * `llvm-general` must be installed with flag `-fshared-llvm`.
  * The development versions tend to depend on other development versions which
    will not be available on Hackage yet, so you will need to manually download
    those from github and add/build those yourself.


Adding new backends
-------------------

Adding support for new Accelerate backends should require only a few minor
additions to the cabal file and the file 'lib/ParseArgs.hs'. See that file for
instructions, and/or follow the example of the CUDA backend (grep
'ACCELERATE_CUDA_BACKEND').

