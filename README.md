Examples programs using Accelerate
==================================

[![Build Status](https://travis-ci.org/AccelerateHS/accelerate-examples.svg?branch=master)](https://travis-ci.org/AccelerateHS/accelerate-examples)

Example programs using the Accelerate library. If you add new features to the
base library or find a bug, please add a test case. The aim is for this program
to evolve and be useful for both performance and regression testing.

For a list of available options and test programs, run:
> accelerate-nofib --help

If you have found a bug, please report it to:
https://github.com/AccelerateHS/accelerate/issues

Current status for each backend:
https://github.com/AccelerateHS/accelerate-examples/wiki/Status-of-regression-tests


Adding new backends:
--------------------

Adding support for new Accelerate backends should require only a few minor
additions to the cabal file and the file 'lib/ParseArgs.hs'. See that file for
instructions, and/or follow the example of the CUDA backend (grep
'ACCELERATE_CUDA_BACKEND').

