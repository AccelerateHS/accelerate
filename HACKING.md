Developer Notes
===============

If you like to hack Accelerate, and especially if you like to contribute changes back, please fork 

    https://github.com/mchakravarty/accelerate
  
and send pull request with your changes.  In your pull request, please describe the testing that you have performed.

In general, testing should involve both the interpreter and the CUDA backend (if you have got access to CUDA compatible hardware).  A fairly comprehensive set of tests is available in the `accelerate-examples` directory.  This directory contains its own Cabal-based build system.  If you built Accelerate without the CUDA backend, you need to configure `accelerate-examples` with the additional option `-f-cuda` to disable testing of the CUDA backend.

New backends
------------

If you are considering writing a new backend, you can do so in a separate, standalone package.  The Accelerate frontend is entirely independent of any backend and package `accelerate` exports all the internals necessary to implement a backend.  If you run into any problems, please contact Manuel M T Chakravarty <chak@cse.unsw.edu.au>.

