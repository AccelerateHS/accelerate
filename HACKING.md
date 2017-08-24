Developer Notes
===============

If you like to hack Accelerate, and especially if you like to contribute changes back, please fork:

    https://github.com/AccelerateHS/accelerate

and send pull request with your changes. In your pull request, please describe the testing that you have performed.

In general, testing should involve both the interpreter and the CUDA backend (if you have got access to CUDA compatible hardware). A fairly comprehensive set of tests is available in the `accelerate-examples` package. This directory contains its own cabal-based build system. If you built Accelerate without the CUDA backend, you need to configure `accelerate-examples` with the additional option `-f-cuda` to disable testing of the CUDA backend.


Installing from source
----------------------

Requirements:

  * Glasgow Haskell Compiler (GHC), version 7.8.3 or later
  * For the CUDA backend, CUDA version 5.0 or later
  * Haskell libraries as specified in the relevant cabal files

The recommended way to install from source is to use [`stack`](https://www.haskellstack.org), for example by including the following in the `stack.yaml` file for your project:

```yaml
resolver: lts-9.0
extra-deps:
- 'accelerate-llvm-1.1.0.0'
- 'accelerate-llvm-native-1.1.0.0'
- 'accelerate-llvm-ptx-1.1.0.0'
```


New backends
------------

If you are considering writing a new backend, you can do so in a separate, standalone package.  The Accelerate frontend is entirely independent of any backend, and package `accelerate` exports all the necessary internals. If you run into problems, please contact the [mailing list][Google-Group] or [github issues][Issues] page for assistance.


  [Issues]:                 https://github.com/AccelerateHS/accelerate/issues
  [Google-Group]:           http://groups.google.com/group/accelerate-haskell

