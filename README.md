An Embedded Language for Accelerated Array Computations
=======================================================

`Data.Array.Accelerate` defines an embedded language of array computations for high-performance computing in Haskell. Computations on multi-dimensional, regular arrays are expressed in the form of parameterised collective operations (such as maps, reductions, and permutations). These computations are online-compiled and executed on a range of architectures.

For more details, see our papers:
 * [Accelerating Haskell Array Codes with Multicore GPUs][CKLM+11]
 * [Optimising Purely Functional GPU Programs][MCKL13]

There are also slides from some fairly recent presentations:
 * [Embedded Languages for High-Performance Computing in Haskell][Embedded]
 * [GPGPU Programming in Haskell with Accelerate][YLJ13-slides] ([video][YLJ13-video]) ([workshop][YLJ13-workshop])

Chapter 6 of Simon Marlow's book [Parallel and Concurrent Programming in Haskell][Mar13] contains a tutorial introduction to Accelerate.

A simple example
----------------

As a simple example, consider the computation of a dot product of two vectors of single-precision floating-point numbers:

    dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
    dotp xs ys = fold (+) 0 (zipWith (*) xs ys)

Except for the type, this code is almost the same as the corresponding Haskell code on lists of floats. The types indicate that the computation may be online-compiled for performance — for example, using `Data.Array.Accelerate.CUDA.run` it may be on-the-fly off-loaded to a GPU.

Availability
------------

Package accelerate is available from

 * Hackage: [accelerate][Hackage] — install with `cabal install accelerate`
 * GitHub: [AccelerateHS/accelerate][GitHub] - get the source with `git clone https://github.com/AccelerateHS/accelerate.git`

Additional components
---------------------

The following supported addons are available as separate packages on Hackage and included as submodules in the GitHub repository:

  * [`accelerate-cuda`][accelerate-cuda] Backend targeting CUDA-enabled NVIDA GPUs — requires the NVIDIA CUDA SDK and hardware with compute capability 1.2 or greater (see the [table on Wikipedia][wiki-cc])
  * [`accelerate-examples`][accelerate-examples] Computational kernels and applications showcasing the use of Accelerate as well as a regression test suite (supporting function and performance testing)
  * [`accelerate-io`][accelerate-io] Fast conversion between Accelerate arrays and other array formats (including Repa arrays)
  * [`accelerate-backend-kit`][accelerate-backend-kit] Simplified internal AST to get going on writing backends
  * [`accelerate-buildbot`][accelerate-buildbot] Build bot for automatic performance & regression testing

Install them from Hackage with `cabal install PACKAGENAME`.

The following additional components are experimental and incomplete:

  * [`accelerate-opencl`][accelerate-opencl] Backend targeting GPUs via the OpenCL standard
  * [`accelerate-repa`][accelerate-repa] Backend targeting multicore CPUs via the [Repa][repa] parallel array library

Requirements
------------

  * Glasgow Haskell Compiler (GHC), 7.6 or later
  * For the CUDA backend, CUDA version 5.0 or later
  * Haskell libraries as specified in the [`accelerate.cabal`][accelerate-cabal] and optionally [`accelerate-cuda.cabal`][accelerate-cuda-cabal] files.

Documentation
-------------

  * Haddock documentation is included in the package and linked from the [Hackage page][Hackage].
  * Online documentation is on the [GitHub wiki][Wiki].
  * The idea behind the HOAS (higher-order abstract syntax) to de-Bruijn conversion used in the library is [described separately.][HOAS-conv]

Examples
--------

The GitHub repository contains a submodule [accelerate-examples][accelerate-examples], which provides a range of computational kernels and a few complete applications. To install these from Hackage, issue `cabal install accelerate-examples`. The examples include:

  * An implementation of [canny edge detection][wiki-canny]
  * An interactive [mandelbrot set][wiki-mandelbrot] generator
  * An [N-body simulation][wiki-nbody] of gravitational attraction between solid particles
  * An implementation of the [PageRank][wiki-pagerank] algorithm
  * A simple [ray-tracer][wiki-raytracing]
  * A particle based simulation of stable fluid flows
  * A cellular automata simulation
  * A "password recovery" tool, for dictionary lookup of MD5 hashes

[![Mandelbrot](http://www.cse.unsw.edu.au/~tmcdonell/images/mandelbrot-small.jpg "accelerate-mandelbrot")](http://www.cse.unsw.edu.au/~tmcdonell/images/mandelbrot.jpg)
[![Raytracer](http://www.cse.unsw.edu.au/~tmcdonell/images/ray-small.jpg "accelerate-ray")](http://www.cse.unsw.edu.au/~tmcdonell/images/ray.jpg)


Mailing list and contacts
-------------------------

  * Mailing list: [`accelerate-haskell@googlegroups.com`](mailto:accelerate-haskell@googlegroups.com) (discussions on both use and development are welcome)
  * Sign up for the mailing list at the [Accelerate Google Groups page][Google-Group].
  * Bug reports and issues tracking: [GitHub project page][Issues].

The maintainers of Accelerate are Manuel M T Chakravarty <chak@cse.unsw.edu.au> and Trevor L McDonell <tmcdonell@cse.unsw.edu.au>.

What's missing?
---------------

Here is a list of features that are currently missing:

 * Preliminary API (parts of the API may still change in subsequent releases)



  [CKLM+11]:                http://www.cse.unsw.edu.au/~chak/papers/CKLM+11.html
  [MCKL13]:                 http://www.cse.unsw.edu.au/~chak/papers/MCKL13.html
  [HIW'09]:                 http://haskell.org/haskellwiki/HaskellImplementorsWorkshop
  [Mar13]:                  http://chimera.labs.oreilly.com/books/1230000000929
  [Embedded]:               https://speakerdeck.com/mchakravarty/embedded-languages-for-high-performance-computing-in-haskell
  [Hackage]:                http://hackage.haskell.org/package/accelerate
  [accelerate-cuda]:        https://github.com/AccelerateHS/accelerate-cuda
  [accelerate-examples]:    https://github.com/AccelerateHS/accelerate-examples
  [accelerate-io]:          https://github.com/AccelerateHS/accelerate-io
  [accelerate-backend-kit]: https://github.com/AccelerateHS/accelerate-backend-kit
  [accelerate-buildbot]:    https://github.com/AccelerateHS/accelerate-buildbot
  [accelerate-repa]:        https://github.com/blambo/accelerate-repa
  [accelerate-opencl]:      https://github.com/hiPERFIT/accelerate-opencl
  [accelerate-cabal]:       https://github.com/AccelerateHS/accelerate/accelerate.cabal
  [accelerate-cuda-cabal]:  https://github.com/AccelerateHS/accelerate-cuda/accelerate-cuda.cabal
  [GitHub]:                 https://github.com/AccelerateHS/accelerate
  [Wiki]:                   https://github.com/AccelerateHS/accelerate/wiki
  [Issues]:                 https://github.com/AccelerateHS/accelerate/issues
  [Google-Group]:           http://groups.google.com/group/accelerate-haskell
  [HOAS-conv]:              http://www.cse.unsw.edu.au/~chak/haskell/term-conv/
  [repa]:                   http://hackage.haskell.org/package/repa
  [wiki-cc]:                http://en.wikipedia.org/wiki/CUDA#Supported_GPUs
  [YLJ13-video]:            http://youtu.be/ARqE4yT2Z0o
  [YLJ13-slides]:           https://speakerdeck.com/tmcdonell/gpgpu-programming-in-haskell-with-accelerate
  [YLJ13-workshop]:         https://speakerdeck.com/tmcdonell/gpgpu-programming-in-haskell-with-accelerate-workshop
  [wiki-canny]:             http://en.wikipedia.org/wiki/Canny_edge_detector
  [wiki-mandelbrot]:        http://en.wikipedia.org/wiki/Mandelbrot_set
  [wiki-nbody]:             http://en.wikipedia.org/wiki/N-body
  [wiki-raytracing]:        http://en.wikipedia.org/wiki/Ray_tracing
  [wiki-pagerank]:          http://en.wikipedia.org/wiki/Pagerank

