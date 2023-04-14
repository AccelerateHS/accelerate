<div align="center">
<img width="450" src="https://github.com/AccelerateHS/accelerate/raw/master/images/accelerate-logo-text-v.png?raw=true" alt="henlo, my name is Theia"/>

# High-performance parallel arrays for Haskell

[![CI](https://github.com/tmcdonell/accelerate/actions/workflows/ci.yml/badge.svg)](https://github.com/tmcdonell/accelerate/actions/workflows/ci.yml)
[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg)](https://gitter.im/AccelerateHS/Lobby)
[![Hackage](https://img.shields.io/hackage/v/accelerate.svg)](https://hackage.haskell.org/package/accelerate)
[![Stackage LTS](https://stackage.org/package/accelerate/badge/lts)](https://stackage.org/lts/package/accelerate)
[![Stackage Nightly](https://stackage.org/package/accelerate/badge/nightly)](https://stackage.org/nightly/package/accelerate)

</div>

`Data.Array.Accelerate` defines an embedded language of array computations for high-performance computing in Haskell. Computations on multi-dimensional, regular arrays are expressed in the form of parameterised collective operations (such as maps, reductions, and permutations). These computations are online-compiled and executed on a range of architectures.

For more details, see our papers:

 * [Accelerating Haskell Array Codes with Multicore GPUs][CKLM+11]
 * [Optimising Purely Functional GPU Programs][MCKL13] ([slides][MCKL13-slides])
 * [Embedding Foreign Code][CMCK14]
 * [Type-safe Runtime Code Generation: Accelerate to LLVM][MCGN15] ([slides][MCGN15-slides]) ([video][MCGN15-video])
 * [Streaming Irregular Arrays][CMCK17] ([video][CMCK17-video])

There are also slides from some fairly recent presentations:

 * [Embedded Languages for High-Performance Computing in Haskell][Embedded]
 * [GPGPU Programming in Haskell with Accelerate][YLJ13-slides] ([video][YLJ13-video]) ([workshop][YLJ13-workshop])

Chapter 6 of Simon Marlow's book [Parallel and Concurrent Programming in Haskell][Mar13] contains a tutorial introduction to Accelerate.

[Trevor's PhD thesis][Trevor-thesis] details the design and implementation of frontend optimisations and CUDA backend.


**Table of Contents**

- [An Embedded Language for Accelerated Array Computations](#an-embedded-language-for-accelerated-array-computations)
  - [A simple example](#a-simple-example)
  - [Availability](#availability)
  - [Additional components](#additional-components)
  - [Requirements](#requirements)
  - [Documentation](#documentation)
  - [Examples](#examples)
  - [Who are we?](#who-are-we)
  - [Mailing list and contacts](#mailing-list-and-contacts)
  - [Citing Accelerate](#citing-accelerate)
  - [What's missing?](#whats-missing)

A simple example
----------------

As a simple example, consider the computation of a dot product of two vectors of single-precision floating-point numbers:

    dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
    dotp xs ys = fold (+) 0 (zipWith (*) xs ys)

Except for the type, this code is almost the same as the corresponding Haskell code on lists of floats. The types indicate that the computation may be online-compiled for performance; for example, using `Data.Array.Accelerate.LLVM.PTX.run` it may be on-the-fly off-loaded to a GPU.

Availability
------------

Package _Accelerate_ is available from:

 * Hackage: [accelerate][Hackage] - install with `cabal install accelerate`
 * GitHub: [AccelerateHS/accelerate][GitHub] - get the source with `git clone https://github.com/AccelerateHS/accelerate.git` 

To install the Haskell toolchain try [GHCup](https://www.haskell.org/ghcup/).

Additional components
---------------------

The following supported add-ons are available as separate packages:

  * [accelerate-llvm-native][accelerate-llvm-native]: Backend targeting multicore CPUs
  * [accelerate-llvm-ptx][accelerate-llvm-ptx]: Backend targeting CUDA-enabled NVIDIA GPUs. Requires a GPU with compute capability 2.0 or greater (see the [table on Wikipedia][wiki-cc])
  * [accelerate-examples][accelerate-examples]: Computational kernels and applications showcasing the use of Accelerate as well as a regression test suite (supporting function and performance testing)
  * Conversion between various formats:
    * [accelerate-io](https://hackage.haskell.org/package/accelerate-io): For copying data directly between raw pointers
    * [accelerate-io-array](https://hackage.haskell.org/package/accelerate-io-array): Immutable arrays
    * [accelerate-io-bmp](https://hackage.haskell.org/package/accelerate-io-bmp): Uncompressed BMP image files
    * [accelerate-io-bytestring](https://hackage.haskell.org/package/accelerate-io-bytestring): Compact, immutable binary data
    * [accelerate-io-cereal](https://hackage.haskell.org/package/accelerate-io-cereal): Binary serialisation of arrays using [cereal](https://hackage.haskell.org/package/cereal)
    * [accelerate-io-JuicyPixels](https://hackage.haskell.org/package/accelerate-io-JuicyPixels): Images in various pixel formats
    * [accelerate-io-repa](https://hackage.haskell.org/package/accelerate-io-repa): Another Haskell library for high-performance parallel arrays
    * [accelerate-io-serialise](https://hackage.haskell.org/package/accelerate-io-serialise): Binary serialisation of arrays using [serialise](https://hackage.haskell.org/package/serialise)
    * [accelerate-io-vector](https://hackage.haskell.org/package/accelerate-io-vector): Efficient boxed and unboxed one-dimensional arrays
  * [accelerate-fft][accelerate-fft]: Fast Fourier transform implementation, with FFI bindings to optimised implementations
  * [accelerate-blas][accelerate-blas]: BLAS and LAPACK operations, with FFI bindings to optimised implementations
  * [accelerate-bignum][accelerate-bignum]: Fixed-width large integer arithmetic
  * [colour-accelerate][colour-accelerate]: Colour representations in Accelerate (RGB, sRGB, HSV, and HSL)
  * [containers-accelerate](http://hackage.haskell.org/package/containers-accelerate): Hashing-based container types
  * [gloss-accelerate][gloss-accelerate]: Generate [gloss][gloss] pictures from Accelerate
  * [gloss-raster-accelerate][gloss-raster-accelerate]: Parallel rendering of raster images and animations
  * [hashable-accelerate](http://hackage.haskell.org/package/hashable-accelerate): A class for types which can be converted into a hash value
  * [lens-accelerate][lens-accelerate]: [Lens][lens] operators for Accelerate types
  * [linear-accelerate][linear-accelerate]: [Linear][linear] vector spaces in Accelerate
  * [mwc-random-accelerate][mwc-random-accelerate]: Generate Accelerate arrays filled with high quality pseudorandom numbers
  * [numeric-prelude-accelerate][numeric-prelude-accelerate]: Lifting the [numeric-prelude][numeric-prelude] to Accelerate
  * [wigner-ville-accelerate](https://github.com/Haskell-mouse/wigner-ville-accelerate): Wigner-Ville time-frequency distribution.

Install them from Hackage with `cabal install PACKAGENAME`.


Documentation
-------------

  * Haddock documentation is included and linked with the individual package releases on [Hackage][Hackage].
  <!-- * Haddock documentation for in-development components can be found [here](http://tmcdonell-bot.github.io/accelerate-travis-buildbot/). -->
  * The idea behind the HOAS (higher-order abstract syntax) to de-Bruijn conversion used in the library is [described separately][HOAS-conv].

Examples
--------

### accelerate-examples

The [accelerate-examples][accelerate-examples] package provides a range of computational kernels and a few complete applications. To install these from Hackage, issue `cabal install accelerate-examples`. The examples include:

  * An implementation of [canny edge detection][wiki-canny]
  * An interactive [mandelbrot set][wiki-mandelbrot] generator
  * An [N-body simulation][wiki-nbody] of gravitational attraction between solid particles
  * An implementation of the [PageRank][wiki-pagerank] algorithm
  * A simple [ray-tracer][wiki-raytracing]
  * A particle based simulation of stable fluid flows
  * A cellular automata simulation
  * A "password recovery" tool, for dictionary lookup of MD5 hashes

[![Mandelbrot](https://i.imgur.com/5Tbsp1j.jpg "accelerate-mandelbrot")](https://i.imgur.com/RgXRqsc.jpg)
[![Raytracer](https://i.imgur.com/7ohhKm9.jpg "accelerate-ray")](https://i.imgur.com/ZNEGEJK.jpg)

<!--
<video width=400 height=300 controls=false autoplay loop>
  <source="http://www.cse.unsw.edu.au/~tmcdonell/images/ray.mp4" type="video/mp4">
</video>
-->


### LULESH

[LULESH-accelerate][lulesh-accelerate] is in implementation of the Livermore Unstructured Lagrangian Explicit Shock Hydrodynamics (LULESH) mini-app. [LULESH][LULESH] represents a typical hydrodynamics code such as [ALE3D][ALE3D], but is a highly simplified application, hard-coded to solve the Sedov blast problem on an unstructured hexahedron mesh.

![LULESH mesh](https://i.imgur.com/bIkODKd.jpg)


### Additional examples

Accelerate users have also built some substantial applications of their own.
Please feel free to add your own examples!

  * Jonathan Fraser, [GPUVAC](https://github.com/GeneralFusion/gpuvac): An explicit advection magnetohydrodynamics simulation
  * David van Balen, [Sudokus](https://github.com/dpvanbalen/Sudokus): A sudoku solver
  * Trevor L. McDonell, [lol-accelerate][lol-accelerate]: A backend to the Λ ○ λ ([Lol][lol]) library for ring-based lattice cryptography
  * Henning Thielemann, [patch-image](http://hackage.haskell.org/package/patch-image): Combine a collage of overlapping images
  * apunktbau, [bildpunkt](https://github.com/abau/bildpunkt): A ray-marching distance field renderer
  * klarh, [hasdy](https://github.com/klarh/hasdy): Molecular dynamics in Haskell using Accelerate
  * Alexandros Gremm used Accelerate as part of the [2014 CSCS summer school](http://user.cscs.ch/blog/2014/cscs_usi_summer_school_2014_30_june_10_july_2014_in_serpiano_tessin/index.html) ([code](https://github.com/agremm/cscs))


Who are we?
-----------

The Accelerate team (past and present) consists of:

  * Manuel M T Chakravarty ([@mchakravarty])  <!-- 2008..2017? -->
  * Gabriele Keller ([@gckeller])             <!-- 2008..     -->
  * Trevor L. McDonell ([@tmcdonell])         <!-- 2009..     -->
  * Robert Clifton-Everest ([@robeverest])    <!-- 2013..     -->
  * Frederik M. Madsen ([@fmma])              <!-- 2014       -->
  * Ryan R. Newton ([@rrnewton])              <!-- 2012..2013 -->
  * Joshua Meredith ([@JoshMeredith])         <!-- 2018..     -->
  * Ben Lever ([@blever])                     <!-- 2010..2011 -->
  * Sean Seefried ([@sseefried])              <!-- 2010..2011 -->
  * Ivo Gabe de Wolff ([@ivogabe])            <!-- 2019..     -->

The maintainer and principal developer of Accelerate is Trevor L.
McDonell <trevor.mcdonell@gmail.com>.


Mailing list and contacts
-------------------------

  * Mailing list: [`accelerate-haskell@googlegroups.com`](mailto:accelerate-haskell@googlegroups.com) (discussions on both use and development are welcome)
  * Sign up for the mailing list at the [Accelerate Google Groups page][Google-Group]
  * Bug reports and issues tracking: [GitHub project page][Issues]
  * Chat with us on [gitter](https://gitter.im/AccelerateHS/Lobby)


Citing Accelerate
-----------------

If you use Accelerate for academic research, you are encouraged (though not
required) to cite the following papers:
<!-- ([BibTeX](http://www.cse.unsw.edu.au/~tmcdonell/papers/accelerate.bib)): -->

  * Manuel M. T. Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell, and Vinod Grover.
    [Accelerating Haskell Array Codes with Multicore GPUs][CKLM+11].
    In _DAMP '11: Declarative Aspects of Multicore Programming_, ACM, 2011.

  * Trevor L. McDonell, Manuel M. T. Chakravarty, Gabriele Keller, and Ben Lippmeier.
    [Optimising Purely Functional GPU Programs][MCKL13].
    In _ICFP '13: The 18th ACM SIGPLAN International Conference on Functional Programming_, ACM, 2013.

  * Robert Clifton-Everest, Trevor L. McDonell, Manuel M. T. Chakravarty, and Gabriele Keller.
    [Embedding Foreign Code][CMCK14].
    In _PADL '14: The 16th International Symposium on Practical Aspects of Declarative Languages_, Springer-Verlag, LNCS, 2014.

  * Trevor L. McDonell, Manuel M. T. Chakravarty, Vinod Grover, and Ryan R. Newton.
    [Type-safe Runtime Code Generation: Accelerate to LLVM][MCGN15].
    In _Haskell '15: The 8th ACM SIGPLAN Symposium on Haskell_, ACM, 2015.

  * Robert Clifton-Everest, Trevor L. McDonell, Manuel M. T. Chakravarty, and Gabriele Keller.
    [Streaming Irregular Arrays][CMCK17].
    In Haskell '17: The 10th ACM SIGPLAN Symposium on Haskell, ACM, 2017.


Accelerate is primarily developed by academics, so citations matter a lot to us.
As an added benefit, you increase Accelerate's exposure and potential user (and
developer!) base, which is a benefit to all users of Accelerate. Thanks in advance!


What's missing?
---------------

Here is a list of features that are currently missing:

 * Preliminary API (parts of the API may still change in subsequent releases)
 * Many more features... contact us!

  [@mchakravarty]:              https://github.com/mchakravarty
  [@gckeller]:                  https://github.com/gckeller
  [@tmcdonell]:                 https://github.com/tmcdonell
  [@robeverest]:                https://github.com/robeverest
  [@fmma]:                      https://github.com/fmma
  [@rrnewton]:                  https://github.com/rrnewton
  [@JoshMeredith]:              https://github.com/JoshMeredith
  [@blever]:                    https://github.com/blever
  [@sseefried]:                 https://github.com/sseefried
  [@ivogabe]:                   https://github.com/ivogabe

  [CKLM+11]:                    https://github.com/tmcdonell/tmcdonell.github.io/raw/master/papers/acc-cuda-damp2011.pdf
  [MCKL13]:                     https://github.com/tmcdonell/tmcdonell.github.io/raw/master/papers/acc-optim-icfp2013.pdf
  [MCKL13-slides]:              https://speakerdeck.com/tmcdonell/optimising-purely-functional-gpu-programs
  [CMCK14]:                     https://github.com/tmcdonell/tmcdonell.github.io/raw/master/papers/acc-ffi-padl2014.pdf
  [MCGN15]:                     https://github.com/tmcdonell/tmcdonell.github.io/raw/master/papers/acc-llvm-haskell2015.pdf
  [MCGN15-slides]:              https://speakerdeck.com/tmcdonell/type-safe-runtime-code-generation-accelerate-to-llvm
  [MCGN15-video]:               https://www.youtube.com/watch?v=snXhXA5noVc
  [HIW'09]:                     https://wiki.haskell.org/HaskellImplementorsWorkshop
  [CMCK17]:                     https://github.com/tmcdonell/tmcdonell.github.io/raw/master/papers/acc-seq2-haskell2017.pdf
  [CMCK17-video]:               https://www.youtube.com/watch?v=QIWSqp7AaNo
  [Mar13]:                      http://chimera.labs.oreilly.com/books/1230000000929
  [Embedded]:                   https://speakerdeck.com/mchakravarty/embedded-languages-for-high-performance-computing-in-haskell
  [Hackage]:                    http://hackage.haskell.org/package/accelerate
  [accelerate-cuda]:            https://github.com/AccelerateHS/accelerate-cuda
  [accelerate-examples]:        https://github.com/AccelerateHS/accelerate-examples
  [accelerate-io]:              https://github.com/AccelerateHS/accelerate-io
  [accelerate-fft]:             https://github.com/AccelerateHS/accelerate-fft
  [accelerate-blas]:            https://github.com/tmcdonell/accelerate-blas
  [accelerate-backend-kit]:     https://github.com/AccelerateHS/accelerate-backend-kit
  [accelerate-buildbot]:        https://github.com/AccelerateHS/accelerate-buildbot
  [accelerate-repa]:            https://github.com/blambo/accelerate-repa
  [accelerate-opencl]:          https://github.com/hiPERFIT/accelerate-opencl
  [accelerate-cabal]:           https://github.com/AccelerateHS/accelerate/accelerate.cabal
  [accelerate-cuda-cabal]:      https://github.com/AccelerateHS/accelerate-cuda/accelerate-cuda.cabal
  [accelerate-llvm]:            https://github.com/AccelerateHS/accelerate-llvm
  [accelerate-llvm-native]:     https://github.com/AccelerateHS/accelerate-llvm
  [accelerate-llvm-ptx]:        https://github.com/AccelerateHS/accelerate-llvm
  [accelerate-bignum]:          https://github.com/tmcdonell/accelerate-bignum
  [GitHub]:                     https://github.com/AccelerateHS/accelerate
  [Wiki]:                       https://github.com/AccelerateHS/accelerate/wiki
  [Issues]:                     https://github.com/AccelerateHS/accelerate/issues
  [Google-Group]:               http://groups.google.com/group/accelerate-haskell
  [HOAS-conv]:                  https://github.com/mchakravarty/hoas-conv
  <!-- [HOAS-conv]:                  https://web.archive.org/web/20180805092417/http://www.cse.unsw.edu.au/~chak/haskell/term-conv/ -->
  [repa]:                       http://hackage.haskell.org/package/repa
  [wiki-cc]:                    https://en.wikipedia.org/wiki/CUDA#Supported_GPUs
  [YLJ13-video]:                http://youtu.be/ARqE4yT2Z0o
  [YLJ13-slides]:               https://speakerdeck.com/tmcdonell/gpgpu-programming-in-haskell-with-accelerate
  [YLJ13-workshop]:             https://speakerdeck.com/tmcdonell/gpgpu-programming-in-haskell-with-accelerate-workshop
  [wiki-canny]:                 https://en.wikipedia.org/wiki/Canny_edge_detector
  [wiki-mandelbrot]:            https://en.wikipedia.org/wiki/Mandelbrot_set
  [wiki-nbody]:                 https://en.wikipedia.org/wiki/N-body
  [wiki-raytracing]:            https://en.wikipedia.org/wiki/Ray_tracing
  [wiki-pagerank]:              https://en.wikipedia.org/wiki/Pagerank
  [Trevor-thesis]:              https://github.com/tmcdonell/tmcdonell.github.io/raw/master/papers/TrevorMcDonell_PhD_Thesis.pdf
  [colour-accelerate]:          https://github.com/tmcdonell/colour-accelerate
  [gloss]:                      https://hackage.haskell.org/package/gloss
  [gloss-accelerate]:           https://github.com/tmcdonell/gloss-accelerate
  [gloss-raster-accelerate]:    https://github.com/tmcdonell/gloss-raster-accelerate
  [lens]:                       https://hackage.haskell.org/package/lens
  [lens-accelerate]:            https://github.com/tmcdonell/lens-accelerate
  [linear]:                     https://hackage.haskell.org/package/linear
  [linear-accelerate]:          https://github.com/tmcdonell/linear-accelerate
  [mwc-random-accelerate]:      https://github.com/tmcdonell/mwc-random-accelerate
  [numeric-prelude]:            https://hackage.haskell.org/package/numeric-prelude
  [numeric-prelude-accelerate]: https://github.com/tmcdonell/numeric-prelude-accelerate
  [LULESH]:                     https://codesign.llnl.gov/lulesh.php
  [ALE3D]:                      https://wci.llnl.gov/simulation/computer-codes/ale3d
  [lulesh-accelerate]:          https://github.com/tmcdonell/lulesh-accelerate
  [lol]:                        https://hackage.haskell.org/package/lol
  [lol-accelerate]:             https://github.com/tmcdonell/lol-accelerate

