An Embedded Language for Accelerated Array Computations
=======================================================

`Data.Array.Accelerate` defines an embedded language of array computations for high-performance computing in Haskell. Computations on multi-dimensional, regular arrays are expressed in the form of parameterised collective operations (such as maps, reductions, and permutations). These computations are online-compiled and executed on a range of architectures.

For more details, see our recent paper [Accelerating Haskell Array Codes with Multicore GPUs][CKLM+11]. There are also some slightly outdated slides and a video of a talk that I gave at the [Haskell Implementors Workshop 2009][HIW'09] (in Edinburgh): [Haskell Arrays, Accelerated (Using GPUs).][Cha09]

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
 * GitHub: [mchakravarty/accelerate][GitHub] - get the source with `git clone https://github.com/mchakravarty/accelerate.git`

Requirements
------------

  * Glasgow Haskell Compiler (GHC), 7.0.3 or later
  * Haskell libraries as specified in `accelerate.cabal`
  * For the CUDA backend, CUDA version 3.0 or later

Examples and documentation
--------------------------

The GitHub repository contains a subdirectory `accelerate-examples`, which provides a range of computational kernels and a few complete applications. These examples are also available from Hackage in a separate package called [accelerate-examples][Hackage-examples]. Install it with `cabal install accelerate-examples`.

  * Haddock documentation is included in the package and linked from the [Hackage page][Hackage].
  * Online documentation is on the [GitHub wiki][Wiki].
  * The idea behind the HOAS (higher-order abstract syntax) to de-Bruijn conversion used in the library is [described separately.][HOAS-conv]

Contacts
--------

The maintainer of this package is Manuel M T Chakravarty <chak@cse.unsw.edu.au> (aka TacticalGrace on #haskell and related channels).

Both user and developer questions and discussions are welcome at `accelerate@projects.haskell.org`.  **Sorry, this mailing list is currently unavailable.**

Bug reports and issues tracking are on the [GitHub project page][Issues].

What's missing?
---------------

Here is a list of features that are currently missing:

 * The CUDA backend does not support arrays of type Char and Bool at the moment.
 * Preliminary API (parts of the API may still change in subsequent releases)



  [CKLM+11]:          http://www.cse.unsw.edu.au/~chak/papers/CKLM+11.html
  [HIW'09]:           http://haskell.org/haskellwiki/HaskellImplementorsWorkshop
  [Cha09]:            http://justtesting.posterous.com/running-haskell-array-computations-on-a-gpu
  [Hackage]:          http://hackage.haskell.org/package/accelerate
  [Hackage-examples]: http://hackage.haskell.org/package/accelerate-examples
  [GitHub]:           https://github.com/AccelerateHS/accelerate
  [Wiki]:             https://github.com/AccelerateHS/accelerate/wiki
  [Issues]:           https://github.com/AccelerateHS/accelerate/issues
  [HOAS-conv]:        http://www.cse.unsw.edu.au/~chak/haskell/term-conv/
  