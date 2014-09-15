
0.15.0.0

  * Bug fixes and performance improvements.

0.14.0.0

  * New iteration constructs.

  * Additional Prelude-like functions.

  * Improved code generation and fusion optimisation.

  * Concurrent kernel execution in the CUDA backend.

  * Bug fixes.

0.13.0.0

  * New array fusion optimisation.

  * New foreign function interface for array and scalar expressions.

  * Additional Prelude-like functions.

  * New example programs.

  * Bug fixes and performance improvements.

0.12.0.0

  * Full sharing recovery in scalar expressions and array computations.

  * Two new example applications in package `accelerate-examples`: Real-time
    Canny edge detection and an interactive fluid flow simulator (both including
    a graphical frontend).

  * Bug fixes.

0.11.0.0

  * New Prelude-like functions `zip*`, `unzip*`, `fill`, `enumFrom*`, `tail`,
    `init`, `drop`, `take`, `slit`, `gather*`, `scatter*`, and `shapeSize`.

  * New simplified AST (in package `accelerate-backend-kit`) for backend writers
    who want to avoid the complexities of the type-safe AST.

0.10.0.0

  * Complete sharing recovery for scalar expressions (but currently disabled by
    default).

  * Also bug fixes in array sharing recovery and a few new convenience
    functions.

0.9.0.0

  * Streaming computations

  * Precompilation

  * Repa-style array indices

  * Additional collective operations supported by the CUDA backned: `stencil`s,
    more `scan`s, rank-polymorphic `fold`, `generate`.

  * Conversions to other array formats

  * Bug fixes

0.8.1.0

  * Bug fixes and some performance tweaks.

0.8.0.0

  * More collective operations supported by the CUDA backend: `replicate`,
    `slice` and `foldSeg`. Frontend and interpreter support for `stencil`.

  * Bug fixes.

0.7.1.0

  * Initial release of the CUDA backend

