# Change Log

Notable changes to the project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and the
project adheres to the [Haskell Package Versioning
Policy (PVP)](https://pvp.haskell.org)

## [next]
### Added
  * Additional EKG monitoring hooks ([#340])

## [1.0.0.0] - 2017-03-31
  * Many API and internal changes
  * Bug fixes and other enhancements

## [0.15.1.0]
  * Fix type of `allocateArray`

## [0.15.0.0]
  * Bug fixes and performance improvements.

## [0.14.0.0]
  * New iteration constructs.
  * Additional Prelude-like functions.
  * Improved code generation and fusion optimisation.
  * Concurrent kernel execution in the CUDA backend.
  * Bug fixes.

## [0.13.0.0]
  * New array fusion optimisation.
  * New foreign function interface for array and scalar expressions.
  * Additional Prelude-like functions.
  * New example programs.
  * Bug fixes and performance improvements.

## [0.12.0.0]
  * Full sharing recovery in scalar expressions and array computations.
  * Two new example applications in package `accelerate-examples` (both including a graphical frontend):
    * A real-time Canny edge detection 
    * An interactive fluid flow simulator
  * Bug fixes.

## [0.11.0.0]
  * New Prelude-like functions `zip*`, `unzip*`, `fill`, `enumFrom*`, `tail`,
    `init`, `drop`, `take`, `slit`, `gather*`, `scatter*`, and `shapeSize`.
  * New simplified AST (in package `accelerate-backend-kit`) for backend writers
    who want to avoid the complexities of the type-safe AST.

## [0.10.0.0]
  * Complete sharing recovery for scalar expressions (but currently disabled by default).
  * Also bug fixes in array sharing recovery and a few new convenience functions.

## [0.9.0.0]
  * Streaming computations
  * Precompilation
  * Repa-style array indices
  * Additional collective operations supported by the CUDA backend: `stencil`s,
    more `scan`s, rank-polymorphic `fold`, `generate`.
  * Conversions to other array formats
  * Bug fixes

## 0.8.1.0
  * Bug fixes and some performance tweaks.

## 0.8.0.0
  * More collective operations supported by the CUDA backend: `replicate`,
    `slice` and `foldSeg`. Frontend and interpreter support for `stencil`.
  * Bug fixes.

## [0.7.1.0]
  * Initial release of the CUDA backend


[next]:             https://github.com/AccelerateHS/accelerate/compare/1.0.0.0...HEAD
[1.0.0.0]:          https://github.com/AccelerateHS/accelerate/compare/0.15.1.0...1.0.0.0
[0.15.1.0]:         https://github.com/AccelerateHS/accelerate/compare/0.15.0.0...0.15.1.0
[0.15.0.0]:         https://github.com/AccelerateHS/accelerate/compare/0.14.0.0...0.15.0.0
[0.14.0.0]:         https://github.com/AccelerateHS/accelerate/compare/0.13.0.0...0.14.0.0
[0.13.0.0]:         https://github.com/AccelerateHS/accelerate/compare/0.12.0.0...0.13.0.0
[0.12.0.0]:         https://github.com/AccelerateHS/accelerate/compare/0.11.0.0...0.12.0.0
[0.11.0.0]:         https://github.com/AccelerateHS/accelerate/compare/0.10.0.0...0.11.0.0
[0.10.0.0]:         https://github.com/AccelerateHS/accelerate/compare/0.9.0.0...0.10.0.0
[0.9.0.0]:          https://github.com/AccelerateHS/accelerate/compare/0_8_1_0...0.9.0.0
[0.7.1.0]:          https://github.com/AccelerateHS/accelerate/compare/0_6_0_0...0_7_1_0

[#340]:             https://github.com/AccelerateHS/accelerate/issues/340

