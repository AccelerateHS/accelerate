# Change Log

Notable changes to the project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and the
project adheres to the [Haskell Package Versioning
Policy (PVP)](https://pvp.haskell.org)

## [next]
### Added
  * Added debugging functions in module `Data.Array.Accelerate.Debug.Trace` ([#485](https://github.com/AccelerateHS/accelerate/pull/485))

### Changed
  * Removed dependency on lens ([#493](https://github.com/AccelerateHS/accelerate/pull/493))

### Fixed
  * Graphviz graph generation of `-ddump-dot` and `-ddump-simpl-dot` ([#384](https://github.com/AccelerateHS/accelerate/issues/384))

### Contributors

Special thanks to those who contributed patches as part of this release:

  * Ivo Gabe de Wolff (@ivogabe)
  * David van Balen (@dpvanbalen)
  * Tom Smeding (@tomsmeding)
  * Trevor L. McDonell (@tmcdonell)


## [1.3.0.0] - 2020-08-26
### Added
  * Instances of `Elt` are now derivable via `Generic` for simple (Haskell'98)
    product _and_ sum data types.
  * Pattern synonyms for manipulating custom product and sum types can now be
    created; see `Pattern`, `mkPattern`
  * Added pattern synonyms for accessing tuples and indices, as an alternative
    to `lift` and `unlift`.
  * Support for pattern matching in the embedded language; see `match`

### Changed
  * The `stencil` functions now support fusion. Note however that the source
    (delayed) array will be evaluated at _every_ access to the stencil pattern;
    if the delayed function is expensive, you may wish to explicitly `compute`
    the source array first, matching the old behaviour.
  * Removed `Slice` constraint from some indexing operations
  * Improve fusion for `zipWith*` ([#453](https://github.com/AccelerateHS/accelerate/pull/453))
  * The indexing function to `permute` now returns a `Maybe` type ([#87](https://github.com/AccelerateHS/accelerate/issues/87))

  * (internal) Visible type applications are used instead of `Proxy` types
  * (internal) `EltR` is now a class-associated type of `Elt`
  * (internal) `GArrayData` has been simplified
  * (internal) SIMD representation has been improved and generalised
  * (internal) Internal refactoring ([#449](https://github.com/AccelerateHS/accelerate/pull/449), [#455](https://github.com/AccelerateHS/accelerate/pull/455), [#457](https://github.com/AccelerateHS/accelerate/pull/457), [#460](https://github.com/AccelerateHS/accelerate/pull/460))

  * Probably many others I have forgotten about

### Removed
  * Drop support for GHC-7.10 .. 8.4.

### Contributors

Special thanks to those who contributed patches as part of this release:

  * Trevor L. McDonell (@tmcdonell)
  * Joshua Meredith (@JoshMeredith)
  * Ivo Gabe de Wolff (@ivogabe)
  * David van Balen (@dpvanbalen)
  * Jaro Reinders (@noughtmare)
  * Alex Lang (@alang9)
  * Paul Wilson (@status_failed)
  * @lennonhill
  * Travis Whitaker (@TravisWhitaker)
  * Roger Bosman (@rogerbosman)
  * Robbert van der Helm (@robbert-vdh)
  * Sam (@sam-340453)
  * Lars van den Haak (@sakehl)
  * Rinat Striungis (@Haskell-mouse)
  * Viktor Kronvall (@considerate)
  * Tom Smeding (@tomsmeding)
  * Ryan Scott (@RyanGlScott)


## [1.2.0.1] - 2018-10-06
### Fixed
  * Build fix for ghc-8.6

## [1.2.0.0] - 2018-04-03
### Changed
  * Internal debugging/RTS options handling has been changed. Compiling this package now implies that backends are also compiled in debug mode (no need to set the `-fdebug` cabal flag for those packages as well).
  * Complex numbers are stored in the C-style array-of-struct representation.
  * Improve numeric handling of complex numbers.
  * Coercions (`bitcast`) now occur between the underlying representation types
  * Front-end performance improvements

### Added
  * Support for half-precision floating-point numbers.
  * Support for struct-of-array-of-struct representations. Currently this is limited to fields of 2,3,4,8, or 16-elements wide.
  * Add equivalents for `Data.Functor`, `Data.Semigroup` (ghc-8+)
  * Add instances and helper functions for `Maybe` and `Either` types
  * Add rank generalised versions of `take`, `drop`, `head`, `tail`, `init`, `slit`, `reverse` and `transpose`.
  * Implement counters and reporting for `-ddump-gc-stats`

### Contributors

Special thanks to those who contributed patches as part of this release:

  * Trevor L. McDonell (@tmcdonell)
  * Ryan Scott (@ryanglscott)
  * Rinat Striungis (@Haskell-mouse)


## [1.1.1.0] - 2017-09-26
### Changed
  * Improve and colourise the pretty-printer


## [1.1.0.0] - 2017-09-21
### Added
  * Additional EKG monitoring hooks ([#340](https://github.com/AccelerateHS/accelerate/issues/340))

  * Operations from `RealFloat`

### Changed
  * Changed type of `scanl'`, `scanr'` to return an `Acc` tuple, rather than a
    tuple of `Acc` arrays.
  * Specialised folds `sum`, `product`, `minimum`, `maximum`, `and`, `or`,
    `any`, `all` now reduce along the innermost dimension only, rather than
    reducing all elements. You can recover the old behaviour by first
    `flatten`-ing the input array.
  * Add new stencil boundary condition `function`, to apply the given function
    to out-of-bounds indices.

### Fixed
  * [#390](https://github.com/AccelerateHS/accelerate/issues/390): Wrong number of arguments in printf


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


[next]:             https://github.com/AccelerateHS/accelerate/compare/v1.3.0.0...HEAD
[1.3.0.0]:          https://github.com/AccelerateHS/accelerate/compare/v1.2.0.1...v1.3.0.0
[1.2.0.1]:          https://github.com/AccelerateHS/accelerate/compare/v1.2.0.0...v1.2.0.1
[1.2.0.0]:          https://github.com/AccelerateHS/accelerate/compare/v1.1.0.0...v1.2.0.0
[1.1.1.0]:          https://github.com/AccelerateHS/accelerate/compare/v1.1.0.0...v1.1.1.0
[1.1.0.0]:          https://github.com/AccelerateHS/accelerate/compare/1.0.0.0...v1.1.0.0
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

