-- |
-- Module      : Data.Array.Accelerate
-- Copyright   : [2008..2016] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2016] Trevor L. McDonell
--               [2013..2016] Robert Clifton-Everest
--               [2014..2014] Frederik M. Madsen
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- @Data.Array.Accelerate@ defines an embedded language of array computations
-- for high-performance computing in Haskell. Computations on multi-dimensional,
-- regular arrays are expressed in the form of parameterised collective
-- operations such as maps, reductions, and permutations. These computations are
-- online compiled and can be executed on a range of architectures.
--
-- [/Abstract interface:/]
--
-- The types representing array computations are only exported abstractly;
-- client code can generate array computations and submit them for execution,
-- but it cannot inspect these computations. This is to allow for more
-- flexibility for future extensions of this library.
--
-- [/Stratified language:/]
--
-- Accelerate distinguishes the types of collective operations 'Acc' from the
-- type of scalar operations 'Exp' to achieve a stratified language. Collective
-- operations comprise many scalar computations that are executed in parallel,
-- but scalar computations /can not/ contain collective operations. This
-- separation excludes /nested, irregular/ data-parallelism statically; instead,
-- Accelerate is limited to /flat data-parallelism/ involving only regular,
-- multi-dimensional arrays.
--
-- [/Optimisations:/]
--
-- Accelerate uses a number of scalar and array optimisations, including
-- /array fusion/, in order to improve the performance of programs. Fusing
-- a program entails combining successive traversals (loops) over an array into
-- a single traversal, which reduces memory traffic and eliminates intermediate
-- arrays.
--
-- [/Code execution:/]
--
-- Several backends are available which can be used to evaluate accelerate
-- programs:
--
-- * "Data.Array.Accelerate.Interpreter": simple interpreter in Haskell as a
--   reference implementation defining the semantics of the Accelerate language
--
-- * <http://hackage.haskell.org/package/accelerate-llvm-native accelerate-llvm-native>:
--   implementation supporting parallel execution on multicore CPUs (e.g. x86).
--
-- * <http://hackage.haskell.org/package/accelerate-llvm-ptx accelerate-llvm-ptx>:
--   implementation supporting parallel execution on CUDA-capable NVIDIA GPUs.
--
-- * <http://hackage.haskell.org/package/accelerate-cuda accelerate-cuda>:
--   an older implementation supporting parallel execution on CUDA-capable
--   NVIDIA GPUs. /__NOTE:__ This backend is being deprecated in favour of @accelerate-llvm-ptx@./
--
-- [/Examples:/]
--
-- * The <http://hackage.haskell.org/package/accelerate-examples accelerate-examples>
--   package demonstrates a range of computational kernels and several complete
--   applications:
--
--      - Implementation of the <https://en.wikipedia.org/wiki/Canny_edge_detector canny edge detector>
--      - Interactive <https://en.wikipedia.org/wiki/Mandelbrot_set Mandelbrot set> generator
--      - <https://en.wikipedia.org/wiki/N-body N-body simulation> of gravitational attraction between large bodies
--      - Implementation of the <https://en.wikipedia.org/wiki/Pagerank PageRank> algorithm
--      - A simple, real-time, interactive <https://en.wikipedia.org/wiki/Ray_tracing ray tracer>.
--      - A particle based simulation of stable fluid flows
--      - A cellular automaton simulation
--      - A "password recovery" tool, for dictionary attacks on MD5 hashes.
--
--      <<http://i.imgur.com/5Tbsp1j.jpg accelerate-mandelbrot>>
--      <<http://i.imgur.com/7ohhKm9.jpg accelerate-ray>>
--
-- * <http://hackage.haskell.org/package/lulesh-accelerate lulesh-accelerate>
--   is an implementation of the Livermore Unstructured Lagrangian Explicit
--   Shock Hydrodynamics (LULESH) application. LULESH is representative of
--   typical hydrodynamics codes, although simplified and hard-coded to solve
--   the Sedov blast problem on an unstructured hexahedron mesh.
--
--      - For more information on LULESH: <https://codesign.llnl.gov/lulesh.php>.
--
--      <<https://codesign.llnl.gov/images/sedov-3d-LLNL.png>>
--
-- [/Additional components:/]
--
-- * <https://hackage.haskell.org/package/accelerate-io accelerate-io>: Fast
-- conversion between Accelerate arrays and other formats (e.g. Repa, Vector).
--
-- * <https://hackage.haskell.org/package/accelerate-fft accelerate-fft>: Fast
-- Fourier transform, with FFI bindings to optimised implementations.
--
-- * <https://hackage.haskell.org/package/colour-accelerate colour-accelerate>:
-- Colour representations in Accelerate (RGB, sRGB, HSV, and HSL).
--
-- * <https://hackage.haskell.org/package/gloss-accelerate gloss-accelerate>:
-- Generate <https://hackage.haskell.org/package/gloss gloss> pictures from
-- Accelerate.
--
-- * <https://hackage.haskell.org/package/gloss-raster-accelerate gloss-raster-accelerate>:
-- Parallel rendering of raster images and animations.
--
-- * <https://hackage.haskell.org/package/lens-accelerate lens-accelerate>:
-- <https://hackage.haskell.org/package/lens Lens> operators for Accelerate
-- types.
--
-- * <https://hackage.haskell.org/package/linear-accelerate linear-accelerate>:
-- <https://hackage.haskell.org/package/linear Linear> vector space types for
-- Accelerate.
--
-- * <https://hackage.haskell.org/package/mwc-random-accelerate mwc-random-accelerate>:
-- Generate Accelerate arrays filled with high-quality pseudorandom numbers.
--
-- [/Contact:/]
--
-- * Mailing list for both use and development discussion:
--
--     * <mailto:accelerate-haskell@googlegroups.com>
--     * http://groups.google.com/group/accelerate-haskell
--
-- * Bug reports: https://github.com/AccelerateHS/accelerate/issues
--
-- * Maintainers:
--
--     * Trevor L. McDonell: <mailto:tmcdonell@cse.unsw.edu.au>
--     * Manuel M T Chakravarty: <mailto:chak@cse.unsw.edu.au>
--
-- [/Tip:/]
--
-- Accelerate tends to stress GHC's garbage collector, so it helps to increase
-- the default GC allocation sizes. This can be done when running an executable
-- by specifying RTS options on the command line, for example:
--
-- > ./foo +RTS -A64M -n2M -RTS
--
-- You can make these settings the default by adding the following @ghc-options@
-- to your @.cabal@ file or similar:
--
-- > ghc-options: -with-rtsopts=-n2M -with-rtsopts=-A64M
--
-- To specify RTS options you will also need to compile your program with @-rtsopts@.
--

module Data.Array.Accelerate (

  -- * The /Accelerate/ Array Language
  -- ** Embedded array computations
  Acc,

  -- *** Arrays
  Array, Arrays, Scalar, Vector, Segments,

  -- *** Array elements
  Elt,

  -- *** Array shapes & indices
  -- $shapes_and_indices
  --
  Z(..), (:.)(..),
  DIM0, DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7, DIM8, DIM9,
  Shape, Slice, All(..), Any(..),
  -- Split(..), Divide(..), Division(..),

  -- ** Array access
  -- *** Element indexing
  (!), (!!), the,

  -- *** Shape information
  null, length, shape, size, shapeSize,

  -- ** Construction
  -- *** Introduction
  use, unit,

  -- *** Initialisation
  generate, fill,

  -- *** Enumeration
  enumFromN, enumFromStepN,

  -- *** Concatenation
  (++),

  -- ** Composition
  -- *** Flow control
  (?|), acond, awhile,
  IfThenElse(..),

  -- *** Controlling execution
  (>->),
  compute,

  -- ** Element-wise operations
  -- *** Indexing
  indexed,

  -- *** Mapping
  map, imap,

  -- *** Zipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6, zipWith7, zipWith8, zipWith9,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6, izipWith7, izipWith8, izipWith9,
  zip, zip3, zip4, zip5, zip6, zip7, zip8, zip9,

  -- *** Unzipping
  unzip, unzip3, unzip4, unzip5, unzip6, unzip7, unzip8, unzip9,

  -- ** Modifying Arrays
  -- *** Shape manipulation
  reshape, flatten,

  -- *** Replication
  replicate,

  -- *** Extracting sub-arrays
  slice,
  init, tail, take, drop, slit,

  -- *** Permutations
  -- **** Forward permutation (scatter)
  permute,
  ignore,
  scatter,

  -- **** Backward permutation (gather)
  backpermute,
  gather,

  -- **** Specialised permutations
  reverse, transpose,

  -- *** Filtering
  filter,

  -- ** Folding
  fold, fold1, foldAll, fold1All,

  -- *** Segmented reductions
  foldSeg, fold1Seg,

  -- *** Specialised reductions
  all, any, and, or, sum, product, minimum, maximum,

  -- ** Scans (prefix sums)
  scanl, scanl1, scanl', scanr, scanr1, scanr',
  prescanl, postscanl, prescanr, postscanr,

  -- *** Segmented scans
  scanlSeg, scanl1Seg, scanl'Seg, prescanlSeg, postscanlSeg,
  scanrSeg, scanr1Seg, scanr'Seg, prescanrSeg, postscanrSeg,

  -- ** Stencils
  stencil, stencil2,

  -- *** Stencil Specification
  Stencil, Boundary(..),

  -- *** Common stencil patterns
  Stencil3, Stencil5, Stencil7, Stencil9,
  Stencil3x3, Stencil5x3, Stencil3x5, Stencil5x5,
  Stencil3x3x3, Stencil5x3x3, Stencil3x5x3, Stencil3x3x5, Stencil5x5x3, Stencil5x3x5,
  Stencil3x5x5, Stencil5x5x5,

  -- ** Sequence data types
  Seq,

  -- ** Sequence elimination
  collect,

  -- ** Sequence producers
  streamIn, subarrays, produce, produceScalar, fromSegs, fromShapes, fromOffsets,
  toSeq, toSeqInner, toSeq2ndInner, toSeqOuter,

  -- ** Sequence transducers
  mapSeq, zipWithSeq, mapSeqE, zipWithSeqE, zipSeq, unzipSeq,

  -- ** Sequence consumers
  foldSeqE, fromSeq, elements, shapes, tabulate, foldBatch, foldSeqFlatten,

  -- ---------------------------------------------------------------------------
  -- * The /Accelerate/ Expression Language
  -- ** Scalar data types
  Exp,

  -- ** Type classes
  -- *** Basic type classes
  Eq(..),
  Ord(..),
  -- Enum, -- vacuous
  Bounded, minBound, maxBound,

  -- *** Numeric type classes
  Num, (+), (-), (*), negate, abs, signum, fromInteger,
  -- Real, -- vacuous
  Integral, quot, rem, div, mod, quotRem, divMod,
  Fractional, (/), recip, fromRational,
  Floating, pi, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh, exp, sqrt, log, (**), logBase,
  RealFrac(..), div', mod', divMod',
  RealFloat(..),

  -- *** Numeric conversion classes
  FromIntegral(..),
  ToFloating(..),

  -- ** Lifting and Unlifting
  -- $lifting_and_unlifting
  --
  Lift(..), Unlift(..),
  lift1, lift2, lift3,
  ilift1, ilift2, ilift3,

  -- ** Scalar operations
  -- *** Introduction
  constant,

  -- *** Tuples
  fst, afst, snd, asnd, curry, uncurry,

  -- *** Flow control
  (?), caseof, cond, while, iterate,

  -- *** Scalar reduction
  sfoldl,

  -- *** Logical operations
  (&&), (||), not,

  -- *** Numeric operations
  subtract, even, odd, gcd, lcm, (^), (^^),

  -- *** Shape manipulation
  index0, index1, unindex1, index2, unindex2, index3, unindex3,
  indexHead, indexTail,
  toIndex, fromIndex,
  intersect, indexTrans,
  toSlice,

  -- *** Conversions
  ord, chr, boolToInt, bitcast,

  -- ---------------------------------------------------------------------------
  -- * Foreign Function Interface (FFI)
  foreignAcc,
  foreignExp,
  VectorisedForeign(..), LiftedType(..),

  -- ---------------------------------------------------------------------------
  -- * Plain arrays
  -- ** Operations
  arrayRank, arrayShape, arraySize, indexArray,

  -- ** Getting data in
  -- $getting_data_in

  -- *** Function
  fromFunction,

  -- *** Lists
  fromList, toList,

  -- ---------------------------------------------------------------------------
  -- * Prelude re-exports
  (.), ($), error, undefined, const,

  -- ---------------------------------------------------------------------------
  -- Types
  Int, Int8, Int16, Int32, Int64,
  Word, Word8, Word16, Word32, Word64,
  Float, Double,
  Bool(..), Char,

  CFloat, CDouble,
  CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong,
  CChar, CSChar, CUChar,

  -- | Avoid using these in your own functions wherever possible.
  IsScalar, IsNum, IsBounded, IsIntegral, IsFloating, IsNonNum,

) where

-- friends
import Data.Array.Accelerate.Array.Sugar                            hiding ( (!), rank, shape, size, toIndex, fromIndex, intersect, ignore, transpose, toSlice )
import Data.Array.Accelerate.Classes
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Prelude
import Data.Array.Accelerate.Trafo                                  () -- show instances
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Sugar                  as S

-- re-exported from D.A.A.Classes.Num but not found (GHC<8 bug)
import Prelude                                                      ( (.), ($), undefined, error, const, fromInteger )


-- Renamings
-- ---------
--
-- FIXME: these all need to go into a separate module for separate importing!

-- rename as '(!)' is already used by the EDSL for indexing

-- | Array indexing in plain Haskell code.
--
indexArray :: Array sh e -> sh -> e
indexArray = (S.!)

-- | Rank of an array.
--
arrayRank :: Shape sh => sh -> Int
arrayRank = S.rank

-- |Array shape in plain Haskell code.
--
arrayShape :: Shape sh => Array sh e -> sh
arrayShape = S.shape
-- rename as 'shape' is already used by the EDSL to query an array's shape

-- | Total number of elements in an array of the given 'Shape'.
--
arraySize :: Shape sh => sh -> Int
arraySize = S.size

-- Named documentation chunks
-- --------------------------

-- $shapes_and_indices
--
-- Operations in Accelerate take the form of collective operations over arrays
-- of the type @'Array' sh e@. Much like the
-- <https://hackage.haskell.org/package/repa repa> library, arrays in Accelerate
-- are parameterised by a type /sh/ which determines the dimensionality of the
-- array and the type of each index, as well as the type of each element of the
-- array /e/.
--
-- Shape types, and multidimensional array indices, are built like lists
-- (technically; a heterogeneous snoc-list) using 'Z' and (':.'):
--
-- > data Z = Z
-- > data tail :. head = tail :. head
--
-- Here, the constructor 'Z' corresponds to a shape with zero dimension (or
-- a 'Scalar' array, with one element) and is used to mark the end of the list.
-- The constructor (':.') adds additional dimensions to the shape on the
-- /right/. For example:
--
-- > Z :. Int
--
-- is the type of the shape of a one-dimensional array ('Vector') indexed by an
-- 'Int', while:
--
-- > Z :. Int :. Int
--
-- is the type of the shape of a two-dimensional array (a matrix) indexed by an
-- 'Int' in each dimension.
--
-- This style is used to construct both the /type/ and /value/ of the shape. For
-- example, to define the shape of a vector of ten elements:
--
-- > sh :: Z :. Int
-- > sh = Z :. 10
--
-- Note that the right-most index is the /innermost/ dimension. This is the
-- fastest-varying index, and corresponds to the elements of the array which are
-- adjacent in memory.
--

-- $lifting_and_unlifting
--
-- A value of type 'Int' is a plain Haskell value (unlifted), whereas an @Exp
-- Int@ is a /lifted/ value, that is, an integer lifted into the domain of
-- embedded expressions (an abstract syntax tree in disguise). Both 'Acc' and
-- 'Exp' are /surface types/ into which values may be lifted. Lifting plain
-- array and scalar surface types is equivalent to 'use' and 'constant'
-- respectively.
--
-- In general an @Exp Int@ cannot be unlifted into an 'Int', because the actual
-- number will not be available until a later stage of execution (e.g. during
-- GPU execution, when 'run' is called). Similarly an @Acc array@ can not be
-- unlifted to a vanilla 'array'; you should instead 'run' the expression with
-- a specific backend to evaluate it.
--
-- Lifting and unlifting are also used to pack and unpack an expression into and
-- out of constructors such as tuples, respectively. Those expressions, at
-- runtime, will become tuple dereferences. For example:
--
-- >>> let sh = constant (Z :. 4 :. 10)   :: Exp DIM2
-- >>> let Z :. x :. y = unlift sh        :: Z :. Exp Int :. Exp Int
-- >>> let t = lift (x,y)                 :: Exp (Int, Int)
--
-- >>> let r  = scanl' f z xs             :: (Acc (Vector Int), Acc (Scalar Int))
-- >>> let r' = lift r                    :: Acc (Vector Int, Scalar Int)
--
-- [/Note:/]
--
-- Use of 'lift' and 'unlift' is probably the most common source of type errors
-- when using Accelerate. GHC is not very good at determining the type the
-- [un]lifted expression should have, so it is often necessary to add an
-- explicit type signature.
--
-- For example, in the following GHC will complain that it can not determine the
-- type of 'y', even though we might expect that to be obvious (or for it to not
-- care):
--
-- > fst :: (Elt a, Elt b) => Exp (a,b) -> Exp a
-- > fst t = let (x,y) = unlift t in x
--
-- The fix is to instead add an explicit type signature. Note that this requires
-- the @ScopedTypeVariables@ extension and to bring the type variables @a@ and
-- @b@ into scope with @forall@:
--
-- > fst :: forall a b. (Elt a, Elt b) => Exp (a,b) -> Exp a
-- > fst t = let (x,y) = unlift t  :: (Exp a, Exp b)
-- >         in x
--

-- $getting_data_in
-- #getting_data_in#
--
-- We often need to generate or read data into an 'Array' so that it can be used
-- in Accelerate. The base @accelerate@ library includes basic conversions
-- routines, but for additional functionality see the
-- <http://hackage.haskell.org/package/accelerate-io accelerate-io> package,
-- which includes conversions between:
--
--  * <https://hackage.haskell.org/package/repa repa>: another Haskell library for high-performance parallel arrays
--  * <https://hackage.haskell.org/package/vector vector>: efficient boxed and unboxed one-dimensional arrays
--  * <https://hackage.haskell.org/package/array array>: immutable arrays
--  * <https://hackage.haskell.org/package/bmp BMP>: uncompressed BMP image files
--  * <https://hackage.haskell.org/package/bytestring bytestring> compact, immutable binary data
--  * As well as copying data directly from raw 'Foreign.Ptr.Ptr's
--
