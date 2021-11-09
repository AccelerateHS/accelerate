{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate
-- Description : The Accelerate standard prelude
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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
-- [/Examples:/]
--
-- * A short tutorial-style example for generating a <https://en.wikipedia.org/wiki/Mandelbrot_set Mandelbrot set>:
--   http://www.acceleratehs.org/examples/mandelbrot.html
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
--      <<https://i.imgur.com/bIkODKd.jpg>>
--
-- [/Starting a new project:/]
--
-- Accelerate and its associated packages are available on both Hackage and
-- Stackage. A project template is available to help create a new projects using
-- the <https://docs.haskellstack.org/en/stable/README/ stack> build tool. To
-- create a new project using the template:
--
-- > stack new PROJECT_NAME https://github.com/AccelerateHS/accelerate/raw/stable/accelerate.hsfiles
--
-- [/Additional components:/]
--
-- * <https://hackage.haskell.org/package/accelerate-io accelerate-io>: Fast
-- conversion between Accelerate arrays and other formats (e.g. Repa, Vector).
--
-- * <https://hackage.haskell.org/package/accelerate-fft accelerate-fft>: Fast
-- Fourier transform, with FFI bindings to optimised implementations.
--
-- * <https://hackage.haskell.org/package/accelerate-blas accelerate-blas>: BLAS
-- and LAPACK operations, with FFI bindings to optimised implementations.
--
-- * <https://hackage.haskell.org/package/accelerate-bignum accelerate-bignum>:
-- Fixed-width large integer arithmetic.
--
-- * <https://hackage.haskell.org/package/containers-accelerate containers-accelerate>:
-- Container types for use with Accelerate.
--
-- * <https://hackage.haskell.org/package/hashable-accelerate hashable-accelerate>:
-- Class for types which can be converted to a value.
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
-- * Maintainer: Trevor L. McDonell: <mailto:trevor.mcdonell@gmail.com>
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
  Array, Arrays, Scalar, Vector, Matrix, Segments,

  -- *** Array elements
  Elt,

  -- *** Array shapes & indices
  -- $shapes_and_indices
  --
  Z(..), (:.)(..),
  DIM0, DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7, DIM8, DIM9,
  Shape, Slice(..), All(..), Any(..),
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
  (++), concatOn,

  -- *** Expansion
  expand,

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
  initOn, tailOn, takeOn, dropOn, slitOn,

  -- *** Permutations
  -- **** Forward permutation (scatter)
  permute,
  scatter,

  -- **** Backward permutation (gather)
  backpermute,
  gather,

  -- **** Specialised permutations
  reverse, transpose,
  reverseOn, transposeOn,

  -- *** Filtering
  filter, compact,

  -- ** Folding
  fold, fold1, foldAll, fold1All,

  -- *** Segmented reductions
  foldSeg,  fold1Seg,
  foldSeg', fold1Seg',

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

  -- *** Stencil specification
  Stencil, Boundary,
  clamp, mirror, wrap, function,

  -- *** Common stencil patterns
  Stencil3, Stencil5, Stencil7, Stencil9,
  Stencil3x3, Stencil5x3, Stencil3x5, Stencil5x5,
  Stencil3x3x3, Stencil5x3x3, Stencil3x5x3, Stencil3x3x5, Stencil5x5x3, Stencil5x3x5,
  Stencil3x5x5, Stencil5x5x5,

  -- -- ** Sequence operations
  -- collect,

  -- -- ** Sequence producers
  -- streamIn, toSeq, generateSeq,

  -- -- ** Sequence transducers
  -- mapSeq, zipWithSeq, scanSeq,

  -- -- ** Sequence consumers
  -- foldSeq, foldSeqFlatten, fromSeq, fromSeqElems, fromSeqShapes,
  -- toSeqInner, toSeqOuter2, toSeqOuter3,

  -- ---------------------------------------------------------------------------
  -- * The /Accelerate/ Expression Language
  -- ** Scalar data types
  Exp,

  -- ** SIMD vectors
  Vec, VecElt,

  -- ** Type classes
  -- *** Basic type classes
  Eq(..),
  Ord(..), Ordering(..), pattern LT_, pattern EQ_, pattern GT_,
  Enum, succ, pred,
  Bounded, minBound, maxBound,
  -- Functor(..), (<$>), ($>), void,
  -- Monad(..),

  -- *** Numeric type classes
  Num, (+), (-), (*), negate, abs, signum, fromInteger,
  Integral, quot, rem, div, mod, quotRem, divMod,
  Rational(..),
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

  -- ** Pattern synonyms
  -- $pattern_synonyms
  --
  pattern Pattern,
  pattern T2,  pattern T3,  pattern T4,  pattern T5,  pattern T6,
  pattern T7,  pattern T8,  pattern T9,  pattern T10, pattern T11,
  pattern T12, pattern T13, pattern T14, pattern T15, pattern T16,

  pattern Z_, pattern Ix, pattern (::.), pattern All_, pattern Any_,
  pattern I0, pattern I1, pattern I2, pattern I3, pattern I4,
  pattern I5, pattern I6, pattern I7, pattern I8, pattern I9,

  pattern Vec2, pattern V2,
  pattern Vec3, pattern V3,
  pattern Vec4, pattern V4,
  pattern Vec8, pattern V8,
  pattern Vec16, pattern V16,

  mkPattern, mkPatterns,

  -- ** Scalar operations
  -- *** Introduction
  constant,

  -- *** Tuples
  fst, afst, snd, asnd, curry, uncurry,

  -- *** Flow control
  (?), match, cond, while, iterate,

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
  intersect,

  -- *** Conversions
  ord, chr, boolToInt, bitcast,

  -- ** Annotations
  HasAnnotations(),
  TraverseAnnotations(),
  context, alwaysInline, unrollIters,

  -- ---------------------------------------------------------------------------
  -- * Foreign Function Interface (FFI)
  foreignAcc,
  foreignExp,

  -- ---------------------------------------------------------------------------
  -- * Plain arrays
  -- ** Operations
  arrayRank, arrayShape, arraySize, arrayReshape,
  indexArray, linearIndexArray,

  -- ** Getting data in
  -- $getting_data_in

  -- *** Function
  fromFunction,
  fromFunctionM,

  -- *** Lists
  fromList, toList,

  -- ---------------------------------------------------------------------------
  -- * Useful re-exports
  (.), ($), (&), flip, error, undefined, const, otherwise,
  Show, Generic, HasCallStack,
  fromString, -- -XOverloadedStrings
  fromListN,  -- -XOverloadedLists

  -- ---------------------------------------------------------------------------
  -- Types
  Int, Int8, Int16, Int32, Int64,
  Word, Word8, Word16, Word32, Word64,
  Half(..), Float, Double,
  Bool(..),   pattern True_,    pattern False_,
  Maybe(..),  pattern Nothing_, pattern Just_,
  Either(..), pattern Left_,    pattern Right_,
  Char,

  CFloat, CDouble,
  CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong,
  CChar, CSChar, CUChar,

) where

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.Classes.Bounded
import Data.Array.Accelerate.Classes.Enum
import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Floating
import Data.Array.Accelerate.Classes.Fractional
import Data.Array.Accelerate.Classes.FromIntegral
import Data.Array.Accelerate.Classes.Integral
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Classes.Rational
import Data.Array.Accelerate.Classes.RealFloat
import Data.Array.Accelerate.Classes.RealFrac
import Data.Array.Accelerate.Classes.ToFloating
import Data.Array.Accelerate.Data.Either
import Data.Array.Accelerate.Data.Maybe
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Pattern.TH
import Data.Array.Accelerate.Prelude
import Data.Array.Accelerate.Pretty                                 () -- show instances
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Array                            ( Array, Arrays, Scalar, Vector, Matrix, Segments, fromFunction, fromFunctionM, toList, fromList )
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape                            hiding ( size, toIndex, fromIndex, intersect )
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type
import Data.Primitive.Vec
import qualified Data.Array.Accelerate.Sugar.Array                  as S
import qualified Data.Array.Accelerate.Sugar.Shape                  as S

import Data.Function                                                ( (&) )
import Prelude                                                      ( (.), ($), Char, Show, flip, undefined, error, const, otherwise )

import GHC.Exts                                                     ( fromListN, fromString )
import GHC.Generics                                                 ( Generic )


-- $setup
-- >>> :seti -XTypeOperators
-- >>> import Data.Array.Accelerate.Interpreter
-- >>> :{
--   let runExp :: Elt e => Exp e -> e
--       runExp e = indexArray (run (unit e)) Z
-- :}

-- Renamings
-- ---------
--
-- FIXME: these all need to go into a separate module for separate importing!

-- rename as '(!)' is already used by the EDSL for indexing

-- | Array indexing in plain Haskell code.
--
{-# INLINE indexArray #-}
indexArray :: (Shape sh, Elt e) => Array sh e -> sh -> e
indexArray = (S.!)

-- | Linear array indexing in plain Haskell code.
--
{-# INLINE linearIndexArray #-}
linearIndexArray :: Elt e => Array sh e -> Int -> e
linearIndexArray = (S.!!)

-- | Rank of an array (as a plain Haskell value)
--
{-# INLINE arrayRank #-}
arrayRank :: forall sh e. Shape sh => Array sh e -> Int
arrayRank _ = S.rank @sh

-- | Shape of an array (as a plain Haskell value)
--
{-# INLINE arrayShape #-}
arrayShape :: Shape sh => Array sh e -> sh
arrayShape = S.shape
-- rename as 'shape' is already used by the EDSL to query an array's shape

-- | Total number of elements in an array (as a plain Haskell value)
--
{-# INLINE arraySize #-}
arraySize :: Shape sh => Array sh e -> Int
arraySize = S.size . S.shape

-- | Change the shape of an array without altering its contents. The 'arraySize'
-- of the source and result arrays must be identical.
--
{-# INLINE arrayReshape #-}
arrayReshape :: (Shape sh, Shape sh') => sh -> Array sh' e -> Array sh e
arrayReshape = S.reshape


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
-- >>> let xs = use $ fromList (Z:.10) [0..]   :: Acc (Vector Int)
-- >>> let ys = use $ fromList (Z:.3:.4) [0..] :: Acc (Matrix Int)
-- >>> let r  = (xs,ys)                        :: (Acc (Vector Int), Acc (Matrix Int))
-- >>> let r' = lift r                         :: Acc (Vector Int, Matrix Int)
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
-- For an alternative, see section <#pattern_synonyms Pattern synonyms>.
--

-- $pattern_synonyms
-- #pattern_synonyms#
--
-- Pattern synonyms can be used as an alternative to 'lift' and 'unlift' for
-- constructing and accessing data types isomorphic to simple product (tuple)
-- types.
--
-- In contrast to 'lift' and 'unlift' however, pattern synonyms do /not/ require
-- these data types to be fully polymorphic.
--
-- For example, let's say we have regular Haskell data type representing a point
-- in two-dimensional space:
--
-- > data Point = Point_ Float Float
-- >   deriving (Generic, Elt)
--
-- Here we derive instance an instance of the 'Elt' class (via 'Generic'),
-- so that this data type can be used within scalar Accelerate expressions
--
-- In order to access the individual fields of the data constructor from within
-- an Accelerate expression, we define the following pattern synonym:
--
-- > pattern Point :: Exp Float -> Exp Float -> Exp Point
-- > pattern Point x y = Pattern (x,y)
--
-- Notice how we named the constructor of our original datatype with a trailing
-- underscore, so that we can use the undecorated name for the pattern synonym;
-- these must have unique names.
--
-- In essence, the 'Pattern' pattern is really telling GHC how to treat our @Point@
-- type as a regular pair for use in Accelerate code. The pattern can then be
-- used on both the left and right hand side of an expression:
--
-- > addPoint :: Exp Point -> Exp Point -> Exp Point
-- > addPoint (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)
--
-- Similarly, we can define pattern synonyms for values in 'Acc'. We can also
-- use record syntax to generate field accessors, if we desire:
--
-- > data SparseVector a = SparseVector_ (Vector Int) (Vector a)
-- >   deriving (Generic, Arrays)
-- >
-- > pattern SparseVector :: Elt a => Acc (Vector Int) -> Acc (Vector a) -> Acc (SparseVector a)
-- > pattern SparseVector { indices, values } = Pattern (indices, values)
--
-- For convenience, we have defined several pattern synonyms for regular tuples,
-- 'T2' (for pairs), 'T3' (for triples), and so on up to 'T16'. These are
-- occasionally more convenient to use than 'lift' and 'unlift' together with
-- the regular tuple syntax.
--
-- @since 1.3.0.0
--

-- $getting_data_in
-- #getting_data_in#
--
-- We often need to generate or read data into an 'Array' so that it can be used
-- in Accelerate. The base @accelerate@ library includes basic conversions
-- routines, but additional functionality is contained in external
-- libraries, for example:
--
--  * <https://hackage.haskell.org/package/accelerate-io accelerate-io>: For copying data directly from raw 'Foreign.Ptr.Ptr's
--  * <https://hackage.haskell.org/package/accelerate-io-array accelerate-io-array>: immutable arrays
--  * <https://hackage.haskell.org/package/accelerate-io-bmp accelerate-io-bmp>: uncompressed BMP image files
--  * <https://hackage.haskell.org/package/accelerate-io-bytestring accelerate-io-bytestring>: compact, immutable binary data
--  * <https://hackage.haskell.org/package/accelerate-io-cereal accelerate-io-cereal>: binary serialisation of arrays using <https://hackage.haskell.org/package/cereal cereal>
--  * <https://hackage.haskell.org/package/accelerate-io-JuicyPixels accelerate-io-JuicyPixels>: images in various pixel formats
--  * <https://hackage.haskell.org/package/accelerate-io-repa accelerate-io-repa>: another Haskell library for high-performance parallel arrays
--  * <https://hackage.haskell.org/package/accelerate-io-serialise accelerate-io-serialise>: binary serialisation of arrays using <https://hackage.haskell.org/package/serialise serialise>
--  * <https://hackage.haskell.org/package/accelerate-io-vector accelerate-io-vector>: efficient boxed and unboxed one-dimensional arrays
--

