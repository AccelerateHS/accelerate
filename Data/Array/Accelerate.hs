-- |
-- Module      : Data.Array.Accelerate
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2008..2009] Sean Lee
--               [2009..2014] Trevor L. McDonell
--               [2013..2014] Robert Clifton-Everest
--               [2014..2014] Frederik M. Madsen
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module defines an embedded language of array computations for
-- high-performance computing. Computations on multi-dimensional, regular
-- arrays are expressed in the form of parameterised collective operations
-- (such as maps, reductions, and permutations). These computations are online
-- compiled and executed on a range of architectures.
--
-- [/Abstract interface:/]
--
-- The types representing array computations are only exported abstractly —
-- i.e., client code can generate array computations and submit them for
-- execution, but it cannot inspect these computations. This is to allow for
-- more flexibility for future extensions of this library.
--
-- [/Code execution:/]
--
-- Access to the various backends is via a 'run' function in backend-specific
-- top level modules. Currently, we have the following:
--
-- * "Data.Array.Accelerate.Interpreter": simple interpreter in Haskell as a
--   reference implementation defining the semantics of the Accelerate language
--
-- * "Data.Array.Accelerate.CUDA": an implementation supporting parallel
--   execution on CUDA-capable NVIDIA GPUs
--
-- [/Examples and documentation:/]
--
-- * A (draft) tutorial is available on the GitHub wiki:
--   <https://github.com/AccelerateHS/accelerate/wiki>
--
-- * The @accelerate-examples@ package demonstrates a range of computational
--   kernels and several complete applications:
--   <http://hackage.haskell.org/package/accelerate-examples>
--

module Data.Array.Accelerate (

  -- * The /Accelerate/ Array Language
  -- ** Array data types
  Acc, Seq, Arrays, Array, Scalar, Vector, Segments,

  -- ** Array element types
  Elt,

  -- ** Shapes & Indices
  --
  -- | Array indices are snoc type lists; that is, they are backwards and the
  -- end-of-list token, `Z`, occurs on the left. For example, the type of a
  -- rank-2 array index is @Z :. Int :. Int@.
  --
  Z(..), (:.)(..), Shape, All(..), Any(..), Split(..), Divide(..), Slice(..), Division(..),
  DIM0, DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7, DIM8, DIM9,

  -- ** Accessors
  -- *** Indexing
  (!), (!!), the,

  -- *** Shape information
  null, length, shape, size, shapeSize,

  -- *** Extracting sub-arrays
  slice,
  init, tail, take, drop, slit,

  -- ** Construction
  -- *** Introduction
  use, unit,

  -- *** Initialisation
  generate, replicate, fill,

  -- *** Enumeration
  enumFromN, enumFromStepN,

  -- *** Concatenation
  (++),

  -- ** Composition
  -- *** Flow control
  (?|), acond, awhile,
  IfThenElse(..),

  -- *** Pipelining
  (>->),

  -- *** Controlling execution
  compute,

  -- ** Modifying Arrays
  -- *** Shape manipulation
  reshape, flatten,

  -- *** Permutations
  permute, backpermute, ignore,

  -- *** Specialised permutations
  reverse, transpose,

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

  -- ** Working with predicates
  -- *** Filtering
  filter,

  -- *** Scatter
  scatter, scatterIf,

  -- *** Gather
  gather,  gatherIf,

  -- ** Folding
  fold, fold1, foldAll, fold1All,

  -- *** Segmented reductions
  foldSeg, fold1Seg,

  -- *** Specialised folds
  all, any, and, or, sum, product, minimum, maximum,

  -- ** Prefix sums (scans)
  scanl, scanl1, scanl', scanr, scanr1, scanr',
  prescanl, postscanl, prescanr, postscanr,

  -- *** Segmented scans
  scanlSeg, scanl1Seg, scanl'Seg, prescanlSeg, postscanlSeg,
  scanrSeg, scanr1Seg, scanr'Seg, prescanrSeg, postscanrSeg,

  -- ** Stencil
  stencil, stencil2,

  -- ** Sequence elimination
  collect,

  -- ** Sequence producers
  streamIn, toSeq, generateSeq,

  -- ** Sequence transducers
  mapSeq, zipWithSeq, scanSeq,

  -- ** Sequence consumers
  foldSeq, foldSeqFlatten, fromSeq, fromSeqElems, fromSeqShapes,
  toSeqInner, toSeqOuter2, toSeqOuter3,

  -- *** Specification
  Stencil, Boundary(..),

  -- *** Common stencil patterns
  Stencil3, Stencil5, Stencil7, Stencil9,
  Stencil3x3, Stencil5x3, Stencil3x5, Stencil5x5,
  Stencil3x3x3, Stencil5x3x3, Stencil3x5x3, Stencil3x3x5, Stencil5x5x3, Stencil5x3x5,
  Stencil3x5x5, Stencil5x5x5,

  -- ** Foreign
  foreignAcc, foreignAcc2, foreignAcc3,
  foreignExp, foreignExp2, foreignExp3,

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
  RealFrac(..),
  RealFloat(..),

  -- *** Numeric conversion classes
  FromIntegral(..),
  ToFloating(..),

  -- *** Primitive types
  --
  -- Avoid using these in your own functions wherever possible.
  IsScalar, IsNum, IsBounded, IsIntegral, IsFloating, IsNonNum,

  -- ** Element types
  Int, Int8, Int16, Int32, Int64,
  Word, Word8, Word16, Word32, Word64,
  Float, Double,
  Bool(..), Char,

  CFloat, CDouble,
  CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong,
  CChar, CSChar, CUChar,

  -- ** Lifting and Unlifting

  -- | A value of type `Int` is a plain Haskell value (unlifted), whereas an
  -- @Exp Int@ is a /lifted/ value, that is, an integer lifted into the domain
  -- of expressions (an abstract syntax tree in disguise). Both `Acc` and `Exp`
  -- are /surface types/ into which values may be lifted. Lifting plain array
  -- and scalar surface types is equivalent to 'use' and 'constant'
  -- respectively.
  --
  -- In general an @Exp Int@ cannot be unlifted into an `Int`, because the
  -- actual number will not be available until a later stage of execution (e.g.
  -- during GPU execution, when `run` is called). Similarly an @Acc array@ can
  -- not be unlifted to a vanilla `array`; you should instead `run` the
  -- expression with a specific backend to evaluate it.
  --
  -- Lifting and unlifting are also used to pack and unpack an expression into
  -- and out of constructors such as tuples, respectively. Those expressions, at
  -- runtime, will become tuple dereferences. For example:
  --
  -- > Exp (Z :. Int :. Int)
  -- >     -> unlift    :: (Z :. Exp Int :. Exp Int)
  -- >     -> lift      :: Exp (Z :. Int :. Int)
  -- >     -> ...
  --
  -- > Acc (Scalar Int, Vector Float)
  -- >     -> unlift    :: (Acc (Scalar Int), Acc (Vector Float))
  -- >     -> lift      :: Acc (Scalar Int, Vector Float)
  -- >     -> ...
  --
  Lift(..), Unlift(..),
  lift1, lift2, lift3,
  ilift1, ilift2, ilift3,

  -- ** Operations
  --
  -- | Some of the standard Haskell 98 typeclass functions need to be
  -- reimplemented because their types change. If so, function names kept the
  -- same and infix operations are suffixed by an asterisk. If not reimplemented
  -- here, the standard typeclass instances apply.
  --

  -- *** Introduction
  constant,

  -- *** Tuples
  fst, afst, snd, asnd, curry, uncurry,

  -- *** Flow control
  (?), caseof, cond, while, iterate,

  -- *** Scalar reduction
  sfoldl,

  -- *** Basic operations
  (&&*), (||*), not,

  -- *** Shape manipulation
  index0, index1, unindex1, index2, unindex2, index3, unindex3,
  indexHead, indexTail,
  toIndex, fromIndex,
  intersect,

  -- *** Conversions
  ord, chr, boolToInt, bitcast,

  -- ---------------------------------------------------------------------------

  -- * Plain arrays
  -- ** Operations
  arrayRank, arrayShape, arraySize, indexArray,

  -- ** Conversions
  --
  -- | For additional conversion routines, see the accelerate-io package:
  -- <http://hackage.haskell.org/package/accelerate-io>

  -- *** Function
  fromFunction,

  -- *** Lists
  fromList, toList,

  -- *** 'Data.Array.IArray.IArray'
  fromIArray, toIArray,

) where

-- friends
import Data.Array.Accelerate.Array.Sugar                            hiding ( (!), rank, shape, size, toIndex, fromIndex, intersect, ignore )
import Data.Array.Accelerate.Classes
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Prelude
import Data.Array.Accelerate.Trafo                                  () -- show instances
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Sugar                  as S

-- re-exported from D.A.A.Classes.Num but not found ??
import Prelude                                                      ( fromInteger )


-- Renamings
--

-- FIXME: these all need to go into a separate module for separate importing!

-- rename as '(!)' is already used by the EDSL for indexing

-- |Array indexing in plain Haskell code.
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

-- | Create an array from its representation function.
--
{-# INLINE fromFunction #-}
fromFunction :: (Shape sh, Elt e) => sh -> (sh -> e) -> Array sh e
fromFunction = newArray

