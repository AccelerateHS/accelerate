-- |
-- Module      : Data.Array.Accelerate
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--               [2009..2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
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
  L.Acc, S.Arrays, S.Array, S.Scalar, S.Vector, S.Segments,

  -- ** Array element types
  S.Elt,

  -- ** Shapes & Indices
  --
  -- | Array indices are snoc type lists; that is, they are backwards and the
  -- end-of-list token, `Z`, occurs on the left. For example, the type of a
  -- rank-2 array index is @Z :. Int :. Int@.
  --
  S.Z(..), (S.:.)(..), S.Shape, S.All(..), S.Any(..), S.Slice(..),
  S.DIM0, S.DIM1, S.DIM2, S.DIM3, S.DIM4, S.DIM5, S.DIM6, S.DIM7, S.DIM8, S.DIM9,

  -- ** Accessors
  -- *** Indexing
  (L.!), (L.!!), P.the,

  -- *** Shape information
  P.null, L.shape, L.size, L.shapeSize,

  -- *** Extracting sub-arrays
  L.slice,
  P.init, P.tail, P.take, P.drop, P.slit,

  -- ** Construction
  -- *** Introduction
  L.use, L.unit,

  -- *** Initialisation
  L.generate, L.replicate, P.fill,

  -- *** Enumeration
  P.enumFromN, P.enumFromStepN,

  -- *** Concatenation
  (P.++),

  -- ** Composition
  -- *** Flow control
  (P.?|), L.acond, L.awhile,

  -- *** Pipelining
  (L.>->),

  -- ** Modifying Arrays
  -- *** Shape manipulation
  L.reshape, P.flatten,

  -- *** Permutations
  L.permute, L.backpermute, L.ignore,

  -- *** Specialised permutations
  P.reverse, P.transpose,

  -- ** Element-wise operations
  -- *** Mapping
  L.map,

  -- *** Zipping
  L.zipWith, P.zipWith3, P.zipWith4, P.zipWith5, P.zipWith6, P.zipWith7, P.zipWith8, P.zipWith9,
  P.zip, P.zip3, P.zip4, P.zip5, P.zip6, P.zip7, P.zip8, P.zip9,

  -- *** Unzipping
  P.unzip, P.unzip3, P.unzip4, P.unzip5, P.unzip6, P.unzip7, P.unzip8, P.unzip9,

  -- ** Working with predicates
  -- *** Filtering
  P.filter,

  -- *** Scatter
  P.scatter, P.scatterIf,

  -- *** Gather
  P.gather,  P.gatherIf,

  -- ** Folding
  L.fold, L.fold1, P.foldAll, P.fold1All,

  -- *** Segmented reductions
  L.foldSeg, L.fold1Seg,

  -- *** Specialised folds
  P.all, P.any, P.and, P.or, P.sum, P.product, P.minimum, P.maximum,

  -- ** Prefix sums (scans)
  L.scanl, L.scanl1, L.scanl', L.scanr, L.scanr1, L.scanr',
  P.prescanl, P.postscanl, P.prescanr, P.postscanr,

  -- *** Segmented scans
  P.scanlSeg, P.scanl1Seg, P.scanl'Seg, P.prescanlSeg, P.postscanlSeg,
  P.scanrSeg, P.scanr1Seg, P.scanr'Seg, P.prescanrSeg, P.postscanrSeg,

  -- ** Stencil
  L.stencil, L.stencil2,

  -- *** Specification
  L.Stencil, L.Boundary(..),

  -- *** Common stencil patterns
  L.Stencil3, L.Stencil5, L.Stencil7, L.Stencil9,
  L.Stencil3x3, L.Stencil5x3, L.Stencil3x5, L.Stencil5x5,
  L.Stencil3x3x3, L.Stencil5x3x3, L.Stencil3x5x3, L.Stencil3x3x5, L.Stencil5x5x3, L.Stencil5x3x5,
  L.Stencil3x5x5, L.Stencil5x5x5,

  -- ** Foreign
  L.foreignAcc, L.foreignAcc2, L.foreignAcc3,
  L.foreignExp, L.foreignExp2, L.foreignExp3,

  -- ---------------------------------------------------------------------------

  -- * The /Accelerate/ Expression Language
  -- ** Scalar data types
  L.Exp,

  -- ** Type classes
  T.IsScalar, T.IsNum, T.IsBounded, T.IsIntegral, T.IsFloating, T.IsNonNum,

  -- ** Element types
  T.Int, T.Int8, T.Int16, T.Int32, T.Int64, T.Word, T.Word8, T.Word16, T.Word32, T.Word64,
  T.CShort, T.CUShort, T.CInt, T.CUInt, T.CLong, T.CULong, T.CLLong, T.CULLong,
  Float, Double, T.CFloat, T.CDouble,
  Bool, Char, T.CChar, T.CSChar, T.CUChar,

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
  P.Lift(..), P.Unlift(..), P.lift1, P.lift2, P.ilift1, P.ilift2,

  -- ** Operations
  --
  -- | Some of the standard Haskell 98 typeclass functions need to be
  -- reimplemented because their types change. If so, function names kept the
  -- same and infix operations are suffixed by an asterisk. If not reimplemented
  -- here, the standard typeclass instances apply.
  --

  -- *** Introduction
  L.constant,

  -- *** Tuples
  P.fst, P.snd, P.curry, P.uncurry,

  -- *** Flow control
  (P.?), P.caseof, L.cond, L.while, P.iterate,

  -- *** Scalar reduction
  P.sfoldl,

  -- *** Basic operations
  (L.&&*), (L.||*), L.not,
  (L.==*), (L./=*), (L.<*), (L.<=*), (L.>*), (L.>=*),

  -- *** Numeric functions
  L.truncate, L.round, L.floor, L.ceiling, L.even, L.odd,

  -- *** Bitwise functions
  L.bit, L.setBit, L.clearBit, L.complementBit, L.testBit,
  L.shift,  L.shiftL,  L.shiftR,
  L.rotate, L.rotateL, L.rotateR,

  -- *** Shape manipulation
  P.index0, P.index1, P.unindex1, P.index2, P.unindex2,
  L.indexHead, L.indexTail,
  L.toIndex, L.fromIndex,
  L.intersect,

  -- *** Conversions
  L.ord, L.chr, L.boolToInt, L.fromIntegral,

  -- ---------------------------------------------------------------------------

  -- * Plain arrays
  -- ** Operations
  arrayDim, arrayShape, arraySize, indexArray,

  -- ** Conversions
  --
  -- | For additional conversion routines, see the accelerate-io package:
  -- <http://hackage.haskell.org/package/accelerate-io>

  -- *** Function
  fromFunction,

  -- *** Lists
  S.fromList, S.toList,

  -- *** 'Data.Array.IArray.IArray'
  S.fromIArray, S.toIArray,

) where

-- friends
import Data.Array.Accelerate.Trafo                  () -- show instances
import qualified Data.Array.Accelerate.Array.Sugar  as S
import qualified Data.Array.Accelerate.Language     as L
import qualified Data.Array.Accelerate.Prelude      as P
import qualified Data.Array.Accelerate.Type         as T

-- system
import Prelude (Float, Double, Bool, Char)
import qualified Prelude


-- Renamings
--

-- FIXME: these all need to go into a separate module for separate importing!

-- rename as '(!)' is already used by the EDSL for indexing

-- |Array indexing in plain Haskell code.
--
indexArray :: S.Array sh e -> sh -> e
indexArray = (S.!)

-- | Rank of an array.
--
arrayDim :: S.Shape sh => sh -> T.Int
arrayDim = S.dim
-- FIXME: Rename to rank

-- |Array shape in plain Haskell code.
--
arrayShape :: S.Shape sh => S.Array sh e -> sh
arrayShape = S.shape
-- rename as 'shape' is already used by the EDSL to query an array's shape

-- | Total number of elements in an array of the given 'Shape'.
--
arraySize :: S.Shape sh => sh -> T.Int
arraySize = S.size

-- | Create an array from its representation function.
--
{-# INLINE fromFunction #-}
fromFunction :: (S.Shape sh, S.Elt e) => sh -> (sh -> e) -> S.Array sh e
fromFunction = S.newArray

