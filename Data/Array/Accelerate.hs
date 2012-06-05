{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
  -- only for the deprecated class aliases

-- |
-- Module      : Data.Array.Accelerate
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module defines an embedded language of array computations for
-- high-performance computing.  Computations on multi-dimensional, regular
-- arrays are expressed in the form of parameterised collective operations
-- (such as maps, reductions, and permutations).  These computations are online
-- compiled and executed on a range of architectures.
--
-- /Abstract interface/
--
-- The types representing array computations are only exported abstractly —
-- i.e., client code can generate array computations and submit them for
-- for execution, but it cannot inspect these computations.  This is to allow
-- for more flexibility for future extensions of this library.
--
-- /Code execution/
--
-- Access to the various backends is via a 'run' function in
-- backend-specific toplevel modules.  Currently, we have the following:
--
-- * "Data.Array.Accelerate.Interpreter": simple interpreter in Haskell as a
--   reference implementation defining the semantics of the Accelerate language
--
-- * "Data.Array.Accelerate.CUDA": an implementation supporting parallel
--    execution on CUDA-capable NVIDIA GPUs
--

module Data.Array.Accelerate (

  -- * The /Accelerate/ Array Language
  -- ** Array data types
  Acc, Arrays, Array, Scalar, Vector, Segments,

  -- ** Array element type
  Elt,

  -- ** Shapes & Indices
  --
  -- | Array indices are snoc type lists; that is, they are backwards and the
  -- end-of-list token, `Z`, occurs on the left. For example, the type of a
  -- rank-2 array index is @Z :. Int :. Int@.
  --
  Z(..), (:.)(..), Shape, All(..), Any(..), Slice(..),
  DIM0, DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7, DIM8, DIM9,

  -- ** Accessors
  -- *** Shape information
  shape, size, shapeSize,

  -- *** Indexing
  (!), the,

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

  -- ** Composition
  -- *** Flow control
  (?|), cond,

  -- *** Pipelining
  (>->),

  -- ** Modifying Arrays
  -- *** Shape manipulation
  reshape, flatten,

  -- *** Permutations
  permute, backpermute, ignore,

  -- ** Element-wise operations
  -- *** Mapping
  map,

  -- *** Zipping
  zipWith, zip, zip3, zip4,

  -- *** Unzipping
  unzip, unzip3, unzip4,

  -- ** Working with predicates
  -- *** Filtering
  filter,

  -- *** Scatter
  scatter, scatterIf,

  -- *** Gather
  gather,  gatherIf,

  -- ** Folding
  fold, foldAll, fold1, fold1All,

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

  -- *** Specification
  Stencil, Boundary(..),

  -- *** Common stencil patterns
  Stencil3, Stencil5, Stencil7, Stencil9,
  Stencil3x3, Stencil5x3, Stencil3x5, Stencil5x5,
  Stencil3x3x3, Stencil5x3x3, Stencil3x5x3, Stencil3x3x5, Stencil5x5x3, Stencil5x3x5,
  Stencil3x5x5, Stencil5x5x5,

  -- ---------------------------------------------------------------------------

  -- * The /Accelerate/ Expression Language
  -- ** Scalar data types
  Exp,

  -- ** Type classes
  IsScalar, IsNum, IsBounded, IsIntegral, IsFloating, IsNonNum,

  -- ** Element types
  Int, Int8, Int16, Int32, Int64, Word, Word8, Word16, Word32, Word64,
  CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong,
  Float, Double, CFloat, CDouble,
  Bool, Char, CChar, CSChar, CUChar,

  -- ** Lifting and Unlifting

  -- | A value of type `Int` is a plain Haskell value (unlifted), whereas an
  -- @Exp Int@ is a /lifted/ value, that is, an integer lifted into the domain
  -- of expressions (an abstract syntax tree in disguise).  Both `Acc` and `Exp`
  -- are /surface types/ into which values may be lifted.
  --
  -- In general an @Exp Int@ cannot be unlifted into an `Int`, because the
  -- actual number will not be available until a later stage of execution (e.g.
  -- GPU execution, when `run` is called).  However, in some cases unlifting
  -- makes sense.  For example, unlifting can convert, or unpack, an expression
  -- of tuple type into a tuple of expressions; those expressions, at runtime,
  -- will become tuple dereferences.
  --
  Lift(..), Unlift(..), lift1, lift2, ilift1, ilift2,

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
  fst, snd, curry, uncurry,

  -- *** Conditional
  (?),

  -- *** Basic operations
  (&&*), (||*), not,
  (==*), (/=*), (<*), (<=*), (>*), (>=*), max, min,

  -- *** Numeric functions
  truncate, round, floor, ceiling,

  -- *** Bitwise functions
  bit, setBit, clearBit, complementBit, testBit,
  shift,  shiftL,  shiftR,
  rotate, rotateL, rotateR,

  -- *** Shape manipulation
  index0, index1, unindex1, index2, unindex2,

  -- *** Conversions
  boolToInt, fromIntegral,

  -- ---------------------------------------------------------------------------

  -- * Plain arrays
  -- ** Operations
  arrayDim, arrayShape, arraySize, indexArray,

  -- ** Conversions
  --
  -- | For additional conversion routines, see the accelerate-io package:
  -- <http://hackage.haskell.org/package/accelerate-io>

  -- *** Lists
  fromList, toList,

  -- *** 'Data.Array.IArray.IArray'
  fromIArray, toIArray,

  -- * Miscellaneous
  -- ** Deprecated aliases
  Elem, Ix, SliceIx, tuple, untuple,

  -- ** Diagnostics
  initTrace

) where

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Debug
import Data.Array.Accelerate.Prelude
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Array.Sugar                hiding ((!), shape, dim, size, ignore)
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar

-- system
import Prelude (Float, Double, Bool, Char)
import qualified Prelude


-- Renamings
--

-- FIXME: these all need to go into a separate module for separate importing!

-- rename as '(!)' is already used by the EDSL for indexing

-- |Array indexing in plain Haskell code
--
indexArray :: Array sh e -> sh -> e
indexArray = (Sugar.!)

-- | Rank of an array
--
arrayDim :: Shape sh => sh -> Int
arrayDim = Sugar.dim
-- FIXME: Rename to rank

-- |Array shape in plain Haskell code
--
arrayShape :: Shape sh => Array sh e -> sh
arrayShape = Sugar.shape
-- rename as 'shape' is already used by the EDSL to query an array's shape

-- | Total number of elements in an array of the given 'Shape'
--
arraySize :: Shape sh => sh -> Int
arraySize = Sugar.size

-- Deprecated aliases for backwards compatibility
--

{-# DEPRECATED Elem "Use 'Elt' instead" #-}
class Elt e => Elem e
instance Elt e => Elem e

{-# DEPRECATED Ix "Use 'Shape' instead" #-}
class Shape sh => Ix sh
instance Shape sh => Ix sh

{-# DEPRECATED SliceIx "Use 'Slice' instead" #-}
class Slice sh => SliceIx sh
instance Slice sh => SliceIx sh

{-# DEPRECATED tuple "Use 'lift' instead" #-}
tuple :: Lift Exp e => e -> Exp (Plain e)
tuple = lift

{-# DEPRECATED untuple "Use 'unlift' instead" #-}
untuple :: Unlift Exp e => Exp (Plain e) -> e
untuple = unlift

