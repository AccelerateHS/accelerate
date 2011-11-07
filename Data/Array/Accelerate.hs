{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
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

  -- * Scalar element types
  Int, Int8, Int16, Int32, Int64, Word, Word8, Word16, Word32, Word64,
  CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong,
  Float, Double, CFloat, CDouble,
  Bool, Char, CChar, CSChar, CUChar,

  -- * Scalar type classes
  IsScalar, IsNum, IsBounded, IsIntegral, IsFloating, IsNonNum,

  -- * Array data types
  Arrays, Array, Scalar, Vector, Segments,

  -- * Array element types
  Elt,

  -- * Array shapes & indices
  Z(..), (:.)(..), Shape, All(..), Any(..), Slice(..),
  DIM0, DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7, DIM8, DIM9,

  -- * Operations to use Accelerate arrays from plain Haskell
  arrayDim, arrayShape, arraySize, indexArray, fromIArray, toIArray, fromList, toList,

  -- * The /Accelerate/ language
  module Data.Array.Accelerate.Language,
  module Data.Array.Accelerate.Prelude,

  -- * Deprecated names for backwards compatibility
  Elem, Ix, SliceIx, tuple, untuple,
  
  -- * Diagnostics
  initTrace

) where

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.AST (Arrays)
import Data.Array.Accelerate.Array.Sugar hiding ((!), shape, dim, size)
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Prelude
import Data.Array.Accelerate.Debug


-- Renamings
--

-- FIXME: these all need to go into a separate module for separate importing!

-- rename as '(!)' is already used by the EDSL for indexing

-- |Array indexing in plain Haskell code
--
indexArray :: Array sh e -> sh -> e
indexArray = (Sugar.!)

-- rename as 'shape' is already used by the EDSL to query an array's shape

-- |Array shape in plain Haskell code
--
arrayShape :: Shape sh => Array sh e -> sh
arrayShape = Sugar.shape

-- FIXME: Rename to rank
arrayDim :: Shape sh => sh -> Int
arrayDim = Sugar.dim

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
tuple :: Lift e => e -> Exp (Plain e)
tuple = lift

{-# DEPRECATED untuple "Use 'unlift' instead" #-}
untuple :: Unlift e => Exp (Plain e) -> e
untuple = unlift
