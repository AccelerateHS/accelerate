-- |
-- Module      : Data.Array.Accelerate
-- Copyright   : [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
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
  Array, Scalar, Vector, Segments,

  -- * Array element types
  Elem,

  -- * Array shapes & indices
  Ix(dim, size), All(..), SliceIx(..), DIM0, DIM1, DIM2, DIM3, DIM4, DIM5,
  
  -- * Operations to use Accelerate arrays from plain Haskell
  arrayShape, indexArray, fromIArray, toIArray, fromList, toList,

  -- * The /Accelerate/ language
  module Data.Array.Accelerate.Language,

) where

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar hiding ((!), shape)
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import Data.Array.Accelerate.Language


-- Renamings
--

-- rename as '(!)' is already used by the EDSL for indexing

-- |Array indexing in plain Haskell code
--
indexArray :: Array dim e -> dim -> e
indexArray = (Sugar.!)

-- rename as 'shape' is already used by the EDSL to query an array's shape

-- |Array shape in plain Haskell code
--
arrayShape :: Ix dim => Array dim e -> dim
arrayShape = Sugar.shape
