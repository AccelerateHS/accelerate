-- |An embedded language of accelerated array computations 
--
--  Copyright (c) [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--
--  Abstract interface
--  ~~~~~~~~~~~~~~~~~~
--  The types representing array computations are only exported abstractly.
--  This gives us more flexibility for later changes.
--
--  Code execution
--  ~~~~~~~~~~~~~~
--  Access to the various backends is via the 'run' function in
--  backend-specific toplevel modules.  Currently, we have the following:
--
--  * 'Data.Array.Accelerate.Interpreter': simple interpreter in Haskell as a
--      reference implementation defining the semantics of the array language


module Data.Array.Accelerate (

  -- * Scalar element types
  Int, Int8, Int16, Int32, Int64, Word, Word8, Word16, Word32, Word64, 
  CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong,
  Float, Double, CFloat, CDouble,
  Bool, Char, CChar, CSChar, CUChar,

  -- * Array data types
  Array, Scalar, Vector, Segments,

  -- * Array element types
  Elem,

  -- * Array shapes & indices
  Ix(dim, size), All(..), SliceIx(..), DIM0, DIM1, DIM2, DIM3, DIM4, DIM5,
  
  -- * Array operations
  arrayShape, indexArray, fromIArray, toIArray, fromList, toList,

  -- * Surface language
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
indexArray :: Array dim e -> dim -> e
indexArray = (Sugar.!)

-- rename as 'shape' is already used by the EDSL to query an array's shape
arrayShape :: Ix dim => Array dim e -> dim
arrayShape = Sugar.shape
