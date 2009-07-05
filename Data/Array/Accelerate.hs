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
--  * 'Data.Array.Accelerate.Interpreter': simple interpreter in Haskell; it's
--      slow, but doesn't have any further dependencies and also serves as a
--      reference implementation


module Data.Array.Accelerate (

  -- * Scalar element types
  Int, Int8, Int16, Int32, Int64, Word, Word8, Word16, Word32, Word64, 
  CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong,
  Float, Double, CFloat, CDouble,
  Bool, Char, CChar, CSChar, CUChar,

  -- * Array data types
  Array, Arr, DIM0, DIM1, DIM2,

  -- * Array shapes & indices
  Index(..),

  -- * Abstract types of array computations
  Exp,

  -- * FIXME
  mkVal, mkNumVal,

  -- * Smart expression constructors
  module Data.Array.Accelerate.Language,

) where

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.AST   (Index(..))
import Data.Array.Accelerate.Smart (Exp,
                                    mkVal, mkNumVal)  -- FIXME: can't we avoid that
import Data.Array.Accelerate.Language
