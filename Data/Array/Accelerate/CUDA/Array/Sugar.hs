-- |
-- Module      : Data.Array.Accelerate.CUDA.Array.Sugar
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Array.Sugar (

  module Data.Array.Accelerate.Array.Sugar,
  newArray, allocateArray, useArray

) where

import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.Array.Sugar		hiding (newArray, allocateArray)
import qualified Data.Array.Accelerate.Array.Sugar	as Sugar


-- Create an array from its representation function, uploading the result to the
-- device
--
-- FIXME: small arrays are moved by the GC?
--
newArray :: (Shape sh, Elt e) => sh -> (sh -> e) -> CIO (Array sh e)
newArray sh f =
  let arr = Sugar.newArray sh f
  in do
      mallocArray arr
      pokeArrayAsync arr Nothing
      return arr


-- Allocate a new, uninitialised Accelerate array on host and device
--
allocateArray :: (Shape dim, Elt e) => dim -> CIO (Array dim e)
allocateArray sh =
  let arr = Sugar.allocateArray sh
  in do
      mallocArray arr
      return arr

