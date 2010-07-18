{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
--- Description ---------------------------------------------------------------
--
-- Embedded array processing language: device arrays
--

module Data.Array.Accelerate.CUDA.Array.Device
  where

import Control.Applicative

import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Sugar (Array(..))


-- |
-- Characterises the types that may be retrieved from the CUDA device as the
-- result of executing an array program
--
class Arrays as where
  collect :: as -> CIO as       -- ^ Copy from device to host, and decrement the usage counter

instance Arrays () where
  collect () = return ()

instance Arrays (Array dim e) where
  collect arr@(Array sh ad) = peekArray ad (size sh) >> freeArray ad >> return arr

instance (Arrays as1, Arrays as2) => Arrays (as1, as2) where
  collect (a1,a2) = (,) <$> collect a1 <*> collect a2

