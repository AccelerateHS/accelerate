{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Accelerate.CUDA
-- Copyright   : [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
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
import Data.Array.Accelerate.Array.Sugar		(Array(..))
import Data.Array.Accelerate.Array.Representation


-- |
-- Characterises the types that may be retrieved from the CUDA device as the
-- result of executing an array program
--
class Arrays as where
  collect :: as -> CIO as

instance Arrays () where
  collect () = return ()

instance Arrays (Array dim e) where
  collect arr@(Array sh ad) = peekArray ad (size sh) >> return arr

instance (Arrays as1, Arrays as2) => Arrays (as1, as2) where
  collect (a1,a2) = (,) <$> collect a1 <*> collect a2

