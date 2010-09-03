{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module is the CUDA backend for the embedded array language.
--

module Data.Array.Accelerate.CUDA (

    -- * Generate and execute CUDA code for an array expression
    Arrays, run

  ) where

import Prelude hiding (catch)
import Control.Exception
import System.IO.Unsafe

import Foreign.CUDA.Driver.Error

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Compile
import Data.Array.Accelerate.CUDA.Execute
import Data.Array.Accelerate.CUDA.Array.Device
import qualified Data.Array.Accelerate.CUDA.Smart as Sugar

#include "accelerate.h"


-- Accelerate: CUDA
-- ~~~~~~~~~~~~~~~~

-- | Compile and run a complete embedded array program using the CUDA backend
--
{-# NOINLINE run #-}
run :: Arrays a => Sugar.Acc a -> a
run acc
  = unsafePerformIO
  $ evalCUDA (execute (Sugar.convertAcc acc) >>= collect)
             `catch`
             \e -> INTERNAL_ERROR(error) "unhandled" (show (e :: CUDAException))

execute :: Arrays a => Acc a -> CIO a
execute acc = compileAcc acc >> executeAcc acc

