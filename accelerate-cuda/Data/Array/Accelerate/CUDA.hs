{-# LANGUAGE CPP, GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements the CUDA backend for the embedded array language.
--

module Data.Array.Accelerate.CUDA (

  -- * Generate and execute CUDA code for an array expression
  Arrays, run, run1, stream

) where

-- standard library
import Prelude hiding (catch)
import Control.Exception
import Control.Applicative
import Control.Monad.Trans
import System.Mem
import System.IO.Unsafe
import Foreign.CUDA.Driver.Error

-- friends
import Data.Array.Accelerate.AST                  (Arrays(..), ArraysR(..))
import Data.Array.Accelerate.Smart                (Acc, convertAcc, convertAccFun1)
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Compile
import Data.Array.Accelerate.CUDA.Execute

#include "accelerate.h"


-- Accelerate: CUDA
-- ----------------

-- |Compile and run a complete embedded array program using the CUDA backend
--
run :: Arrays a => Acc a -> a
{-# NOINLINE run #-}
run a = unsafePerformIO $ execute
  where
    acc     = convertAcc a
    execute = evalCUDA (compileAcc acc >>= executeAcc >>= collect)
              `catch`
              \e -> INTERNAL_ERROR(error) "unhandled" (show (e :: CUDAException))


-- |Prepare and execute an embedded array program of one argument
--
-- TLM: ensure this can be partially applied, such that compilation only occurs
--      once. Structure so that we would have had type IO (a -> b) ?
--
run1 :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b
{-# NOINLINE run1 #-}
run1 f a = unsafePerformIO $ execute =<< evalCUDA (compileAfun1 acc)
  where
    acc          = convertAccFun1 f
    execute afun = evalCUDA (executeAfun1 afun a >>= collect)
                   `catch`
                   \e -> INTERNAL_ERROR(error) "unhandled" (show (e :: CUDAException))


-- |Stream a lazily read list of input arrays through the given program,
-- collecting results as we go
--
stream :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> [a] -> [b]
stream f arrs = map (run1 f) arrs


-- Copy from device to host, and decrement the usage counter. This last step
-- should result in all transient arrays having been removed from the device.
--
collect :: Arrays arrs => arrs -> CIO arrs
collect arrs = collectR arrays arrs <* cleanupArrayData
  where
    collectR :: ArraysR arrs -> arrs -> CIO arrs
    collectR ArraysRunit         ()             = return ()
    collectR ArraysRarray        arr            = peekArray arr >> return arr
    collectR (ArraysRpair r1 r2) (arrs1, arrs2) = (,) <$> collectR r1 arrs1
                                                      <*> collectR r2 arrs2

