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
    Arrays, precompile, run, stream

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


-- | Stream a lazily read list of input arrays through the given program,
-- collecting results as we go
--
-- TODO:
--  * avoid re-analysing the array code in the frontend
--  * overlap host-device & device->host transfers, as well as computation
--
{-# NOINLINE stream #-}
stream :: (Arrays a, Arrays b) => (a -> Sugar.Acc b) -> [a] -> [b]
stream acc as
  = unsafePerformIO
  $ sequence' . flip map as
  $ \s -> evalCUDA (execute (Sugar.convertAcc (acc s)) >>= collect)
                   `catch`
                   \e -> INTERNAL_ERROR(error) "unhandled" (show (e :: CUDAException))

-- A lazier version of Control.Monad.sequence
--
sequence' :: [IO a] -> IO [a]
sequence' ms = foldr k (return []) ms
    where k m m' = do { x <- m; xs <- unsafeInterleaveIO m'; return (x:xs) }


-- | Populate the internal cache with the compiled functions required to execute
-- the given array program. This is the same as 'run', except that (1) no data
-- is transferred to the device, and (2) the generated code is not invoked.
--

-- TODO: we would like the following to hold, but falls over in 
-- D.A.A.Array.Sugar.arrayType
--
-- Note that it is not necessary to create an unused array argument. For
-- example:
--
-- > dotp :: Vector a -> Vector a -> Acc (Scalar a)
-- > dotp xs ys = fold (+) 0 $ zipWith (*) (use xs) (use ys)
--
-- It is sufficient to:
--
-- > precompile (dotp undefined undefined :: Acc (Scalar Float))
--
{-# NOINLINE precompile #-}
precompile :: Arrays a => Sugar.Acc a -> ()
precompile acc
  = unsafePerformIO
  $ evalCUDA (precompileAcc (Sugar.convertAcc acc) >> return ())
             `catch`
             \e -> INTERNAL_ERROR(error) "unhandled" (show (e :: CUDAException))

