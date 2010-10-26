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
-- This module implements the CUDA backend for the embedded array language.
--

module Data.Array.Accelerate.CUDA (

  -- * Generate and execute CUDA code for an array expression
  run, stream, precompile

) where

-- standard library
import Prelude hiding (catch)
import Control.Exception
import Control.Applicative
import System.IO.Unsafe

-- CUDA binding
import Foreign.CUDA.Driver.Error

-- friends
import Data.Array.Accelerate.Array.Sugar          (Array(..))
import Data.Array.Accelerate.Array.Representation (size)
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Compile
import Data.Array.Accelerate.CUDA.Execute
import qualified Data.Array.Accelerate.CUDA.Smart as Sugar

#include "accelerate.h"


-- Accelerate: CUDA
-- ----------------

-- | Compile and run a complete embedded array program using the CUDA backend
--
run :: Arrays a => Sugar.Acc a -> a
{-# NOINLINE run #-}
run = unsafePerformIO . execute

-- | Stream a lazily read list of input arrays through the given program,
-- collecting results as we go
--

-- TODO:
--  * avoid re-analysing the array code in the frontend
--  * overlap host-device & device->host transfers, as well as computation
--
stream :: (Arrays a, Arrays b) => (a -> Sugar.Acc b) -> [a] -> [b]
{-# NOINLINE stream #-}
stream acc = unsafePerformIO . sequence' . map (execute . acc)


execute :: Arrays a => Sugar.Acc a -> IO a
execute a =
  let acc = Sugar.convertAcc a
  in  evalCUDA (compileAcc acc >> executeAcc acc >>= collect)
      `catch`
      \e -> INTERNAL_ERROR(error) "unhandled" (show (e :: CUDAException))
  where
    -- Copy from device to host, and decrement the usage counter.
    --
    collect :: Arrays arrs => arrs -> CIO arrs
    collect arrs = collectR arrays arrs
      where
        collectR :: ArraysR arrs -> arrs -> CIO arrs
        collectR ArraysRunit         ()                = return ()
        collectR ArraysRarray        arr@(Array sh ad) 
          = peekArray ad (size sh) >> freeArray ad >> return arr
        collectR (ArraysRpair r1 r2) (arrs1, arrs2)    
          = (,) <$> collectR r1 arrs1 <*> collectR r2 arrs2

sequence' :: [IO a] -> IO [a]
sequence' ms = foldr k (return []) ms
    where k m m' = do { x <- m; xs <- unsafeInterleaveIO m'; return (x:xs) }


-- | Populate the internal cache with the compiled functions required to execute
-- the given array program. This is the same as 'run', except that (1) no data
-- is transferred to the device, and (2) the generated code is not invoked.
--

-- TODO: we would like the following to hold, but falls over in 
-- D.A.A.Analysis.Type.arrayType
--  * We could provide a 'undefinedArray' (or 'noArray') value that has the 'Array' constructor, 
--    but no payload   -=chak
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
precompile :: Arrays a => Sugar.Acc a -> ()
{-# NOINLINE precompile #-}
precompile acc
  = unsafePerformIO
  $ evalCUDA (precompileAcc (Sugar.convertAcc acc))
             `catch`
             \e -> INTERNAL_ERROR(error) "unhandled" (show (e :: CUDAException))
