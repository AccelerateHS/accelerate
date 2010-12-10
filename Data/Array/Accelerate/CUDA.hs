{-# LANGUAGE CPP, GADTs #-}
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
  run, stream

) where

-- standard library
import Prelude hiding (catch)
import Control.Exception
import Control.Applicative
import Control.Monad
import System.IO.Unsafe

-- CUDA binding
import Foreign.CUDA.Driver.Error

-- friends
import qualified Data.Array.Accelerate.AST     as AST
import Data.Array.Accelerate.Smart                (Acc, convertAcc, convertAccFun1)
import Data.Array.Accelerate.Array.Representation (size)
import Data.Array.Accelerate.Array.Sugar          (Array(..))
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Compile
import Data.Array.Accelerate.CUDA.Execute

#include "accelerate.h"


-- Accelerate: CUDA
-- ----------------

-- | Compile and run a complete embedded array program using the CUDA backend
--
run :: AST.Arrays a => Acc a -> a
{-# NOINLINE run #-}
run = unsafePerformIO . execute

-- | Stream a lazily read list of input arrays through the given program,
-- collecting results as we go
--

-- TODO:
--  * avoid re-analysing the array code in the frontend
--  * overlap host->device & device->host transfers, as well as computation
--
stream :: (AST.Arrays a, AST.Arrays b) => (Acc a -> Acc b) -> [a] -> [b]
{-# NOINLINE stream #-}
stream f as = unsafePerformIO $ do
  let acc = convertAccFun1 f 
  s <- liftM snd $ runCUDA (compileAccFun1 acc)
  stream' s acc as

--stream acc = unsafePerformIO . sequence' . map (execute . acc)

stream' :: (AST.Arrays a, AST.Arrays b) => CUDAState -> AST.Afun (a -> b) -> [a] -> IO ([b])
stream' _ _ [] = return []
stream' state acc (aArr:as) = do
  (bArr,s) <- runCUDAWith state (executeAccFun1 aArr acc >>= collect)
  bs <- unsafeInterleaveIO (stream' s acc as)
  return (bArr:bs)


execute :: AST.Arrays a => Acc a -> IO a
execute a =
  let acc = convertAcc a
  in  evalCUDA (compileAcc acc >> executeAcc acc >>= collect)
      `catch`
      \e -> INTERNAL_ERROR(error) "unhandled" (show (e :: CUDAException))

-- Copy from device to host, and decrement the usage counter.
--
collect :: AST.Arrays arrs => arrs -> CIO arrs
collect arrs = collectR AST.arrays arrs
  where
    collectR :: AST.ArraysR arrs -> arrs -> CIO arrs
    collectR AST.ArraysRunit         ()                = return ()
    collectR AST.ArraysRarray        arr@(Array sh ad)
      = peekArray ad (size sh) >> freeArray ad >> return arr
    collectR (AST.ArraysRpair r1 r2) (arrs1, arrs2)
      = (,) <$> collectR r1 arrs1 <*> collectR r2 arrs2

sequence' :: [IO a] -> IO [a]
sequence' = foldr k (return [])
  where k m ms = do { x <- m; xs <- unsafeInterleaveIO ms; return (x:xs) }

