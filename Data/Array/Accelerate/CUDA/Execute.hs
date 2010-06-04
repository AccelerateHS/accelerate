{-# LANGUAGE GADTs, TupleSections #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Execute
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Execute (execute)
  where

import Prelude hiding (id, (.), mod)
import Control.Category

import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.Map                               as M

import System.FilePath
import System.Posix.Process
import System.Exit                                      (ExitCode(..))
import System.Posix.Types                               (ProcessID)

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Sugar                (Array(..))
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar

import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Compile
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.Analysis.Launch

import qualified Foreign.CUDA.Driver                    as CUDA


-- |
-- Execute an embedded array program using the CUDA backend. The function will
-- block if the compilation has not yet completed, but subsequent invocations of
-- the same kernel will be able to extract the loaded kernel directly.
--
execute :: OpenAcc aenv a -> CIO a
execute (Let  x y) = execute x >> execute y
execute (Let2 x y) = execute x >> execute y
execute (Avar _)   = error "execute: avar"
execute (Unit _)   = error "execute: unit"
execute (Use xs)   = return xs

execute acc = do
  krn <- fromMaybe (error "code generation failed") . M.lookup key <$> getM kernelEntry
  mdl <- either' (get kernelStatus krn) return $ \pid -> do
    liftIO (waitFor pid)
    mdl <- liftIO    $ CUDA.loadFile (get kernelName krn `replaceExtension` ".cubin")
    modM kernelEntry $ M.insert key  (set kernelStatus (Right mdl) krn)
    return mdl

  -- determine dispatch pattern, extract parameters, allocate storage, run
  --
  dispatch acc mdl

  where
    key           = accToKey acc
    either' e r l = either l r e


-- Setup and initiate the computation. This may require several kernel launches.
--
dispatch :: OpenAcc aenv a -> CUDA.Module -> CIO a
dispatch acc@(Map _ ad) mdl = do
  fn             <- liftIO $ CUDA.getFun mdl "map"
  (Array sh in0) <- execute ad
  let res@(Array sh' out) = newArray (Sugar.toElem sh)
      n                   = size sh'

  mallocArray out n
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0

  launch acc fn (d_out ++ d_in0 ++ [CUDA.IArg n])
  free   in0
  return res

dispatch acc@(ZipWith _ ad0 ad1) mdl = do
  fn              <- liftIO $ CUDA.getFun mdl "zipWith"
  (Array sh0 in0) <- execute ad0
  (Array sh1 in1) <- execute ad1
  let res@(Array sh' out) = newArray (Sugar.toElem (sh0 `intersect` sh1))
      n    = size sh'

  mallocArray out n
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  d_in1 <- devicePtrs in1

  launch acc fn (d_out ++ d_in0 ++ d_in1 ++ [CUDA.IArg n])
  free   in0
  free   in1
  return res

dispatch acc@(Fold _ x ad) mdl = do
  fn              <- liftIO $ CUDA.getFun mdl "fold"
  (Array sh in0)  <- execute ad
  (cta,grid,smem) <- launchConfig acc fn
  let res@(Array _ out) = newArray grid

  mallocArray out grid
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0

  launch' (cta,grid,smem) fn (d_out ++ d_in0 ++ [CUDA.IArg (size sh)])
  free in0
  if grid > 1 then dispatch (Fold undefined x (Use res)) mdl
              else return (Array (Sugar.fromElem ()) out)

dispatch acc@(Scanl _ x ad) mdl = do
  fscan           <- liftIO $ CUDA.getFun mdl "scanl"
  fadd            <- liftIO $ CUDA.getFun mdl "vectorAddUniform4"
  (Array sh in0)  <- execute ad
  (cta,grid,smem) <- launchConfig acc fscan
  let res@(Array _ out)  = newArray (Sugar.toElem sh)
      bks@(Array _ sums) = newArray grid
      n                  = size sh

  mallocArray out (size sh)
  mallocArray sums grid
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  d_bks <- devicePtrs sums

  -- Single row, multi-block, non-full block scan
  --
  launch' (cta,grid,smem) fscan (d_out ++ d_in0 ++ d_bks ++ map CUDA.IArg [n,1,1])
  free in0
  free sums

  -- Now, take the last value of all of the sub-blocks and scan those. This will
  -- give a new value that must be added to each block to get the final result
  --
  when (grid > 1) $ do
    (Array _ sums') <- fst <$> dispatch (Scanl undefined x (Use bks)) mdl
    d_bks'          <- devicePtrs sums'
    launch' (cta,grid,0) fadd (d_out ++ d_bks' ++ map CUDA.IArg [n,4,4,0,0])

  -- What to do about the reduction parameter? Seems like the block sums could
  -- be useful, but might require additional code generation in earlier phases
  --
  return (res, undefined)


-- Initiate the device computation. The first version selects launch parameters
-- automatically, the second requires them explicitly. This tuple contains
-- threads per block, grid size, and dynamic shared memory, respectively.
--
launch :: OpenAcc aenv a -> CUDA.Fun -> [CUDA.FunParam] -> CIO ()
launch acc fn args =
  launchConfig acc fn >>= \cfg ->
  launch' cfg fn args

launch' :: (Int,Int,Integer) -> CUDA.Fun -> [CUDA.FunParam] -> CIO ()
launch' (cta,grid,smem) fn args =
  liftIO $ do
    CUDA.setParams     fn args
    CUDA.setSharedSize fn smem
    CUDA.setBlockShape fn (cta,1,1)
    CUDA.launch        fn (grid,1) Nothing


-- Create a new array (obviously...)
--
{-# INLINE newArray #-}
newArray :: (Sugar.Ix dim, Sugar.Elem e) => dim -> Array dim e
newArray sh = ad `seq` Array (Sugar.fromElem sh) ad
  where
    -- FIXME: small arrays are relocated by the GC
    ad = fst . runArrayData $ (,undefined) <$> newArrayData (1024 `max` Sugar.size sh)


-- |
-- Wait for the compilation process to finish
--
waitFor :: ProcessID -> IO ()
waitFor pid = do
  status <- getProcessStatus True True pid
  case status of
       Just (Exited ExitSuccess) -> return ()
       _                         -> error  $ "nvcc (" ++ shows pid ") terminated abnormally"

