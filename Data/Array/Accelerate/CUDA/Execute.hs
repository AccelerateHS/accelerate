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

import Foreign.Marshal.Utils
import qualified Foreign.CUDA.Driver                    as CUDA


-- |
-- Execute an embedded array program using the CUDA backend. The function will
-- block if the compilation has not yet completed, but subsequent invocations of
-- the same kernel will be able to extract the loaded kernel directly.
--
execute :: OpenAcc aenv a -> CIO a
execute (Use xs) = return xs
execute acc      = do
  krn <- fromMaybe (error "code generation failed") . M.lookup (accToKey acc) <$> getM kernelEntry
  fun <- either' (get kernelStatus krn) return $ \pid -> do
    let name = get kernelName krn

    liftIO (waitFor pid)
    mdl <- liftIO $ CUDA.loadFile   (replaceExtension name ".cubin")
    fun <- liftIO $ CUDA.getFun mdl (takeBaseName name) -- TLM: really only needs to be "fold" or "map", etc...

    modM kernelEntry $ M.insert (accToKey acc) (set kernelStatus (Right fun) krn)
    return fun

  -- determine dispatch pattern, extract parameters, allocate storage
  --
  dispatch acc fun

  where
    either' :: Either a b -> (b -> c) -> (a -> c) -> c
    either' e r l = either l r e


-- Setup and initiate the computation. This may require several kernel launches.
--
-- TLM: focus more on dispatch patterns (reduction, permutation, map, etc),
--      rather than the actual functions. So ZipWith and Map should resolve to
--      the same procedure, for example (more or less, hopefully ... *vague*).
--
dispatch :: OpenAcc aenv a -> CUDA.Fun -> CIO a

-- Map
dispatch acc@(Map _ ad) fn = do
  (Array sh in0) <- execute ad
  let res@(Array sh' out) = newArray (Sugar.toElem sh)
      n                   = size sh'

  mallocArray out n
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0

  launch acc fn (d_out ++ d_in0 ++ [CUDA.IArg n])
  free   in0
  return res

-- ZipWith
dispatch acc@(ZipWith _ ad0 ad1) fn = do
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

-- Fold
dispatch acc@(Fold _ x ad) fn = do
  (Array sh in0)  <- execute ad
  (cta,grid,smem) <- launchConfig acc fn

  let res@(Array _ out) = newArray grid

  mallocArray out grid
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0

  launch' (cta,grid,smem) fn (d_out ++ d_in0 ++ [CUDA.IArg (size sh)])
  free in0
  if grid > 1 then dispatch (Fold undefined x (Use res)) fn
              else return (Array (Sugar.fromElem ()) out)


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

