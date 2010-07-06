{-# LANGUAGE TemplateHaskell, TupleSections, TypeOperators #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.State
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--
-- This module defines a state monad which keeps track of the code generator
-- state, including memory transfers and external compilation processes.
--

module Data.Array.Accelerate.CUDA.State
  (
    evalCUDA, runCUDA, CIO,   unique, outputDir, deviceProps, memoryEntry, kernelEntry,
    KernelEntry(KernelEntry), kernelName, kernelStatus, Key,
    MemoryEntry(MemoryEntry), refcount, arena,

    freshVar,
    module Data.Record.Label
  )
  where

import Prelude hiding (id, (.), mod)
import Control.Category

import Data.Maybe
import Data.Binary
import Data.Record.Label
import Control.Arrow
import Control.Applicative
import Control.Exception
import Control.Monad.State              (StateT(..), liftM)
import Data.Map                         (Map)
import Data.IntMap                      (IntMap)
import qualified Data.Map               as M
import qualified Data.IntMap            as IM

import System.Directory
import System.FilePath
import System.Posix.Process
import System.Posix.Types               (ProcessID)

import Foreign.Ptr
import qualified Foreign.CUDA.Driver    as CUDA


-- The CUDA State Monad
-- ~~~~~~~~~~~~~~~~~~~~

-- Return the output directory for compilation by-products, creating if it does
-- not exist. This currently maps to a temporary directory, but could be mode to
-- point towards a persistent cache (eg: getAppUserDataDirectory)
--
getOutputDir :: IO FilePath
getOutputDir = do
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  dir <- canonicalizePath $ tmp </> "ac" ++ show pid
  createDirectoryIfMissing True dir
  return dir

-- Store the kernel module map to file to the given directory
--
save :: FilePath -> Map Key KernelEntry -> IO ()
save f m = encodeFile f . map (second _kernelName) . filter compiled $ M.toAscList m
  where
    compiled (_,KernelEntry _ (Right _)) = True
    compiled _                           = False

-- Read the kernel index map file (if it exists), loading modules into the
-- current context
--
load :: FilePath -> IO (Map Key KernelEntry)
load f = do
  x <- doesFileExist f
  if x then M.fromDistinctAscList <$> (mapM reload =<< decodeFile f)
       else return M.empty
  where
    reload (k,n) =
      (k,) . KernelEntry n . Right <$> CUDA.loadFile (n `replaceExtension` ".cubin")

-- |
-- Evaluate a CUDA array computation under a newly initialised environment,
-- discarding the final state.
--
evalCUDA :: CIO a -> IO a
evalCUDA =  liftM fst . runCUDA

runCUDA :: CIO a -> IO (a, CUDAState)
runCUDA acc =
  bracket (initialise Nothing) finalise $ \(dev,_ctx) -> do
    props <- CUDA.props dev
    dir   <- getOutputDir
    dict  <- load (dir </> ".dict")
    (a,s) <- runStateT acc (CUDAState (M.size dict) dir props IM.empty dict)
    --
    -- TLM 2010-06-05: assert all memory has been released ??
    --                 does CUDA.destroy release memory ??
    save (dir </> ".dict") (_kernelEntry s)
    return (a,s)

  where
    finalise     = CUDA.destroy . snd
    initialise n = do
      CUDA.initialise []
      dev <- CUDA.device (fromMaybe 0 n)        -- TLM: select the "best" device ??
      ctx <- CUDA.create dev [CUDA.SchedAuto]
      return (dev, ctx)


-- | The state token for accelerated CUDA array operations
--
type CIO       = StateT CUDAState IO
data CUDAState = CUDAState
  {
    _unique      :: Int,
    _outputDir   :: FilePath,
    _deviceProps :: CUDA.DeviceProperties,
    _memoryEntry :: IntMap MemoryEntry,
    _kernelEntry :: Map Key KernelEntry
  }

-- |
-- Associate an array expression with an external compilation tool (nvcc) or the
-- loaded function module
--
type Key         = String
data KernelEntry = KernelEntry
  {
    _kernelName   :: String,
    _kernelStatus :: Either ProcessID CUDA.Module
  }


-- |
-- Reference tracking for device memory allocations. Associates the products of
-- an `Array dim e' with data stored on the graphics device. Facilitates reuse
-- and delayed allocation at the cost of explicit release.
--
-- This maps to a single concrete array. Arrays of pairs, for example, which are
-- represented internally as pairs of arrays, will generate two entries.
--
data MemoryEntry = MemoryEntry
  {
    _refcount :: Int,
    _arena    :: WordPtr
  }

$(mkLabels [''CUDAState, ''MemoryEntry, ''KernelEntry])


-- The derived labels (documentation goes here)
--
unique       :: CUDAState :-> Int
outputDir    :: CUDAState :-> FilePath
deviceProps  :: CUDAState :-> CUDA.DeviceProperties
memoryEntry  :: CUDAState :-> IntMap MemoryEntry
kernelEntry  :: CUDAState :-> Map Key KernelEntry

refcount     :: MemoryEntry :-> Int
arena        :: MemoryEntry :-> WordPtr

kernelName   :: KernelEntry :-> String
kernelStatus :: KernelEntry :-> Either ProcessID CUDA.Module


-- | A unique name supply
--
freshVar :: CIO Int
freshVar =  getM unique <* modM unique (+1)

