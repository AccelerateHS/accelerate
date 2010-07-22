{-# LANGUAGE TemplateHaskell, TupleSections, TypeOperators #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.State
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--
-- This module defines a state monad token which keeps track of the code
-- generator state, including memory transfers and external compilation
-- processes.
--

module Data.Array.Accelerate.CUDA.State
  (
    evalCUDA, runCUDA, CIO, unique, outputDir, deviceProps, memoryTable, kernelTable,
    AccTable, KernelEntry(KernelEntry), kernelName, kernelStatus,
    MemTable, MemoryEntry(MemoryEntry), refcount, memsize, arena,

    freshVar,
    module Data.Record.Label
  )
  where

import Prelude hiding (id, (.), mod)
import Control.Category

import Data.Int
import Data.Maybe
import Data.Binary
import Data.Record.Label
import Control.Arrow
import Control.Applicative
import Control.Exception
import Control.Monad.State              (StateT(..), liftM)
import Data.HashTable                   (HashTable)
import qualified Data.HashTable         as HT

import System.Directory
import System.FilePath
import System.Posix.Process
import System.Posix.Types               (ProcessID)

import Foreign.Ptr
import qualified Foreign.CUDA.Driver    as CUDA


-- Types
-- ~~~~~

type AccTable = HashTable String  KernelEntry
type MemTable = HashTable WordPtr MemoryEntry

-- | The state token for accelerated CUDA array operations
--
type CIO       = StateT CUDAState IO
data CUDAState = CUDAState
  {
    _unique      :: Int,
    _outputDir   :: FilePath,
    _deviceProps :: CUDA.DeviceProperties,
    _memoryTable :: MemTable,
    _kernelTable :: AccTable
  }

-- |
-- Associate an array expression with an external compilation tool (nvcc) or the
-- loaded function module
--
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
    _memsize  :: Int64,
    _arena    :: WordPtr
  }



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
save :: FilePath -> AccTable -> IO ()
save f m = encodeFile f . map (second _kernelName) . filter compiled =<< HT.toList m
  where
    compiled (_,KernelEntry _ (Right _)) = True
    compiled _                           = False

-- Read the kernel index map file (if it exists), loading modules into the
-- current context
--
load :: FilePath -> IO (AccTable, Int)
load f = do
  x <- doesFileExist f
  e <- if x then mapM reload =<< decodeFile f
            else return []

  (,length e) <$> HT.fromList HT.hashString e
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
    tab   <- HT.new (==) fromIntegral
    (k,n) <- load (dir </> "_index")
    (a,s) <- runStateT acc (CUDAState n dir props tab k)
    --
    -- TLM 2010-06-05: assert all memory has been released ??
    --                 does CUDA.destroy release memory ??
    save (dir </> "_index") (_kernelTable s)
    return (a,s)

  where
    finalise     = CUDA.destroy . snd
    initialise n = do
      CUDA.initialise []
      dev <- CUDA.device (fromMaybe 0 n)        -- TLM: select the "best" device ??
      ctx <- CUDA.create dev [CUDA.SchedAuto]
      return (dev, ctx)


$(mkLabels [''CUDAState, ''MemoryEntry, ''KernelEntry])

-- The derived labels (documentation goes here)
--
unique       :: CUDAState :-> Int
outputDir    :: CUDAState :-> FilePath
deviceProps  :: CUDAState :-> CUDA.DeviceProperties
memoryTable  :: CUDAState :-> MemTable
kernelTable  :: CUDAState :-> AccTable

refcount     :: MemoryEntry :-> Int
memsize      :: MemoryEntry :-> Int64
arena        :: MemoryEntry :-> WordPtr

kernelName   :: KernelEntry :-> String
kernelStatus :: KernelEntry :-> Either ProcessID CUDA.Module


-- | A unique name supply
--
freshVar :: CIO Int
freshVar =  getM unique <* modM unique (+1)

