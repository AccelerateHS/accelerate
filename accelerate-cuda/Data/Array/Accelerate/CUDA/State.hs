{-# LANGUAGE CPP, GADTs, PatternGuards, TemplateHaskell #-}
{-# LANGUAGE TupleSections, TypeFamilies, TypeOperators #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.State
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
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

module Data.Array.Accelerate.CUDA.State (

  -- Types
  CIO, KernelTable, KernelEntry(KernelEntry),

  -- Evaluating computations
  evalCUDA, unique, deviceProps, memoryTable, kernelTable, kernelName,
  kernelStatus

) where

-- friends
import Data.Array.Accelerate.CUDA.Analysis.Device
import Data.Array.Accelerate.CUDA.Analysis.Hash
import Data.Array.Accelerate.CUDA.Array.Table

-- library
import Data.Tuple
import Data.Record.Label
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State.Strict                       (StateT(..))
import System.Posix.Types                               (ProcessID)
import System.IO.Unsafe
import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Data.HashTable.IO                      as Hash

#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
import Data.Binary                                      (encodeFile, decodeFile)
import Control.Arrow                                    (second)
import Paths_accelerate                                 (getDataDir)
#endif


-- An exact association between an accelerate computation and its
-- implementation, which is either a reference to the external compiler (nvcc)
-- or the resulting binary module.
--
-- We aren't concerned with true (typed) equality of an OpenAcc expression,
-- since we largely want to disregard the array environment; we really only want
-- to assert the type and index of those variables that are accessed by the
-- computation and no more, but we can not do that. Instead, this is keyed to
-- the generated kernel code.
--
type KernelTable = Hash.BasicHashTable AccKey KernelEntry
data KernelEntry = KernelEntry
  {
    _kernelName   :: FilePath,
    _kernelStatus :: Either ProcessID CUDA.Module
  }


-- The state token for accelerated CUDA array operations
--
type CIO       = StateT CUDAState IO
data CUDAState = CUDAState
  {
    _unique        :: Int,
    _deviceProps   :: CUDA.DeviceProperties,
    _deviceContext :: CUDA.Context,
    _kernelTable   :: KernelTable,
    _memoryTable   :: MemoryTable
  }

$(mkLabels [''CUDAState, ''KernelEntry])


-- Execution State
-- ---------------

-- |Evaluate a CUDA array computation
--
evalCUDA :: CIO a -> IO a
evalCUDA acc = modifyMVar onta (liftM swap . runStateT acc)
  where
    -- hic sunt dracones: truly unsafe use of unsafePerformIO
    {-# NOINLINE onta #-}
    onta = unsafePerformIO
         $ do s <- initialise
              r <- newMVar s
              addMVarFinalizer r $ CUDA.destroy (getL deviceContext s)
              return r


-- Select and initialise the CUDA device, and create a new execution context.
-- This will be done only once per program execution, as initialising the CUDA
-- context is relatively expensive.
--
initialise :: IO CUDAState
initialise = do
  CUDA.initialise []
  (d,prp) <- selectBestDevice
  ctx     <- CUDA.create d [CUDA.SchedAuto]
  knl     <- Hash.new
  mem     <- new
  return $ CUDAState 0 prp ctx knl mem


-- Persistent caching (deprecated)
-- -------------------------------

#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
-- Load and save the persistent kernel index file
--
indexFileName :: IO FilePath
indexFileName = do
  tmp <- (</> "cache") `fmap` getDataDir
  dir <- createDirectoryIfMissing True tmp >> canonicalizePath tmp
  return (dir </> "_index")

saveIndexFile :: CUDAState -> IO ()
saveIndexFile s = do
  ind <- indexFileName
  encodeFile ind . map (second _kernelName) =<< Hash.toList (_kernelTable s)

-- Read the kernel index map file (if it exists), loading modules into the
-- current context
--
loadIndexFile :: IO (KernelTable, Int)
loadIndexFile = do
  f <- indexFileName
  x <- doesFileExist f
  e <- if x then mapM reload =<< decodeFile f
            else return []
  (,length e) <$> Hash.fromList hashAccKey e
  where
    reload (k,n) = (k,) . KernelEntry n . Right <$> CUDA.loadFile (n `replaceExtension` ".cubin")
#endif

