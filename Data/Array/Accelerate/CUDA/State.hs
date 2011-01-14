{-# LANGUAGE CPP, GADTs, PatternGuards, TemplateHaskell, TupleSections #-}
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
    evalCUDA, runCUDA, runCUDAWith, CIO, CUDAState, StableAccName(..),
    unique, outputDir, deviceProps, deviceContext,
    memoryTable, kernelTable, computeTable,
    cleanup,

    MemoryEntry(MemoryEntry), refcount, memsize, arena,
    KernelEntry(KernelEntry), kernelName, kernelStatus,
    AccEntry(AccEntry), accKey, accKernel,

    freshVar,
    module Data.Record.Label
  )
  where

import Prelude hiding (id, (.))
import Control.Category
import Data.Record.Label

import Data.Int
import Data.IORef
import Data.Typeable
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict       (StateT(..))
import Data.HashTable                   (HashTable)
import Foreign.Ptr
import qualified Data.HashTable         as Hash
import qualified Foreign.CUDA.Driver    as CUDA

import System.Directory
import System.FilePath
import System.Posix.Types               (ProcessID)
import System.Mem.Weak
import System.Mem.StableName
import System.IO.Unsafe

import Data.Array.Accelerate.CUDA.Analysis.Device

#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
import Data.Binary                      (encodeFile, decodeFile)
import Control.Arrow                    (second)
import Paths_accelerate                 (getDataDir)
#else
import System.Posix.Process             (getProcessID)
#endif

#include "accelerate.h"


-- Types
-- -----

type MemoryTable  = HashTable WordPtr MemoryEntry
type KernelTable  = HashTable String  KernelEntry
type ComputeTable = HashTable StableAccName AccEntry


-- | The state token for accelerated CUDA array operations
--
type CIO       = StateT CUDAState IO
data CUDAState = CUDAState
  {
    _unique        :: Int,
    _outputDir     :: FilePath,
    _deviceProps   :: CUDA.DeviceProperties,
    _deviceContext :: CUDA.Context,
    _memoryTable   :: MemoryTable,
    _kernelTable   :: KernelTable,
    _computeTable  :: ComputeTable
  }

-- | Associate an array expression with an external compilation tool (nvcc) or
-- the loaded function module
--
data KernelEntry = KernelEntry
  {
    _kernelName   :: FilePath,
    _kernelStatus :: Either ProcessID CUDA.Module
  }

-- | In contrast to the kernel entry table, which relates Accelerate
-- computations to kernel functions invariantly, the Acc computation table
-- associates the nodes of a particular AST directly to the object code that
-- will be used to realise the result.
--
-- Its function is to provide a fast cache of compiled modules for streaming
-- applications, avoiding (string-based) key generation for fast,
-- not-quite-exact comparisons.
--
data AccEntry = AccEntry
  {
    _accKey    :: String,       -- for assertions
    _accKernel :: CUDA.Module
  }

-- Stable keys to nodes of an Acc computation
--
data StableAccName where
  StableAccName :: Typeable a => StableName a -> StableAccName

instance Eq StableAccName where
  StableAccName sn1 == StableAccName sn2
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

hashStableAcc :: StableAccName -> Int32
hashStableAcc (StableAccName sn) = fromIntegral (hashStableName sn)

newComputeTable :: IO (ComputeTable)
newComputeTable = Hash.new (==) hashStableAcc

-- | Reference tracking for device memory allocations. Associates the products
-- of an `Array dim e' with data stored on the graphics device. Facilitates
-- reuse and delayed allocation at the cost of explicit release.
--
-- This maps to a single concrete array. Arrays of tuples, which are represented
-- internally as tuples of arrays, will generate multiple entries.
--
data MemoryEntry = MemoryEntry
  {
    _refcount :: Maybe Int,     -- let bound arrays have no reference count
    _memsize  :: Int64,
    _arena    :: WordPtr
  }


$(mkLabels [''CUDAState, ''MemoryEntry, ''KernelEntry, ''AccEntry])


-- Execution State
-- ---------------

-- Return the output directory for compilation by-products, creating if it does
-- not exist.
--
getOutputDir :: IO FilePath
getOutputDir = do
#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
  tmp <- getDataDir
  let dir = tmp </> "cache"
#else
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  let dir = tmp </> "ac" ++ show pid
#endif
  createDirectoryIfMissing True dir
  canonicalizePath dir

-- Store the kernel module map to file
--
saveIndexFile :: CUDAState -> IO ()
#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
saveIndexFile s = encodeFile (_outputDir s </> "_index") . map (second _kernelName) =<< Hash.toList (_kernelTable s)
#else
saveIndexFile _ = return ()
#endif

-- Read the kernel index map file (if it exists), loading modules into the
-- current context
--
loadIndexFile :: FilePath -> IO (KernelTable, Int)
#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
loadIndexFile f = do
  x <- doesFileExist f
  e <- if x then mapM reload =<< decodeFile f
            else return []
  (,length e) <$> Hash.fromList Hash.hashString e
  where
    reload (k,n) = (k,) . KernelEntry n . Right <$> CUDA.loadFile (n `replaceExtension` ".cubin")
#else
loadIndexFile _  = (,0) <$> Hash.new (==) Hash.hashString
#endif


-- Select and initialise the CUDA device, and create a new execution context.
-- This will be done only once per program execution, as initialising the CUDA
-- context is relatively expensive.
--
-- Would like to put the finaliser on the state token, since finalising the
-- context affects the various hash tables. However, this places the finaliser
-- on the CUDAState "box", and the box is removed by optimisations causing the
-- finaliser to fire prematurely.
--
initialise :: IO CUDAState
initialise = do
  CUDA.initialise []
  (d,prp) <- selectBestDevice
  ctx     <- CUDA.create d [CUDA.SchedAuto]
  dir     <- getOutputDir
  mem     <- Hash.new (==) fromIntegral
  (knl,n) <- loadIndexFile (dir </> "_index")
  addFinalizer ctx (CUDA.destroy ctx)
  return $ CUDAState n dir prp ctx mem knl undefined


sanitise :: CUDAState -> IO CUDAState
sanitise st = do
  compute <- newComputeTable
  entries <- length <$> Hash.toList (getL memoryTable st)
  INTERNAL_ASSERT "debugMemTable" (entries == 0) $ return (setL computeTable compute st)


cleanup :: CUDAState -> IO ()
cleanup st = do
  mapM_ release =<< Hash.toList tab
  writeIORef onta st
  where
    tab	          = getL memoryTable st
    release (k,v) = do
      CUDA.free $ CUDA.wordPtrToDevPtr (getL arena v)
      Hash.delete tab k


-- | Evaluate a CUDA array computation under the standard global environment
--
evalCUDA :: CIO a -> IO a
evalCUDA = liftM fst . runCUDA

runCUDA :: CIO a -> IO (a, CUDAState)
runCUDA acc = readIORef onta >>= sanitise >>= flip runCUDAWith acc


-- | Execute a computation under the provided state, returning the updated
-- environment structure and replacing the global state.
--
runCUDAWith :: CUDAState -> CIO a -> IO (a, CUDAState)
runCUDAWith state acc = do
  (a,s) <- runStateT acc state
  saveIndexFile s
  writeIORef onta s
  return (a,s)
  

-- hic sunt dracones: truly unsafe use of unsafePerformIO
onta :: IORef CUDAState
{-# NOINLINE onta #-}
onta = unsafePerformIO (initialise >>= newIORef)


-- Utility
-- -------

-- | A unique name supply
--
freshVar :: CIO Int
freshVar =  getM unique <* modM unique (+1)

