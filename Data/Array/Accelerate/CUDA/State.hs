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

  evalCUDA, runCUDA, runCUDAWith, CIO,
  CUDAState, unique, deviceProps, deviceContext, memoryTable, kernelTable,

  KernelTable, KernelEntry(KernelEntry), kernelName, kernelStatus,
  MemoryEntry(..), AccArrayData(..), refcount, newAccMemoryTable

) where

-- friends
import Data.Array.Accelerate.CUDA.Analysis.Device
import Data.Array.Accelerate.CUDA.Analysis.Hash
import qualified Data.Array.Accelerate.Array.Data       as AD

-- library
import Data.Int
import Data.IORef
import Data.Maybe
import Data.Typeable
import Data.Record.Label
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict                       (StateT(..))
import System.Posix.Types                               (ProcessID)
import System.Mem.Weak
import System.IO.Unsafe
import Foreign.Ptr
import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Data.HashTable                         as Hash

#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
import Data.Binary                                      (encodeFile, decodeFile)
import Control.Arrow                                    (second)
import Paths_accelerate                                 (getDataDir)
#endif

#include "accelerate.h"


-- An exact association between an accelerate computation and its
-- implementation, which is either a reference to the external compiler (nvcc)
-- or the resulting binary module. This is keyed by a string representation of
-- the generated kernel code.
--
-- An Eq instance of Accelerate expressions does not facilitate persistent
-- caching.
--
type KernelTable = Hash.HashTable AccKey KernelEntry
data KernelEntry = KernelEntry
  {
    _kernelName   :: FilePath,
    _kernelStatus :: Either ProcessID CUDA.Module
  }

-- Associations between host- and device-side arrays, with reference counting.
-- Facilitates reuse and delayed allocation at the cost of explicit release.
--
-- This maps to a single concrete array. Arrays of tuples, which are represented
-- internally as tuples of arrays, will generate multiple entries.
--
type MemoryTable = Hash.HashTable AccArrayData MemoryEntry

data AccArrayData where
  AccArrayData :: (Typeable a, AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e)
               => AD.ArrayData e
               -> AccArrayData

instance Eq AccArrayData where
  AccArrayData ad1 == AccArrayData ad2
    | Just p1 <- gcast (AD.ptrsOfArrayData ad1) = p1 == AD.ptrsOfArrayData ad2
    | otherwise                                 = False

data MemoryEntry where
  MemoryEntry :: Typeable a
              => Maybe Int         -- if Nothing, the array is not released by 'freeArray'
              -> CUDA.DevicePtr a
              -> MemoryEntry

newAccMemoryTable :: IO MemoryTable
newAccMemoryTable = Hash.new (==) hashAccArray
  where
    hashAccArray :: AccArrayData -> Int32
    hashAccArray (AccArrayData ad) = fromIntegral . ptrToIntPtr
                                   $ AD.ptrsOfArrayData ad

refcount :: MemoryEntry :-> Maybe Int
refcount = lens get set
  where
    get   (MemoryEntry c _) = c
    set c (MemoryEntry _ p) = MemoryEntry c p


-- The state token for accelerated CUDA array operations
--
-- TLM: the memory table is not persistent between computations. Move elsewhere?
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

#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
indexFileName :: IO FilePath
indexFileName = do
  tmp <- (</> "cache") `fmap` getDataDir
  dir <- createDirectoryIfMissing True tmp >> canonicalizePath tmp
  return (dir </> "_index")
#endif

-- Store the kernel module map to file
--
saveIndexFile :: CUDAState -> IO ()
#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
saveIndexFile s = do
  ind <- indexFileName
  encodeFile ind . map (second _kernelName) =<< Hash.toList (_kernelTable s)
#else
saveIndexFile _ = return ()
#endif

-- Read the kernel index map file (if it exists), loading modules into the
-- current context
--
loadIndexFile :: IO (KernelTable, Int)
#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
loadIndexFile = do
  f <- indexFileName
  x <- doesFileExist f
  e <- if x then mapM reload =<< decodeFile f
            else return []
  (,length e) <$> Hash.fromList hashAccKey e
  where
    reload (k,n) = (k,) . KernelEntry n . Right <$> CUDA.loadFile (n `replaceExtension` ".cubin")
#else
loadIndexFile = (,0) <$> Hash.new (==) hashAccKey
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
  (knl,n) <- loadIndexFile
  addFinalizer ctx (CUDA.destroy ctx)
  return $ CUDAState n prp ctx knl undefined


-- | Evaluate a CUDA array computation under the standard global environment
--
evalCUDA :: CIO a -> IO a
evalCUDA = liftM fst . runCUDA

runCUDA :: CIO a -> IO (a, CUDAState)
runCUDA acc = readIORef onta >>= flip runCUDAWith acc


-- | Execute a computation under the provided state, returning the updated
-- environment structure and replacing the global state.
--
runCUDAWith :: CUDAState -> CIO a -> IO (a, CUDAState)
runCUDAWith state acc = do
  (a,s) <- runStateT acc state
  saveIndexFile s
  writeIORef onta =<< sanitise s
  return (a,s)
  where
    -- The memory table and compute table are transient data structures: they
    -- exist only for the life of a single computation [stream]. Don't record
    -- them into the persistent state token.
    --
    sanitise :: CUDAState -> IO CUDAState
    sanitise st = do
      entries <- filter (isJust . getL refcount . snd) <$> Hash.toList (getL memoryTable st)
      INTERNAL_ASSERT "runCUDA.sanitise" (null entries)
        $ return (setL memoryTable undefined st)


-- Nasty global statesses
-- ----------------------

{--
-- Execute an IO action at most once
--
mkOnceIO :: IO a -> IO (IO a)
mkOnceIO io = do
  mvar   <- newEmptyMVar
  demand <- newEmptyMVar
  forkIO (takeMVar demand >> io >>= putMVar mvar)
  return (tryPutMVar demand ()  >>  readMVar mvar)
--}

-- hic sunt dracones: truly unsafe use of unsafePerformIO
--
onta :: IORef CUDAState
{-# NOINLINE onta #-}
onta = unsafePerformIO (initialise >>= newIORef)

