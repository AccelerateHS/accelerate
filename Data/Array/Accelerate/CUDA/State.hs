{-# LANGUAGE CPP, TemplateHaskell, TupleSections, TypeOperators #-}
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

import Prelude hiding (id, (.))
import Control.Category

import Data.Int
import Data.IORef
import Data.Record.Label
import Control.Applicative
import Control.Monad
import Control.Monad.State              (StateT(..))
import Data.HashTable                   (HashTable)
import Foreign.Ptr
import qualified Data.HashTable         as HT
import qualified Foreign.CUDA.Driver    as CUDA

import System.Directory
import System.FilePath
import System.Posix.Types               (ProcessID)
import System.IO.Unsafe

import Data.Array.Accelerate.CUDA.Analysis.Device

#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
import Data.Binary                      (encodeFile, decodeFile)
import Control.Arrow                    (second)
import Paths_accelerate                 (getDataDir)
#else
import System.Posix.Process             (getProcessID)
#endif


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

$(mkLabels [''CUDAState, ''MemoryEntry, ''KernelEntry])


-- Execution State
-- ~~~~~~~~~~~~~~~

-- Return the output directory for compilation by-products, creating if it does
-- not exist.
--
getOutputDir :: IO FilePath
getOutputDir = do
#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
  tmp <- getDataDir
  dir <- canonicalizePath $ tmp </> "cache"
#else
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  dir <- canonicalizePath $ tmp </> "ac" ++ show pid
#endif
  createDirectoryIfMissing True dir
  return dir

-- Store the kernel module map to file
--
saveIndexFile :: CUDAState -> IO ()
#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
saveIndexFile s = encodeFile (_outputDir s </> "_index") . map (second _kernelName) =<< HT.toList (_kernelTable s)
#else
saveIndexFile _ = return ()
#endif

-- Read the kernel index map file (if it exists), loading modules into the
-- current context
--
loadIndexFile :: FilePath -> IO (AccTable, Int)
#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
loadIndexFile f = do
  x <- doesFileExist f
  e <- if x then mapM reload =<< decodeFile f
            else return []
  (,length e) <$> HT.fromList HT.hashString e
  where
    reload (k,n) = (k,) . KernelEntry n . Right <$> CUDA.loadFile (n `replaceExtension` ".cubin")
#else
loadIndexFile _  = (,0) <$> HT.new (==) HT.hashString
#endif


-- Select and initialise the CUDA device, and create a new execution context.
-- This will be done only once per program execution, as initialising the CUDA
-- context is relatively expensive.
--
initialise :: IO CUDAState
initialise = do
  CUDA.initialise []
  (d,prp) <- selectBestDevice
  _       <- CUDA.create d [CUDA.SchedAuto]
  dir     <- getOutputDir
  mem     <- HT.new (==) fromIntegral
  (knl,n) <- loadIndexFile (dir </> "_index")
  return $ CUDAState n dir prp mem knl


-- |
-- Evaluate a CUDA array computation under a newly initialised environment,
-- discarding the final state.
--
evalCUDA :: CIO a -> IO a
evalCUDA = liftM fst . runCUDA

runCUDA :: CIO a -> IO (a, CUDAState)
runCUDA acc = do
  state <- readIORef ref
  clearMemTable state   -- ugly kludge
  (a,s) <- runStateT acc state
  saveIndexFile s
  writeIORef ref s
  return (a,s)
  where
    {-# NOINLINE ref #-} -- hic sunt dracones: truly unsafe use of unsafePerformIO
    ref = unsafePerformIO $ do
      r <- initialise >>= newIORef
      _ <- mkWeakIORef r (CUDA.pop >>= CUDA.destroy)
      return r


-- In case of memory leaks, which we should fix, manually release any lingering
-- device arrays. These would otherwise remain until the program exits.
--
clearMemTable :: CUDAState -> IO ()
clearMemTable st = do
  CUDA.sync     -- TLM: not blocking??
  entries <- HT.toList (_memoryTable st)
  forM_ entries $ \(k,v) -> do
    HT.delete (_memoryTable st) k
    CUDA.free (CUDA.wordPtrToDevPtr (_arena v))


-- Utility
-- ~~~~~~~

-- | A unique name supply
--
freshVar :: CIO Int
freshVar =  getM unique <* modM unique (+1)

