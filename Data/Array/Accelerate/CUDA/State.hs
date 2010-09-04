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
import Data.Binary
import Data.Record.Label
import Control.Arrow
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad                    (filterM)
import Control.Monad.State              (StateT(..), liftM)
import Data.HashTable                   (HashTable)
import qualified Data.HashTable         as HT

import Data.Array.Accelerate.CUDA.Analysis.Device

import System.Directory
import System.FilePath
import System.Posix.Types               (ProcessID)
import System.IO.Unsafe

import Foreign.Ptr
import qualified Foreign.CUDA.Driver    as CUDA

#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
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


-- Top-Level Mutable State
-- ~~~~~~~~~~~~~~~~~~~~~~~

-- We want the CUDA execution context to be generated as a shared on-demand IO
-- action for each device. Initialising a context is relatively expensive, so we
-- would like to only have to do this once per program execution.
--
-- Note that once created, a context is never destroyed (by the driver).
--
-- This uses the method proposed by Claus Reinke
-- <http://haskell.org/haskellwiki/Top_level_mutable_state>
--
mkOnceIO :: IO a -> IO (IO a)
mkOnceIO io = do
  mvar   <- newEmptyMVar
  demand <- newEmptyMVar
  _      <- forkIO $ takeMVar demand >> io >>= putMVar mvar
  return $  tryPutMVar demand () >> readMVar mvar


{-# NOINLINE deviceContext #-}
deviceContext :: CUDA.Device -> IO (CUDA.Context)
deviceContext (CUDA.Device dev) =
  let ctx = mapM (\n -> CUDA.create (CUDA.Device n) [CUDA.SchedAuto])
          =<< enumFromTo 0 . subtract 1 . fromIntegral <$> CUDA.count
  in
  (!! fromIntegral dev) <$> unsafePerformIO (mkOnceIO ctx)


-- The CUDA State Monad
-- ~~~~~~~~~~~~~~~~~~~~

-- Return the output directory for compilation by-products, creating if it does
-- not exist. This currently maps to a temporary directory, but could be mode to
-- point towards a persistent cache (eg: getAppUserDataDirectory)
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

-- Store the kernel module map to file. Additionally, this will unload the
-- compiled modules from the current context, and delete source files that
-- failed to compile.
--
save :: FilePath -> AccTable -> IO ()
save f m = encodeFile f . map (second _kernelName) =<< filterM compiled =<< HT.toList m
  where
    compiled (_,KernelEntry _ (Right h)) = CUDA.unload h >> return True
    compiled (_,KernelEntry n (Left  _)) = removeFile n  >> return False

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
  bracketOnError initialise finalise $ \(_,props,_) -> do
    dir   <- getOutputDir
    tab   <- HT.new (==) fromIntegral
    (k,n) <- load (dir </> "_index")
    (a,s) <- runStateT acc (CUDAState n dir props tab k)
    save (dir </> "_index") (_kernelTable s)

    -- TLM 2010-09-03:
    --   In case of memory leaks (which we should fix), manually release any
    --   lingering device arrays. These would otherwise remain until the next
    --   context release.
    --
    mapM_ (CUDA.free . CUDA.wordPtrToDevPtr . _arena . snd) =<< HT.toList (_memoryTable s)
    return (a,s)

  where
    finalise (_,_,ctx) = CUDA.destroy ctx
    initialise         = do
      CUDA.initialise []
      (dev,props) <- selectBestDevice
      ctx         <- deviceContext dev
      return (dev,props,ctx)


$(mkLabels [''CUDAState, ''MemoryEntry, ''KernelEntry])

-- | A unique name supply
--
freshVar :: CIO Int
freshVar =  getM unique <* modM unique (+1)

