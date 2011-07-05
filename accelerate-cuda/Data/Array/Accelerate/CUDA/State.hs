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

  evalCUDA, runCUDA, CIO,
  CUDAState, unique, deviceProps, deviceContext, memoryTable, kernelTable,

  KernelTable, KernelEntry(KernelEntry), kernelName, kernelStatus

) where

-- friends
import Data.Array.Accelerate.CUDA.Analysis.Device
import Data.Array.Accelerate.CUDA.Analysis.Hash
import Data.Array.Accelerate.CUDA.Array.Prim

-- library
import Data.IORef
import Data.Record.Label
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict                       (StateT(..))
import System.Posix.Types                               (ProcessID)
import System.IO.Unsafe
import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Data.HashTable                         as Hash

#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
import Data.Binary                                      (encodeFile, decodeFile)
import Control.Arrow                                    (second)
import Paths_accelerate                                 (getDataDir)
#endif


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
initialise :: IO CUDAState
initialise = do
  CUDA.initialise []
  (d,prp) <- selectBestDevice
  ctx     <- CUDA.create d [CUDA.SchedAuto]
  mem     <- newMT
  (knl,n) <- loadIndexFile
  return $ CUDAState n prp ctx knl mem


-- | Evaluate a CUDA array computation under the standard global environment
--
evalCUDA :: CIO a -> IO a
evalCUDA = liftM fst . runCUDA

runCUDA :: CIO a -> IO (a, CUDAState)
runCUDA acc =
  let -- hic sunt dracones: truly unsafe use of unsafePerformIO
      {-# NOINLINE onta #-}
      onta = unsafePerformIO
           $ do s <- initialise
                r <- newIORef s
                _ <- mkWeakIORef r $ CUDA.destroy (getL deviceContext s)
                return r
  in do
    (a,s) <- runStateT acc =<< readIORef onta
    saveIndexFile s
    writeIORef onta s
    return (a,s)

