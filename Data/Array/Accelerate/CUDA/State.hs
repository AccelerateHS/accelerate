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
    evalCUDA, runCUDA, runCUDAWith, CIO, CUDAState,
    unique, deviceProps, deviceContext, memoryTable, kernelTable, computeTable,

    KernelEntry(KernelEntry), kernelName, kernelStatus,
    MemoryEntry(MemoryEntry), refcount, memsize, arena,
    AccNode(AccNode), usecount, executable,

    newAccHashTable, AccHashTable, StableAccName(..),

    module Data.Record.Label
  )
  where

import Prelude hiding (id, (.))
import Control.Category
import Data.Record.Label

import Data.Int
import Data.Char
import Data.IORef
import Data.Maybe
import Data.Typeable
import Data.ByteString.Lazy.Char8                       (ByteString)
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict                       (StateT(..))
import Data.HashTable                                   (HashTable)
import Foreign.Ptr
import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Data.HashTable                         as Hash
import qualified Data.ByteString.Lazy.Char8             as L

import System.Posix.Types                               (ProcessID)
import System.Mem.Weak
import System.Mem.StableName
import System.IO.Unsafe

import Data.Array.Accelerate.CUDA.Analysis.Device

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
type KernelTable = HashTable ByteString KernelEntry
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
type MemoryTable = HashTable WordPtr MemoryEntry
data MemoryEntry = MemoryEntry
  {
    _refcount :: Maybe Int,     -- set to 'Nothing', the array will never be released
    _memsize  :: Int64,
    _arena    :: WordPtr
  }

-- Opaque stable names for array computations
--
-- TLM: This should be more specific: "StableName (OpenAcc aenv a)", so that it
--      is not possible to insert the stable names logically different things.
--
data StableAccName where
  StableAccName :: Typeable a => StableName a -> StableAccName

instance Show StableAccName where
  show (StableAccName sn) = show $ hashStableName sn

instance Eq StableAccName where
  StableAccName sn1 == StableAccName sn2
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

-- Hash table keyed on the stable name of array computations
--
type AccHashTable v = Hash.HashTable StableAccName v

newAccHashTable :: IO (AccHashTable v)
newAccHashTable = Hash.new (==) hashStableAcc
  where
    hashStableAcc (StableAccName sn) = fromIntegral (hashStableName sn)

-- Relate a particular computation node directly to the object code that will be
-- used to execute the computation, together with a use count information for
-- the resultant array.
--
data AccNode = AccNode
  {
    _usecount   :: (Int,Int),
    _executable :: Maybe CUDA.Module
  }
  -- TLM: The scanl' and scanr' primitives return two arrays, which we need to
  --      keep separate usage counts for. For all other node types, ignore the
  --      second component of the tuple.
  --


-- The state token for accelerated CUDA array operations
--
type CIO       = StateT CUDAState IO
data CUDAState = CUDAState
  {
    _unique        :: Int,
    _deviceProps   :: CUDA.DeviceProperties,
    _deviceContext :: CUDA.Context,
    _kernelTable   :: KernelTable,

    _memoryTable   :: MemoryTable,              -- TLM: these are non-persistent between computations,
    _computeTable  :: AccHashTable AccNode      --      so maybe they should live elsewhere?
  }

$(mkLabels [''CUDAState, ''MemoryEntry, ''KernelEntry, ''AccNode])


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
  (,length e) <$> Hash.fromList hashByteString e
  where
    reload (k,n) = (k,) . KernelEntry n . Right <$> CUDA.loadFile (n `replaceExtension` ".cubin")
#else
loadIndexFile = (,0) <$> Hash.new (==) hashByteString
#endif


-- Reimplementation of Data.HashTable.hashString to fold over a lazy bytestring
-- rather than a list of characters.
--
hashByteString :: ByteString -> Int32
hashByteString = L.foldl' f golden
  where
    f m c  = fromIntegral (ord c) * magic + Hash.hashInt (fromIntegral m)
    magic  = 0xdeadbeef
    golden = 1013904242 -- = round ((sqrt 5 - 1) * 2^32)


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
  return $ CUDAState n prp ctx knl undefined undefined


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
      entries <- length . filter (isJust . getL refcount . snd) <$> Hash.toList (getL memoryTable st)
      INTERNAL_ASSERT "runCUDA.sanitise" (entries == 0)
        $ return (setL memoryTable undefined . setL computeTable undefined $ st)


-- hic sunt dracones: truly unsafe use of unsafePerformIO
onta :: IORef CUDAState
{-# NOINLINE onta #-}
onta = unsafePerformIO (initialise >>= newIORef)

