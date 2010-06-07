{-# LANGUAGE TemplateHaskell, TypeOperators #-}
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
    evalCUDA, runCUDA, CIO,   unique, deviceProps, memoryEntry, kernelEntry,
    KernelEntry(KernelEntry), kernelName, kernelStatus, Key,
    MemoryEntry(MemoryEntry), refcount, arena,

    freshVar,
    module Data.Record.Label
  )
  where

import Prelude hiding (id, (.), mod)
import Control.Category

import Data.Maybe
import Data.Record.Label
import Control.Applicative
import Control.Exception
import Control.Monad.State              (StateT(..), liftM)
import System.Posix.Types               (ProcessID)
import Data.Map                         (Map)
import Data.IntMap                      (IntMap)
import qualified Data.Map               as M  (empty)
import qualified Data.IntMap            as IM (empty)

import Foreign.Ptr
import qualified Foreign.CUDA.Driver    as CUDA


-- The CUDA State Monad
-- ~~~~~~~~~~~~~~~~~~~~

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
    runStateT acc (CUDAState 0 props IM.empty M.empty)
    --
    -- TLM 2010-06-05: assert all memory has been released ??
    --                 does CUDA.destroy release memory ??

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

