{-# LANGUAGE GADTs, TypeOperators #-}
{-# LANGUAGE TemplateHaskell      #-}
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
    CIO,
    CUDAState(CUDAState),     unique, deviceProps, memoryEntry, kernelEntry,
    KernelEntry(KernelEntry), compilerPID, kernelName,  kernelConfig, Key,
    MemoryEntry(MemoryEntry), refcount, arena,

    module Data.Record.Label, modM'
  )
  where

import Prelude hiding (id, (.), mod)
import Control.Category

import Data.Map                         (Map)
import Data.IntMap                      (IntMap)
import System.Posix.Types               (ProcessID)
import Control.Monad.State              (StateT, MonadState)
import Text.Show.Functions              ()
import Data.Record.Label

import Foreign.Ptr
import qualified Foreign.CUDA           as CUDA


-- |
-- The state token for accelerated CUDA array operations
--
type CIO       = StateT CUDAState IO
data CUDAState = CUDAState
  {
    _unique      :: Int,
    _deviceProps :: CUDA.DeviceProperties,
    _memoryEntry :: IntMap MemoryEntry,
    _kernelEntry :: Map Key KernelEntry
  }
  deriving (Show)

-- |
-- Associate an array expression with an external compilation tool (nvcc) and
-- parameters for optimal execution.
--
type Key         = (Int, String)
data KernelEntry = KernelEntry
  {
    _compilerPID  :: ProcessID,
    _kernelName   :: String,
    _kernelConfig :: (Int -> Int, Int -> Int)
  }
  deriving (Show)

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
  deriving (Show)

$(mkLabels [''CUDAState, ''MemoryEntry, ''KernelEntry])


-- The derived labels (documentation goes here)
--
unique       :: CUDAState :-> Int
deviceProps  :: CUDAState :-> CUDA.DeviceProperties
memoryEntry  :: CUDAState :-> IntMap MemoryEntry
kernelEntry  :: CUDAState :-> Map Key KernelEntry

refcount     :: MemoryEntry :-> Int
arena        :: MemoryEntry :-> WordPtr

compilerPID  :: KernelEntry :-> ProcessID
kernelName   :: KernelEntry :-> String
kernelConfig :: KernelEntry :-> (Int -> Int, Int -> Int)

-- |
-- Modify a value with a function in the state pointed to by the specified
-- label, returning the old value
--
modM' :: MonadState s m => s :-> b -> (b -> b) -> m b
modM' l f = do
  v <- getM l
  modM l f
  return v

