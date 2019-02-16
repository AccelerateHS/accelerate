{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Timed
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Debug.Timed (

  timed,
  elapsed,

) where

import Data.Array.Accelerate.Debug.Trace
import Data.Array.Accelerate.Debug.Flags

import Control.Monad.Trans                              ( MonadIO )
import Text.Printf

#if ACCELERATE_DEBUG
import Control.Applicative
import Control.Monad.Trans                              ( liftIO )
import Data.List
import System.CPUTime
import Prelude

import GHC.Base
import GHC.Int
import GHC.Num
import GHC.Stats
import GHC.Word
#endif


-- | Execute an action and time the results. If GC stats have been enabled (with
-- @+RTS -t@ for example) then timing and memory usage information is displayed,
-- otherwise only timing information is shown.
--
{-# INLINEABLE timed #-}
timed :: MonadIO m => Flag -> (Double -> Double -> String) -> m a -> m a
#ifdef ACCELERATE_DEBUG
timed f fmt action = do
  enabled <- liftIO $ getFlag f
  if enabled
    then do
      with_gc <- liftIO $ (&&) <$> getRTSStatsEnabled <*> getFlag verbose
      if with_gc
        then timed_gc    fmt action
        else timed_simpl fmt action
    else
      action
#else
timed _ _ action = action
#endif

#ifdef ACCELERATE_DEBUG
{-# INLINEABLE timed_simpl #-}
timed_simpl :: MonadIO m => (Double -> Double -> String) -> m a -> m a
timed_simpl fmt action = do
  wall0 <- liftIO getMonotonicTime
  cpu0  <- liftIO getCPUTime
  res   <- action
  wall1 <- liftIO getMonotonicTime
  cpu1  <- liftIO getCPUTime
  --
  let wallTime = wall1 - wall0
      cpuTime  = D# (doubleFromInteger (cpu1 - cpu0) *## 1E-12##)
  --
  liftIO $ putTraceMsg (fmt wallTime cpuTime)
  return res

foreign import ccall unsafe "clock_gettime_monotonic_seconds" getMonotonicTime :: IO Double


{-# INLINEABLE timed_gc #-}
timed_gc :: MonadIO m => (Double -> Double -> String) -> m a -> m a
timed_gc fmt action = do
#if __GLASGOW_HASKELL__ < 802
  gc0   <- liftIO getGCStats
  res   <- action
  gc1   <- liftIO getGCStats
#else
  rts0  <- liftIO getRTSStats
  res   <- action
  rts1  <- liftIO getRTSStats
#endif
  --
  let
      w64 (W64# w#) = D# (word2Double# w#)
      i64 (I64# i#) = D# (int2Double# i#)
      --
#if __GLASGOW_HASKELL__ < 802
      allocated   = i64 (bytesAllocated gc1 - bytesAllocated gc0)
      copied      = i64 (bytesCopied gc1 - bytesCopied gc0)
      totalWall   = wallSeconds gc1 - wallSeconds gc0
      totalCPU    = cpuSeconds gc1 - cpuSeconds gc0
      mutatorWall = mutatorWallSeconds gc1 - mutatorWallSeconds gc0
      mutatorCPU  = mutatorCpuSeconds gc1 - mutatorCpuSeconds gc0
      gcWall      = gcWallSeconds gc1 - gcWallSeconds gc0
      gcCPU       = gcCpuSeconds gc1 - gcCpuSeconds gc0
      totalGCs    = numGcs gc1 - numGcs gc0
#else
      allocated   = w64 (allocated_bytes rts1 - allocated_bytes rts0)
      copied      = w64 (copied_bytes rts1 - copied_bytes rts0)
      totalWall   = i64 (elapsed_ns rts1 - elapsed_ns rts0) * 1.0E-9
      totalCPU    = i64 (cpu_ns rts1 - cpu_ns rts0) * 1.0E-9
      mutatorWall = i64 (mutator_elapsed_ns rts1 - mutator_elapsed_ns rts0) * 1.0E-9
      mutatorCPU  = i64 (mutator_cpu_ns rts1 - mutator_cpu_ns rts0) * 1.0E-9
      gcWall      = i64 (gc_elapsed_ns rts1 - gc_elapsed_ns rts0) * 1.0E-9
      gcCPU       = i64 (gc_cpu_ns rts1 - gc_cpu_ns rts0) * 1.0E-9
      totalGCs    = gcs rts1 - gcs rts0
#endif

  liftIO . putTraceMsg $ intercalate "\n"
    [ fmt totalWall totalCPU
    , printf "    %s allocated on the heap" (showFFloatSIBase (Just 1) 1024 allocated "B")
    , printf "    %s copied during GC (%d collections)" (showFFloatSIBase (Just 1) 1024 copied "B") totalGCs
    , printf "    MUT: %s" (elapsed mutatorWall mutatorCPU)
    , printf "    GC:  %s" (elapsed gcWall gcCPU)
    ]
  --
  return res

#if __GLASGOW_HASKELL__ < 802
getRTSStatsEnabled :: IO Bool
getRTSStatsEnabled = getGCStatsEnabled
#endif
#endif

elapsed :: Double -> Double -> String
elapsed wallTime cpuTime =
  printf "%s (wall), %s (cpu)"
    (showFFloatSIBase (Just 3) 1000 wallTime "s")
    (showFFloatSIBase (Just 3) 1000 cpuTime  "s")

