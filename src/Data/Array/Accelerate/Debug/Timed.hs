{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Timed
-- Copyright   : [2016..2017] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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
import Data.Time.Clock
import System.CPUTime
import Prelude

import GHC.Stats
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
timed_simpl :: MonadIO m => (Double -> Double -> String) -> m a -> m a
timed_simpl fmt action = do
  wall0 <- liftIO getCurrentTime
  cpu0  <- liftIO getCPUTime
  res   <- action
  wall1 <- liftIO getCurrentTime
  cpu1  <- liftIO getCPUTime
  --
  let wallTime = realToFrac (diffUTCTime wall1 wall0)
      cpuTime  = fromIntegral (cpu1 - cpu0) * 1E-12
  --
  liftIO $ putTraceMsg (fmt wallTime cpuTime)
  return res


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
  let toDouble :: Integral a => a -> Double
      toDouble = fromIntegral
      --
#if __GLASGOW_HASKELL__ < 802
      allocated   = toDouble (bytesAllocated gc1 - bytesAllocated gc0)
      copied      = toDouble (bytesCopied gc1 - bytesCopied gc0)
      totalWall   = wallSeconds gc1 - wallSeconds gc0
      totalCPU    = cpuSeconds gc1 - cpuSeconds gc0
      mutatorWall = mutatorWallSeconds gc1 - mutatorWallSeconds gc0
      mutatorCPU  = mutatorCpuSeconds gc1 - mutatorCpuSeconds gc0
      gcWall      = gcWallSeconds gc1 - gcWallSeconds gc0
      gcCPU       = gcCpuSeconds gc1 - gcCpuSeconds gc0
      totalGCs    = numGcs gc1 - numGcs gc0
#else
      gc0         = gc rts0
      gc1         = gc rts1
      allocated   = toDouble (gcdetails_allocated_bytes gc1 - gcdetails_allocated_bytes gc0)
      copied      = toDouble (gcdetails_copied_bytes gc1 - gcdetails_copied_bytes gc0)
      totalWall   = toDouble (elapsed_ns rts1 - elapsed_ns rts0) * 1.0E-9
      totalCPU    = toDouble (cpu_ns rts1 - cpu_ns rts0) * 1.0E-9
      mutatorWall = toDouble (mutator_elapsed_ns rts1 - mutator_elapsed_ns rts0) * 1.0E-9
      mutatorCPU  = toDouble (mutator_cpu_ns rts1 - mutator_cpu_ns rts0) * 1.0E-9
      gcWall      = toDouble (gcdetails_elapsed_ns gc1 - gcdetails_elapsed_ns gc0) * 1.0E-9
      gcCPU       = toDouble (gcdetails_cpu_ns gc1 - gcdetails_cpu_ns gc0) * 1.0E-9
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

