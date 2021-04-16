{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Internal.Timed
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Debug.Internal.Timed (

  timed,
  elapsed,

) where

import Data.Array.Accelerate.Debug.Internal.Flags
import Data.Array.Accelerate.Debug.Internal.Trace

import Control.Monad.Trans                              ( MonadIO )
import Data.Text.Lazy.Builder
import Data.Text.Format

#if ACCELERATE_DEBUG
import Data.Array.Accelerate.Debug.Internal.Clock

import Control.Applicative
import Control.Monad.Trans                              ( liftIO )
import System.CPUTime
import Prelude

import GHC.Int
import GHC.Num
import GHC.Prim
import GHC.Stats
import GHC.Types
import GHC.Word
#endif


-- | Execute an action and time the results. If GC stats have been enabled (with
-- @+RTS -t@ for example) then timing and memory usage information is displayed,
-- otherwise only timing information is shown.
--
{-# INLINEABLE timed #-}
timed :: MonadIO m => Flag -> (Double -> Double -> Builder) -> m a -> m a
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
timed_simpl :: MonadIO m => (Double -> Double -> Builder) -> m a -> m a
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


{-# INLINEABLE timed_gc #-}
timed_gc :: MonadIO m => (Double -> Double -> Builder) -> m a -> m a
timed_gc fmt action = do
  rts0  <- liftIO getRTSStats
  res   <- action
  rts1  <- liftIO getRTSStats
  --
  let
      w64 (W64# w#) = D# (word2Double# w#)
      i64 (I64# i#) = D# (int2Double# i#)
      --
      allocated   = w64 (allocated_bytes rts1 - allocated_bytes rts0)
      copied      = w64 (copied_bytes rts1 - copied_bytes rts0)
      totalWall   = i64 (elapsed_ns rts1 - elapsed_ns rts0) * 1.0E-9
      totalCPU    = i64 (cpu_ns rts1 - cpu_ns rts0) * 1.0E-9
      mutatorWall = i64 (mutator_elapsed_ns rts1 - mutator_elapsed_ns rts0) * 1.0E-9
      mutatorCPU  = i64 (mutator_cpu_ns rts1 - mutator_cpu_ns rts0) * 1.0E-9
      gcWall      = i64 (gc_elapsed_ns rts1 - gc_elapsed_ns rts0) * 1.0E-9
      gcCPU       = i64 (gc_cpu_ns rts1 - gc_cpu_ns rts0) * 1.0E-9
      totalGCs    = gcs rts1 - gcs rts0

  liftIO
    . putTraceMsg
    $ foldr1 (\a b -> a <> singleton '\n' <> b)
    [ fmt totalWall totalCPU
    , build "    {} allocated on the heap" (Only (showFFloatSIBase (Just 1) 1024 allocated "B"))
    , build "    {} copied during GC ({} collections)" (showFFloatSIBase (Just 1) 1024 copied "B", totalGCs)
    , build "    MUT: {}" (Only (elapsed mutatorWall mutatorCPU))
    , build "    GC:  {}" (Only (elapsed gcWall gcCPU))
    ]
  --
  return res
#endif

elapsed :: Double -> Double -> Builder
elapsed wallTime cpuTime =
  build "{} (wall), {} (cpu)"
    ( showFFloatSIBase (Just 3) 1000 wallTime "s"
    , showFFloatSIBase (Just 3) 1000 cpuTime "s"
    )

