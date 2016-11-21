{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Monitoring
-- Copyright   : [2016] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Debug.Monitoring (

  beginMonitoring,
  initAccMetrics,

  Processor(..),
  withProcessor, addProcessorTime,
  didAllocateBytes,
  didEvictLRU,
  didMajorGC,

) where

#ifdef ACCELERATE_MONITORING
import Data.Atomic                                                  ( Atomic )
import qualified Data.Atomic                                        as Atomic

import System.Metrics
import System.Metrics.Counter                                       ( Counter )
import System.Metrics.Gauge                                         ( Gauge )
import qualified System.Metrics.Counter                             as Counter
import qualified System.Metrics.Gauge                               as Gauge

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.IORef
import Data.Text                                                    ( Text )
import Data.Time.Clock
import System.IO.Unsafe
import System.Remote.Monitoring
import Text.Printf
import qualified Data.HashMap.Strict                                as Map
#endif

import Data.Int
import Prelude


-- | Launch a monitoring server that will collect statistics on the running
-- application. This should be called as soon as the application starts. The
-- program will need to be run with the RTS option -T.
--
beginMonitoring :: IO ()
#ifdef ACCELERATE_MONITORING
beginMonitoring = do
  store <- initAccMetrics
  registerGcMetrics store
  r     <- withAsync (forkServerWith store "localhost" 8000 >> threadDelay 10000) waitCatch
  case r of
    Right _ -> printf "EKG monitor started at: http://localhost:8000\n"
    Left _  -> printf "Failed to start EKG monitor\n"
#else
beginMonitoring = return ()
#endif


-- | Initialise and return the Accelerate monitoring store. To enable monitoring
-- of your application:
--
-- > import Data.Array.Accelerate.Debug
-- >
-- > import System.Metrics
-- > import System.Remote.Monitoring
-- >
-- > main :: IO ()
-- > main = do
-- >   store  <- initAccMetrics
-- >   registerGcMetrics store      -- optional
-- >
-- >   server <- forkServerWith store "localhost" 8000
-- >
-- >   ...
--
#ifndef ACCELERATE_MONITORING
initAccMetrics :: IO a
initAccMetrics = error "Data.Array.Accelerate: Monitoring is disabled. Reinstall package 'accelerate' with '-fekg' to enable it."
#else
initAccMetrics :: IO Store
initAccMetrics = do
  store <- newStore

  registerRate    "acc.load.llvm_native"            (calculateProcessorLoad _active_ns_llvm_native) store
  registerRate    "acc.load.llvm_ptx"               (calculateProcessorLoad _active_ns_llvm_ptx)    store
  registerRate    "acc.load.cuda"                   (calculateProcessorLoad _active_ns_cuda)        store
  registerCounter "acc.gc.bytes_allocated"          (Counter.read _bytesAllocated)                  store
  registerCounter "acc.gc.bytes_copied_to_remote"   (Counter.read _bytesCopiedToRemote)             store
  registerCounter "acc.gc.bytes_copied_from_remote" (Counter.read _bytesCopiedFromRemote)           store
  registerGauge   "acc.gc.current_bytes_active"     (Gauge.read   _bytesActive)                     store
  registerGauge   "acc.gc.current_bytes_nursery"    (Gauge.read   _bytesNursery)                    store
  registerCounter "acc.gc.num_gcs"                  (Counter.read _numMajorGC)                      store
  registerCounter "acc.gc.num_lru_evict"            (Counter.read _numEvictions)                    store

  return store


-- Abusing 'registerGroup' to perform the rate calculation on every wake-up.
--
registerRate :: Text -> (IORef EMAState -> IO Int64) -> Store -> IO ()
registerRate name sample store = do
  now <- getCurrentTime
  st  <- newIORef (ES now 0 0)
  registerGroup (Map.singleton name Gauge) (sample st) store
#endif


-- Recording metrics
-- -----------------

data Processor = Native | PTX | CUDA

-- | Execute the given action and assign the elapsed wall-clock time as active
-- time for the given processing element.
--
{-# INLINE withProcessor #-}
withProcessor :: Processor -> IO a -> IO a
#ifndef ACCELERATE_MONITORING
withProcessor _      = id
#else
withProcessor Native = withProcessor' _active_ns_llvm_native
withProcessor PTX    = withProcessor' _active_ns_llvm_ptx
withProcessor CUDA   = withProcessor' _active_ns_cuda

withProcessor' :: Atomic -> IO a -> IO a
withProcessor' var action = do
  wall0 <- getCurrentTime
  !r    <- action
  wall1 <- getCurrentTime
  addProcessorTime' var (realToFrac (diffUTCTime wall1 wall0))
  return r
#endif

-- | Record the given number of seconds as active processing time for the given
-- processing element.
--
{-# INLINE addProcessorTime #-}
addProcessorTime :: Processor -> Double -> IO ()
#ifndef ACCELERATE_MONITORING
addProcessorTime _ _    = return ()
#else
addProcessorTime Native = addProcessorTime' _active_ns_llvm_native
addProcessorTime PTX    = addProcessorTime' _active_ns_llvm_ptx
addProcessorTime CUDA   = addProcessorTime' _active_ns_cuda

addProcessorTime' :: Atomic -> Double -> IO ()
addProcessorTime' var secs =
  let ns   = round (secs * 1.0E9)
  in  void $ Atomic.add var ns
#endif


didAllocateBytes :: Int64 -> IO ()
didEvictLRU      :: IO ()
didMajorGC       :: IO ()

#ifndef ACCELERATE_MONITORING
didAllocateBytes _ = return ()
didEvictLRU        = return ()
didMajorGC         = return ()
#else
didAllocateBytes n = do
  Counter.add _bytesAllocated n
  Gauge.add   _bytesActive    n

didEvictLRU = Counter.inc _numEvictions

didMajorGC  = do
  Counter.inc _numMajorGC
  Gauge.set   _bytesNursery 0    -- ???
#endif


-- Implementation
-- --------------

#ifdef ACCELERATE_MONITORING

-- In order to calculate the processor load we need to remember the previous
-- values. Storing this state in an IORef has a bit of extra overhead (as
-- indirection) compared to the rest of the monitoring counters (which are
-- unboxed values on the heap manipulated directly with atomic primops), but
-- since 'calculateProcessorLoad' will only be called by the EKG monitor
-- whenever it refreshes the value for display, rather than running continuously
-- in the background, we should be okay.
--
data EMAState = ES
  { old_time  :: {-# UNPACK #-} !UTCTime
  , old_inst  :: {-# UNPACK #-} !Double
  , old_avg   :: {-# UNPACK #-} !Double
  }

-- Estimate the load on the processor as a moving exponential average
--
calculateProcessorLoad :: Atomic -> IORef EMAState -> IO Int64
calculateProcessorLoad !var !ref = do
  ES{..} <- readIORef ref
  time   <- getCurrentTime
  sample <- Atomic.and var 0
  --
  let
      active_ns   = fromIntegral sample
      elapsed_s   = realToFrac (diffUTCTime time old_time)
      elapsed_ns  = 1.0E9 * elapsed_s
      --
      load_inst   = 100 * (active_ns / elapsed_ns)                -- instantaneous load
      load_avg    = ema 0.2 elapsed_s old_avg old_inst load_inst  -- moving average load
  --
  writeIORef ref (ES time load_inst load_avg)
  return (round load_avg)


{--
-- Compute the current load on a processor as a percentage of time spent working
-- over the elapsed time. This is meant to run continuously by a background
-- thread, updating the gauge each time it wakes up.
--
monitorProcessorLoad :: Gauge -> Atomic -> UTCTime -> Double -> Double -> IO ()
monitorProcessorLoad !gauge !var !old_time !old_inst !old_avg = do
  time   <- getCurrentTime
  sample <- Atomic.and var 0

  let
      active_ns   = fromIntegral sample
      elapsed_s   = realToFrac (diffUTCTime time old_time)
      elapsed_ns  = 1.0E9 * elapsed_s

      load_inst   = 100 * (active_ns / elapsed_ns)                -- instantaneous load
      load_avg    = ema 0.2 elapsed_s old_avg old_inst load_inst  -- moving average load

  -- Set what we thing the processor load over the previous interval should be
  Gauge.set gauge (round load_avg)

  -- Sleep for a bit, then do it all again
  threadDelay 500000    -- 500 ms
  monitorProcessorLoad gauge var time load_inst load_avg
--}

-- Exponential moving average for irregular time series
--
ema :: Double -> Double -> Double -> Double -> Double -> Double
ema !alpha !dt !old_ema !old_sample !new_sample =
  let
      a = dt / alpha
      u = exp ( -a )
      v = ( 1 - u ) / a
  in
  (u * old_ema) + ((v-u) * old_sample) + ((1-v) * new_sample)


-- Monitoring variables
-- --------------------

-- Number of nanoseconds a backend has spent doing real work since the last
-- check. This is an integer amount because there are no built-in functions for
-- atomic memory access on double precision (as specified by the Intel docs).
--
{-# NOINLINE _active_ns_llvm_native #-}
_active_ns_llvm_native :: Atomic
_active_ns_llvm_native = unsafePerformIO (Atomic.new 0)

{-# NOINLINE _active_ns_llvm_ptx #-}
_active_ns_llvm_ptx :: Atomic
_active_ns_llvm_ptx = unsafePerformIO (Atomic.new 0)

{-# NOINLINE _active_ns_cuda #-}
_active_ns_cuda :: Atomic
_active_ns_cuda = unsafePerformIO (Atomic.new 0)

-- Total number of bytes allocated in the remote address space (e.g. on the GPU)
--
{-# NOINLINE _bytesAllocated #-}
_bytesAllocated :: Counter
_bytesAllocated = unsafePerformIO Counter.new

-- Total number of bytes copied from the host to the remote memory space
--
{-# NOINLINE _bytesCopiedToRemote #-}
_bytesCopiedToRemote :: Counter
_bytesCopiedToRemote = unsafePerformIO Counter.new

-- Total number of bytes copied from the remote memory space back to the device
--
{-# NOINLINE _bytesCopiedFromRemote #-}
_bytesCopiedFromRemote :: Counter
_bytesCopiedFromRemote = unsafePerformIO Counter.new

-- Current working remote memory size
--
{-# NOINLINE _bytesActive #-}
_bytesActive :: Gauge
_bytesActive = unsafePerformIO Gauge.new

-- Current size of the nursery
--
{-# NOINLINE _bytesNursery #-}
_bytesNursery :: Gauge
_bytesNursery = unsafePerformIO Gauge.new

-- Number of times the nursery was flushed
--
{-# NOINLINE _numMajorGC #-}
_numMajorGC :: Counter
_numMajorGC = unsafePerformIO Counter.new

-- number of LRU eviction events
--
{-# NOINLINE _numEvictions #-}
_numEvictions :: Counter
_numEvictions = unsafePerformIO Counter.new

#endif

