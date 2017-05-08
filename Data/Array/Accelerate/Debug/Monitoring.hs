{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Monitoring
-- Copyright   : [2016..2017] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Debug.Monitoring (

  beginMonitoring,
  initAccMetrics,

  -- Load monitoring
  Processor(..),
  withProcessor, addProcessorTime,

  -- GC subsystem monitoring
  didAllocateBytesLocal, didAllocateBytesRemote,
  didCopyBytesToRemote, didCopyBytesFromRemote,
  increaseCurrentBytesRemote, decreaseCurrentBytesRemote,
  setCurrentBytesNursery,
  didRemoteGC,
  didEvictBytes,

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
-- Note that aside from the processor load metrics, counters are shared between
-- all active backends.
--
-- Registered rates:
--
-- [@acc.load.llvm_native@] Current processor load (%) of the LLVM CPU backend.
-- This only includes time spent executing Accelerate functions; compare this to
-- the total processor load (e.g. via top) to estimate the productivity of the
-- Accelerate program.
--
-- [@acc.load.llvm_ptx@] Current processor load (%) of the GPU in the LLVM PTX
-- backend. This only takes into account how much time the GPU spent executing
-- Accelerate code, and does not consider the number of active cores during that
-- time.
--
-- Registered gauges:
--
-- [@acc.gc.current_bytes_remote@] Total number of bytes currently considered
-- live in the remote address space.
--
-- [@acc.gc.current_bytes_nursery@] Total number of bytes allocated in the
-- remote address space but not currently live (available for reallocation).
--
-- Registered counters:
--
-- [@acc.gc.bytes_allocated_local@] Total number of bytes allocated in the local
-- address space.
--
-- [@acc.gc.bytes_allocated_remote@] Total number of bytes allocated in the
-- remote address space.
--
-- [@acc.gc.bytes_copied_to_remote@] Total number of bytes copied from the host
-- to the remote address space (e.g. from the CPU to the GPU).
--
-- [@acc.gc.bytes_copied_from_remote@] Total number of bytes copied from the
-- remote address space back to the host (e.g. from the GPU back to the CPU).
--
-- [@acc.gc.bytes_evicted_from_remote@] Total number of bytes evicted from the
-- remote address space by the LRU memory manager, in order to make space for
-- new allocations. A subset of __acc.gc.bytes_copied_from_remote__.
--
-- [@acc.gc.num_gcs@] Number of garbage collections of the remote address space
-- performed.
--
-- [@acc.gc.num_lru_evict@] Total number of evictions from the remote address
-- space performed.
--
#ifndef ACCELERATE_MONITORING
initAccMetrics :: IO a
initAccMetrics = error "Data.Array.Accelerate: Monitoring is disabled. Reinstall package 'accelerate' with '-fekg' to enable it."
#else
initAccMetrics :: IO Store
initAccMetrics = do
  store <- newStore

  registerRate    "acc.load.llvm_native"              (estimateProcessorLoad _active_ns_llvm_native)           store
  registerRate    "acc.load.llvm_ptx"                 (estimateProcessorLoad _active_ns_llvm_ptx)              store
  registerGauge   "acc.gc.current_bytes_remote"       (Gauge.read _current_bytes_remote)                       store
  registerGauge   "acc.gc.current_bytes_nursery"      (Gauge.read _current_bytes_nursery)                      store
  registerCounter "acc.gc.bytes_allocated_local"      (Counter.read _total_bytes_allocated_local)              store
  registerCounter "acc.gc.bytes_allocated_remote"     (Counter.read _total_bytes_allocated_remote)             store
  registerCounter "acc.gc.bytes_copied_to_remote"     (Counter.read _total_bytes_copied_to_remote)             store
  registerCounter "acc.gc.bytes_copied_from_remote"   (Counter.read _total_bytes_copied_from_remote)           store
  registerCounter "acc.gc.bytes_evicted_from_remote"  (Counter.read _total_bytes_evicted_from_remote)          store
  registerCounter "acc.gc.num_gcs"                    (Counter.read _num_remote_gcs)                           store
  registerCounter "acc.gc.num_lru_evict"              (Counter.read _num_evictions)                            store

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

data Processor = Native | PTX

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

addProcessorTime' :: Atomic -> Double -> IO ()
addProcessorTime' var secs =
  let ns   = round (secs * 1.0E9)
  in  void $ Atomic.add var ns
#endif


-- | Allocated the number of bytes in the local memory space
--
didAllocateBytesLocal :: Int64 -> IO ()
#ifndef ACCELERATE_MONITORING
didAllocateBytesLocal _ = return ()
#else
didAllocateBytesLocal n = do
  -- void $ Atomic.add _active_bytes_allocated_local n
  Counter.add _total_bytes_allocated_local n
#endif

-- | Allocated the number of bytes of /new/ memory in the remote memory space
--
didAllocateBytesRemote :: Int64 -> IO ()
#ifndef ACCELERATE_MONITORING
didAllocateBytesRemote _ = return ()
#else
didAllocateBytesRemote n = do
 -- void $ Atomic.add _active_bytes_allocated_remote n
 Counter.add _total_bytes_allocated_remote n
#endif

{-# INLINE increaseCurrentBytesRemote #-}
increaseCurrentBytesRemote :: Int64 -> IO ()
#ifndef ACCELERATE_MONITORING
increaseCurrentBytesRemote _ = return ()
#else
increaseCurrentBytesRemote n = Gauge.add _current_bytes_remote n
#endif

{-# INLINE decreaseCurrentBytesRemote #-}
decreaseCurrentBytesRemote :: Int64 -> IO ()
#ifndef ACCELERATE_MONITORING
decreaseCurrentBytesRemote _ = return ()
#else
decreaseCurrentBytesRemote n = Gauge.subtract _current_bytes_remote n
#endif

-- | Copied data between the local and remote memory spaces
--
didCopyBytesToRemote :: Int64 -> IO ()
#ifndef ACCELERATE_MONITORING
didCopyBytesToRemote _ = return ()
#else
didCopyBytesToRemote n = Counter.add _total_bytes_copied_to_remote n
#endif

didCopyBytesFromRemote :: Int64 -> IO ()
#ifndef ACCELERATE_MONITORING
didCopyBytesFromRemote _ = return ()
#else
didCopyBytesFromRemote n = Counter.add _total_bytes_copied_from_remote n
#endif


-- TLM: This is required for the 'cleanup' function (which deletes everything
-- from the nursery) and is somewhat useful for the add/remove functions, since
-- we keep track of the size anyway, but we do lose track of the number of
-- allocations/deletions to/from the nursery.
--
{-# INLINE setCurrentBytesNursery #-}
setCurrentBytesNursery :: Int64 -> IO ()
#ifndef ACCELERATE_MONITORING
setCurrentBytesNursery _ = return ()
#else
setCurrentBytesNursery n = Gauge.set _current_bytes_nursery n
#endif


-- | Performed a major GC of the remote memory space
--
didRemoteGC :: IO ()
#ifndef ACCELERATE_MONITORING
didRemoteGC = return ()
#else
didRemoteGC = Counter.inc _num_remote_gcs
#endif

-- | Performed an eviction of a remote array of the given number of bytes
--
didEvictBytes :: Int64 -> IO ()
#ifndef ACCELERATE_MONITORING
didEvictBytes _ = return ()
#else
didEvictBytes n = do
  Counter.inc _num_evictions
  Counter.add _total_bytes_evicted_from_remote n
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
-- (weight of previous measurement = 0.2).
--
estimateProcessorLoad :: Atomic -> IORef EMAState -> IO Int64
estimateProcessorLoad !var !ref = do
  ES{..} <- readIORef ref
  time   <- getCurrentTime
  sample <- Atomic.and var 0
  --
  let
      active_ns   = fromIntegral sample
      elapsed_s   = realToFrac (diffUTCTime time old_time)
      elapsed_ns  = 1.0E9 * elapsed_s
      --
      new_inst    = 100 * (active_ns / elapsed_ns)                -- instantaneous load
      new_avg     = ema 0.2 elapsed_s old_avg old_inst new_inst   -- moving average load
  --
  writeIORef ref (ES time new_inst new_avg)
  return (round new_avg)


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

-- Total number of bytes allocated in the local and remote (e.g. on the GPU)
-- address spaces
--
{-# NOINLINE _total_bytes_allocated_local #-}
_total_bytes_allocated_local :: Counter
_total_bytes_allocated_local = unsafePerformIO Counter.new

{-# NOINLINE _total_bytes_allocated_remote #-}
_total_bytes_allocated_remote :: Counter
_total_bytes_allocated_remote = unsafePerformIO Counter.new

-- Total number of bytes copied to and from the remote memory space
--
{-# NOINLINE _total_bytes_copied_to_remote #-}
_total_bytes_copied_to_remote :: Counter
_total_bytes_copied_to_remote = unsafePerformIO Counter.new

{-# NOINLINE _total_bytes_copied_from_remote #-}
_total_bytes_copied_from_remote :: Counter
_total_bytes_copied_from_remote = unsafePerformIO Counter.new

-- Total number of bytes copied out of the remote memory space due to evictions.
--
{-# NOINLINE _total_bytes_evicted_from_remote #-}
_total_bytes_evicted_from_remote :: Counter
_total_bytes_evicted_from_remote = unsafePerformIO Counter.new

-- Current working remote memory size
--
{-# NOINLINE _current_bytes_remote #-}
_current_bytes_remote :: Gauge
_current_bytes_remote = unsafePerformIO Gauge.new

-- Current size of the nursery
--
{-# NOINLINE _current_bytes_nursery #-}
_current_bytes_nursery :: Gauge
_current_bytes_nursery = unsafePerformIO Gauge.new

-- Number of times the remote memory was forcibly garbage collected, and nursery
-- flushed.
--
{-# NOINLINE _num_remote_gcs #-}
_num_remote_gcs :: Counter
_num_remote_gcs = unsafePerformIO Counter.new

-- number of LRU eviction events
--
{-# NOINLINE _num_evictions #-}
_num_evictions :: Counter
_num_evictions = unsafePerformIO Counter.new

#endif

