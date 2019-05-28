{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeOperators            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.Debug.Flags
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Option parsing for debug flags
--

module Data.Array.Accelerate.Debug.Flags (

  Value,
  unfolding_use_threshold,
  getValue,
  setValue,

  Flag,
  acc_sharing, exp_sharing, array_fusion, simplify, flush_cache, force_recomp,
  fast_math, debug, verbose, dump_phases, dump_sharing, dump_fusion,
  dump_simpl_stats, dump_simpl_iterations, dump_vectorisation, dump_dot,
  dump_simpl_dot, dump_gc, dump_gc_stats, dump_cc, dump_ld, dump_asm, dump_exec,
  dump_sched,

  getFlag,
  setFlag, setFlags,
  clearFlag, clearFlags,

  when,
  unless,

) where


import Data.Bits
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.Storable

import Control.Monad.IO.Class                                       ( MonadIO, liftIO )
import qualified Control.Monad                                      as M

newtype Flag  = Flag  Int           -- can switch to an Enum now if we wished
newtype Value = Value (Ptr Int32)


-- | Conditional execution of a monadic debugging expression.
--
-- This does nothing unless the program is compiled in debug mode.
--
{-# INLINEABLE when #-}
when :: MonadIO m => Flag -> m () -> m ()
#if ACCELERATE_DEBUG
when f action = do
  yes <- liftIO $ getFlag f
  M.when yes action
#else
when _ _ = return ()
#endif


-- | The opposite of 'when'.
--
-- This does nothing unless the program is compiled in debug mode.
--
{-# INLINEABLE unless #-}
unless :: MonadIO m => Flag -> m () -> m ()
#ifdef ACCELERATE_DEBUG
unless f action = do
  yes <- liftIO $ getFlag f
  M.unless yes action
#else
unless _ _ = return ()
#endif


setValue   :: Value -> Int -> IO ()
#ifdef ACCELERATE_DEBUG
setValue (Value f) v = poke f (fromIntegral v)
#else
setValue _         _ = notEnabled
#endif

getValue   :: Value -> IO Int
#ifdef ACCELERATE_DEBUG
getValue (Value f) = fromIntegral `fmap` peek f
#else
getValue _         = notEnabled
#endif

getFlag    :: Flag -> IO Bool
#ifdef ACCELERATE_DEBUG
getFlag (Flag i) = do
  flags  <- peek cmd_line_flags
  return $! testBit flags i
#else
getFlag _        = notEnabled
#endif

setFlag    :: Flag -> IO ()
#ifdef ACCELERATE_DEBUG
setFlag (Flag i) = do
  flags <- peek cmd_line_flags
  poke cmd_line_flags (setBit flags i)
#else
setFlag _        = notEnabled
#endif

clearFlag  :: Flag -> IO ()
#ifdef ACCELERATE_DEBUG
clearFlag (Flag i) = do
  flags <- peek cmd_line_flags
  poke cmd_line_flags (clearBit flags i)
#else
clearFlag _        = notEnabled
#endif

setFlags   :: [Flag] -> IO ()
setFlags = mapM_ setFlag

clearFlags :: [Flag] -> IO ()
clearFlags = mapM_ clearFlag

notEnabled :: a
notEnabled = error $ unlines [ "Data.Array.Accelerate: Debugging options are disabled."
                             , "Reinstall package 'accelerate' with '-fdebug' to enable them." ]


-- Import the underlying flag variables. These are defined in the file
-- cbits/flags.c as a bitfield and initialised at program initialisation.
--
foreign import ccall "&__cmd_line_flags" cmd_line_flags :: Ptr Word32

-- These @-f<blah>=INT@ values are used by the compiler
--
foreign import ccall "&__unfolding_use_threshold" unfolding_use_threshold :: Value  -- the magic cut-off figure for inlining

-- These @-f<blah>@ flags can be reversed with @-fno-<blah>@
--
seq_sharing           = Flag  0 -- recover sharing of sequence expressions
acc_sharing           = Flag  1 -- recover sharing of array computations
exp_sharing           = Flag  2 -- recover sharing of scalar expressions
array_fusion          = Flag  3 -- fuse array expressions
simplify              = Flag  4 -- simplify scalar expressions
fast_math             = Flag  5 -- delete persistent compilation cache(s)
flush_cache           = Flag  6 -- force recompilation of array programs
force_recomp          = Flag  7 -- use faster, less precise math library operations

-- These debugging flags are disable by default and are enabled with @-d<blah>@
--
debug                 = Flag  8 -- compile code with debugging symbols (-g)
verbose               = Flag  9 -- be very chatty
dump_phases           = Flag 10 -- print information about each phase of the compiler
dump_sharing          = Flag 11 -- sharing recovery phase
dump_fusion           = Flag 12 -- array fusion phase
dump_simpl_stats      = Flag 13 -- statistics form fusion/simplification
dump_simpl_iterations = Flag 14 -- output from each simplifier iteration
dump_vectorisation    = Flag 15 -- output from the vectoriser
dump_dot              = Flag 16 -- generate dot output of the program
dump_simpl_dot        = Flag 17 -- generate simplified dot output
dump_gc               = Flag 18 -- trace garbage collector
dump_gc_stats         = Flag 19 -- print final GC statistics
dump_cc               = Flag 20 -- trace code generation & compilation
dump_ld               = Flag 21 -- trace runtime linker
dump_asm              = Flag 22 -- trace assembler
dump_exec             = Flag 23 -- trace execution
dump_sched            = Flag 24 -- trace scheduler

