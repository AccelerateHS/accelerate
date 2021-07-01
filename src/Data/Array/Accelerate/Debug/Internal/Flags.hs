{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeOperators            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds   #-}
{-# OPTIONS_GHC -fobject-code                #-} -- SEE: [linking to .c files]
-- |
-- Module      : Data.Array.Accelerate.Debug.Internal.Flags
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Option parsing for debug flags
--

module Data.Array.Accelerate.Debug.Internal.Flags (

  Value,
  unfolding_use_threshold,
  max_simplifier_iterations,
  getValue,
  setValue,

  Flag(..),
  seq_sharing, acc_sharing, exp_sharing, array_fusion, simplify, inplace, flush_cache, force_recomp,
  fast_math, fast_permute_const, debug, verbose, dump_phases, dump_sharing, dump_fusion,
  dump_simpl_stats, dump_simpl_iterations, dump_vectorisation, dump_dot,
  dump_simpl_dot, dump_gc, dump_gc_stats, dump_cc, dump_ld, dump_asm, dump_exec,
  dump_sched,

  getFlag,
  setFlag, setFlags,
  clearFlag, clearFlags,

  when,
  unless,

  __cmd_line_flags,

) where


import Control.Monad.IO.Class                                       ( MonadIO, liftIO )
import Data.Bits
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath
import qualified Control.Monad                                      as M

newtype Flag  = Flag  Int
newtype Value = Value (Ptr Word32)    -- see flags.c

-- We aren't using a "real" enum so that we can make use of the unused top
-- bits for other configuration options, not controlled by the command line
-- flags.
--
instance Enum Flag where
  toEnum            = Flag
  fromEnum (Flag x) = x

-- SEE: [layout of command line options bitfield]
instance Show Flag where
  show (Flag x) =
    case x of
      0  -> "seq-sharing"
      1  -> "acc-sharing"
      2  -> "exp-sharing"
      3  -> "fusion"
      4  -> "simplify"
      5  -> "inplace"
      6  -> "fast-math"
      7  -> "fast-permute-const"
      8  -> "flush_cache"
      9  -> "force-recomp"
      10 -> "debug"
      11 -> "verbose"
      12 -> "dump-phases"
      13 -> "dump-sharing"
      14 -> "dump-fusion"
      15 -> "dump-simpl-stats"
      16 -> "dump-simpl-iterations"
      17 -> "dump-vectorisation"
      18 -> "dump-dot"
      19 -> "dump-simpl-dot"
      20 -> "dump-gc"
      21 -> "dump-gc-stats"
      22 -> "dump-cc"
      23 -> "dump-ld"
      24 -> "dump-asm"
      25 -> "dump-exec"
      26 -> "dump-sched"
      _  -> show x

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


setValue :: Value -> Word32 -> IO ()
setValue (Value f) v = poke f v

getValue :: Value -> IO Word32
getValue (Value f) = peek f

getFlag :: Flag -> IO Bool
getFlag (Flag i) = do
  flags  <- peek __cmd_line_flags
  return $! testBit flags i

setFlag :: Flag -> IO ()
setFlag (Flag i) = do
  flags <- peek __cmd_line_flags
  poke __cmd_line_flags (setBit flags i)

clearFlag :: Flag -> IO ()
clearFlag (Flag i) = do
  flags <- peek __cmd_line_flags
  poke __cmd_line_flags (clearBit flags i)

setFlags :: [Flag] -> IO ()
setFlags = mapM_ setFlag

clearFlags :: [Flag] -> IO ()
clearFlags = mapM_ clearFlag

-- notEnabled :: a
-- notEnabled = error $ unlines [ "Data.Array.Accelerate: Debugging options are disabled."
--                              , "Reinstall package 'accelerate' with '-fdebug' to enable them." ]

-- Note: [HLS and GHC IDE]
--
-- HLS requires stubs because it does not process the 'addForeignFilePath'
-- calls when evaluating Template Haskell
--
-- > https://github.com/haskell/haskell-language-server/issues/365
--
#ifndef __GHCIDE__

-- Import the underlying flag variables. These are defined in the file
-- cbits/flags.h as a bitfield and initialised at program initialisation.
--
-- SEE: [layout of command line options bitfield]
-- SEE: [linking to .c files]
--
foreign import ccall "&__cmd_line_flags" __cmd_line_flags :: Ptr Word32

-- These @-f<blah>=INT@ values are used by the compiler
--
foreign import ccall "&__unfolding_use_threshold"   unfolding_use_threshold   :: Value  -- the magic cut-off figure for inlining
foreign import ccall "&__max_simplifier_iterations" max_simplifier_iterations :: Value  -- maximum number of scalar simplification passes

#else

__cmd_line_flags :: Ptr Word32
__cmd_line_flags = undefined

unfolding_use_threshold :: Value
unfolding_use_threshold = undefined

max_simplifier_iterations :: Value
max_simplifier_iterations = undefined

#endif

-- These @-f<blah>@ flags can be reversed with @-fno-<blah>@
--
seq_sharing           = Flag  0 -- recover sharing of sequence expressions
acc_sharing           = Flag  1 -- recover sharing of array computations
exp_sharing           = Flag  2 -- recover sharing of scalar expressions
array_fusion          = Flag  3 -- fuse array expressions
simplify              = Flag  4 -- simplify scalar expressions
inplace               = Flag  5 -- allow (safe) in-place array updates
fast_math             = Flag  6 -- use faster, less precise math library operations
fast_permute_const    = Flag  7 -- allow non-atomic permute const for product types
flush_cache           = Flag  8 -- delete persistent compilation cache(s)
force_recomp          = Flag  9 -- force recompilation of array programs

-- These debugging flags are disable by default and are enabled with @-d<blah>@
--
debug                 = Flag 10 -- compile code with debugging symbols (-g)
verbose               = Flag 11 -- be very chatty
dump_phases           = Flag 12 -- print information about each phase of the compiler
dump_sharing          = Flag 13 -- sharing recovery phase
dump_fusion           = Flag 14 -- array fusion phase
dump_simpl_stats      = Flag 15 -- statistics form fusion/simplification
dump_simpl_iterations = Flag 16 -- output from each simplifier iteration
dump_vectorisation    = Flag 17 -- output from the vectoriser
dump_dot              = Flag 18 -- generate dot output of the program
dump_simpl_dot        = Flag 19 -- generate simplified dot output
dump_gc               = Flag 20 -- trace garbage collector
dump_gc_stats         = Flag 21 -- print final GC statistics
dump_cc               = Flag 22 -- trace code generation & compilation
dump_ld               = Flag 23 -- trace runtime linker
dump_asm              = Flag 24 -- trace assembler
dump_exec             = Flag 25 -- trace execution
dump_sched            = Flag 26 -- trace scheduler


-- Note: [linking to .c files]
--
-- We use Template Haskell to tell GHC which .c files need to be compiled
-- for a particular module, rather than relying on Cabal as is traditional.
-- Using Cabal:
--
--  * loading Accelerate into GHCi only works _after_ compiling the entire
--    package (which defeats the purpose), presumably because the .c files
--    are compiled last. This would often lead to errors such "can not find
--    symbol __cmd_line_flags" etc.
--
--  * Cabal would refuse to re-compile .c files when changing command
--    line flags, see: https://github.com/haskell/cabal/issues/4937
--
--  * Linking problems also prevented us from using Template Haskell in
--    some locations, because GHC was unable to load the project into the
--    interpreter to run the splices.
--
-- Note that for this fix to work in GHCi we also require modules using it
-- to be loaded as object code.
--
runQ $ do
  addForeignFilePath LangC "cbits/flags.c"
  addForeignFilePath LangC "cbits/getopt_long.c"
  return []

