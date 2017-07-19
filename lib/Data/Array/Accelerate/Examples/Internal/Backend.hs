{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal.Backend
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- This file is all that should need editing in order to add support for new
-- backends to the accelerate-examples package.
--

module Data.Array.Accelerate.Examples.Internal.Backend
  where

import Prelude                                                      as P
import Data.Label
import System.Console.GetOpt

import Data.Array.Accelerate
import Data.Array.Accelerate.Trafo                                  ( Afunction, AfunctionR )
import qualified Data.Array.Accelerate.Interpreter                  as Interp
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import qualified Data.Array.Accelerate.LLVM.Native                  as CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.LLVM.PTX                     as PTX
#endif


-- | Execute Accelerate expressions
--
run :: Arrays a => Backend -> Acc a -> a
run Interpreter = Interp.run
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
run CPU         = CPU.run
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
run PTX         = PTX.run
#endif

run1 :: (Arrays a, Arrays b) => Backend -> (Acc a -> Acc b) -> a -> b
run1 = runN

runN :: Afunction f => Backend -> f -> AfunctionR f
runN Interpreter = Interp.runN
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
runN CPU         = CPU.runN
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
runN PTX         = PTX.runN
#endif


-- | The set of backends available to execute the program.
--
data Backend = Interpreter
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
             | CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
             | PTX
#endif
  deriving (P.Eq, P.Enum, P.Bounded)


-- The choice of show instance is important because this will be used to
-- generate the command line flag.
--
instance Show Backend where
  show Interpreter      = "interpreter"
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  show CPU              = "llvm-cpu"
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
  show PTX              = "llvm-ptx"
#endif


-- The default backend to use. Use one of the accelerated backends whenever
-- available.
--
defaultBackend :: Backend
defaultBackend =
  case maxBound of
    Interpreter -> Interpreter
    _           -> succ Interpreter


-- The set of available backnds. This will be used for both the command line
-- options as well as the fancy header generation.
--
availableBackends :: (options :-> Backend) -> [OptDescr (options -> options)]
availableBackends optBackend =
  [ Option  [] [show Interpreter]
            (NoArg (set optBackend Interpreter))
            "reference implementation (sequential, slow)"
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  , Option  [] [show CPU]
            (NoArg (set optBackend CPU))
            "LLVM based implementation for multicore CPUs (parallel)"
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
  , Option  [] [show PTX]
            (NoArg (set optBackend PTX))
            "LLVM based implementation for NVIDIA GPUs (parallel)"
#endif
  ]


-- By default, how many instances of the backend can be run concurrently?
--
-- If this returns `Nothing` then we make no changes to the configuration and
-- inherit the RTS options. If this is `Just n`, then by default only `n`
-- threads will be used to run tests at once. This option can still be
-- overridden via the command line flags, it just sets a default.
--
-- e.g.
--   * A multicore CPU backend may specify `Just 1`, so that only one instance
--     runs at once, and it inherits all threads specified via `+RTS -Nn`
--
--   * A thread-safe accelerator backend can specify `Nothing`, while a
--     non-thread-safe backend (perhaps it requires exclusive access to the
--     accelerator board) should specify `Just 1`.
--
-- Both the LLVM-CPU and LLVM-PTX backends are safe to run concurrently given
-- the same execution context. Although this results in over-subscription,
-- particularly for the CPU backend, it still improves performance because the
-- majority of the time is spent in the reference implementation / checking
-- results, so running multiple tests concurrently is still useful.
--
concurrentBackends :: Backend -> Maybe Int
concurrentBackends Interpreter  = Nothing
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
concurrentBackends CPU          = Nothing
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
concurrentBackends PTX          = Nothing
#endif

