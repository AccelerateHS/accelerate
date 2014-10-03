{-# LANGUAGE CPP #-}

module Data.Array.Accelerate.Examples.Internal.Interactive (

  -- * Interactive/GUI
  runInteractive,

) where

import Data.Array.Accelerate.Examples.Internal.ParseArgs

#ifdef ACCELERATE_ENABLE_GUI
import Data.Label
import Control.Monad
import System.Environment
#endif

-- | If accelerate-examples is configured to enable GUI programs, and we are
-- not in benchmarking mode, then execute the given action.
--
runInteractive :: Options -> [String] -> IO () -> IO ()
#ifndef ACCELERATE_ENABLE_GUI
runInteractive _ _ _
  = return ()
#else
runInteractive opt argv gui
  = unless (get optBenchmark opt)
  $ withArgs argv gui
#endif

