{-# LANGUAGE CPP #-}
-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal.Interactive
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- This module exposes some internal infrastructure that is useful and common to
-- the accelerate-examples test and benchmark program suite. This is not
-- intended for general consumption.
--

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

