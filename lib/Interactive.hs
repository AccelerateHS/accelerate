{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

module Interactive
  where

import ParseArgs

#ifdef ACCELERATE_ENABLE_GUI
import Data.Label
import Control.Monad
import System.Environment
#endif

-- If we accelerate-examples is configured to enable GUI programs, and we are
-- not in benchmarking mode, then execute the interactive program.
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

