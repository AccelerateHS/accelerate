{-# LANGUAGE CPP #-}
-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal.Report
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Examples.Internal.Report (

  processReports,

) where

import Criterion.Types
import Data.Array.Accelerate.Examples.Internal.ParseArgs

#ifdef ACCELERATE_ENABLE_CODESPEED
import Data.Array.Accelerate.Examples.Internal.Codespeed
#endif


-- | Post-process the benchmark reports.
--
processReports :: Options -> [Report] -> IO ()
processReports _opt _reports = do
#ifdef ACCELERATE_ENABLE_CODESPEED
  uploadReports _opt _reports
#endif

  return ()

