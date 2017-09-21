{-# LANGUAGE CPP #-}
-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal.Monitoring
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Examples.Internal.Monitoring (

  -- * Resource monitoring
  beginMonitoring,

) where

#ifdef ACCELERATE_ENABLE_EKG
import qualified Data.Array.Accelerate.Debug                        as Debug
#endif


-- | Launch a monitoring server that will collect statistics on the running
-- application. This should be called as soon as the application starts. The
-- program will need to be run with the RTS option -T.
--
beginMonitoring :: IO ()
beginMonitoring =
#ifdef ACCELERATE_ENABLE_EKG
  if Debug.monitoringIsEnabled
    then Debug.beginMonitoring
    else putStrLn "Monitoring is not enabled. Recompile package 'accelerate' with flag '-fekg'"
#else
  return ()
#endif

