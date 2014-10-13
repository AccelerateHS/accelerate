{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad
import System.Remote.Monitoring
#endif


-- | Launch a monitoring server that will collect statistics on the running
-- application. This should be called as soon as the application starts. The
-- program will need to be run with the RTS option -T.
--
beginMonitoring :: IO ()
#ifdef ACCELERATE_ENABLE_EKG
beginMonitoring = do
  putStrLn "EKG monitor started at: http://localhost:8000\n"
  void $ forkServer "localhost" 8000
#else
beginMonitoring = return ()
#endif

