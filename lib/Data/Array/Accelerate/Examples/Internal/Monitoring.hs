{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

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

