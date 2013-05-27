{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Monitoring where

#ifdef ACCELERATE_ENABLE_EKG
import Control.Monad
import System.Remote.Monitoring
#endif


beginMonitoring :: IO ()
#ifdef ACCELERATE_ENABLE_EKG
beginMonitoring = do
  putStrLn "EKG monitor started at: http://localhost:8000\n"
  void $ forkServer "localhost" 8000
#else
beginMonitoring = return ()
#endif

