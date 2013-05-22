{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Monitoring where

import Control.Monad

#ifdef ACCELERATE_ENABLE_EKG
import System.Remote.Monitoring
#endif


beginMonitoring :: IO ()
#ifdef ACCELERATE_ENABLE_EKG
beginMonitoring = void $ forkServer "localhost" 8000
#else
beginMonitoring = return ()
#endif

