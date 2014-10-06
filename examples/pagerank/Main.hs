{-# LANGUAGE PatternGuards #-}

-- An implementation of PageRank based off of Ben Lippmeier's Repa implementation (http://repa.ouroborus.net/).
--

import Config
import Count
import Monitoring
import ParseArgs
import Rank

import Control.Monad
import Data.Char
import Data.Label                 ( get )
import System.Environment
import System.Exit


main :: IO ()
main
 = do
        beginMonitoring
        argv    <- getArgs
        (config, _, linksPath : titlesPath : _)  <- parseArgs optHelp optBackend options defaults header footer argv

        if get optCount config
          then void (countPages linksPath)
          else rank (get optBackend config) (get optNoSeq config) (get optSteps config) (get optChunkSize config) linksPath titlesPath

