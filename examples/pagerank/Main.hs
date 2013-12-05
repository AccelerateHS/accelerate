{-# LANGUAGE PatternGuards #-}

-- An implementation of PageRank based off of Ben Lippmeier's Repa implementation (http://repa.ouroborus.net/).
--

import Config
import ParseArgs
import Count
import Rank
import Data.Label                 ( get )
import System.Environment
import System.Exit
import Data.Char
import Control.Monad


main :: IO ()
main
 = do   argv    <- getArgs
        (config, _, linksPath : titlesPath : _)  <- parseArgs optHelp optBackend options defaults header footer argv

        if get optCount config
          then void (countPages linksPath)
          else rank (get optBackend config) (get optSteps config) (get optChunkSize config) linksPath titlesPath

