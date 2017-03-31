{-# LANGUAGE PatternGuards #-}

-- An implementation of PageRank based off of Ben Lippmeier's Repa
-- implementation (http://repa.ouroborus.net/).
--

import Config
import Count
import Rank

import Data.Array.Accelerate.Examples.Internal

import Control.Monad
import Data.Label                                                   ( get )


main :: IO ()
main = do
  beginMonitoring
  (conf, opts, linksPath : titlesPath : _) <- parseArgs options defaults header footer

  let backend   = get optBackend opts
      steps     = get configSteps conf
      chunk     = get configChunkSize conf
      noSeq     = get configNoSeq conf

  if get configCount conf
     then void (countPages linksPath)
     else rank backend noSeq steps chunk linksPath titlesPath

