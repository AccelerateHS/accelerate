{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Data.Array.Accelerate.Examples.Internal.Criterion.Config (

  -- ** Criterion options
  Config, defaultConfig,
  module Data.Array.Accelerate.Examples.Internal.Criterion.Config

) where

import Prelude                                  hiding ( (.), id )
import Data.Char
import Data.Label
import Data.Label.Derive
import Control.Category                         ( (.), id )
import System.Console.GetOpt
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Map                       as M

import Criterion.Analysis                       ( validateAccessors )
import Criterion.Types                          ( Config, measureKeys, measureAccessors )
import Criterion.Main.Options                   ( defaultConfig )

$(mkLabelsNamed id [''Config])


-- A GetOpt version of Criterion's command line options parser. It is
-- unfortunate that we need to do this to integrate with the other frameworks.
--
defaultOptions :: [OptDescr (Config -> Config)]
defaultOptions =
  [ Option  [] ["ci"]
            (ReqArg (set confInterval . read) "CI")
            (describe confInterval "confidence interval")

  , Option  [] ["no-gc"]
            (NoArg (set forceGC False))
            "do not collect garbage between iterations"

  , Option  [] ["time-limit"]
            (ReqArg (set timeLimit . read) "SECS")
            (describe timeLimit "time limit to run a benchmark")

  , Option  [] ["resamples"]
            (ReqArg (set resamples . read) "INT")
            (describe resamples "number of bootstrap resamples to perform")

  , Option  [] ["regress"]
            (ReqArg (\v -> modify regressions (regressParams v :)) "RESP:PRED..")
            "regressions to perform"

  , Option  [] ["raw"]
            (OptArg (set rawDataFile) "FILE")
            (describe rawDataFile "file to write raw data to")

  , Option  [] ["output"]
            (OptArg (set reportFile) "FILE")
            (describe reportFile "file to write report to")

  , Option  [] ["csv"]
            (OptArg (set csvFile) "FILE")
            (describe csvFile "file to write CSV summary to")

  , Option  [] ["junit"]
            (OptArg (set junitFile) "FILE")
            (describe junitFile "file to write JUnit summary to")

  , Option  [] ["verbosity"]
            (ReqArg (set verbosity . toEnum . range (0,2) . read) "LEVEL")
            (describe' fromEnum verbosity "verbosity level")

  , Option  [] ["template"]
            (ReqArg (set template) "FILE")
            (describe template "template to use for report")
  ]
  where
    describe :: Show a => (Config :-> a) -> String -> String
    describe = describe' id

    describe' :: Show a => (b -> a) -> (Config :-> b) -> String -> String
    describe' p f msg
      = msg ++ " (" ++ show (p (get f defaultConfig)) ++ ")"

    range (n,m) x
      | n <= x && x <= m = x
      | otherwise        = error $ printf "%d is outside range (%d,%d)" x n m

-- The following options are not part of the configuration structure, but will
-- be intercepted when calling 'defaultMainWith', and control the execution
-- mode. We include these extra options when generating the help text, but don't
-- include them when processing the 'Config' structure.
--
extraOptions :: [OptDescr (a -> a)]
extraOptions =
  [ Option  [] ["match"]
            (ReqArg (flip const) "MATCH")
            "how to match benchmark names"

  , Option  [] ["only-run"]
            (ReqArg (flip const) "ITERS")
            "run benchmarks, don't analyse"

  , Option  [] ["list"]
            (NoArg id)
            "list benchmarks"

  , Option  [] ["help"]
            (NoArg id)
            "Shows this help text"
  ]


-- Check and parse the arguments to '--regress'. Copied from
-- Criterion.Main.Options
--
regressParams :: String -> ([String], String)
regressParams m
  | null r      = error "regression parameters: no responder specified"
  | null ps     = error "regression parameters: no predictors specified"
  | otherwise   = validate `seq` ret
  where
    repl ','    = ' '
    repl c      = c

    tidy        = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    (r,ps)      = break (==':') m

    ret         = (words . map repl . drop 1 $ ps, tidy r)
    validate    = either error id $ uncurry validateAccessors ret

-- Generate the help string to describe the possible arguments to '--regress'.
-- Copied from Criterion.Main.Options.
--
regressHelp :: String
regressHelp
  = show
  $ text "Criterion regression metrics for use with --regress:"
  <$> tabulate [(text n, text d) | (n,(_,d)) <- map f measureKeys]
  where
    f k = (k, measureAccessors M.! k)


tabulate :: [(Doc,Doc)] -> Doc
tabulate = tabulate' 24
  where
    tabulate' _    []    = empty
    tabulate' size table = vcat
        [ indent 2 (fillBreak size key <+> value) | (key, value) <- table ]

