{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal.Codespeed
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Examples.Internal.Codespeed
  where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Examples.Internal.TH
import Data.Array.Accelerate.Examples.Internal.ParseArgs

import Data.Aeson
import Data.Char
import Data.Text                                        ( Text )
import Data.Label
import Data.Monoid
import Control.Monad
import Criterion.Types                                  ( Report(..), SampleAnalysis(..), Regression(..) )
import Statistics.Resampling.Bootstrap                  ( Estimate(..), scale )
import Network.HTTP                                     ( simpleHTTP, postRequestWithBody, Response(..) )
import Network.HTTP.Types                               ( urlEncode )
import System.IO
import qualified Data.ByteString.Char8                  as BS
import qualified Data.ByteString.Lazy.Char8             as BL
import qualified Data.Map                               as Map
import qualified Data.Text


data Sample = Sample
  {
    sampleName          :: String       -- benchmark name
  , sampleTime          :: Estimate     -- sample result (via OLS regression)
  , sampleRSquare       :: Estimate     -- R^2 goodness-of-fit estimate of regression
  , sampleMean          :: Estimate     -- result mean
  , sampleStdDev        :: Estimate     -- standard deviation
  , sampleUnits         :: Unit
  }

data Unit = Unit
  {
    unitName            :: String       -- e.g. seconds
  , unitType            :: String       -- description of the unit, e.g. Time
  , lessIsMore          :: Bool         -- so python, much philosophical
  }


-- Treat a sample as being in milliseconds
--
ms :: Sample -> Sample
ms Sample{..} =
  Sample { sampleTime           = scale 1000 sampleTime
         , sampleRSquare        = scale 1000 sampleRSquare
         , sampleMean           = scale 1000 sampleMean
         , sampleStdDev         = scale 1000 sampleStdDev
         , sampleUnits          = Unit { unitName   = "ms"
                                       , unitType   = "Time"
                                       , lessIsMore = True }
         , ..
         }


-- Extract benchmark information from the Criterion report
--
-- TODO: Extract any other regressions present in the report into separate
--       samples, e.g. GC statistics
--
sample :: Report -> Sample
sample Report{..} = ms $ Sample
  { sampleName          = reportName
  , sampleTime          = regIters
  , sampleRSquare       = regRSquare
  , sampleMean          = anMean
  , sampleStdDev        = anStdDev
  , sampleUnits         = error "sample has no unit assigned"
  }
  where
    SampleAnalysis{..}  = reportAnalysis
    Regression{..}      = head anRegress
    regIters            = case Map.lookup "iters" regCoeffs of
                            Nothing -> $internalError "sample" "no benchmark result found"
                            Just t  -> t

-- Encode a sample to a JSON value suitable for upload to codespeed
--
toValue :: Options -> Sample -> Value
toValue opt Sample{..} = object
  [ "commitid"          .= $(_HEAD)
  , "branch"            .= $(_BRANCH)
  , "project"           .= ("accelerate-examples" :: Text)
  , "executable"        .= get optVariant opt
  , "environment"       .= get optHostname opt
  , "benchmark"         .= sampleName
  , "result_value"      .= estPoint sampleTime
  , "min"               .= estLowerBound sampleTime
  , "max"               .= estUpperBound sampleTime
  , "std_dev"           .= estPoint sampleStdDev
  , "units"             .= unitName sampleUnits
  , "units_title"       .= unitType sampleUnits
  , "lessismore"        .= lessIsMore sampleUnits
  ]


-- Upload a set of criterion reports to the codespeed server
--
uploadReports :: Options -> [Report] -> IO ()
uploadReports opt reports =
  when upload $ do
    putStr ("Uploading results to " ++ server ++ " ... ") >> hFlush stdout
    rsp <- simpleHTTP $ postRequestWithBody url contentType content
    case rsp of
      Left err -> do
        putStrLn "failed"
        hPutStrLn stderr (show err)

      Right Response{..} ->
        case rspCode of
          (2,0,2) -> putStrLn "Ok"
          (a,b,c) -> do putStrLn $ map intToDigit [a,b,c] ++ ' ' : rspReason
                        hPutStrLn stderr rspBody
  where
    (upload, server)    = maybe (False, undefined) (True,) (get optCodespeed opt)

    contentType         = "application/x-www-form-urlencoded"
    url                 = server <> "/result/add/json/"
    payload             = "json=" <> encode (map (toValue opt . sample) reports)
    content             = BS.unpack . urlEncode False $ BL.toStrict payload

