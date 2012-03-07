
module Main where

import Data.Label
import Test.Framework
import System.Environment

import Config
import Test.Reduction
import Test.PrefixSum


main :: IO ()
main = do
  (config, options)     <- processArgs =<< getArgs
  putStrLn              $  "running with " ++ shows (get optBackend config) " backend"
  --
  defaultMainWithOpts
    [ test_reduction config
    , test_prefixsum config
    ]
    options

