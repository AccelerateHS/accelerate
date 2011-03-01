{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Args
-- Copyright   : [2011] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Args where

import Data.Version
import Paths_accelerate_buildbot

import System.Console.CmdArgs

data Args = Args
  {
    accelerate_repo     :: String
  , daily_at            :: Maybe String
  , with_ghc            :: String
  , output              :: Maybe FilePath
  , with_timestamp      :: Bool
  , upload_to           :: Maybe String
  , compare_to          :: Maybe FilePath
  , compare_swing       :: Maybe Double
  , mail_from           :: Maybe String
  , mail_to             :: [String]
  , mail_fail_to        :: [String]
  , mail_banner         :: Maybe FilePath
  , history             :: Maybe FilePath
  , send_test_email     :: Bool
  }
  deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs =  Args
  {
    accelerate_repo
       = "http://code.haskell.org/accelerate"
      &= name "r"
      &= help "The Accelerate repository to test"
      &= typ  "DARCS_PATH"

  , with_ghc
       = "ghc"
      &= name "w"
      &= help "Give path to a particular compiler"
      &= typ  "PATH"

  , daily_at
       = def
      &= name "a"
      &= help "Run every day at this time, else once now"
      &= typ  "HH:MM:SS"

  , output
       = def
      &= help "Write results to this file"
      &= typFile

  , with_timestamp
       = def
      &= name "t"
      &= help "Append time stamp to result files"

  , upload_to
       = def
      &= help "Upload result files to this address"
      &= typ  "SCP_PATH"

  , compare_to
       = def
      &= name "c"
      &= help "Compare to results in this file"
      &= typFile

  , compare_swing
       = def
      &= name "s"
      &= help "Treat this fractional swing as interesting"
      &= typ  "DOUBLE"

  , mail_from
       = def
      &= help "Send test results from this address"
      &= typ  "ADDRESS"

  , mail_to
       = def
      &= help "Send test results to this address"
      &= typ  "ADDRESS"

  , mail_fail_to
       = def
      &= help "Send failure notifications to this address"
      &= typ  "ADDRESS"

  , mail_banner
       = def
      &= help "Append a banner to the result email"
      &= typFile

  , history
       = def
      &= explicit
      &= name "history"
      &= help "Where to stash the buildbot history"
      &= typFile

  , send_test_email
       = def
      &= help "Test the mailer configuration"

  } &= program "accelerate-buildbot"
    &= summary "accelerate-buildbot (c) 2011 The Accelerate Team"
    &= versionArg [summary $ "accelerate-buildbot-" ++ showVersion version]
    &= verbosityArgs [help "Verbose logging of build commands"] [ignore]

