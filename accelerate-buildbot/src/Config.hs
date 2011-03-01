{-# LANGUAGE RecordWildCards, TupleSections #-}
-- |
-- Module      : Config
-- Copyright   : [2011] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Config where

import Args
import Util
import BuildBox

import System.IO
import System.FilePath
import System.Directory
import System.Console.CmdArgs


-- TLM: Should probably try to validate these inputs...
type SCPPath      = String

data Config = Config
  {
    -- darcs source location and build directories
    configDarcsRepo      :: DarcsPath                            -- something comprehensible to "darcs get"
  , configScratchDir     :: FilePath                             -- where to download and test the repo

    -- testing and comparison
  , configHistory        :: Maybe FilePath
  , configSchedule       :: Maybe TimeOfDay                      -- A time to run at every day, else once now
  , configAgainstResults :: Maybe FilePath                       -- compare against previous run statistics
  , configSwingFraction  :: Maybe Double                         -- fractional swing from baseline, else all results

    -- programs 'n stuff
  , configWithMailer     :: Mailer                               -- send results using this mail transfer agent
  , configWithGHC        :: FilePath                             -- path to a specific version of GHC

    -- what to do with results
  , configUploadResults  :: Maybe SCPPath                        -- an SCP destination
  , configWriteResults   :: Maybe (FilePath, Bool)               -- filename, add timestamp?
  , configMailFromTo     :: Maybe (EmailAddress, [EmailAddress]) -- addresses to email the performance summary
  , configMailFailTo     :: Maybe [EmailAddress]                 -- alternate address to send failure notifications
  , configMailBanner     :: Maybe FilePath                       -- Read a banner to prefix to emails
  , configSendTestEmail  :: Bool
  }
  deriving Show

-- Baked-in email settings...
--
defaultMailer :: Mailer
defaultMailer =  MailerSendmail
  {
    mailerPath          = "sendmail"
  , mailerExtraFlags    = []
  }

-- Parse command line options and return an appropriately populated
-- configuration structure.
--
processArgs :: BuildState -> IO (Config, BuildState)
processArgs st = do
  Args{..}   <- cmdArgs defaultArgs
  stamp      <- getStampyTime
  verbose    <- isLoud
  --
  runAt      <- readLocalTimeOfDayAsUTC =<<< daily_at
  historyTo  <- canonicalizeDirectory   =<<< history
  compareTo  <- canonicalizeDirectory   =<<< compare_to
  outputTo   <- canonicalizeDirectory   =<<< output
  mailBanner <- canonicalizeDirectory   =<<< mail_banner
  --
  let state  = st { buildStateLogSystem = bool Nothing (Just stdout) verbose }
      config = Config
        {
          configDarcsRepo      = accelerate_repo
        , configScratchDir     = buildStateScratchDir st </> "accelerate-" ++ stamp
        , configHistory        = historyTo
        , configSchedule       = runAt
        , configAgainstResults = compareTo
        , configSwingFraction  = compare_swing
        , configWithMailer     = defaultMailer
        , configWithGHC        = with_ghc
        , configWriteResults   = (,with_timestamp) `fmap` outputTo
        , configMailFromTo     = bool ((,mail_to)  `fmap` mail_from) Nothing $ null mail_to
        , configMailFailTo     = bool (Just mail_fail_to) Nothing            $ null mail_fail_to
        , configMailBanner     = mailBanner
        , configUploadResults  = upload_to
        , configSendTestEmail  = send_test_email
        }
  return (config, state)

  where
    (=<<<) :: (a -> IO b) -> Maybe a -> IO (Maybe b)
    _ =<<< Nothing = return Nothing
    f =<<< Just x  = return `fmap` f x

    canonicalizeDirectory :: FilePath -> IO FilePath
    canonicalizeDirectory path =
      replaceDirectory path `fmap` canonicalizePath (takeDirectory path)

