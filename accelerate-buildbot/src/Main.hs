{-# LANGUAGE PatternGuards #-}
-- |
-- Module      : Main
-- Copyright   : [2011] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Main where

import Util
import Build
import Config
import BuildBox

import Data.List
import Data.Maybe
import System.Random
import System.Directory
import System.Posix.Env
import Control.Monad
import Control.Monad.Error.Class


-- The main buildbot. Process command line arguments and either run the test
-- once now, or initiate a cron loop to run every day at the specified time.
--
main :: IO ()
main = do
  uid      <- getStdRandom $ randomR (0,1000000)
  tmp      <- getTemporaryDirectory
  (cfg,st) <- processArgs (buildStateDefault uid tmp)

  -- Send a test email
  --
  when (configSendTestEmail cfg) $ do
    successfully . runBuildPrintWithState st $ sendTestEmail cfg
    exitSuccess

  -- Run the build-bot
  let once = runAccBuildTest cfg
      loop | Just at <- configSchedule cfg = cronLoop $ makeSchedule [("buildbot-daily", Daily at, Nothing, once)]
           | otherwise                     = once

  successfully (runBuildPrintWithState st loop)


-- Runs the build/test cycle and sends an email on failure
--
runAccBuildTest :: Config -> Build ()
runAccBuildTest config = do
  outLINE
  outLn "* Starting Build..."
  outLn . ("  - current time is " ++) . show =<< io getZonedTime
  outBlank

  whenM (modified config) $
    runBuildTest config `catchError` handleBuildError config

  outLn "* Done"
  outLINE


-- Determine if any patches have been submitted to the repository since the last
-- successfully recorded build time. Returns true if we have no saved history.
--
modified :: Config -> Build Bool
modified cfg =
  maybe' (configHistory cfg) (return True) $ \hist -> do
    buildT <- io $ read `fmap` readFile hist
    patchT <- (darcsTimestamp . head) `fmap` changesN (Just $ configDarcsRepo cfg) 1
    return (buildT /= patchT)


-- Run the complete fetch/build/test cycle once
--
runBuildTest :: Config -> Build ()
runBuildTest config =
  inTempDir    (configScratchDir config) $
  inScratchDir (configScratchDir config) $ do
    -- Check the current environment
    env <- getEnvironmentWith
             [ ("GHC", getVersionGHC $ configWithGHC config)
             , ("GCC", getVersionGCC "gcc") ]
    outLn . render . ppr $ env
    outBlank

    -- Get and build the accelerate library. Sets the current directory to the
    -- newly checked-out repository.
    fetchAcc config
    buildAcc config

    -- Build and run the example programs
    buildTest config
    runTest   config env
    postTest  config


inTempDir :: String -> Build a -> Build a
inTempDir new thing =
  let before  = io $ do old <- getTemporaryDirectory
                        setEnv "TMPDIR" new True
                        return old
      after d = io $ setEnv "TMPDIR" d True
  in do
    a <- before
    r <- thing `catchError` \e -> after a >> throwError e
    _ <- after a
    return r


-- Send a test email
--
sendTestEmail :: Config -> Build ()
sendTestEmail cfg = do
  banner <- maybe (return []) (io . readFile) (configMailBanner cfg)
  mail   <- createMailWithCurrentTime from to
              "[accelerate-buildbot] Test Email" $ unlines [ banner, "Looks like it worked...\n" ]
  io $ writeFile "accelerate-buildbot.mail" (render $ renderMail mail)
  sendMailWithMailer mail (configWithMailer cfg)
  where
    to         = intercalate ", " toL
    (from,toL) = fromMaybe (error "Must specify --mail-from and --mail-to with --send-test-email")
                           (configMailFromTo cfg)

