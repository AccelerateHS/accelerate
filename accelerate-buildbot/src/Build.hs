-- |
-- Module      : Build
-- Copyright   : [2011] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Build where

import Util
import Config
import BuildBox
import Benchmarks

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import System.Directory
import System.FilePath


-- Accelerate library
-- ------------------

fetchAcc :: Config -> Build ()
fetchAcc cfg = do
  outLn "* Getting Accelerate"
  ssystem $ "darcs get " ++ configDarcsRepo cfg ++ " accelerate"
  io $ setCurrentDirectory "accelerate"
  outBlank

buildAcc :: Config -> Build ()
buildAcc cfg = do
  outLn "* Building Accelerate"
  ssystem $ unwords [ "cabal", "configure", "-fcuda", "-fio", "-finplace"
                    , "--disable-library-profiling"
                    , "--with-compiler=" ++ configWithGHC cfg ]
  ssystem $ unwords [ "cabal", "build" ]
  outBlank


-- The benchmark programs
-- ----------------------

buildTest :: Config -> Build ()
buildTest _cfg =
  inDir "accelerate-examples" $ do
    outLn "* Building accelerate-examples"
    ssystem "make inplace"
    outBlank

runTest :: Config -> Environment -> Build ()
runTest cfg env = do
  outLn "* Running regression tests"

  -- Load the baseline statistics file, if it was given
  --
  baseline <- maybe (return []) (\f -> io $ read `fmap` readFile f) (configAgainstResults cfg)

  -- Run the tests, get statistics
  --
  results  <- runAccBenchmarks cfg
  let comparison = compareManyBenchResults baseline results

  -- Write statistics and baseline comparison to file
  --
  resFiles <- maybe' (configWriteResults cfg)
    (return $ error "oops... result files not saved")
    $ \(f,s) -> do
        stamp <- io getStampyTime
        let (base,ext)           = splitExtension f
            cmpfile              = filename `replaceExtension` "cmp"
            filename | s         = base ++ '-' : stamp <.> ext
                     | otherwise = f

        outLn $ "* Writing results to \"" ++ filename ++ "\""
        io    $ do
          writeFile filename . show  $ results
          unless (null comparison)   $
            writeFile cmpfile . show $ reportBenchResults Nothing comparison

        return [filename, cmpfile]

  -- Upload result files; requires previous step
  --
  maybe' (configUploadResults cfg) (return ())
    $ \dst -> do
        outLn $ "* Uploading results to \"" ++ dst ++ "\""
        forM_ resFiles (\f -> ssystem $ unwords ["scp", f, dst])

  -- Send aieeeeee-mail
  --
  maybe' (configMailFromTo cfg) (return ())
    $ \(from,recipients) -> do
        let to = intercalate ", " recipients
        outLn $ "* Mailing results to \"" ++ to ++ "\""

        banner <- maybe' (configMailBanner cfg)
          (return blank)
          (\f -> text `fmap` io (readFile f))

        mail   <- createMailWithCurrentTime from to
                    "[accelerate-buildbot] Performance Results"
                    . render $ vcat
                    [ banner
                    , ppr env
                    , blank
                    , nest 2 $ reportBenchResults (configSwingFraction cfg) comparison
                    , blank ]

        sendMailWithMailer mail (configWithMailer cfg)

  -- And now, a nap.
  --
  outBlank


-- Record this as the last time the repository built successfully
--
postTest :: Config -> Build ()
postTest cfg =
  maybe' (configHistory cfg) (return ()) $ \fn -> do
    time <- darcsTimestamp . head <$> changesN Nothing 1
    io    $ writeFile fn (show time)


-- Handling build errors
-- ---------------------

-- Send an email if the build fails
--
handleBuildError :: Config -> BuildError -> Build ()
handleBuildError cfg err = do
  outBlank
  outLine
  outLn "* Build failed"
  outBlank
  outLn $ render (ppr err)

  maybe' (configMailFromTo cfg) (return ())
    $ \(from,to) -> do

        -- Patches since the last successful buildbot
        hist <- maybe' (configHistory cfg) (return []) $ \fn ->
          changesAfter Nothing . read =<< io (readFile fn)

        -- Send email to the default list, and all recent submitters
        let failTo = intercalate ", "
                   . nub
                   . sort
                   $ fromMaybe to (configMailFailTo cfg) ++ map darcsAuthor hist

        outBlank
        outLn $ "* Mailing report to \"" ++ failTo ++ "\""

        mail <- createMailWithCurrentTime from failTo
                  "[accelerate-buildbot] Test Failed"
                  . render $ vcat
                  [ text "Accelerate test failed )="
                  , blank
                  , blank
                  , nest 2 $ ppr err
                  , blank
                  , text "Patches since last build:"
                  , blank
                  , text . unlines $ map show hist
                  , blank
                  ]
        sendMailWithMailer mail (configWithMailer cfg)

