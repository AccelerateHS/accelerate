
module Data.Array.Accelerate.Examples.Internal.Test (

  -- * Test-framework runner
  runTests,

) where

import Data.Array.Accelerate.Examples.Internal.ParseArgs

import Data.Label
import Control.Monad
import Control.Exception
import System.Exit
import System.Environment

import Test.Framework


-- | Run the given tests, if enabled.
--
runTests :: Options -> [String] -> [Test] -> IO ()
runTests opts argv tests
  = when (get optTest opts)
  $ withArgs argv
  $ defaultMainWithOpts tests (get optTestFramework opts)
    -- test-framework wants to have a nap on success; don't let it.
    `catch` \e -> case e of
                    ExitSuccess -> return()
                    _           -> throwIO e

