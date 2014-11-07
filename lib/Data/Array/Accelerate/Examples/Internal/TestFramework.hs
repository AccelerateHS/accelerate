-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal.TestFramework
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Examples.Internal.TestFramework (

  -- * Test-framework runner
  Test, runTests,

  -- ** QuickCheck
  Property, testProperty, testGroup, (~=?), (~?=),

  -- ** HUnit
  Assertion, testCase, assertEqual,

  -- ** Similarity
  module Data.Array.Accelerate.Examples.Internal.Similar,

) where

import Data.Array.Accelerate.Examples.Internal.ParseArgs
import Data.Array.Accelerate.Examples.Internal.Similar

import Data.Label
import Control.Monad
import Control.Exception
import System.Exit
import System.Environment

import Test.Framework
import Test.HUnit                                       ( Assertion, assertFailure )
import Test.QuickCheck                                  ( Property, counterexample, ioProperty )
import Test.Framework.Providers.HUnit                   ( testCase )
import Test.Framework.Providers.QuickCheck2             ( testProperty )


-- | Run the given tests, if enabled.
--
runTests :: Options -> [String] -> [Test] -> IO ()
runTests opts argv tests
  = when (get optTest opts)
  $ withArgs argv
  $ do
       putStrLn "running tests..."
       defaultMainWithOpts tests (get optTestFramework opts)
       -- test-framework wants to have a nap on success; don't let it.
       `catch` \e -> case e of
                       ExitSuccess -> putStrLn ""
                       _           -> throwIO e


failure :: Show a => a -> a -> String
failure expected actual =
  unlines [ "*** Expected:", show expected
          , "*** Received:", show actual ]

-- | Assert that the specified actual value is equal-ish to the expected value.
-- If we are in verbose mode, the output message will contain the expected and
-- actual values.
--
assertEqual
    :: (Similar a, Show a)
    => a        -- ^ The expected value
    -> a        -- ^ The actual value
    -> Assertion
assertEqual expected actual =
  unless (expected ~= actual)
         (assertFailure (failure expected actual))

infix 1 ~=?, ~?=

-- | Short hand for a test case that asserts similarity, with the actual value
-- on the right hand side and the expected value on the left.
--
(~=?) :: (Similar a, Show a) => a -> a -> Property
expected ~=? actual =
  ioProperty $ do
    actual' <- evaluate actual
    return  $! counterexample (failure expected actual') (expected ~= actual')

-- | Short hand for a test case that asserts similarity, with the actual value
-- on the left hand side and the expected value on the right.
--
(~?=) :: (Similar a, Show a) => a -> a -> Property
(~?=) = flip (~=?)

