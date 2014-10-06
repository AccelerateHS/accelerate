{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.Filter (

  test_filter

) where

import Prelude                                                  as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import Test.Base
import QuickCheck.Arbitrary.Array                               ()
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Examples.Internal                  as A

--
-- Filter ----------------------------------------------------------------------
--

test_filter :: Backend -> Config -> Test
test_filter backend opt = testGroup "filter" $ catMaybes
  [ testIntegralElt configInt8   (undefined :: Int8)
  , testIntegralElt configInt16  (undefined :: Int16)
  , testIntegralElt configInt32  (undefined :: Int32)
  , testIntegralElt configInt64  (undefined :: Int64)
  , testIntegralElt configWord8  (undefined :: Word8)
  , testIntegralElt configWord16 (undefined :: Word16)
  , testIntegralElt configWord32 (undefined :: Word32)
  , testIntegralElt configWord64 (undefined :: Word64)
  , testFloatingElt configFloat  (undefined :: Float)
  , testFloatingElt configDouble (undefined :: Double)
  ]
  where
    testIntegralElt :: forall e. (Elt e, Integral e, IsIntegral e, Arbitrary e, Similar e) => (Config :-> Bool) -> e -> Maybe Test
    testIntegralElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just
      $ testProperty (show (typeOf (undefined :: e))) (run_filter A.even P.even :: Vector e -> Property)

    testFloatingElt :: forall e. (Elt e, RealFrac e, IsFloating e, Arbitrary e, Similar e) => (Config :-> Bool) -> e -> Maybe Test
    testFloatingElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just
      $ testProperty (show (typeOf (undefined :: e))) (run_filter (>* 0) (> 0) :: Vector e -> Property)

    run_filter f g xs
      = toList (run1 backend (A.filter f) xs) ~?= P.filter g (toList xs)

