{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.Filter (

  test_filter

) where

import Prelude                                                  as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck                                          ( Arbitrary )
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import Test.Base
import QuickCheck.Arbitrary.Array                               ()
import Data.Array.Accelerate                                    as A hiding ( size )
import Data.Array.Accelerate.Examples.Internal                  as A
import Data.Array.Accelerate.Array.Sugar                        as Sugar

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
    testIntegralElt :: forall e. (P.Integral e, A.Integral e, Arbitrary e, Similar e) => (Config :-> Bool) -> e -> Maybe Test
    testIntegralElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined::e)))
          [ testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, Slice sh, P.Eq sh, Arbitrary (Array (sh:.Int) e)) => (sh:.Int) -> Test
        testDim sh = testGroup ("DIM" P.++ show (rank sh))
          [ testProperty "even" (run_filter A.even P.even :: Array (sh:.Int) e -> Property)
          ]

    testFloatingElt :: forall e. (P.RealFrac e, A.RealFrac e, Arbitrary e, Similar e) => (Config :-> Bool) -> e -> Maybe Test
    testFloatingElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined::e)))
          [ testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, Slice sh, P.Eq sh, Arbitrary (Array (sh:.Int) e)) => (sh:.Int) -> Test
        testDim sh = testGroup ("DIM" P.++ show (rank sh))
          [ testProperty "positive" (run_filter (A.> 0) (P.> 0) :: Array (sh:.Int) e -> Property)
          ]

    run_filter f g xs
      = run1 backend (A.filter f) xs ~?= filterRef g xs


filterRef
    :: (Shape sh, Elt e)
    => (e -> Bool)
    -> Array (sh:.Int) e
    -> (Vector e, Array sh Int)
filterRef f arr = (fromList (Z:.total) (concat result), fromList sh len)
  where
    sh :. n   = arrayShape arr
    result    = P.take (size sh) [ P.filter f sub | sub <- splitEvery n (toList arr) ]
    len       = P.map P.length result
    total     = P.sum len

