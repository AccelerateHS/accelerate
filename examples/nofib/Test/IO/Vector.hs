{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.IO.Vector (test_vector)
  where

import Prelude                                                  as P
import Config
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.Base
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import QuickCheck.Arbitrary.Array                               ()

import Data.Array.Accelerate
import Data.Array.Accelerate.IO                                 ( toVectors, fromVectors )
import Data.Array.Accelerate.Array.Sugar                        as Sugar
import Data.Array.Accelerate.Examples.Internal


test_vector :: Config -> Test
test_vector opt = testGroup "vector" $ catMaybes
  [ testElt configInt8   (undefined :: Int8)
  , testElt configInt16  (undefined :: Int16)
  , testElt configInt32  (undefined :: Int32)
  , testElt configInt64  (undefined :: Int64)
  , testElt configWord8  (undefined :: Word8)
  , testElt configWord16 (undefined :: Word16)
  , testElt configWord32 (undefined :: Word32)
  , testElt configWord64 (undefined :: Word64)
  , testElt configFloat  (undefined :: Float)
  , testElt configDouble (undefined :: Double)
  ]
  where
    testElt :: forall a. (Elt a, Arbitrary a, Similar a) => (Config :-> Bool) -> a -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, P.Eq sh, Arbitrary (Array sh a)) => sh -> Test
        testDim sh = testProperty ("DIM" P.++ show (rank sh)) (roundtrip :: Array sh a -> Property)

        roundtrip arr =
          let sh = arrayShape arr
          in  fromVectors sh (toVectors arr) ~?= arr

