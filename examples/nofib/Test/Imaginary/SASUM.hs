{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Imaginary.SASUM (

  test_sasum,

) where

import Config
import QuickCheck.Arbitrary.Array                               ()

import Prelude                                                  as P
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Examples.Internal                  as A

import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck


test_sasum :: Backend -> Config -> Test
test_sasum backend opt = testGroup "sasum" $ catMaybes
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
    testElt :: forall a. (P.Num a, A.Num a, Similar a, Arbitrary a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just
      $ testProperty (show (typeOf (undefined :: a))) (run_sasum :: Vector a -> Property)

    run_sasum xs =
      run1 backend sasum xs `indexArray` Z
      ~?=
      P.sum (P.map abs (toList xs))


-- Accelerate implementation ---------------------------------------------------

sasum :: A.Num e => Acc (Vector e) -> Acc (Scalar e)
sasum = A.fold (+) 0 . A.map abs

