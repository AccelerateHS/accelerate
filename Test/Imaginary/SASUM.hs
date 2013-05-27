{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Imaginary.SASUM (

  test_sasum,

) where

import Prelude                                                  as P
import Data.Array.Accelerate                                    as A
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import ParseArgs
import Test.Base
import QuickCheck.Arbitrary.Array                               ()


test_sasum :: Config -> Test
test_sasum opt = testGroup "sasum" $ catMaybes
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
    backend = get configBackend opt

    testElt :: forall a. (Elt a, IsNum a, Similar a, Arbitrary a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just
      $ testProperty (show (typeOf (undefined :: a))) (run_sasum :: Vector a -> Property)

    run_sasum xs =
      run1 backend sasum xs `indexArray` Z
      .==.
      P.sum (P.map abs (toList xs))


-- Accelerate implementation ---------------------------------------------------

sasum :: (Elt e, IsNum e) => Acc (Vector e) -> Acc (Scalar e)
sasum = A.fold (+) 0 . A.map abs

