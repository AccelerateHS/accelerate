{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Spectral.SMVM (

  test_smvm

) where

-- from the accelerate-smvm program
import SMVM

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
import QuickCheck.Arbitrary.Array


test_smvm :: Config -> Test
test_smvm opt = testGroup "smvm" $ catMaybes
--  [ testElt configInt8   (undefined :: Int8)
--  , testElt configInt16  (undefined :: Int16)
--  , testElt configInt32  (undefined :: Int32)
--  , testElt configInt64  (undefined :: Int64)
--  , testElt configWord8  (undefined :: Word8)
--  , testElt configWord16 (undefined :: Word16)
--  , testElt configWord32 (undefined :: Word32)
--  , testElt configWord64 (undefined :: Word64)
  [ testElt configFloat  (undefined :: Float)
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
      $ testProperty (show (typeOf (undefined :: a))) (run_smvm (undefined :: a))

    run_smvm :: forall a. (Elt a, IsNum a, Similar a, Arbitrary a) => a -> Property
    run_smvm _ =
      forAll arbitraryCSRMatrix         $ \(segd, svec :: Vector (Int32,a), cols) ->
      forAll (arbitraryArray (Z :. cols)) $ \vec ->
        run2 backend smvm (segd, svec) vec
        ~?=
        smvmRef segd svec vec


-- Reference implementation
-- ------------------------

smvmRef :: (Elt a, IsNum a)
        => Segments Int32
        -> Vector (Int32, a)
        -> Vector a
        -> Vector a
smvmRef segd smat vec =
  fromList (arrayShape segd)
           [ P.sum [ val * indexArray vec (Z :. P.fromIntegral i) | (i,val) <- row ]
                   | row <- splitPlaces (toList segd) (toList smat) ]

