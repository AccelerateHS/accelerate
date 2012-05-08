{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module VectorCopy where

import Data.Array.Accelerate hiding (fromList)
import Data.Array.Accelerate.Array.Sugar (EltRepr)
import Data.Array.Accelerate.IO

import Data.Vector.Storable

import Foreign

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Monadic

roundtrip :: ( Arbitrary a
             , Eq a
             , Elt a
             , Storable a
             , BlockPtrs (EltRepr a) ~ ((), Ptr a) )
          => [a] -> Property
roundtrip xs = monadicIO $ do
  let xsv = fromList xs
  xsarr <- run $ fromVectorIO xsv
  xsv'  <- run $ toVectorIO xsarr
  assert (xsv == xsv')

unsaferoundtrip :: ( Arbitrary a
                   , Eq a
                   , Elt a
                   , Storable a
                   , BlockPtrs (EltRepr a) ~ ((), Ptr a) )
                => [a] -> Bool
unsaferoundtrip xs = xsv == (toVector (fromVector xsv))
  where xsv = fromList xs

prop_Int8_roundtrip :: [Int8] -> Property
prop_Int8_roundtrip = roundtrip
prop_Int8_unsaferoundtrip :: [Int8] -> Bool
prop_Int8_unsaferoundtrip = unsaferoundtrip

prop_Int16_roundtrip :: [Int16] -> Property
prop_Int16_roundtrip = roundtrip
prop_Int16_unsaferoundtrip :: [Int16] -> Bool
prop_Int16_unsaferoundtrip = unsaferoundtrip

prop_Int32_roundtrip :: [Int32] -> Property
prop_Int32_roundtrip = roundtrip
prop_Int32_unsaferoundtrip :: [Int32] -> Bool
prop_Int32_unsaferoundtrip = unsaferoundtrip

prop_Int64_roundtrip :: [Int64] -> Property
prop_Int64_roundtrip = roundtrip
prop_Int64_unsaferoundtrip :: [Int64] -> Bool
prop_Int64_unsaferoundtrip = unsaferoundtrip

prop_Int_roundtrip :: [Int] -> Property
prop_Int_roundtrip = roundtrip
prop_Int_unsaferoundtrip :: [Int] -> Bool
prop_Int_unsaferoundtrip = unsaferoundtrip

prop_Float_roundtrip :: [Float] -> Property
prop_Float_roundtrip = roundtrip
prop_Float_unsaferoundtrip :: [Float] -> Bool
prop_Float_unsaferoundtrip = unsaferoundtrip

prop_Double_roundtrip :: [Double] -> Property
prop_Double_roundtrip = roundtrip
prop_Double_unsaferoundtrip :: [Double] -> Bool
prop_Double_unsaferoundtrip = unsaferoundtrip

test :: IO Bool
test = $quickCheckAll