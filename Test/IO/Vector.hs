
module VectorCopy (run) where

-- friends
import Data.Array.Accelerate
import Data.Array.Accelerate.IO         ( toVectors, fromVectors )

-- standard library
import Test.QuickCheck


-- Print expected/received message on inequality
--
infix 4 .==.
(.==.) :: (Eq a, Show a) => a -> a -> Property
(.==.) ans ref = printTestCase message (ref == ans)
  where
    message = unlines ["*** Expected:", show ref
                      ,"*** Received:", show ans ]


roundtrip :: (Arbitrary a, Eq a, Elt a)
          => [a]
          -> Property
roundtrip xs =
  let n   = length xs
      sh  = Z:.n
      arr = fromList sh xs
  in
  xs .==. toList (fromVectors sh (toVectors arr))


prop_Int8_roundtrip :: [Int8] -> Property
prop_Int8_roundtrip = roundtrip

prop_Int16_roundtrip :: [Int16] -> Property
prop_Int16_roundtrip = roundtrip

prop_Int32_roundtrip :: [Int32] -> Property
prop_Int32_roundtrip = roundtrip

prop_Int64_roundtrip :: [Int64] -> Property
prop_Int64_roundtrip = roundtrip

prop_Int_roundtrip :: [Int] -> Property
prop_Int_roundtrip = roundtrip

prop_Float_roundtrip :: [Float] -> Property
prop_Float_roundtrip = roundtrip

prop_Double_roundtrip :: [Double] -> Property
prop_Double_roundtrip = roundtrip


run :: IO ()
run = mapM_ quickCheck
    [ property prop_Int8_roundtrip
    , property prop_Int16_roundtrip
    , property prop_Int32_roundtrip
    , property prop_Int64_roundtrip
    , property prop_Int_roundtrip
    , property prop_Float_roundtrip
    , property prop_Double_roundtrip
    ]

