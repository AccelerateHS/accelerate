{-# LANGUAGE CPP, FlexibleContexts #-}

module Main where

import Data.Bits
import Properties
import Test.QuickCheck
import Text.Printf

import Data.Array.Accelerate
import Data.Array.Accelerate.Type


-- All tests, all element types (coffee time!)
--
main :: IO ()
main = do
  mapM_ (\(s,t) -> printf "===> %s\n" s >> runTests t >> putStrLn "") $
    [ ("Int",    prop_integral (undefined :: Int))
    , ("Int8",   prop_integral (undefined :: Int8))
    , ("Int16",  prop_integral (undefined :: Int16))
    , ("Int32",  prop_integral (undefined :: Int32))
    , ("Int64",  prop_integral (undefined :: Int64))
    , ("Word",   prop_integral (undefined :: Word))
    , ("Word8",  prop_integral (undefined :: Word8))
    , ("Word16", prop_integral (undefined :: Word16))
    , ("Word32", prop_integral (undefined :: Word32))
    , ("Word64", prop_integral (undefined :: Word64))
    , ("Float",  prop_floating (undefined :: Float))
    , ("Double", prop_floating (undefined :: Double))
    ]

-- Execute a sequence of (name,test) pairs
--
runTests :: [(String, IO b)] -> IO ()
runTests tests = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests


-- The test sets
--
prop_integral :: forall a. (Integral a, Bits a, IsIntegral a, Elem a, Similar a, Arbitrary a, Arbitrary (Acc (Vector a)))
              => a -> [(String, IO ())]
prop_integral dummy =
  [ test2 prop_Add
  , test2 prop_Sub
  , test2 prop_Mul
  , test1 prop_Abs
  , test1 prop_Negate
  , test1 prop_Signum
  , test2 prop_Quot
  , test2 prop_Rem
  , test2 prop_Idiv
  , test2 prop_Mod
  , test2 prop_Band
  , test2 prop_BOr
  , test2 prop_BXor
  , test1 prop_BNot
  , test1 prop_BShiftL
  , test1 prop_BShiftR
  , test1 prop_BRotateL
  , test1 prop_BRotateR
  , test2 prop_Lt
  , test2 prop_Gt
  , test2 prop_LtEq
  , test2 prop_GtEq
  , test2 prop_Eq
  , test2 prop_NEq
  , test2 prop_Min
  , test2 prop_Max
  ]
  ++ prop_comps dummy
  where
    test1 (s,t) = (s, quickCheck (t :: [a] -> Property))
    test2 (s,t) = (s, quickCheck (t :: [a] -> [a] -> Property))

prop_floating :: forall a. (RealFrac a, IsFloating a, Elem a, Similar a, Arbitrary a, Arbitrary (Acc (Vector a)))
                => a -> [(String, IO ())]
prop_floating dummy =
  [ test2 prop_Add
  , test2 prop_Sub
  , test2 prop_Mul
  , test1 prop_Abs
  , test1 prop_Negate
  , test1 prop_Signum
  , test2 prop_FDiv
  , test1 prop_Recip
  , test1 prop_Sin
  , test1 prop_Cos
  , test1 prop_Tan
  , test1 prop_ASin
  , test1 prop_ACos
  , test1 prop_ATan
  , test1 prop_ASinh
  , test1 prop_ACosh
  , test1 prop_ATanh
  , test1 prop_Exp
  , test1 prop_Sqrt
  , test1 prop_Log
  , test2 prop_Pow
  , test2 prop_LogBase
  , test2 prop_Lt
  , test2 prop_Gt
  , test2 prop_LtEq
  , test2 prop_GtEq
  , test2 prop_Eq
  , test2 prop_NEq
  , test2 prop_Min
  , test2 prop_Max
  ]
  ++ prop_comps dummy
  where
    test1 (s,t) = (s, quickCheck (t :: [a] -> Property))
    test2 (s,t) = (s, quickCheck (t :: [a] -> [a] -> Property))


prop_comps :: forall a. (IsNum a, Ord a, Elem a, Similar a, Arbitrary a, Arbitrary (Acc (Vector a)))
           => a -> [(String, IO ())]
prop_comps _dummy =
  [ test1 prop_Sum
  , test1 prop_Product
  , test1 prop_Minimum
  , test1 prop_Maximum
  , test2 prop_Zip
  , testPair prop_FstUnzip
  , testPair prop_SndUnzip
  , test1 prop_Backpermute
  , test1 prop_Scanl
  , test1 prop_ScanlRdx
  , test1 prop_Scanr
  , test1 prop_ScanrRdx
  , test1 prop_Square
  , testSaxpy prop_Saxpy
  , test2 prop_Dotp
  , test1 prop_Filter
  , testPair prop_MapAddPair
  , testPair prop_ScanlPair
  , testPair prop_ScanrPair
#ifdef ACCELERATE_CUDA_BACKEND
  , ("arbitrary", quickCheck (test_arbitrary dummy))
#endif
  ]
  where
    test1 (s,t) = (s, quickCheck (t :: [a] -> Property))
    test2 (s,t) = (s, quickCheck (t :: [a] -> [a] -> Property))
    testPair  (s,t) = (s, quickCheck (t :: [(a,a)] -> Property)) -- mix and match types?
    testSaxpy (s,t) = (s, quickCheck (t :: a -> [a] -> [a] -> Property))

