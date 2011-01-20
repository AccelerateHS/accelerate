{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, TypeOperators #-}
module Main where

-- standard libraries
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Foreign.Ptr

-- friends
import Data.Array.Accelerate
import Data.Array.Accelerate.Array.BlockCopy
import Data.Array.Accelerate.CUDA
import qualified Data.Array.Accelerate as A


main = defaultMain tests

tests = [ testGroup "Block copies" [
            testCase "Testing block copy to array of primitive type" testBlockCopyPrim,
            testCase "Testing block copy to array of tuples" testBlockCopyTuples,
            testCase "Testing block copy from array of ints" testBlockCopyFromArray,
            testCase "Testing block copy from array of ints with functions" testBlockCopyFromArrayWithFunctions,
            testCase "Testing block copy from array of int16s" testBlockCopyFromArrayInt16,
            testCase "Testing block copy from array of int64s" testBlockCopyFromArrayInt64 ]

        ]


assertEqualWithin :: (Ord a, Fractional a) => String -> a -> a -> a -> Assertion
assertEqualWithin str a b prec = assertBool str (abs (a - b ) < prec)

testBlockCopyPrim :: Assertion
testBlockCopyPrim = do
  ptr <- oneToTen
  (arr :: Array (Z :. Int) Int) <- blockCopyToArray (Z :. 10) ((), ptr)
  assertEqual "Not equal" [1..10] (toList arr)

testBlockCopyTuples :: Assertion
testBlockCopyTuples = do
  intPtr <- oneToTen
  doublePtr <- tenToOne
  (arr :: Array (Z :. Int) (Int, Double)) <- blockCopyToArray (Z :. 10) (((), intPtr), doublePtr)
  assertEqual "Not equal" [ (x, fromIntegral (11 - x)) | x <- [1..10]] (toList arr)

testBlockCopyFromArray :: Assertion
testBlockCopyFromArray = do
  let (arr :: Array (Z:.Int:.Int) Int) = fromList (Z:.10:.10) [2*x | x <- [0..99]]
  ohi <- nInts 100
  blockCopyFromArray arr ((), ohi)
  b <- isFilledWithEvens ohi 100
  assertEqual "Not equal" 1 b

testBlockCopyFromArrayWithFunctions :: Assertion
testBlockCopyFromArrayWithFunctions = do
  let n = 5^3
  let (arr :: Array (Z:.Int:.Int:.Int) Int) = fromList (Z:.5:.5:.5) [2*x | x <- [0..n-1]]
  ohi <- nInts n
  blockCopyFromArrayWithFunctions arr ((), memcpy ohi)
  b <- isFilledWithEvens ohi n
  assertEqual "Not equal" 1 b

testBlockCopyFromArrayInt16 :: Assertion
testBlockCopyFromArrayInt16 = do
  let n = 50
  let (arr :: Array (Z:.Int) Int) = fromList (Z:.n) [2*x | x <- [0..n-1]]
  ohi <- nInt16s n
  blockCopyFromArray arr ((), ohi)
  b <- isFilledWithEvens ohi n
  assertEqual "Not equal" 1 b

testBlockCopyFromArrayInt64 :: Assertion
testBlockCopyFromArrayInt64 = do
  let n = 73
  let (arr :: Array (Z:.Int) Int) = fromList (Z:.n) [2*x | x <- [0..n-1]]
  ohi <- nInt64s n
  blockCopyFromArray arr ((), ohi)
  b <- isFilledWithEvens ohi n
  assertEqual "Not equal" 1 b

foreign import ccall "one_to_ten" oneToTen :: IO (Ptr Int)
foreign import ccall "ten_to_one" tenToOne :: IO (Ptr Double)
foreign import ccall "n_ints" nInts :: Int -> IO (Ptr Int)
foreign import ccall "n_int_16s" nInt16s :: Int -> IO (Ptr Int)
foreign import ccall "n_int_64s" nInt64s :: Int -> IO (Ptr Int)
foreign import ccall "is_filled_with_evens" isFilledWithEvens :: Ptr Int -> Int -> IO Int
foreign import ccall memcpy :: Ptr a -> Ptr b -> Int -> IO ()
