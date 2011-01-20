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
           testCase "test1" test1, testCase "test2" test2, testCase "test3" test3 ] ]


assertEqualWithin :: (Ord a, Fractional a) => String -> a -> a -> a -> Assertion
assertEqualWithin str a b prec = assertBool str (abs (a - b ) < prec)

test1 :: Assertion
test1 = do
  ptr <- oneToTen
  (arr :: Array (Z :. Int) Int) <- blockCopyToArray (Z :. 10) ((), ptr)
  assertEqual "test1" [1..10] (toList arr)

test2 :: Assertion
test2 = do
  intPtr <- oneToTen
  doublePtr <- tenToOne
  (arr :: Array (Z :. Int) (Int, Double)) <- blockCopyToArray (Z :. 10) (((), intPtr), doublePtr)
  assertEqual "test2" [ (x, fromIntegral (11 - x)) | x <- [1..10]] (toList arr)

foreign import ccall "one_to_ten" oneToTen :: IO (Ptr Int)
foreign import ccall "ten_to_one" tenToOne :: IO (Ptr Double)
