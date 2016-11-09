{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeOperators            #-}

module Test.IO.Ptr (

  test_ptr,

) where

import Config

import Prelude                                  as P
import Foreign.C
import Foreign.Ptr
import Test.Framework
import Test.HUnit                               ( Assertion, (@?=) )
import Test.Framework.Providers.HUnit

import Data.Array.Accelerate
import Data.Array.Accelerate.IO


test_ptr :: Config -> Test
test_ptr _ =
  testGroup "block copy"
    [
      testCase "toPtr Int16"            toPtrInt16
    , testCase "toPtr Int32"            toPtrInt32
    , testCase "toPtr Int64"            toPtrInt64
    , testCase "fromPtr Int32"          fromPtrInt32
    , testCase "fromPtr (Int32,Double)" fromPtrIntDouble
    , testCase "fromArray Int32"        fromArrayInt32
    ]


-- Unit tests ------------------------------------------------------------------
--
intToBool :: P.Integral a => a -> Bool
intToBool 0 = False
intToBool _ = True

fromPtrInt32 :: Assertion
fromPtrInt32 = do
  ptr   <- oneToTen
  arr   <- fromPtr (Z :. 10) ptr        :: IO (Vector Int32)
  toList arr @?= [1..10]

fromPtrIntDouble :: Assertion
fromPtrIntDouble = do
  intPtr        <- oneToTen
  doublePtr     <- tenToOne
  arr           <- fromPtr (Z :. 10) (((), intPtr), doublePtr)    :: IO (Vector (Int32, Double))
  toList arr @?= [ (x, P.fromIntegral (11 - x)) | x <- [1..10]]


toPtrInt16 :: IO ()
toPtrInt16 = do
  let n = 50
      arr :: Vector Int16
      arr = fromList (Z:.n) [2 * P.fromIntegral x | x <- [0..n-1]]
  --
  ohi <- nInt16s (P.fromIntegral n)
  toPtr arr ohi
  b   <- isFilledWithEvens16 ohi (P.fromIntegral n)
  intToBool b @?= True

toPtrInt32 :: IO ()
toPtrInt32 = do
  let n = 100
      arr :: Array DIM2 Int32
      arr = fromList (Z:.10:.10) [2 * P.fromIntegral x | x <- [0..n-1]]
  --
  ohi <- nInt32s n
  toPtr arr ohi
  b   <- isFilledWithEvens32 ohi n
  intToBool b @?= True

toPtrInt64 :: IO ()
toPtrInt64 = do
  let n = 73
      arr :: Vector Int64
      arr = fromList (Z:.n) [2 * P.fromIntegral x | x <- [0..n-1]]
  --
  ohi <- nInt64s (P.fromIntegral n)
  toPtr arr ohi
  b   <- isFilledWithEvens64 ohi (P.fromIntegral n)
  intToBool b @?= True


fromArrayInt32 :: IO ()
fromArrayInt32 = do
  let n = 5 P.^ (3::Int)
      arr :: Array DIM3 Int32
      arr = fromList (Z:.5:.5:.5) [2*x | x <- [0..n-1]]
  --
  ohi <- nInt32s (P.fromIntegral n)
  fromArray arr (memcpy ohi)
  b   <- isFilledWithEvens32 ohi (P.fromIntegral n)
  intToBool b @?= True


-- Foreign functions -----------------------------------------------------------
--
foreign import ccall "one_to_ten" oneToTen :: IO (Ptr Int32)
foreign import ccall "ten_to_one" tenToOne :: IO (Ptr Double)
foreign import ccall "n_int_16s" nInt16s :: CInt -> IO (Ptr Int16)
foreign import ccall "n_int_32s" nInt32s :: CInt -> IO (Ptr Int32)
foreign import ccall "n_int_64s" nInt64s :: CInt -> IO (Ptr Int64)
foreign import ccall "is_filled_with_evens_16" isFilledWithEvens16 :: Ptr Int16 -> CInt -> IO CInt
foreign import ccall "is_filled_with_evens_32" isFilledWithEvens32 :: Ptr Int32 -> CInt -> IO CInt
foreign import ccall "is_filled_with_evens_64" isFilledWithEvens64 :: Ptr Int64 -> CInt -> IO CInt
foreign import ccall memcpy :: Ptr a -> Ptr b -> Int -> IO ()

