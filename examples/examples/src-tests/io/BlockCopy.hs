{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, TypeOperators #-}

module BlockCopy where

-- standard libraries
import Prelude as P
import Foreign.Ptr
import Control.Monad
import Control.Exception

-- friends
import Data.Array.Accelerate
import Data.Array.Accelerate.IO

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual preface expected actual =
  unless (actual == expected) (throw $ AssertionFailed msg)
  where
    msg = (if null preface then "" else preface ++ "\n")  ++
          "expected: " ++ show expected ++ "\n but got: " ++ show actual

run :: IO ()
run =
  mapM_ (\(msg,act) -> putStrLn ("test: " ++ msg) >> act)
    [ ("fromPtr Int",          testBlockCopyPrim)
    , ("fromPtr (Int,Double)", testBlockCopyTuples)
    , ("toPtr Int16",          testBlockCopyFromArrayInt16)
    , ("toPtr Int32",          testBlockCopyFromArrayInt32)
    , ("toPtr Int64",          testBlockCopyFromArrayInt64)
    , ("fromArray Int",        testBlockCopyFromArrayWithFunctions) ]


testBlockCopyPrim :: IO ()
testBlockCopyPrim = do
  ptr <- oneToTen
  (arr :: Vector Int32) <- fromPtr (Z :. 10) ((), ptr)
  assertEqual "Not equal" [1..10] (toList arr)

testBlockCopyTuples :: IO ()
testBlockCopyTuples = do
  intPtr    <- oneToTen
  doublePtr <- tenToOne
  (arr :: Vector (Int32, Double)) <- fromPtr (Z :. 10) (((), intPtr), doublePtr)
  assertEqual "Not equal" [ (x, P.fromIntegral (11 - x)) | x <- [1..10]] (toList arr)

testBlockCopyFromArrayWithFunctions :: IO ()
testBlockCopyFromArrayWithFunctions = do
  let n = 5^(3::Int)
  let (arr :: Array (Z:.Int:.Int:.Int) Int32) = fromList (Z:.5:.5:.5) [2*x | x <- [0..n-1]]
  ohi <- nInt32s (P.fromIntegral n)
  fromArray arr ((), memcpy ohi)
  b   <- isFilledWithEvens32 ohi (P.fromIntegral n)
  assertEqual "Not equal" 1 b

testBlockCopyFromArrayInt16 :: IO ()
testBlockCopyFromArrayInt16 = do
  let n = 50
  let (arr :: Vector Int16) = fromList (Z:.n) [2 * P.fromIntegral x | x <- [0..n-1]]
  ohi <- nInt16s (P.fromIntegral n)
  toPtr arr ((), ohi)
  b   <- isFilledWithEvens16 ohi (P.fromIntegral n)
  assertEqual "Not equal" 1 b

testBlockCopyFromArrayInt32 :: IO ()
testBlockCopyFromArrayInt32 = do
  let (arr :: Array (Z:.Int:.Int) Int32) = fromList (Z:.10:.10) [2*x | x <- [0..99]]
  ohi <- nInt32s 100
  toPtr arr ((), ohi)
  b   <- isFilledWithEvens32 ohi 100
  assertEqual "Not equal" 1 b

testBlockCopyFromArrayInt64 :: IO ()
testBlockCopyFromArrayInt64 = do
  let n = 73
  let (arr :: Vector Int64) = fromList (Z:.n) [2 * P.fromIntegral x | x <- [0..n-1]]
  ohi <- nInt64s (P.fromIntegral n)
  toPtr arr ((), ohi)
  b   <- isFilledWithEvens64 ohi (P.fromIntegral n)
  assertEqual "Not equal" 1 b

foreign import ccall "one_to_ten" oneToTen :: IO (Ptr Int32)
foreign import ccall "ten_to_one" tenToOne :: IO (Ptr Double)
foreign import ccall "n_int_16s" nInt16s :: CInt -> IO (Ptr Int16)
foreign import ccall "n_int_32s" nInt32s :: CInt -> IO (Ptr Int32)
foreign import ccall "n_int_64s" nInt64s :: CInt -> IO (Ptr Int64)
foreign import ccall "is_filled_with_evens_16" isFilledWithEvens16 :: Ptr Int16 -> CInt -> IO CInt
foreign import ccall "is_filled_with_evens_32" isFilledWithEvens32 :: Ptr Int32 -> CInt -> IO CInt
foreign import ccall "is_filled_with_evens_64" isFilledWithEvens64 :: Ptr Int64 -> CInt -> IO CInt
foreign import ccall memcpy :: Ptr a -> Ptr b -> Int -> IO ()

