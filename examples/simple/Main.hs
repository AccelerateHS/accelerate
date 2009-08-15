{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Exception
import Data.Array.Unboxed
import Data.Array.IArray
import System.Random

import qualified Data.Array.Accelerate as Acc
import qualified Data.Array.Accelerate.Interpreter as Interp

import Time
import SAXPY


-- Auxilliary array functions
-- --------------------------

-- To ensure that an unboxed array is fully evaluated, just force one element
-- 
evaluateUArray :: (IArray UArray e) => UArray Int e -> IO ()
evaluateUArray uarr = evaluate (uarr!0) >> return ()

-- To ensure that an unboxed array is fully evaluated, just force one element
-- 
evaluateArray :: Acc.Array Int e -> IO ()
evaluateArray arr = evaluate (arr Acc.! 0) >> return ()

randomUArray :: (Num e, Random e, IArray UArray e) => Int -> IO (UArray Int e)
randomUArray n
  = do
      rg <- newStdGen
      let -- The std random function is too slow to generate really big vectors
          -- with.  Instead, we generate a short random vector and repeat that.
          randvec = take k (randomRs (-100, 100) rg)
          vec     = listArray (0, n - 1) 
                              [randvec !! (i `mod` k) | i <- [0..n - 1]]
      evaluateUArray vec
      return vec
  where
    k = 1000

convertUArray :: (IArray UArray e, Acc.Elem e) 
              => UArray Int e -> IO (Acc.Array Int e)
convertUArray uarr
  = do
      let arr = Acc.fromIArray uarr
      evaluateArray arr
      return arr

validate :: (Show e, Eq e, IArray UArray e) => UArray Int e -> UArray Int e -> IO ()
validate arr_ref arr | arr_ref == arr = putStrLn "Valid."
                     | otherwise      = putStrLn "INVALID!"
                                        >> print arr_ref
                                        >> print arr

-- Timing
-- ------

timeUArray :: IArray UArray e => (() -> UArray Int e) -> IO (UArray Int e)
{-# NOINLINE timeUArray #-}
timeUArray testee 
  = do
      (r, time1) <- oneRun testee
      (r, time2) <- oneRun testee
      (r, time3) <- oneRun testee
      putStrLn $ showMinAvgMax milliseconds [time1, time2, time3] ++
                 "wall & cpu min/avg/max (in ms)"
      return r
  where
    oneRun testee = do
                      start <- getTime
                      let r = testee ()
                      evaluateUArray r
                      end <- getTime
                      return (r, end `minus` start)

timeArray :: (IArray UArray e, Acc.Elem e)
          => (() -> Acc.Array Int e) -> IO (UArray Int e)
{-# NOINLINE timeArray #-}
timeArray testee 
  = do
      (r, time1) <- oneRun testee
      (r, time2) <- oneRun testee
      (r, time3) <- oneRun testee
      putStrLn $ showMinAvgMax milliseconds [time1, time2, time3] ++
                 " (wall - cpu min/avg/max in ms)"
      return $ Acc.toIArray r
  where
    oneRun testee = do
                      start <- getTime
                      let r = testee ()
                      evaluateArray r
                      end <- getTime
                      return (r, end `minus` start)


-- Tests
-- -----

test_saxpy :: Int -> IO ()
test_saxpy n
  = do
      putStrLn "== SAXPY"
      putStrLn $ "Generating data (n =" ++ show n ++ ")..."
      v1_ref <- randomUArray n
      v1     <- convertUArray v1_ref
      print v1_ref
      print (Acc.toIArray v1 :: UArray Int Float)
      v2_ref <- randomUArray n
      v2     <- convertUArray v2_ref
      putStrLn "Running reference code..."
      ref_result <- timeUArray $ \_ -> saxpy_ref 1.5 v1_ref v2_ref
      putStrLn "Running Accelerate code..."
      result <- timeArray $ \_ -> saxpy_interp 1.5 v1 v2
      putStrLn "Validating result..."
      validate ref_result result
  where
    saxpy_interp a arr1 arr2 = Interp.run (saxpy a arr1 arr2)

main :: IO ()
main
  = do
      putStrLn "Data.Array.Accelerate: simple examples"
      putStrLn "--------------------------------------"
      
      test_saxpy 10
