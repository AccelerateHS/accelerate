{-# LANGUAGE FlexibleContexts, ParallelListComp #-}

module Main where

import Control.Exception
import Data.Array.Unboxed
import Data.Array.IArray
import System.Random

import qualified Data.Array.Accelerate as Acc
import qualified Data.Array.Accelerate.Interpreter as Interp

import Time
import SAXPY
import DotP


-- Auxilliary array functions
-- --------------------------

-- To ensure that a singleton unboxed array is fully evaluated
-- 
evaluateUScalar :: (IArray UArray e) => UArray () e -> IO ()
evaluateUScalar uarr = evaluate (uarr!()) >> return ()

-- To ensure that a singleton unboxed array is fully evaluated
-- 
evaluateScalar :: Acc.Scalar e -> IO ()
evaluateScalar arr = evaluate (arr `Acc.indexArray` ()) >> return ()

-- To ensure that an unboxed array is fully evaluated, just force one element
-- 
evaluateUVector :: (IArray UArray e) => UArray Int e -> IO ()
evaluateUVector uarr = evaluate (uarr!0) >> return ()

-- To ensure that an unboxed array is fully evaluated, just force one element
-- 
evaluateVector :: Acc.Vector e -> IO ()
evaluateVector arr = evaluate (arr `Acc.indexArray` 0) >> return ()

randomUVector :: (Num e, Random e, IArray UArray e) => Int -> IO (UArray Int e)
randomUVector n
  = do
      rg <- newStdGen
      let -- The std random function is too slow to generate really big vectors
          -- with.  Instead, we generate a short random vector and repeat that.
          randvec = take k (randomRs (-100, 100) rg)
          vec     = listArray (0, n - 1) 
                              [randvec !! (i `mod` k) | i <- [0..n - 1]]
      evaluateUVector vec
      return vec
  where
    k = 1000

convertUScalar :: (IArray UArray e, Acc.Elem e) 
               => UArray () e -> IO (Acc.Scalar e)
convertUScalar uarr
  = do
      let arr = Acc.fromIArray uarr
      evaluateScalar arr
      return arr

convertUVector :: (IArray UArray e, Acc.Elem e) 
              => UArray Int e -> IO (Acc.Vector e)
convertUVector uarr
  = do
      let arr = Acc.fromIArray uarr
      evaluateVector arr
      return arr

validate :: (Eq e, IArray UArray e, Ix ix) 
         => UArray ix e -> UArray ix e -> IO ()
validate arr_ref arr | arr_ref == arr = putStrLn "Valid."
                     | otherwise      = putStrLn "INVALID!"

validateFloats :: Ix ix
               => UArray ix Float -> UArray ix Float -> IO ()
validateFloats arr_ref arr | arr_ref `similar` arr = putStrLn "Valid."
                           | otherwise             = putStrLn "INVALID!"
  where
    similar arr1 arr2 = all (< epsilon) [abs ((x - y) / x) | x <- elems arr1 
                                                           | y <- elems arr2]
    epsilon = 0.0001


-- Timing
-- ------

timeUScalar :: IArray UArray e => (() -> UArray () e) -> IO (UArray () e)
{-# NOINLINE timeUScalar #-}
timeUScalar testee 
  = do
      (r, time1) <- oneRun testee
      (r, time2) <- oneRun testee
      (r, time3) <- oneRun testee
      putStrLn $ showMinAvgMax milliseconds [time1, time2, time3] ++
                 " (wall - cpu min/avg/max in ms)"
      return r
  where
    oneRun testee = do
                      start <- getTime
                      let r = testee ()
                      evaluateUScalar r
                      end <- getTime
                      return (r, end `minus` start)

timeScalar :: (IArray UArray e, Acc.Elem e)
           => (() -> Acc.Scalar e) -> IO (UArray () e)
{-# NOINLINE timeScalar #-}
timeScalar testee 
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
                      evaluateScalar r
                      end <- getTime
                      return (r, end `minus` start)

timeUVector :: IArray UArray e => (() -> UArray Int e) -> IO (UArray Int e)
{-# NOINLINE timeUVector #-}
timeUVector testee 
  = do
      (r, time1) <- oneRun testee
      (r, time2) <- oneRun testee
      (r, time3) <- oneRun testee
      putStrLn $ showMinAvgMax milliseconds [time1, time2, time3] ++
                 " (wall - cpu min/avg/max in ms)"
      return r
  where
    oneRun testee = do
                      start <- getTime
                      let r = testee ()
                      evaluateUVector r
                      end <- getTime
                      return (r, end `minus` start)

timeVector :: (IArray UArray e, Acc.Elem e)
           => (() -> Acc.Vector e) -> IO (UArray Int e)
{-# NOINLINE timeVector #-}
timeVector testee 
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
                      evaluateVector r
                      end <- getTime
                      return (r, end `minus` start)


-- Tests
-- -----

test_saxpy :: Int -> IO ()
test_saxpy n
  = do
      putStrLn "== SAXPY"
      putStrLn $ "Generating data (n = " ++ show n ++ ")..."
      v1_ref <- randomUVector n
      v1     <- convertUVector v1_ref
      v2_ref <- randomUVector n
      v2     <- convertUVector v2_ref
      putStrLn "Running reference code..."
      ref_result <- timeUVector $ \_ -> saxpy_ref 1.5 v1_ref v2_ref
      putStrLn "Running Accelerate code..."
      result <- timeVector $ \_ -> saxpy_interp 1.5 v1 v2
      putStrLn "Validating result..."
      validateFloats ref_result result
  where
    saxpy_interp a arr1 arr2 = Interp.run (saxpy a arr1 arr2)

test_dotp :: Int -> IO ()
test_dotp n
  = do
      putStrLn "== Dot product"
      putStrLn $ "Generating data (n = " ++ show n ++ ")..."
      v1_ref <- randomUVector n
      v1     <- convertUVector v1_ref
      v2_ref <- randomUVector n
      v2     <- convertUVector v2_ref
      putStrLn "Running reference code..."
      ref_result <- timeUScalar $ \_ -> dotp_ref v1_ref v2_ref
      putStrLn "Running Accelerate code..."
      result <- timeScalar $ \_ -> dotp_interp v1 v2
      putStrLn "Validating result..."
      validateFloats ref_result result
  where
    dotp_interp arr1 arr2 = Interp.run (dotp arr1 arr2)

main :: IO ()
main
  = do
      putStrLn "Data.Array.Accelerate: simple examples"
      putStrLn "--------------------------------------"
      
      test_saxpy 100000
      test_dotp  100000
