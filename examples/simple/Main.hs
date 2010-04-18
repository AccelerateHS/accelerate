{-# LANGUAGE FlexibleContexts, ParallelListComp #-}

module Main where

import Prelude hiding (filter)

import Control.Exception
import Data.Array.Unboxed
import Data.Array.IArray
import System.Random

import qualified Data.Array.Accelerate as Acc
import qualified Data.Array.Accelerate.Interpreter as Interp
import qualified Data.Array.Accelerate.CUDA as CUDA

import System.Time
import SAXPY
import Square
import DotP
import Filter
import Sum


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
                           | otherwise             = putStrLn "INVALID!" >> diff arr_ref arr
  where
    similar arr1 arr2 = all (< epsilon) [abs ((x - y) / x) | x <- elems arr1 
                                                           | y <- elems arr2]
    diff arr1 arr2 = sequence_ [if abs ((x - y) / x) < epsilon
                       then return ()
                       else putStrLn $ ">>> " ++ "(" ++ show x ++ ", " ++ show y ++ ")"
                     | x <- elems arr1 | y <- elems arr2]
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

timeScalar' :: (IArray UArray e, Acc.Elem e)
            => (() -> IO (Acc.Scalar e)) -> IO (UArray () e)
{-# NOINLINE timeScalar' #-}
timeScalar' testee 
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
                      r <- testee ()
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
--  where
{-# NOINLINE oneRun #-}
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


timeVector' :: (IArray UArray e, Acc.Elem e)
            => (() -> IO (Acc.Vector e)) -> IO (UArray Int e)
{-# NOINLINE timeVector' #-}
timeVector' testee 
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
                      r <- testee ()
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
      ref_result <- timeUVector $ saxpy_ref' 1.5 v1_ref v2_ref
      putStrLn "[Interpreter]"
      putStrLn "Running Accelerate code..."
      result <- timeVector $ saxpy_interp 1.5 v1 v2
      putStrLn "Validating result..."
      validateFloats ref_result result
      putStrLn "[CUDA]"
      putStrLn "Running Accelerate code..."
      result_cuda <- timeVector' $ saxpy_cuda 1.5 v1 v2
      putStrLn "Validating result..."
      validateFloats ref_result result_cuda
  where
    -- idiom with NOINLINE and extra parameter needed to prevent optimisations
    -- from sharing results over multiple runs
    {-# NOINLINE saxpy_ref' #-}
    saxpy_ref' a arr1 arr2 () = saxpy_ref a arr1 arr2
    {-# NOINLINE saxpy_interp #-}
    saxpy_interp a arr1 arr2 () = Interp.run (saxpy a arr1 arr2)
    {-# NOINLINE saxpy_cuda #-}
    saxpy_cuda a arr1 arr2 () = CUDA.run (saxpy a arr1 arr2)

test_square :: Int -> IO ()
test_square n
  = do
      putStrLn "== Square"
      putStrLn $ "Generating data (n = " ++ show n ++ ")..."
      v1_ref <- randomUVector n
      v1     <- convertUVector v1_ref
      putStrLn "Running reference code..."
      ref_result <- timeUVector $ square_ref' v1_ref
      putStrLn "[Interpreter]"
      putStrLn "Running Accelerate code..."
      result <- timeVector $ square_interp v1
      putStrLn "Validating result..."
      validateFloats ref_result result
      putStrLn "[CUDA]"
      putStrLn "Running Accelerate code..."
      result_cuda <- timeVector' $ square_cuda v1
      putStrLn "Validating result..."
      validateFloats ref_result result_cuda
  where
    -- idiom with NOINLINE and extra parameter needed to prevent optimisations
    -- from sharing results over multiple runs
    {-# NOINLINE square_ref' #-}
    square_ref' arr1 () = square_ref arr1
    {-# NOINLINE square_interp #-}
    square_interp arr1 () = Interp.run (square arr1)
    {-# NOINLINE square_cuda #-}
    square_cuda arr1 () = CUDA.run (square arr1)

test_sum :: Int -> IO ()
test_sum n
  = do
      putStrLn "== Sum"
      putStrLn $ "Generating data (n = " ++ show n ++ ")..."
      v1_ref <- randomUVector n
      v1     <- convertUVector v1_ref
      putStrLn "Running reference code..."
      ref_result <- timeUScalar $ sum_ref' v1_ref
      putStrLn "[Interpreter]"
      putStrLn "Running Accelerate code..."
      result <- timeScalar $ sum_interp v1
      putStrLn "Validating result..."
      validateFloats ref_result result
      putStrLn "[CUDA]"
      putStrLn "Running Accelerate code..."
      result_cuda <- timeScalar' $ sum_cuda v1
      putStrLn "Validating result..."
      validateFloats ref_result result_cuda
  where
    -- idiom with NOINLINE and extra parameter needed to prevent optimisations
    -- from sharing results over multiple runs
    {-# NOINLINE sum_ref' #-}
    sum_ref' arr1 () = sum_ref arr1
    {-# NOINLINE sum_interp #-}
    sum_interp arr1 () = Interp.run (Sum.sum arr1)
    {-# NOINLINE sum_cuda #-}
    sum_cuda arr1 () = CUDA.run (Sum.sum arr1)

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
      ref_result <- timeUScalar $ dotp_ref' v1_ref v2_ref
      putStrLn "Running Accelerate code..."
      result <- timeScalar $ dotp_interp v1 v2
      putStrLn "Validating result..."
      validateFloats ref_result result
  where
    -- idiom with NOINLINE and extra parameter needed to prevent optimisations
    -- from sharing results over multiple runs
    {-# NOINLINE dotp_ref' #-}
    dotp_ref' arr1 arr2 () = dotp_ref arr1 arr2
    {-# NOINLINE dotp_interp #-}
    dotp_interp arr1 arr2 () = Interp.run (dotp arr1 arr2)

test_filter :: Int -> IO ()
test_filter n
  = do
      putStrLn "== Filter"
      putStrLn $ "Generating data (n = " ++ show n ++ ")..."
      v_ref <- randomUVector n
      v     <- convertUVector v_ref
      putStrLn "Running reference code..."
      ref_result <- timeUVector $ filter_ref' (< 0) v_ref
      putStrLn "Running Accelerate code..."
      result <- timeVector $ filter_interp (Acc.<* Acc.constant 0) (Acc.use v)
      putStrLn "Validating result..."
      validateFloats ref_result result
  where
    -- idiom with NOINLINE and extra parameter needed to prevent optimisations
    -- from sharing results over multiple runs
    {-# NOINLINE filter_ref' #-}
    filter_ref' p arr () = filter_ref p arr
    {-# NOINLINE filter_interp #-}
    filter_interp p arr () = Interp.run (filter p arr)

main :: IO ()
main
  = do
      putStrLn "Data.Array.Accelerate: simple examples"
      putStrLn "--------------------------------------"
      
      test_saxpy 100000
      test_dotp  100000
      test_filter 2000
