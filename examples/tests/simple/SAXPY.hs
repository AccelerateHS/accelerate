{-# LANGUAGE ParallelListComp #-}

module SAXPY where

import Random

import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc

-- SAXPY
-- -----
saxpyAcc :: Float -> Vector Float -> Vector Float -> Acc (Vector Float)
saxpyAcc alpha xs ys
  = let
      xs' = use xs
      ys' = use ys
    in
    Acc.zipWith (\x y -> constant alpha * x + y) xs' ys'

saxpyRef :: Float -> UArray Int Float -> UArray Int Float -> UArray Int Float
saxpyRef alpha xs ys
  = listArray (bounds xs) [alpha * x + y | x <- elems xs | y <- elems ys]


-- Main
-- ----

run :: Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
run nelements = withSystemRandom $ \gen -> do
  v1    <- randomUArrayR (-1,1) gen nelements
  v2    <- randomUArrayR (-1,1) gen nelements
  v1'   <- convertUArray v1
  v2'   <- convertUArray v2
  alpha <- uniform gen
  --
  return (run_ref alpha v1 v2, run_acc alpha v1' v2')
  where
    {-# NOINLINE run_ref #-}
    run_ref alpha xs ys () = saxpyRef alpha xs ys
    run_acc alpha xs ys () = saxpyAcc alpha xs ys

