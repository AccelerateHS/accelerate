{-# LANGUAGE ParallelListComp, PatternGuards #-}

module Main where

import Random
import Benchmark

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
main :: IO ()
main = do
  args <- getArgs'
  case args of
       []                       -> run 1000000
       [a] | [(n,_)] <- reads a -> run n
       _                        -> usage

run :: Int -> IO ()
run nelements = withSystemRandom $ \gen -> do
  v1    <- randomUArrayR (-1,1) gen nelements
  v2    <- randomUArrayR (-1,1) gen nelements
  v1'   <- convertUArray v1
  v2'   <- convertUArray v2
  alpha <- uniform gen
  --
  benchmark "acc-saxpy" (run_ref alpha v1 v2) (run_acc alpha v1' v2')
  where
    {-# NOINLINE run_ref #-}
    run_ref alpha xs ys () = saxpyRef alpha xs ys
    run_acc alpha xs ys () = saxpyAcc alpha xs ys


usage :: IO ()
usage = putStrLn $ unlines
  [ "acc-saxpy (c) [2008..2011] The Accelerate Team"
  , ""
  , "acc-saxpy [OPTIONS]"
  , ""
  , "Options:"
  , "  <N>  Number of elements (default 1000000)"
  ]

