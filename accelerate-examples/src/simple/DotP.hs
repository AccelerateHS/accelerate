{-# LANGUAGE ParallelListComp, PatternGuards #-}

module Main where

import Random
import Benchmark

import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc


-- Dot product
-- -----------
dotpAcc :: Vector Float -> Vector Float -> Acc (Scalar Float)
dotpAcc xs ys
  = let
      xs' = use xs
      ys' = use ys
    in
    Acc.fold (+) 0 (Acc.zipWith (*) xs' ys')

dotpRef :: UArray Int Float
        -> UArray Int Float
        -> UArray ()  Float
dotpRef xs ys
  = listArray ((), ()) [sum [x * y | x <- elems xs | y <- elems ys]]


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
run n = withSystemRandom $ \gen -> do
  v1  <- randomUArrayR (-1,1) gen n
  v2  <- randomUArrayR (-1,1) gen n
  v1' <- convertUArray v1
  v2' <- convertUArray v2
  --
  benchmark "acc-dotp" (run_ref v1 v2) (run_acc v1' v2')
  where
    {-# NOINLINE run_ref #-}
    run_ref xs ys () = dotpRef xs ys
    run_acc xs ys () = dotpAcc xs ys


usage :: IO ()
usage = putStrLn $ unlines
  [ "acc-dotp (c) [2008..2011] The Accelerate Team"
  , ""
  , "acc-dotp [OPTIONS]"
  , ""
  , "Options:"
  , "  N        Number of elements (default 1000000)"
  ]

