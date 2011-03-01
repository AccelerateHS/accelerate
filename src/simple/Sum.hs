{-# LANGUAGE ParallelListComp, PatternGuards #-}

module Main where

import Random
import Benchmark

import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc


-- Reduce
-- ------
sumAcc :: Vector Float -> Acc (Scalar Float)
sumAcc xs
  = Acc.fold (+) 0 (Acc.use xs)

sumRef :: UArray Int Float -> UArray () Float
sumRef xs
  = listArray ((), ()) [Prelude.sum $ elems xs]


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
  vec  <- randomVectorR (-1,1) gen n
  vec' <- convertVector vec
  --
  benchmark "acc-sum" (run_ref vec) (run_acc vec')
  where
    {-# NOINLINE run_ref #-}
    run_ref xs () = sumRef xs
    run_acc xs () = sumAcc xs

usage :: IO ()
usage = putStrLn $ unlines
  [ "acc-sum (c) [2008..2011] The Accelerate Team"
  , ""
  , "acc-sum [OPTIONS]"
  , ""
  , "Options:"
  , "  <N>  Number of elements (default 1000000)"
  ]

