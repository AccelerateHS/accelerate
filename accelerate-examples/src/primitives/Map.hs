{-# LANGUAGE ParallelListComp, PatternGuards #-}

module Main where

import Random
import Benchmark

import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc


-- Square
-- ------
squareAcc :: Vector Float -> Acc (Vector Float)
squareAcc xs
  = Acc.map (\x -> x * x) (Acc.use xs)

squareRef :: UArray Int Float -> UArray Int Float
squareRef xs
  = listArray (bounds xs) [x * x | x <- elems xs]


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
  benchmark "acc-square" (run_ref vec) (run_acc vec')
  where
    {-# NOINLINE run_ref #-}
    run_ref xs () = squareRef xs
    run_acc xs () = squareAcc xs


usage :: IO ()
usage = putStrLn $ unlines
  [ "acc-square (c) [2008..2011] The Accelerate Team"
  , ""
  , "acc-square [OPTIONS]"
  , ""
  , "Options:"
  , "  <N>  Number of elements (default 1000000)"
  ]

