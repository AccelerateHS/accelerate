{-# LANGUAGE PatternGuards #-}

module Main where

import Random
import Benchmark

import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc


-- Sum of absolute values
-- ----------------------
sasumAcc :: Vector Float -> Acc (Scalar Float)
sasumAcc xs
  = Acc.fold (+) 0 . Acc.map abs $ Acc.use xs

sasumRef :: UArray Int Float -> UArray () Float
sasumRef xs
  = listArray ((), ()) [Prelude.sum . Prelude.map abs $ elems xs]


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
  vec  <- randomUArrayR (-1,1) gen n
  vec' <- convertUArray vec
  --
  benchmark "acc-sasum" (run_ref vec) (run_acc vec')
  where
    {-# NOINLINE run_ref #-}
    run_ref xs () = sasumRef xs
    run_acc xs () = sasumAcc xs

usage :: IO ()
usage = putStrLn $ unlines
  [ "acc-sasum (c) [2008..2011] The Accelerate Team"
  , ""
  , "acc-sasum [OPTIONS]"
  , ""
  , "Options:"
  , "  N        Number of elements (default 1000000)"
  ]

