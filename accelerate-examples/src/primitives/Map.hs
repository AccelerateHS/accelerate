{-# LANGUAGE FlexibleContexts, PatternGuards #-}

module Main where

import Random
import Benchmark

import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc


-- Tests
-- -----
sqAcc, absAcc :: Vector Float -> Acc (Vector Float)
absAcc = Acc.map abs . Acc.use
sqAcc  = Acc.map (\x -> x * x) . Acc.use

plusAcc :: Exp Float -> Vector Float -> Acc (Vector Float)
plusAcc alpha = Acc.map (+ alpha) . Acc.use


toUA :: (IArray UArray a, IArray UArray b) => ([a] -> [b]) -> UArray Int a -> UArray Int b
toUA f xs = listArray (bounds xs) $ f (elems xs)

sqRef, absRef :: UArray Int Float -> UArray Int Float
absRef = toUA $ Prelude.map abs
sqRef  = toUA $ Prelude.map (\x -> x*x)

plusRef :: Float -> UArray Int Float -> UArray Int Float
plusRef alpha = toUA $ Prelude.map (+alpha)


-- Main
-- ----
main :: IO ()
main = do
  args <- getArgs'
  case args of
       [alg]                        -> run alg 1000000
       [alg,a] | [(n,_)] <- reads a -> run alg n
       _                            -> usage

run :: String -> Int -> IO ()
run alg n = withSystemRandom $ \gen -> do
  vec   <- randomUArrayR (-1,1) gen n
  vec'  <- convertUArray vec
  alpha <- uniform gen
  --
  let go f g = benchmark ("acc-" ++ alg) (run_ref f vec) (run_acc g vec')

  case alg of
       "abs"    -> go absRef absAcc
       "plus"   -> go (plusRef alpha) (plusAcc $ constant alpha)
       "square" -> go sqRef sqAcc
       _        -> usage

  where
    {-# NOINLINE run_ref #-}
    run_ref f xs () = f xs
    run_acc f xs () = f xs


usage :: IO ()
usage = putStrLn $ unlines
  [ "acc-map (c) [2008..2011] The Accelerate Team"
  , ""
  , "acc-map ALGORITHM [N]"
  , ""
  , "Algorithms:"
  , "  abs      absolute value of values"
  , "  plus     add a constant to each element"
  , "  square   square of each element"
  , ""
  , "Options:"
  , "  N        Number of elements (default 1000000)"
  ]

