
module Main where

import Random
import Benchmark

import System.IO
import System.Exit
import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate

-- Segmented prefix-sum
-- --------------------
prefixSumSegAcc :: Vector Float -> Segments -> Acc (Vector Float)
prefixSumSegAcc xs seg
  = let
      xs'  = use xs
      seg' = use seg
    in
    prescanlSeg (+) 0 xs' seg'


prefixSumSegRef :: UArray Int Float -> UArray Int Int -> UArray Int Float
prefixSumSegRef xs seg
  = listArray (bounds xs)
  $ list_prescanlSeg (+) 0 (elems xs) (elems seg)

list_prescanlSeg :: (a -> a -> a) -> a -> [a] -> [Int] -> [a]
list_prescanlSeg f x xs seg = concatMap (init . Prelude.scanl f x) (split seg xs)
  where
    split [] _      = []
    split _  []     = []
    split (i:is) vs =
      let (h,t) = splitAt i vs
      in  h : split is t


-- Main
-- ----
main :: IO ()
main = do
  args <- getArgs'
  case args of
       [alg]                          -> run alg 1000 1000
       [alg,a,b] | [(n,_)] <- reads a
                 , [(m,_)] <- reads b -> run alg n m
       _                              -> usage

run :: String -> Int -> Int -> IO ()
run alg r n = withSystemRandom $ \gen -> do
  seg  <- randomUArrayR (0,r) gen n
  seg' <- convertUArray seg
  let ne = sum (elems seg)
  vec  <- randomUArrayR (-1,1) gen ne
  vec' <- convertUArray vec
  --
  let go f g = benchmark ("acc-scanseg-" ++ alg) (run_ref f vec seg) (run_acc g vec' seg')
  case alg of
       "sum"    -> go prefixSumSegRef prefixSumSegAcc
       _        -> usage
  where
    {-# NOINLINE run_ref #-}
    run_ref f xs seg () = f xs seg
    run_acc f xs seg () = f xs seg


usage :: IO ()
usage = hPutStrLn stderr help >> exitFailure
  where
    help = unlines
      [ "acc-scanseg (c) [2008..2011] The Accelerate Team"
      , ""
      , "acc-scanseg ALGORITHM [N]"
      , ""
      , "Algorithms:"
      , "  sum      segmented reduction"
      , ""
      , "Options:"
      , "  N M      Maximum segment length, number of segments"
      ]

