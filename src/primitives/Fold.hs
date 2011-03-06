{-# LANGUAGE FlexibleContexts, PatternGuards, TypeOperators #-}

module Main where

import Random
import Benchmark

import Control.Monad
import Control.Exception
import System.Random.MWC
import Data.Array.Unboxed    hiding (Array)
import Data.Array.Accelerate as Acc
import Prelude		     as P


-- one-dimension ah-ha-ha
-- ----------------------

toUA :: (IArray UArray a, IArray UArray b) => ([a] -> b) -> UArray Int a -> UArray () b
toUA f = listArray ((),()) . return . f . elems


sumAcc, prodAcc, maxAcc, minAcc :: Shape ix => Array (ix:.Int) Float -> Acc (Array ix Float)
sumAcc  = Acc.fold (+) 0 . Acc.use
prodAcc = Acc.fold (*) 1 . Acc.use
maxAcc  = Acc.fold1 Acc.max . Acc.use
minAcc  = Acc.fold1 Acc.min . Acc.use

sumRef, prodRef, maxRef, minRef :: UArray Int Float -> UArray () Float
sumRef  = toUA sum
prodRef = toUA product
maxRef  = toUA maximum
minRef  = toUA minimum


-- two-dimensions ah-ha-ha
-- -----------------------

foldU2D :: IArray UArray a => (a -> a -> a) -> a -> UArray (Int,Int) a -> UArray Int a
foldU2D f z arr =
  let (_,(m,_)) = bounds arr
  in  accumArray f z (0,m) [ (i,e) | ((i,_),e) <- assocs arr ]

sum2DRef, prod2DRef :: UArray (Int,Int) Float -> UArray Int Float
sum2DRef  = foldU2D (+) 0
prod2DRef = foldU2D (*) 1


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
  vec  <- randomUArrayR (-1,1) gen n
  vec' <- convertUArray vec
  --
  let u = P.floor . sqrt $ (P.fromIntegral n :: Double)
      v = 2*u+1 :: Int
  mat  <- listArray ((0,0), (u-1,v-1)) `fmap` replicateM (u*v) (uniformR (-1,1) gen)
  mat' <- let m = fromIArray mat :: Array DIM2 Float
          in  evaluate (m `Acc.indexArray` (Z:.0:.0)) >> return m
  --
  let go  f g = benchmark ("acc-" ++ alg) (run_ref f vec) (run_acc g vec')
      go2 f g = benchmark ("acc-" ++ alg) (run_ref f mat) (run_acc g mat')

  case alg of
    "sum"        -> go sumRef sumAcc
    "product"    -> go prodRef prodAcc
    "maximum"    -> go maxRef maxAcc
    "minimum"    -> go minRef minAcc
    "sum-2d"     -> go2 sum2DRef sumAcc
    "product-2d" -> go2 prod2DRef prodAcc
    _            -> usage
  where
    {-# NOINLINE run_ref #-}
    run_ref f xs () = f xs
    run_acc f xs () = f xs


usage :: IO ()
usage = putStrLn $ unlines
  [ "acc-fold (c) [2008..2011] The Accelerate Team"
  , ""
  , "acc-fold ALGORITHM [N]"
  , ""
  , "Algorithms:"
  , "  sum          vector reduction        fold (+) 0"
  , "  product      vector product          fold (*) 1"
  , "  maximum      maximum of a vector     fold1 max"
  , "  minimum      minimum of a vector     fold1 min"
  , "  sum-2d       reduction along innermost matrix dimension"
  , "  product-2d   product along innermost matrix dimension"
  , ""
  , "Options:"
  , "  N            Number of elements (default 1000000)"
  ]

