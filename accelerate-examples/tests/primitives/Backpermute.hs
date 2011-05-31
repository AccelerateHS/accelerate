{-# LANGUAGE TypeOperators #-}

module Backpermute where

import Random

import Control.Monad
import Control.Exception
import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc
import Prelude               as P


-- Tests
-- -----

reverseAcc :: Vector Float -> Acc (Vector Float)
reverseAcc xs =
  let xs' = use xs
      len = unindex1 (shape xs')
  in
  backpermute (shape xs') (\ix -> index1 $ len - (unindex1 ix) - 1) xs'

reverseRef :: UArray Int Float -> UArray Int Float
reverseRef xs = listArray (bounds xs) (reverse (elems xs))


transposeAcc :: Acc.Array DIM2 Float -> Acc (Acc.Array DIM2 Float)
transposeAcc mat =
  let mat' = use mat
      swap = lift1 $ \(Z:.x:.y) -> Z:.y:.x :: Z:.Exp Int:.Exp Int
  in
  backpermute (swap $ shape mat') swap mat'

transposeRef :: UArray (Int,Int) Float -> UArray (Int,Int) Float
transposeRef mat =
  let swap (x,y) = (y,x)
      (u,v)      = bounds mat
  in
  array (swap u, swap v) [(swap ix, e) | (ix, e) <- assocs mat]


-- Main
-- ----

run :: String -> Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
run alg n = withSystemRandom $ \gen -> do
  vec  <- randomUArrayR (-1,1) gen n
  vec' <- convertUArray vec
  --
  let go f g = return (\() -> f vec, \() -> g vec')
  case alg of
    "reverse" -> go reverseRef reverseAcc
    _         -> error $ "unknown variant: " ++ alg

run2d :: String -> Int -> IO (() -> UArray (Int,Int) Float, () -> Acc (Acc.Array DIM2 Float))
run2d alg n = withSystemRandom $ \gen -> do
  let n'    = P.round $ sqrt (P.fromIntegral n :: Double)
      (u,v) = (n'*2, n'`div`2)
  mat  <- listArray ((0,0), (u-1,v-1)) `fmap` replicateM (u*v) (uniformR (-1,1) gen)
  mat' <- let m = fromIArray mat :: Acc.Array DIM2 Float
          in  evaluate (m `Acc.indexArray` (Z:.0:.0)) >> return m
  --
  let go f g = return (\() -> f mat, \() -> g mat')
  case alg of
    "transpose" -> go transposeRef transposeAcc
    _           -> error $ "unknown variant: " ++ alg

