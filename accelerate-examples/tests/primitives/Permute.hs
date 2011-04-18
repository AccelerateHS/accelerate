module Permute where

import Random

import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc
import Prelude               as P


-- Tests
-- -----

histogramAcc :: (Int,Int) -> Vector Float -> Acc (Vector Int)
histogramAcc (m,n) vec =
  let vec'  = use vec
      zeros = generate (constant (Z:. n-m)) (const 0)
      ones  = generate (shape vec') (const 1)
  in
  permute (+) zeros (\ix -> index1 $ Acc.floor (vec' Acc.! ix)) ones

histogramRef :: (Int,Int) -> UArray Int Float -> UArray Int Int
histogramRef (m,n) vec =
  accumArray (+) 0 (0,n-m-1) [(P.floor e, 1) | e <- elems vec]


-- Main
-- ----
run :: String -> Int -> IO (() -> UArray Int Int, () -> Acc (Vector Int))
run alg n = withSystemRandom $ \gen -> do
  vec  <- randomUArrayR (0,100::Float) gen n
  vec' <- convertUArray vec
  --
  let go f g = return (\() -> f vec, \() -> g vec')
  case alg of
    "histogram" -> go (histogramRef (0,100)) (histogramAcc (0,100))
    _           -> error $ "unknown variant: " ++ alg


