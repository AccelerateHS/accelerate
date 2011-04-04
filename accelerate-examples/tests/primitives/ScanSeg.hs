
module ScanSeg where

import Random

import System.IO
import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc
import Prelude               as P


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
list_prescanlSeg f x xs seg = concatMap (init . P.scanl f x) (split seg xs)
  where
    split [] _      = []
    split _  []     = []
    split (i:is) vs =
      let (h,t) = splitAt i vs
      in  h : split is t


-- Main
-- ----
run :: String -> Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
run alg m = withSystemRandom $ \gen -> do
  let n = P.round . sqrt $ (P.fromIntegral m :: Double)
  seg  <- randomUArrayR (0,n) gen n
  seg' <- convertUArray seg
  let ne = sum (elems seg)
  vec  <- randomUArrayR (-1,1) gen ne
  vec' <- convertUArray vec
  --
  let go f g = return (run_ref f vec seg, run_acc g vec' seg')
  case alg of
    "sum" -> go prefixSumSegRef prefixSumSegAcc
    x     -> error $ "unknown variant: " ++ x
  where
    {-# NOINLINE run_ref #-}
    run_ref f xs seg () = f xs seg
    run_acc f xs seg () = f xs seg

