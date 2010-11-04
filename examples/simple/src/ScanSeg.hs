
module ScanSeg (prefixSumSeg, prefixSumSeg_ref) where

import Data.Array.Unboxed
import Data.Array.Accelerate

prefixSumSeg :: Vector Float -> Segments -> Acc (Vector Float)
prefixSumSeg xs seg
  = let
      xs'  = use xs
      seg' = use seg
    in
    prescanlSeg (+) (constant 0.0) xs' seg'


prefixSumSeg_ref :: UArray Int Float -> UArray Int Int -> UArray Int Float
prefixSumSeg_ref xs seg
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

