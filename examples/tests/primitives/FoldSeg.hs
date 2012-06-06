
module FoldSeg where

import Random

import Data.List
import System.IO
import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate    as A
import Prelude                  as P


-- segmented reduction
-- -------------------

sumSegAcc :: Vector Float -> Segments Int32 -> Acc (Vector Float)
sumSegAcc xs seg
  = let xs'     = use xs
        seg'    = use seg
    in
    A.foldSeg (+) 0 xs' seg'

sumSegRef :: UArray Int Float -> UArray Int Int32 -> UArray Int Float
sumSegRef xs seg
  = listArray (bounds seg)
  $ list_foldSeg (+) 0 (elems xs) (elems seg)

list_foldSeg :: (a -> a -> a) -> a -> [a] -> [Int32] -> [a]
list_foldSeg f s xs seg = P.map (foldl' f s) (split seg xs)
  where
    split []     _      = []
    split _      []     = []
    split (i:is) vs     =
      let (h,t) = splitAt (P.fromIntegral i) vs
      in  h : split is t


-- main
-- ----

run :: String -> Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
run alg m = withSystemRandom $ \gen -> do
  -- generate segments
  --
  let n  = P.round $ sqrt (P.fromIntegral m :: Double)
  seg   <- randomUArrayR (0, 2*n) gen (P.fromIntegral n)
  seg'  <- convertUArray seg

  -- generate elements
  --
  let x  = P.fromIntegral $ P.sum (elems seg)
  vec   <- randomUArrayR (-1,1) gen x
  vec'  <- convertUArray vec

  -- super-happy-fun-times
  --
  let go f g    = return (\() -> f vec seg, \() -> g vec' seg')
  case alg of
    "sum"       -> go sumSegRef sumSegAcc
    unknown     -> error $ "unknown variant: " ++ unknown

