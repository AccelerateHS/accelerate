{-# LANGUAGE ParallelListComp #-}

module ZipWith where

import Random

import System.Random.MWC
import Data.Array.Unboxed
import Data.Array.Accelerate as Acc hiding (min)


-- Tests
-- -----

plusAcc :: Vector Float -> Vector Float -> Acc (Vector Float)
plusAcc xs ys = Acc.zipWith (+) (use xs) (use ys)

plusRef :: UArray Int Float -> UArray Int Float -> UArray Int Float
plusRef = zipWithRef (+)

zipWithRef :: (IArray array a, IArray array b, IArray array c)
           => (a -> b -> c) -> array Int a -> array Int b -> array Int c
zipWithRef f xs ys =
  let mn      = bounds xs
      uv      = bounds ys
      newSize = (0, (rangeSize mn `min` rangeSize uv) - 1)
  in
  listArray newSize [f x y | x <- elems xs | y <- elems ys]

-- Main
-- ----
run :: String -> Int -> IO (() -> UArray Int Float, () -> Acc (Vector Float))
run alg n = withSystemRandom $ \gen -> do
  xs  <- randomUArrayR (-1,1) gen n
  ys  <- randomUArrayR (-1,1) gen n
  xs' <- convertUArray xs
  ys' <- convertUArray ys
  let go f g = return (\() -> f xs ys, \() -> g xs' ys')
  case alg of
    "plus" -> go plusRef plusAcc
    _      -> error $ "unknown variant: " ++ alg

