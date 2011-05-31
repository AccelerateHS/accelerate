
module Zip where

import Random

import System.Random.MWC
import Data.Array.Unboxed       as IArray
import Data.Array.Accelerate    as Acc hiding (min)


-- Tests
-- -----

zipAcc :: Vector Float -> Vector Int -> Acc (Vector (Float,Int))
zipAcc xs ys = Acc.zip (use xs) (use ys)


zipRef :: UArray Int Float -> UArray Int Int -> IArray.Array Int (Float,Int)
zipRef xs ys =
  let mn      = bounds xs
      uv      = bounds ys
      newSize = (0, (rangeSize mn `min` rangeSize uv) - 1)
  in
  listArray newSize $ Prelude.zip (elems xs) (elems ys)

-- Main
-- ----
run :: Int -> IO (() -> IArray.Array Int (Float,Int), () -> Acc (Vector (Float,Int)))
run n = withSystemRandom $ \gen -> do
  xs  <- randomUArrayR (-1,1) gen n
  ys  <- randomUArrayR (-1,1) gen n
  xs' <- convertUArray xs
  ys' <- convertUArray ys
  return $ (\() -> zipRef xs ys, \() -> zipAcc xs' ys')

