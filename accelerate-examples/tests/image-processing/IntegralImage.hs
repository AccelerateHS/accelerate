{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module IntegralImage where

import PGM

import Data.Array.Accelerate as Acc


-- |The value of each element in an integral image is the sum of all input elements
-- above and to the left, inclusive. It is calculated by performing an inclusive/post
-- scan from left-to-right then top-to-bottom.
--
integralImage :: (Elt a, IsNum a) => Array DIM2 a -> Acc (Array DIM2 a)
integralImage img = sumTable
  where
    -- scan rows
    rowArr  = reshape (lift $ Z:.(w * h)) arr
    rowSegs = Acc.replicate (lift $ Z:.h) $ unit w
    rowSum  = reshape (lift (Z:.w:.h)) $ Acc.scanl1Seg (+) rowArr rowSegs

    -- scan cols
    colArr  = reshape (lift $ Z:.(h * w)) $ transpose2D rowSum
    colSegs = Acc.replicate (lift $ Z:.w) $ unit h
    colSum  = reshape (lift (Z:.h:.w)) $ Acc.scanl1Seg (+) colArr colSegs

    -- transpose back
    sumTable = transpose2D colSum

    --
    arr     = use img
    Z:.w:.h = unlift $ shape arr


-- |Simple 2D matrix transpose.
--
transpose2D :: Elt a => Acc (Array DIM2 a) -> Acc (Array DIM2 a)
transpose2D arr = backpermute (swap $ shape arr) swap arr
  where
    swap = lift1 $ \(Z:.x:.y) -> Z:.y:.x :: Z :. Exp Int :. Exp Int


-- Run integralImage over the input PGM
--
run :: FilePath -> IO (() -> Acc (Array DIM2 Float))
run file = do
  pgm <- readPGM file
  return (\() -> integralImage pgm)

