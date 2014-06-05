{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module IntegralImage where

import Data.Array.Accelerate    as A
import Data.Array.Accelerate.IO as A


-- |The value of each element in an integral image is the sum of all input elements
-- above and to the left, inclusive. It is calculated by performing an inclusive/post
-- scan from left-to-right then top-to-bottom.
--
integralImage :: (Elt a, IsFloating a) => Array DIM2 Word32 -> Acc (Array DIM2 a)
integralImage img = sumTable
  where
    -- scan rows
    rowArr  = reshape (index1 (w * h))  $ arr
    rowSegs = A.replicate (index1 h)    $ unit w
    rowSum  = reshape (index2 w h)      $ scanl1Seg (+) rowArr rowSegs

    -- scan cols
    colArr  = reshape (index1 (h * w))  $ transpose rowSum
    colSegs = A.replicate (index1 w)    $ unit h
    colSum  = reshape (index2 h w)      $ A.scanl1Seg (+) colArr colSegs

    -- transpose back
    sumTable = transpose colSum

    --
    arr     = A.map luminanceOfRGBA32 (use img)
    Z:.w:.h = unlift $ shape arr


-- Run integralImage over the input PGM
--
run :: FilePath -> IO (() -> Acc (Array DIM2 Float))
run file = do
  bmp <- either (error . show) id `fmap` readImageFromBMP file
  return (\() -> integralImage bmp)

