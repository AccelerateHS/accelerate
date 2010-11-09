{-# LANGUAGE ScopedTypeVariables #-}

import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.CUDA as CUDA


-- |The value of each element in an integral image is the sum of all input elements
-- above and to the left, inclusive. It is calculated by performing an inclusive/post
-- scan from left-to-right then top-to-bottom.
--
integralImage :: (Elem a, IsNum a) => Acc (Array DIM2 a) -> Acc (Array DIM2 a)
integralImage arr = sumTable
  where
    -- scan rows
    rowArr  = reshape (w * h) arr
    rowSegs = Acc.replicate h $ unit w
    rowSum  = reshape (tuple (w, h)) $ Acc.scanl1Seg (+) rowArr rowSegs

    -- scan cols
    colArr  = reshape (h * w) $ transpose2D rowSum
    colSegs = Acc.replicate w $ unit h
    colSum  = reshape (tuple (h, w)) $ Acc.scanl1Seg (+) colArr colSegs

    -- transpose back
    sumTable = transpose2D colSum

    --
    (w, h) = untuple $ shape arr :: (Exp Int, Exp Int)


-- |Simple 2D matrix transpose.
--
transpose2D :: (Elem a) => Acc (Array DIM2 a) -> Acc (Array DIM2 a)
transpose2D arr = backpermute (swap $ shape arr) swap arr
  where
    swap ix = let (x, y) :: (Exp Int, Exp Int) = untuple ix in tuple (y, x)


-- |An image is just a 2D array of 1s. The values don't really matter here but
-- all 1s makes it easier to validate the integral image result.
--
image :: Array DIM2 Int
image = Acc.fromList (20, 40) $ repeat 1


-- |Run integralImage through the Interpreter and CUDA backends.
--
main :: IO ()
main = do
  print image
  print $ CUDA.run $ integralImage $ use image

