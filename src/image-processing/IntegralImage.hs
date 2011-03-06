{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

import PGM
import Benchmark

import System.FilePath
import Data.Array.Accelerate                as Acc
import qualified Data.Array.Accelerate.CUDA as Acc


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


{--
-- |An image is just a 2D array of 1s. The values don't really matter here but
-- all 1s makes it easier to validate the integral image result.
--
image :: Array DIM2 Float
image = Acc.fromList (Z:.20:.40) $ repeat 1
--}

-- |Run integralImage through the CUDA backend
--
main :: IO ()
main = do
  args <- getArgs'
  case args of
       [f]   -> let (base,ext) = splitExtension f
                in  run f (base ++ "-out" <.> ext)
       [f,g] -> run f g
       _     -> usage

-- TLM: should compare to a pre-saved reference image, or the interpreter (if it
--      is not too slow)
run :: FilePath -> FilePath -> IO ()
run inf outf = writePGM outf . Acc.run . integralImage =<< readPGM inf

usage :: IO ()
usage = putStrLn $ unlines
  [ "acc-integralimage (c) [2008..2011] The Accelerate Team"
  , ""
  , "acc-integralimage IN [OUT]"
  , ""
  , "Options:"
  , "  IN       PGM image file to process"
  , "  OUT      output image filename (optional)"
  ]

