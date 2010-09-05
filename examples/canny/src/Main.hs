
module Main where

import Image
import Canny
import Criterion.Main

main :: IO ()
main = do
  img <- readPGM "images/lena_bw.pgm"
  writePGM "images/out.pgm" (canny img)
  defaultMain [bench "canny" (whnf canny img)]

