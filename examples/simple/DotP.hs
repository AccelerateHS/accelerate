module DotP (dotp) where

import Prelude   hiding (replicate, zip, map, filter, max, min, not, zipWith)
import qualified Prelude

import Data.Array.Accelerate

dotp :: Vector Float -> Vector Float -> Acc (Scalar Float)
dotp xs ys 
  = let
      xs' = use xs
      ys' = use ys
    in
    fold (+) 0 (zipWith (*) xs' ys')
