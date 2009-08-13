module SAXPY (saxpy) where

import Prelude   hiding (replicate, zip, map, filter, max, min, not, zipWith)
import qualified Prelude

import Data.Array.Accelerate

saxpy :: Float -> Vector Float -> Vector Float -> Acc (Vector Float)
saxpy alpha xs ys
  = let
      xs' = use xs
      ys' = use ys
    in 
    zipWith (\x y -> constant alpha * x * y) xs' ys'
