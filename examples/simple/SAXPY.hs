module SAXPY where

import Prelude   hiding (replicate, zip, map, filter, max, min, not, zipWith, 
                         exp)
import qualified Prelude

import Data.Array.Accelerate

saxpy :: Float -> Vector Float -> Vector Float -> AP (Vec Float)
saxpy alpha xs ys 
  = do
      xs' <- use xs
      ys' <- use ys
      zipWith (\x y -> exp alpha * x * y) xs' ys'
