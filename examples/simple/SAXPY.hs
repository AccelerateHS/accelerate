module SAXPY where

import Prelude   hiding (replicate, zip, map, filter, max, min, not, zipWith)
import qualified Prelude

import Data.Array.Accelerate

saxpy :: Float -> Array DIM1 Float -> Array DIM1 Float -> AP (Arr DIM1 Float)
saxpy alpha xs ys 
  = do
      xs' <- use xs
      ys' <- use ys
      zipWith (\x y -> mkVal alpha * x * y) xs' ys'
