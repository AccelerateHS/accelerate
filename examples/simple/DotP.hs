module DotP where

import Prelude   hiding (replicate, zip, map, filter, max, min, not, zipWith)
import qualified Prelude

import Data.Array.Accelerate

dotp :: Vector Float -> Vector Float -> AP (Sca Float)
dotp xs ys 
  = do
      xs' <- use xs
      ys' <- use ys
      zipWith (*) xs' ys' >>= fold (+) 0
