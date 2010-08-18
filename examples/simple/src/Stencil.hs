{-# LANGUAGE ParallelListComp #-}

module Stencil (stencil_test, stencil_test_ref) where

import Prelude   hiding (replicate, zip, map, filter, max, min, not, zipWith)
import qualified Prelude

import Data.Array.Unboxed hiding (Array)
import Data.Array.IArray as IArray hiding (Array)

import Data.Array.Accelerate

stencil_test :: Array DIM2 Float -> Acc (Array DIM2 Float)
stencil_test = stencil stencil2D5 Clamp . use

stencil_test_ref :: UArray (Int, Int) Float 
                 -> UArray (Int, Int) Float
stencil_test_ref arr
  = array (bounds arr) [(xy, stencilFun xy) | xy <- indices arr]
  where
    stencilFun (x, y) = (arr IArray.! (clamp (x-1,y)) + 
                         arr IArray.! (clamp (x+1,y)) + 
                         arr IArray.! (clamp (x,y-1)) + 
                         arr IArray.! (clamp (x,y+1)) - 
                         4*arr IArray.! (clamp (x,y))) / 4
    clamp (x, y) = (minx `Prelude.max` x `Prelude.min` maxx, 
                    miny `Prelude.max` y `Prelude.min` maxy) 
      where
        ((minx, miny), (maxx, maxy)) = bounds arr

-- some example stencils

stencil1D :: (Elem a, IsFloating a) 
          => Stencil3 a -> Exp a
stencil1D (x, y, z) = (x + z - 2 * y) / 2

stencil2D5 :: (Elem a, IsFloating a) 
           => Stencil3x3 a -> Exp a
stencil2D5 ( (_, t, _)
           , (l, m, r)
           , (_, b, _)
           ) 
           = (t + l + r + b - 4 * m) / 4

stencil2D :: (Elem a, IsFloating a) 
          => Stencil3x3 a -> Exp a
stencil2D ( (t1, t2, t3)
          , (l , m,  r )
          , (b1, b2, b3)
          ) 
          = (t1/2 + t2 + t3/2 + l + r + b1/2 + b2 + b3/2 - 4 * m) / 4

stencil3D :: (Elem a, IsNum a)
          => Stencil3x3x3 a -> Exp a
stencil3D (front, back, _) =      -- 'b4' is the focal point
  let ((f1, f2, _),
       (f3, f4, _),
       _          ) = front
      ((b1, b2, _),
       (b3, b4, _),
       _          ) = back
  in
  f1 + f2 + f3 + f4 + b1 + b2 + b3 + b4

-- usaging them to ensure the types of the example fit the 'stencil' function

use1D :: Acc (Array DIM1 Float) -> Acc (Array DIM1 Float)
use1D arr = stencil stencil1D Clamp arr

use2D5 :: Acc (Array DIM2 Float) -> Acc (Array DIM2 Float)
use2D5 arr = stencil stencil2D5 Clamp arr

use2D :: Acc (Array DIM2 Float) -> Acc (Array DIM2 Float)
use2D arr = stencil stencil2D Clamp arr

use3D :: Acc (Array DIM3 Float) -> Acc (Array DIM3 Float)
use3D arr = stencil stencil3D Clamp arr
