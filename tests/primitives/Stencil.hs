{-# LANGUAGE FlexibleContexts #-}

module Stencil where

import System.IO
import Control.Monad
import Control.Exception
import System.Random.MWC

import Prelude   hiding (replicate, zip, map, filter, max, min, not, zipWith)
import qualified Prelude

import Data.Array.Unboxed hiding (Array)
import Data.Array.IArray as IArray hiding (Array)

import Data.Array.Accelerate


-- Stencil
-- -------
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

stencil1D :: Floating a
          => (a, a, a) -> a
stencil1D (x, y, z) = (x + z - 2 * y) / 2

stencil2D5 :: Floating (Exp a)
           => Stencil3x3 a -> Exp a
stencil2D5 ( (_, t, _)
           , (l, m, r)
           , (_, b, _)
           )
           = (t + l + r + b - 4 * m) / 4

stencil2D :: Floating (Exp a)
          => Stencil3x3 a -> Exp a
stencil2D ( (t1, t2, t3)
          , (l , m,  r )
          , (b1, b2, b3)
          )
          = (t1/2 + t2 + t3/2 + l + r + b1/2 + b2 + b3/2 - 4 * m) / 4

stencil3D :: Num (Exp a)
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


-- Main
-- ----
run :: String -> Int -> IO (() -> UArray (Int,Int) Float, () -> Acc (Array DIM2 Float))
run alg v = withSystemRandom $ \gen -> do
  let n = Prelude.round . sqrt $ (Prelude.fromIntegral v :: Double)
  mat  <- listArray ((0,0),(n-1,n-1)) `fmap` replicateM (n*n) (uniformR (-1,1) gen)
  mat' <- let m = fromIArray mat :: Array DIM2 Float
          in  evaluate (m `indexArray` (Z:.0:.0)) >> return m
  --
  let go f g = return (run_ref f mat, run_acc g mat')
  case alg of
    "3x3" -> go stencil_test_ref stencil_test
    x     -> error $ "unknown variant: " ++ x
  where
    {-# NOINLINE run_ref #-}
    run_ref f xs () = f xs
    run_acc f xs () = f xs

