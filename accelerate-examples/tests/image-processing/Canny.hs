{-# LANGUAGE TypeOperators #-}

module Canny where

import PGM

import Data.Array.Accelerate                hiding (zipWith)
import qualified Data.Array.Accelerate      as Acc


type Image a      = Array DIM2 a
type Stencil7x1 a = (Stencil3 a, Stencil7 a, Stencil3 a)
type Stencil1x7 a = (Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a)


convolve7x1 :: (Elt a, IsNum a) => [Exp a] -> Stencil7x1 a -> Exp a
convolve7x1 kernel (_, (a,b,c,d,e,f,g), _) =
  sum $ zipWith (*) kernel [a,b,c,d,e,f,g]

convolve1x7 :: (Elt a, IsNum a) => [Exp a] -> Stencil1x7 a -> Exp a
convolve1x7 kernel ((_,a,_), (_,b,_), (_,c,_), (_,d,_), (_,e,_), (_,f,_), (_,g,_)) =
  sum $ zipWith (*) kernel [a,b,c,d,e,f,g]


-- Gaussian smoothing
--
gaussian :: (Elt a, IsFloating a) => [Exp a]
gaussian = [ 0.00442012927963
           , 0.05384819825462
           , 0.24133088157513
           , 0.39788735772974
           , 0.24133088157513
           , 0.05384819825462
           , 0.00442012927963 ]

gaussianX :: (Elt a, IsFloating a) => Acc (Image a) -> Acc (Image a)
gaussianX = stencil (convolve7x1 gaussian) (Constant 0)

gaussianY :: (Elt a, IsFloating a) => Acc (Image a) -> Acc (Image a)
gaussianY = stencil (convolve1x7 gaussian) (Constant 0)


-- Gaussian derivative and gradient quantisation
--
gaussian' :: (Elt a, IsFloating a) => [Exp a]
gaussian' = [ 0.02121662054222
            , 0.17231423441479
            , 0.38612941052022
            , 0.0
            ,-0.38612941052022
            ,-0.17231423441479
            ,-0.02121662054222 ]

gradientX :: (Elt a, IsFloating a) => Acc (Image a) -> Acc (Image a)
gradientX = stencil (convolve7x1 gaussian') (Constant 0)

gradientY :: (Elt a, IsFloating a) => Acc (Image a) -> Acc (Image a)
gradientY = stencil (convolve1x7 gaussian') (Constant 0)

gradientMagnitude :: (Elt a, IsFloating a) => Acc (Image a) -> Acc (Image a) -> Acc (Image a)
gradientMagnitude = Acc.zipWith magdir
  where
    magdir dx dy = let mag = sqrt (dx*dx + dy*dy)
                    -- dir = atan2 dy dx
                   in  mag -- lift (mag, dir)


-- Non-maximum suppression
--
nonMaximumSuppression
  :: (Elt a, IsFloating a)
  => Exp a
  -> Acc (Image a)
  -> Acc (Image a)
  -> Acc (Image a)
  -> Acc (Image a)
nonMaximumSuppression threshold gradX gradY gradM =
  generate (shape gradX) $ \ix ->
    let dx          = gradX ! ix
        dy          = gradY ! ix
        mag         = gradM ! ix
        alpha       = 1.3065629648763766  -- 0.5 / sin (pi / 8.0)
        offsetx     = Acc.round (alpha * dx / mag)
        offsety     = Acc.round (alpha * dy / mag)
        --
        (m,n)       = unindex2 (shape gradX)
        (x,y)       = unindex2 ix
        fwd         = gradM ! lift (clamp (x+offsetx, y+offsety))
        rev         = gradM ! lift (clamp (x-offsetx, y-offsety))
        --
        unindex2 uv = let Z:.u:.v = unlift uv in (u,v)
        clamp (u,v) = lift (Z:. 0 `Acc.max` u `Acc.min` (m-1) :. 0 `Acc.max` v `Acc.min` (n-1))
    in
    (mag <* threshold ||* fwd >* mag ||* rev >* mag) ? (0, 1)


-- Canny edge detection
--
canny :: (Elt a, IsFloating a) => Image a -> Acc (Image a)
canny img =
  let smooth  = gaussianX . gaussianY $ use img
      gradX   = gradientX smooth
      gradY   = gradientY smooth
      gradMag = gradientMagnitude gradX gradY
  in
  nonMaximumSuppression 0.1 gradX gradY gradMag


-- Main
-- ----

-- TLM: should compare to a pre-saved reference image
run :: FilePath -> IO (() -> Acc (Array DIM2 Float))
run file = do
  pgm <- readPGM file
  return (\() -> canny pgm)

