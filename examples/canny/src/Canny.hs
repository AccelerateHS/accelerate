
module Canny where

import SVM

import Data.Array.Accelerate                hiding (map)
import qualified Data.Array.Accelerate      as Acc
import qualified Data.Array.Accelerate.CUDA as Acc

canny :: Array DIM2 Float -> Array DIM2 Float
canny input =
  let (w,h)   = Acc.arrayShape input
      sh      = (constant w, constant h)
      flat    = reshape (constant (w*h)) (use input)
      blurred = Acc.run $ blur sh flat
      gradX   = Acc.run $ gradientXCompute sh (use blurred)
      gradY   = Acc.run $ gradientYCompute sh (use blurred)
      mag     = Acc.run $ gradientIntensityCompute (use gradX) (use gradY)
      orient  = Acc.run $ gradientOrientationCompute (use gradX) (use gradY)
  in  Acc.run . reshape (tuple sh)
               $ nonMaximumSupression sh (use mag) (use orient)


-----------------------------------------------------------------------------
--  Constants and types:
-----------------------------------------------------------------------------
orientUndef, orientHoriz, orientVert, orientPosDiag, orientNegDiag :: Exp Float
orientUndef   = 50
orientHoriz   = 10
orientVert    = 20
orientPosDiag = 30
orientNegDiag = 40

-----------------------------------------------------------------------------
-- Step: Hysteresis Thresholding
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Step: Non-Maximum Supression
-----------------------------------------------------------------------------

nonMaximumSupression
    :: (Exp Int, Exp Int)
    -> Acc (Vector Float)
    -> Acc (Vector Float)
    -> Acc (Vector Float)

nonMaximumSupression (height, width) dMag dOrient
  = Acc.zipWith (*) (Acc.zipWith (isMaximum) dMag intensity1) (Acc.zipWith (isMaximum) dMag intensity2)
    where
        intensity1 = Acc.backpermute (height * width) (\ix -> (isBoundary ix) ? (ix, (getAdjacentPixel (constant 0) (dOrient Acc.! ix) ix))) dMag
        intensity2 = Acc.backpermute (height * width) (\ix -> (isBoundary ix) ? (ix, (getAdjacentPixel (constant 1) (dOrient Acc.! ix) ix))) dMag

        isMaximum a b = (a <=* b) ? (0, 15)

        isBoundary ind
           = row ==* 0 ||* col ==* 0 ||* col ==* width - 1 ||* row ==* height - 1
          where
            row = ind `div` width
            col = ind `mod` width

        getAdjacentPixel :: Exp Int -> Exp Float -> Exp DIM1 -> Exp DIM1
        getAdjacentPixel side orient ind
          = (orient ==* orientHoriz)   ? (((row + offset) * width + col),
            (orient ==* orientVert)    ? ((row * width + col + offset),
            (orient ==* orientPosDiag) ? (((row + offset) * width + col + offset),
            (orient ==* orientNegDiag) ? (((row + offset) * width + col - offset),
                                         ((row * width + col))))))
          where
            row    = ind `div` width
            col    = ind `mod` width
            offset = (side ==* 0) ? (-1, 1) :: Exp Int


-----------------------------------------------------------------------------
-- Steps: X & Y Gradient Calculation
-----------------------------------------------------------------------------

gradientXCompute :: (Exp Int, Exp Int) -> Acc (Vector Float) -> Acc (Vector Float)
gradientXCompute (h, w) input
 = imageConvolveKernel (3, 3) (h, w) kernel input
 where
    kernel = use $ listToKernel2D ([-1, 0, 1,
                                    -2, 0, 2,
                                    -1, 0, 1 ] :: [Float])


gradientYCompute :: (Exp Int, Exp Int) -> Acc (Vector Float) -> Acc (Vector Float)
gradientYCompute (h, w) input
 = imageConvolveKernel (3, 3) (h, w) kernel input
 where
    kernel = use $ listToKernel2D
                      ([1,  2,  1,
                        0,  0,  0,
                       -1, -2, -1 ] :: [Float])


gradientIntensityCompute :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Vector Float)
gradientIntensityCompute dX dY
    = Acc.zipWith (\x y -> abs x + abs y) dX dY


gradientOrientationCompute :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Vector Float)
gradientOrientationCompute dX dY
    = Acc.zipWith orientation dX dY
      where
        orientation :: Exp Float -> Exp Float -> Exp Float
        orientation x y
          = ((x >* -40) &&* (x <* 40)) &&* ((y >* -40) &&* (y <* 40))      ? (orientUndef,
            (atan2 x y >=* (-7 * pi / 8)) &&* (atan2 x y <* (-5 * pi / 8)) ? (orientPosDiag,
            (atan2 x y >=* (-5 * pi / 8)) &&* (atan2 x y <* (-3 * pi / 8)) ? (orientVert,
            (atan2 x y >=* (-3 * pi / 8)) &&* (atan2 x y <* (-1 * pi / 8)) ? (orientNegDiag,
            (atan2 x y >=* (-1 * pi / 8)) &&* (atan2 x y <* ( 1 * pi / 8)) ? (orientHoriz,
            (atan2 x y >=* ( 1 * pi / 8)) &&* (atan2 x y <* ( 3 * pi / 8)) ? (orientPosDiag,
            (atan2 x y >=* ( 3 * pi / 8)) &&* (atan2 x y <* ( 5 * pi / 8)) ? (orientVert,
            (atan2 x y >=* ( 5 * pi / 8)) &&* (atan2 x y <* ( 7 * pi / 8)) ? (orientNegDiag,
                                                                              orientHoriz))))))))


-----------------------------------------------------------------------------
-- Step: Blur
-----------------------------------------------------------------------------

blur :: (Exp Int, Exp Int) -> Acc (Vector Float) -> Acc (Vector Float)
blur sh input = imageConvolveKernel (5,5) sh kernel input
  where
    kernel = use . listToKernel2D
                 $ map (\x -> x / 159)
                           ([2.0,  4.0,  5.0,  4.0, 2.0,
                             4.0,  9.0, 12.0,  9.0, 4.0,
                             5.0, 12.0, 15.0, 12.0, 5.0,
                             4.0,  9.0, 12.0,  9.0, 4.0,
                             2.0,  4.0,  5.0,  4.0, 2.0] :: [Float])

