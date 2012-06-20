{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeOperators #-}

module Canny (canny) where

import Prelude                                  as P

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.IO                 as A

import Data.Array.Repa.Repr.Unboxed             ( U )
import qualified Data.Array.Repa                as R
import qualified Data.Vector.Unboxed            as V
import qualified Data.Vector.Unboxed.Mutable    as VM
-- import qualified Data.Array.Repa.IO.BMP         as R
-- import qualified Data.Array.Repa.Repr.Unboxed   as R


canny :: (forall a. Arrays a => Acc a -> a)
      -> FilePath
      -> Float
      -> Float
      -> IO (R.Array U R.DIM2 Word8)
canny run fileIn threshLow threshHigh = do

  -- Read in the image BMP file
  --
  img           <- either (error . show) id `fmap` readImageFromBMP fileIn

  -- Setup and run the Accelerate kernel stages, which identify strong and weak
  -- edges in the image.
  --
  let low       = unit (constant threshLow)
      high      = unit (constant threshHigh)

      grey      = toGreyscale
      blurred   = gaussianY . gaussianX . grey
      magdir    = gradientMagDir low . blurred
      suppress  = nonMaximumSuppression low high . magdir

      (image,strong)    = run $ stage1 (use img)
      stage1 x          = let img' = suppress x
                          in  lift $ (img', selectStrong img')

  -- Now use Repa to trace out weak edges connected to strong edges
  --
  edges         <- wildfire (A.toRepa image) (A.toRepa strong)

--  R.writeImageToBMP "edges.bmp" (R.zip3 edges edges edges)
  return edges

{--
  -- Benchmark each (accelerate) stage. The "kernels" group is intended to be
  -- run under nvvp, whereas the canny group is for end-to-end benchmarks.
  --
  (args,rest)   <- break (== "--") `fmap` getArgs
  let opts      = if null rest then [] else P.tail rest
      force2 a  = A.indexArray a (Z:.0:.0 :: DIM2) `seq` ()
      force1 a  = A.indexArray a (Z:.0    :: DIM1) `seq` ()

      -- Need to force partial results so that we benchmark individual stages
      --
      grey'     = run $ toGreyscale (use img)
      blurX'    = run $ gaussianX (use grey')
      blurred'  = run $ gaussianY (use blurX')
      gradX'    = run $ gradientX (use blurred')
      gradY'    = run $ gradientY (use blurred')
      magdir'   = run $ gradientMagDir low (use blurred')
      suppress' = run $ nonMaximumSuppression low high (use magdir')

  withArgs opts $ defaultMain
    [ bgroup "kernels"
      [ bench "greyscale"   $ whnf (force2 . run . grey) (use img)
      , bench "blur-x"      $ whnf (force2 . run . gaussianX) (use grey')
      , bench "blur-y"      $ whnf (force2 . run . gaussianY) (use blurX')
      , bench "grad-x"      $ whnf (force2 . run . gradientX) (use blurred')
      , bench "grad-y"      $ whnf (force2 . run . gradientY) (use blurred')
      , bench "mag-orient"  $ whnf (force2 . run . gradientMagDir low) (use blurred')
      , bench "suppress"    $ whnf (force2 . run . nonMaximumSuppression low high) (use magdir')
      , bench "strong"      $ whnf (force1 . run . selectStrong) (use suppress')
      ]

    , bgroup "canny"
      [ bench "run"         $ whnf (force1 . P.snd . run . stage1) (use img)
      , bench "run1"        $ whnf (force1 . P.snd . run1 stage1) img
      ]
    ]
--}

-- Accelerate component --------------------------------------------------------

type RGBA               = Word32
type Image a            = Array DIM2 a

type Stencil5x1 a = (Stencil3 a, Stencil5 a, Stencil3 a)
type Stencil1x5 a = (Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a)

-- Classification of the output pixel
data Orient     = Undef | PosD | Vert | NegD | Horiz
data Edge       = None  | Weak | Strong

orient :: Orient -> Int
orient Undef    = 0
orient PosD     = 64
orient Vert     = 128
orient NegD     = 192
orient Horiz    = 255

orient' :: Orient -> Exp Int
orient' = constant . orient

edge :: Edge -> Float
edge None       = 0
edge Weak       = 0.5
edge Strong     = 1.0

edge' :: Edge -> Exp Float
edge' = constant . edge

convolve5x1 :: (Elt a, IsNum a) => [Exp a] -> Stencil5x1 a -> Exp a
convolve5x1 kernel (_, (a,b,c,d,e), _)
  = P.foldl1 (+)
  $ P.zipWith (*) kernel [a,b,c,d,e]

convolve1x5 :: (Elt a, IsNum a) => [Exp a] -> Stencil1x5 a -> Exp a
convolve1x5 kernel ((_,a,_), (_,b,_), (_,c,_), (_,d,_), (_,e,_))
  = P.foldl1 (+)
  $ P.zipWith (*) kernel [a,b,c,d,e]


-- RGB to Greyscale conversion, in the range [0,255]
--
toGreyscale :: Acc (Image RGBA) -> Acc (Image Float)
toGreyscale = A.map (\rgba -> 255 * luminanceOfRGBA32 rgba)


-- Separable Gaussian blur in the x- and y-directions
--
gaussianX :: Acc (Image Float) -> Acc (Image Float)
gaussianX = stencil (convolve5x1 gaussian) Clamp
  where
    gaussian = [ 1, 4, 6, 4, 1 ]

gaussianY :: Acc (Image Float) -> Acc (Image Float)
gaussianY = stencil (convolve1x5 gaussian) Clamp
  where
    gaussian = P.map (/256) [ 1, 4, 6, 4, 1 ]


-- Gradients in the x- and y- directions
--
gradientX :: Acc (Image Float) -> Acc (Image Float)
gradientX = stencil grad Clamp
  where
    grad :: Stencil3x3 Float -> Exp Float
    grad ((u, _, x)
         ,(v, _, y)
         ,(w, _, z)) = x + (2*y) + z - u - (2*v) - w

gradientY :: Acc (Image Float) -> Acc (Image Float)
gradientY = stencil grad Clamp
  where
    grad :: Stencil3x3 Float -> Exp Float
    grad ((x, y, z)
         ,(_, _, _)
         ,(u, v, w)) = x + (2*y) + z - u - (2*v) - w


-- Classify the magnitude and orientation of the image gradient.
--
-- Because accelerate supports generalised stencil functions, not just
-- convolutions, we can combine the x- and y- sobel operators and save some
-- memory bandwidth.
--
gradientMagDir
    :: Acc (Scalar Float)
    -> Acc (Image Float)
    -> Acc (Array DIM2 (Float,Int))
gradientMagDir threshLow = stencil magdir Clamp
  where
    magdir :: Stencil3x3 Float -> Exp (Float,Int)
    magdir ((v0, v1, v2)
           ,(v3,  _, v4)
           ,(v5, v6, v7)) =
      let
          -- Image gradients
          dx          = v2 + (2*v4) + v7 - v0 - (2*v3) - v5
          dy          = v0 + (2*v1) + v2 - v5 - (2*v6) - v7

          -- Magnitude
          mag         = sqrt (dx * dx + dy + dy)

          -- Direction
          --
          -- Determine the angle of the vector and rotate it around a bit to
          -- make the segments easier to classify
          theta       = atan2 dy dx
          alpha       = (theta - (pi/8)) * (4/pi)

          -- Normalise the angle to between [0..8)
          norm        = alpha + 8 * A.fromIntegral (boolToInt (alpha <=* 0))

          -- Try to avoid doing explicit tests, to avoid warp divergence
          low         = the threshLow
          undef       = abs dx <=* low &&* abs dy <=* low
          dir         = boolToInt (A.not undef) * ((64 * (1 + A.floor norm `mod` 4)) `A.min` 255)
      in
      lift (mag, dir)


-- Non-maximum suppression classifies pixels that are the local maximum along
-- the direction of the image gradient as either strong or weak edges. All other
-- pixels are not considered edges at all.
--
-- The image intensity is in the range [0,1]
--
nonMaximumSuppression
  :: Acc (Scalar Float)
  -> Acc (Scalar Float)
  -> Acc (Image (Float,Int))
  -> Acc (Image Float)
nonMaximumSuppression threshLow threshHigh magdir =
  generate (shape magdir) $ \ix ->
    let -- The input parameters
        --
        low             = the threshLow
        high            = the threshHigh
        (mag, dir)      = unlift (magdir ! ix)
        Z :. h :. w     = unlift (shape magdir)
        Z :. y :. x     = unlift ix

        -- Determine the points that lie either side of this point along to the
        -- direction of the image gradient.
        --
        -- The direction coding:
        --
        --   192   128   64
        --          |
        --   255 --- ---
        --
        offsetx         = dir >* orient' Vert  ? (-1, dir <* orient' Vert ? (1, 0))
        offsety         = dir <* orient' Horiz ? (-1, 0)

        (fwd, _)        = unlift $ magdir ! lift (clamp (Z :. y+offsety :. x+offsetx)) :: (Exp Float, Exp Int)
        (rev, _)        = unlift $ magdir ! lift (clamp (Z :. y-offsety :. x-offsetx)) :: (Exp Float, Exp Int)

        clamp (Z:.u:.v) = Z :. 0 `A.max` u `A.min` (h-1) :. 0 `A.max` v `A.min` (w-1)

        -- Try to avoid doing explicit tests to avoid warp divergence.
        --
        none            = dir ==* orient' Undef ||* mag <* low ||* mag <* fwd ||* mag <* rev
        strong          = mag >=* high
    in
    A.fromIntegral (boolToInt (A.not none) * (1 + boolToInt strong)) * 0.5


-- Extract the linear indices of the strong edges
--
selectStrong
  :: Acc (Image Float)
  -> Acc (Array DIM1 Int)
selectStrong img =
  let strong            = A.map (\x -> boolToInt (x ==* edge' Strong)) (flatten img)
      (targetIdx, len)  = A.scanl' (+) 0 strong
      indices           = A.enumFromN (index1 $ size img) 0
      zeros             = A.fill (index1 $ the len) 0
  in
  A.permute const zeros (\ix -> strong!ix ==* 0 ? (ignore, index1 $ targetIdx!ix)) indices


-- Repa component --------------------------------------------------------------

-- | Trace out strong edges in the final image.
--   Also trace out weak edges that are connected to strong edges.
--
wildfire
    :: R.Array A R.DIM2 Float           -- ^ Image with strong and weak edges set.
    -> R.Array A R.DIM1 Int             -- ^ Array containing flat indices of strong edges.
    -> IO (R.Array U R.DIM2 Word8)

wildfire img arrStrong
 = do   (sh, vec)       <- wildfireIO
        return  $ sh `seq` vec `seq` R.fromUnboxed sh vec

 where  lenImg          = R.size $ R.extent img
        lenStrong       = R.size $ R.extent arrStrong
        shImg           = R.extent img

        wildfireIO
         = do   -- Stack of image indices we still need to consider.
                vStrong  <- R.toUnboxed `fmap` R.computeUnboxedP (R.delay arrStrong)
                vStrong' <- V.thaw vStrong
                vStack   <- VM.grow vStrong' (lenImg - lenStrong)

                -- Burn in new edges.
                vImg    <- VM.unsafeNew lenImg
                VM.set vImg 0
                burn vImg vStack lenStrong
                vImg'   <- V.unsafeFreeze vImg
                return  (R.extent img, vImg')


        burn :: VM.IOVector Word8 -> VM.IOVector Int -> Int -> IO ()
        burn !vImg !vStack !top
         | top == 0
         = return ()

         | otherwise
         = do   let !top'               =  top - 1
                n                       <- VM.unsafeRead vStack top'
                let (R.Z R.:. y R.:. x) = R.fromIndex (R.extent img) n

                let {-# INLINE push #-}
                    push ix t =
                      if R.inShape shImg ix
                         then pushWeak vImg vStack ix t
                         else return t

                VM.write vImg n 255
                 >>  push (R.Z R.:. y - 1 R.:. x - 1) top'
                 >>= push (R.Z R.:. y - 1 R.:. x    )
                 >>= push (R.Z R.:. y - 1 R.:. x + 1)

                 >>= push (R.Z R.:. y     R.:. x - 1)
                 >>= push (R.Z R.:. y     R.:. x + 1)

                 >>= push (R.Z R.:. y + 1 R.:. x - 1)
                 >>= push (R.Z R.:. y + 1 R.:. x    )
                 >>= push (R.Z R.:. y + 1 R.:. x + 1)

                 >>= burn vImg vStack

        -- If this ix is weak in the source then set it to strong in the
        -- result and push the ix onto the stack.
        {-# INLINE pushWeak #-}
        pushWeak vImg vStack ix top
         = do   let n           = R.toIndex (R.extent img) ix
                xDst            <- VM.unsafeRead vImg n
                let xSrc        = img `R.unsafeIndex` ix

                if   xDst == 0
                  && xSrc == edge Weak
                 then do
                        VM.unsafeWrite vStack top (R.toIndex (R.extent img) ix)
                        return (top + 1)

                 else   return top
{-# NOINLINE wildfire #-}

