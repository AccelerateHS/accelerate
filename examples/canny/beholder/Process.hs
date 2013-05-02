{-# LANGUAGE BangPatterns        #-}

-- | Canny Edge Detection
--
module Process where

import Data.Bits
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import System.Mem
import Prelude                                          as P

import qualified Data.Array.Repa                        as R
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector.Unboxed.Mutable            as UM
import qualified Data.Vector.Storable.Mutable           as VM

import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.IO                         as A
import Data.Array.Accelerate.CUDA                       as CUDA
import Data.Array.Accelerate.Array.Data                 ( ptrsOfArrayData )
import Data.Array.Accelerate.Array.Sugar                ( Array(..) )


foreign export ccall processImage
    :: Int -> Bool -> Bool -> Float -> Float -> Int -> Int -> Ptr Word8 -> IO ()

processImage
    :: Int              -- phase select
    -> Bool             -- enable blur
    -> Bool             -- enable invert
    -> Float            -- threshold low
    -> Float            -- threshold high
    -> Int              -- image width
    -> Int              -- image height
    -> Ptr Word8        -- pointer to image data
    -> IO ()
processImage phase enableBlur enableInvert threshLow threshHigh width height buffer
 = do
      -- Copy the input to a new accelerate array (avoid copying?)
      --
      input     <- A.fromPtr (Z :. height :. width) ((), castPtr buffer)
      edges     <- newForeignPtr_ (castPtr buffer)

      -- The canny algorithm, step by step
      --
      let low       = unit (constant threshLow)
          high      = unit (constant threshHigh)

          grey      = toGreyscale
          blurred   = gaussianY . gaussianX . grey
          gradX     = gradientX . (if enableBlur then blurred else grey)
          gradY     = gradientY . (if enableBlur then blurred else grey)
          magdir a  = gradientMagDir low (gradX a) (gradY a)
          suppress  = nonMaximumSuppression low high . magdir

          stage1 x  = let img' = suppress x
                      in  lift $ (img', selectStrong img')

          -- Trace out strong and weak edges on the CPU
          --
          link =
            let (img, str) = run1 stage1 input
            in  wildfire (toRepa img) (toRepa str) edges enableInvert

          -- Render the chosen part of the algorithm and write that to the
          -- output buffer
          --
          render =
            let display = A.map (toRGBA enableInvert)
            in case phase of
              0 -> stage $ run1 (display . grey)
              1 -> stage $ run1 (display . blurred)
              2 -> stage $ run1 (display . gradX)
              3 -> stage $ run1 (display . gradY)
              4 -> stage $ run1 (A.map (toRGBA enableInvert . A.fst)                  . magdir)
              5 -> stage $ run1 (A.map (toRGBA enableInvert . A.fromIntegral . A.snd) . magdir)
              6 -> stage $ run1 (display . suppress)
              _ -> link

          stage f = do
            let Array _ adata   = f input
                ((), ptr)       = ptrsOfArrayData adata
            --
            copyArray buffer (castPtr ptr) (width * height * 4)

      -- Render the appropriate phase of the algorithm into the output buffer
      render

      -- Force a GC to clear out GPU resources
      performGC


-- Image conversion ------------------------------------------------------------
-- ----------------                                                           --

fromRGBA :: Exp RGBA -> Exp Float
fromRGBA rgba =
  let b = (0.11 / 255) * A.fromIntegral ((rgba `div` 0x100)     .&. 0xFF)
      g = (0.59 / 255) * A.fromIntegral ((rgba `div` 0x10000)   .&. 0xFF)
      r = (0.3  / 255) * A.fromIntegral ((rgba `div` 0x1000000) .&. 0xFF)
  in
  r + g + b

toRGBA :: Bool -> Exp Float -> Exp RGBA
toRGBA invert val = r + b + g + a
  where
    u   = A.truncate (255 * (0 `A.max` val `A.min` 1))
    v   = if invert then 0xFF - u else u
    a   = 0xFF
    b   = v * 0x100
    g   = v * 0x10000
    r   = v * 0x1000000


-- Canny Edge Detection --------------------------------------------------------
-- --------------------                                                       --

-- Accelerate component --------------------------------------------------------

type RGBA         = Word32
type Image a      = Array DIM2 a

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
toGreyscale = A.map luminanceOfRGBA

luminanceOfRGBA :: (Elt a, IsFloating a) => Exp RGBA -> Exp a
luminanceOfRGBA rgba =
  let r = 0.3  * A.fromIntegral (rgba                 .&. 0xFF)
      g = 0.59 * A.fromIntegral ((rgba `div` 0x100)   .&. 0xFF)
      b = 0.11 * A.fromIntegral ((rgba `div` 0x10000) .&. 0xFF)
  in
  r + g + b


rgbaOfLuminance :: (Elt a, IsFloating a) => Bool -> Exp a -> Exp RGBA
rgbaOfLuminance invert val = r + b + g + a
  where
    u   = A.truncate (255 * (0 `A.max` val `A.min` 1))
    v   = if invert then 0xFF - u else u
    r   = v
    g   = v * 0x100
    b   = v * 0x10000
    a   =     0xFF000000


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


-- Classify the magnitude and orientation of the image gradient
--
gradientMagDir
  :: Acc (Scalar Float)
  -> Acc (Image Float)
  -> Acc (Image Float)
  -> Acc (Image (Float, Int))
gradientMagDir threshLow = A.zipWith (\dx dy -> lift (magnitude dx dy, direction dx dy))
  where
    low                 = the threshLow
    magnitude dx dy     = sqrt (dx * dx + dy + dy)
    direction dx dy =
      let -- Determine the angle of the vector and rotate it around a bit to
          -- make the segments easier to classify
          --
          theta         = atan2 dy dx
          alpha         = (theta - (pi/8)) * (4/pi)

          -- Normalise the angle to between [0..8)
          --
          norm          = alpha + 8 * A.fromIntegral (boolToInt (alpha <=* 0))

          -- Try to avoid doing explicit tests, to avoid warp divergence
          --
          undef         = abs dx <=* low &&* abs dy <=* low
      in
      boolToInt (A.not undef) * ((64 * (1 + A.floor norm `mod` 4)) `A.min` 255)


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

-- Trace out strong edges in the final image, and also trace out weak edges
-- that are connected to strong edges.
--
wildfire
    :: R.Array A R.DIM2 Float           -- Image with strong and weak edges set
    -> R.Array A R.DIM1 Int             -- Array containing flat indices of strong edges
    -> ForeignPtr RGBA                  -- Buffer to trace connected edges into
    -> Bool                             -- Invert output
    -> IO ()
wildfire img arrStrong edges invert = do

  -- Stack of image indices we still need to consider.
  vStrong       <- U.unsafeThaw . R.toUnboxed =<< R.computeUnboxedP (R.delay arrStrong)
  vStack        <- UM.unsafeGrow vStrong (lenImg - lenStrong)

  -- Burn in new edges.
  let vImg      = VM.unsafeFromForeignPtr0 edges lenImg
  VM.set vImg bg
  burn vImg vStack lenStrong

  where
    shImg       = R.extent img
    lenImg      = R.size shImg
    lenStrong   = R.size (R.extent arrStrong)

    !bg         = if invert then 0xFFFFFFFF else 0x000000FF
    !fg         = if invert then 0x000000FF else 0xFFFFFFFF

    burn :: VM.IOVector RGBA -> UM.IOVector Int -> Int -> IO ()
    burn !vImg !vStack !top
      | top == 0
      = return ()

      | otherwise
      = do let !top'               =  top - 1
           n                       <- UM.unsafeRead vStack top'
           let (R.Z R.:. y R.:. x) =  R.fromIndex (R.extent img) n

           let {-# INLINE push #-}
               push ix t =
                 if R.inShape shImg ix
                    then pushWeak vImg vStack ix t
                    else return t

           VM.unsafeWrite vImg n fg
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
     = do let n         =  R.toIndex (R.extent img) ix
          xDst          <- VM.unsafeRead vImg n
          let xSrc      =  R.index img ix

          if xDst == bg && xSrc == edge Weak
             then do UM.unsafeWrite vStack top (R.toIndex (R.extent img) ix)
                     return (top + 1)

             else    return top

