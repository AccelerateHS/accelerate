{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeOperators #-}

-- | Canny Edge Detection
--
module Process where

import Data.Bits
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import System.Mem
import Prelude                                          as P

import qualified Data.Array.Repa                        as R
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector.Unboxed.Mutable            as UM
import qualified Data.Vector.Storable.Mutable           as VM

import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.IO                         as A hiding ( Array )
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
processImage phase enableBlur enableInvert threshLow threshHigh width height ptr
 = do
      -- Copy the input to a new accelerate array (avoid copying?)
      --
      input     <- A.fromPtr (Z :. height :. width) ((), castPtr ptr)

      let -- The canny algorithm, step by step
          --
          threshold     = A.unit $ A.constant (threshLow, threshHigh)
          greyscale     = A.map fromRGBA (A.use input)
          smooth        = gaussianX $ gaussianY greyscale
          gradX         = gradientX $ if enableBlur then smooth else greyscale
          gradY         = gradientY $ if enableBlur then smooth else greyscale
          gradMag       = gradientMagnitude gradX gradY
          suppress      = nonMaximumSuppression threshold gradX gradY gradMag

          -- Link strong and weak edges on the CPU, write result directly to the
          -- output buffer
          --
          link = do
            let img     =  A.toRepa $ CUDA.run suppress
            --
            strong      <- selectStrong img
            edges       <- newForeignPtr_ (castPtr ptr)
            wildfire img strong edges enableInvert

          -- Fill the output buffer
          --
          render =
            case phase of
              0 -> fill greyscale
              1 -> fill smooth
              2 -> fill gradX
              3 -> fill gradY
              4 -> fill gradMag
              5 -> fill (A.zipWith atan2 gradX gradY)
              6 -> fill suppress
              _ -> link

          fill acc = do
            let Array _ adata   = CUDA.run $ A.map (toRGBA enableInvert) acc
                ((),ptr')       = ptrsOfArrayData adata
            --
            copyArray ptr (castPtr ptr') (width * height * 4)

      -- Render the appropriate phase of the algorithm into the output buffer
      render

      -- Force a GC to clear out GPU resources
      performGC


-- Image conversion ------------------------------------------------------------
-- ----------------                                                           --

fromRGBA :: (Elt a, IsFloating a) => Exp RGBA -> Exp a
fromRGBA rgba =
  let b = (0.11 / 255) * A.fromIntegral ((rgba `div` 0x100)     .&. 0xFF)
      g = (0.59 / 255) * A.fromIntegral ((rgba `div` 0x10000)   .&. 0xFF)
      r = (0.3  / 255) * A.fromIntegral ((rgba `div` 0x1000000) .&. 0xFF)
  in
  r + g + b

toRGBA :: (Elt a, IsFloating a) => Bool -> Exp a -> Exp RGBA
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

type RGBA         = Word32
type Image a      = Array DIM2 a
type Stencil5x1 a = (Stencil3 a, Stencil5 a, Stencil3 a)
type Stencil7x1 a = (Stencil3 a, Stencil7 a, Stencil3 a)
type Stencil1x5 a = (Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a)
type Stencil1x7 a = (Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a)

-- Classification of the output pixel
data Edge       = None | Weak | Strong

edge :: (Elt a, IsFloating a) => Edge -> a
edge None       = 0
edge Weak       = 0.5
edge Strong     = 1.0

edge' :: (Elt a, IsFloating a) => Edge -> Exp a
edge' = constant . edge


convolve7x1 :: (Elt a, IsNum a) => [Exp a] -> Stencil7x1 a -> Exp a
convolve7x1 kernel (_, (a,b,c,d,e,f,g), _)
  = P.foldl1 (+)
  $ P.zipWith (*) kernel [a,b,c,d,e,f,g]

convolve5x1 :: (Elt a, IsNum a) => [Exp a] -> Stencil5x1 a -> Exp a
convolve5x1 kernel (_, (a,b,c,d,e), _)
  = P.foldl1 (+)
  $ P.zipWith (*) kernel [a,b,c,d,e]

convolve1x7 :: (Elt a, IsNum a) => [Exp a] -> Stencil1x7 a -> Exp a
convolve1x7 kernel ((_,a,_), (_,b,_), (_,c,_), (_,d,_), (_,e,_), (_,f,_), (_,g,_))
  = P.foldl1 (+)
  $ P.zipWith (*) kernel [a,b,c,d,e,f,g]

convolve1x5 :: (Elt a, IsNum a) => [Exp a] -> Stencil1x5 a -> Exp a
convolve1x5 kernel ((_,a,_), (_,b,_), (_,c,_), (_,d,_), (_,e,_))
  = P.foldl1 (+)
  $ P.zipWith (*) kernel [a,b,c,d,e]


-- Gaussian smoothing
--
-- This is used to remove high-frequency noise from the image. Since the
-- Gaussian function is symmetric, it is separable into two 1D filters along
-- each dimension.
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
gradientMagnitude = A.zipWith magdir
  where
    magdir dx dy = let mag = sqrt (dx*dx + dy*dy)
                       -- dir = atan2 dy dx
                   in  mag -- lift (mag, dir)


-- Non-maximum suppression
--
nonMaximumSuppression
  :: (Elt a, IsFloating a)
  => Acc (Scalar (a,a))
  -> Acc (Image a)
  -> Acc (Image a)
  -> Acc (Image a)
  -> Acc (Image a)
nonMaximumSuppression threshold gradX gradY gradM =
  generate (shape gradX) $ \ix ->
    let dx              = gradX ! ix
        dy              = gradY ! ix
        mag             = gradM ! ix
        alpha           = 1.3065629648763766  -- 0.5 / sin (pi / 8.0)
        offsetx         = A.round (alpha * dx / mag)
        offsety         = A.round (alpha * dy / mag)
        --
        Z :. m :. n     = unlift (shape gradX)
        Z :. x :. y     = unlift ix
        fwd             = gradM ! lift (clamp (x+offsetx, y+offsety))
        rev             = gradM ! lift (clamp (x-offsetx, y-offsety))
        (low, high)     = unlift $ the threshold
        --
        clamp (u,v)     = lift (Z:. 0 `A.max` u `A.min` (m-1) :. 0 `A.max` v `A.min` (n-1))
    in
    (mag <* low ||* mag <* fwd ||* mag <* rev) ?
      (edge' None, mag <* high ? (edge' Weak, edge' Strong))


-- Select the indices of strong edges.
--
selectStrong
    :: (IsFloating a, Elt a, Storable a, Eq a, R.Repr r a)
    => R.Array r R.DIM2 a
    -> IO (R.Array R.U R.DIM1 Int)
selectStrong img = R.selectP match process (R.size $ R.extent img)
  where
    {-# INLINE match   #-}
    {-# INLINE process #-}
    match ix    = img `R.unsafeLinearIndex` ix == edge Strong
    process ix  = ix


-- Trace out strong edges in the final image, and also trace out weak edges
-- that are connected to strong edges.
--
wildfire
    :: (IsFloating a, Elt a, Storable a, Eq a, R.Repr r a)
    => R.Array r   R.DIM2 a             -- Image with strong and weak edges set
    -> R.Array R.U R.DIM1 Int           -- Array containing flat indices of strong edges
    -> ForeignPtr RGBA                  -- Buffer to trace connected edges into
    -> Bool                             -- Invert output
    -> IO ()
wildfire img arrStrong edges invert = do

  -- Stack of image indices we still need to consider.
  vStrong       <- U.unsafeThaw (R.toUnboxed arrStrong)
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

