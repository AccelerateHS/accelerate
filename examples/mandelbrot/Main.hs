{-# LANGUAGE CPP #-}
--
-- A Mandelbrot set generator.
-- Originally submitted by Simon Marlow as part of Issue #49.
--


import Event
import Config
import Data.Label
import Control.Monad
import Control.Exception
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe
import System.Environment                       ( getArgs, withArgs )
import Criterion.Main                           ( defaultMainWith, bench, whnf )
import Data.Array.Accelerate.Array.Data         ( ptrsOfArrayData )
import Data.Array.Accelerate.Array.Sugar        ( Array(..) )

import Prelude                                  as P
import Data.Array.Accelerate                    as A hiding ( size )
import qualified Graphics.Gloss                 as G


-- Types -----------------------------------------------------------------------

-- Real values. Use single precision, as double is not supported on all devices
type R            = Float

-- Complex numbers
type Complex      = (R,R)
type ComplexPlane = Array DIM2 Complex

-- Image data
type RGBA         = Word32
type Bitmap       = Array DIM2 RGBA

-- Action to render a frame
type Render       = Scalar View -> Bitmap


-- Mandelbrot Set --------------------------------------------------------------

mandelbrot :: Int -> Int -> Int -> Acc (Scalar View) -> Acc (Array DIM2 (Complex,Int))
mandelbrot screenX screenY depth view
  = foldr ($) zs0 (P.take depth (repeat go))
  where
    cs  = genPlane screenX screenY view
    zs0 = mkinit cs

    go :: Acc (Array DIM2 (Complex,Int)) -> Acc (Array DIM2 (Complex,Int))
    go = A.zipWith iter cs


genPlane :: Int
         -> Int
         -> Acc (Scalar View)
         -> Acc ComplexPlane
genPlane screenX screenY view
  = generate (constant (Z:.screenY:.screenX))
             (\ix -> let pr = unindex2 ix
                         x = A.fromIntegral (A.fst pr :: Exp Int)
                         y = A.fromIntegral (A.snd pr :: Exp Int)
                     in
                       lift ( xmin + (x * sizex) / viewx
                            , ymin + (y * sizey) / viewy))
  where
    (xmin,ymin,xmax,ymax) = unlift (the view)

    sizex = xmax - xmin
    sizey = ymax - ymin

    viewx = constant (P.fromIntegral screenX)
    viewy = constant (P.fromIntegral screenY)


next :: Exp Complex -> Exp Complex -> Exp Complex
next c z = c `plus` (z `times` z)


plus :: Exp Complex -> Exp Complex -> Exp Complex
plus = lift2 f
  where f :: (Exp R, Exp R) -> (Exp R, Exp R) -> (Exp R, Exp R)
        f (x1,y1) (x2,y2) = (x1+x2, y1+y2)

times :: Exp Complex -> Exp Complex -> Exp Complex
times = lift2 f
  where f :: (Exp R, Exp R) -> (Exp R, Exp R) -> (Exp R, Exp R)
        f (x,y) (x',y')   =  (x*x'-y*y', x*y'+y*x')

dot :: Exp Complex -> Exp R
dot = lift1 f
  where f :: (Exp R, Exp R) -> Exp R
        f (x,y) = x*x + y*y


iter :: Exp Complex -> Exp (Complex,Int) -> Exp (Complex,Int)
iter c zi = f (A.fst zi) (A.snd zi)
 where
  f :: Exp Complex -> Exp Int -> Exp (Complex,Int)
  f z i =
    let z' = next c z
    in (dot z' >* 4.0) ? ( zi , lift (z', i+1) )


mkinit :: Acc ComplexPlane -> Acc (Array DIM2 (Complex,Int))
mkinit cs = A.zip cs (A.fill (A.shape cs) 0)


-- Rendering -------------------------------------------------------------------

prettyRGBA :: Exp Int -> Exp (Complex, Int) -> Exp RGBA
prettyRGBA lIMIT s' = r + g + b + a
  where
    s   = A.snd s'
    t   = A.fromIntegral $ ((lIMIT - s) * 255) `quot` lIMIT
    r   = (t     `mod` 128 + 64) * 0x1000000
    g   = (t * 2 `mod` 128 + 64) * 0x10000
    b   = (t * 3 `mod` 256     ) * 0x100
    a   = 0xFF


makePicture :: Render -> View -> G.Picture
makePicture render viewport = pic
  where
    view        = A.fromList Z [viewport]
    arrPixels   = render view
    (Z:.h:.w)   = arrayShape arrPixels

    {-# NOINLINE rawData #-}
    rawData     = let (Array _ adata)   = arrPixels
                      ((), ptr)         = ptrsOfArrayData adata
                  in
                  unsafePerformIO       $ newForeignPtr_ (castPtr ptr)

    pic         = G.bitmapOfForeignPtr h w rawData False


-- Main ------------------------------------------------------------------------

main :: IO ()
main
  = do
        (config, critConf, nops) <- processArgs =<< getArgs
        let size        = get optSize config
            limit       = get optLimit config
            fps         = get optFramerate config
            --
            view        = (-0.25, -1.0, 0.0, -0.75)     -- should get this from command line as well
            render      = run1 config
                        $ A.map (prettyRGBA (constant limit))
                        . mandelbrot size size limit
            --
            force arr   = indexArray arr (Z:.0:.0) `seq` arr

        unless (P.null nops) $
          putStrLn $ "Warning: unrecognized options: " ++ show nops

        void $ evaluate (force $ render (fromList Z [view]))

        if get optBench config
           then withArgs nops $ defaultMainWith critConf (return ())
                    [ bench "mandelbrot" $
                      whnf (force . render) $ fromList Z [view] ]

#ifndef ACCELERATE_ENABLE_GUI
           else return ()
#else
           else G.play
                    (G.InWindow "Mandelbrot" (size, size) (10, 10))
                    G.black
                    fps
                    view
                    (makePicture render)
                    (react config)
                    (flip const)
#endif

