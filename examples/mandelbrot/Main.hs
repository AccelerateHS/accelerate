{-# LANGUAGE CPP #-}
--
-- A Mandelbrot set generator. Submitted by Simon Marlow as part of Issue #49.
--


import Config
import Data.Label
import Control.Monad
import Control.Exception
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe
import System.Environment
import Criterion.Main                           ( defaultMain, bench, whnf )
import Data.Array.Accelerate.Array.Data         ( ptrsOfArrayData )
import Data.Array.Accelerate.Array.Sugar        ( Array(..) )

import Prelude                                  as P
import Data.Array.Accelerate                    as A hiding ( size )
import qualified Graphics.Gloss                 as G


-- Mandelbrot Set --------------------------------------------------------------

type F            = Float       -- Double not supported on all devices

type Complex      = (F,F)
type ComplexPlane = Array DIM2 Complex

mandelbrot :: F -> F -> F -> F -> Int -> Int -> Int -> Acc (Array DIM2 (F,F,Int))
mandelbrot x y x' y' screenX screenY depth
  = foldl (flip ($)) zs0 (P.take depth (repeat go))
  where
    cs  = genPlane x y x' y' screenX screenY
    zs0 = mkinit cs

    go :: Acc (Array DIM2 (F,F,Int)) -> Acc (Array DIM2 (F,F,Int))
    go = A.zipWith iter cs


genPlane :: F -> F
         -> F -> F
         -> Int
         -> Int
         -> Acc ComplexPlane
genPlane lowx lowy highx highy viewx viewy
   = generate (constant (Z:.viewy:.viewx))
              (\ix -> let pr = unindex2 ix
                          x = A.fromIntegral (A.fst pr)
                          y = A.fromIntegral (A.snd pr)
                      in
                        lift ( elowx + (x * exsize) / eviewx
                             , elowy + (y * eysize) / eviewy))
   where
      elowx, elowy, exsize, eysize, eviewx, eviewy :: Exp F

      elowx  = constant lowx
      elowy  = constant lowy

      exsize = constant (highx - lowx)
      eysize = constant (highy - lowy)

      eviewx = constant (P.fromIntegral viewx)
      eviewy = constant (P.fromIntegral viewy)


next :: Exp Complex -> Exp Complex -> Exp Complex
next c z = c `plus` (z `times` z)


plus :: Exp Complex -> Exp Complex -> Exp Complex
plus = lift2 f
  where f :: (Exp F, Exp F) -> (Exp F, Exp F) -> (Exp F, Exp F)
        f (x1,y1) (x2,y2) = (x1+x2,y1+y2)

times :: Exp Complex -> Exp Complex -> Exp Complex
times = lift2 f
  where f :: (Exp F, Exp F) -> (Exp F, Exp F) -> (Exp F, Exp F)
        f (x,y) (x',y')   =  (x*x'-y*y', x*y'+y*x')

dot :: Exp Complex -> Exp F
dot = lift1 f
  where f :: (Exp F, Exp F) -> Exp F
        f (x,y) = x*x + y*y


iter :: Exp Complex -> Exp (F,F,Int) -> Exp (F,F,Int)
iter c z = f (unlift z)
 where
  f :: (Exp F, Exp F, Exp Int) -> Exp (F,F,Int)
  f (x,y,i) =
     (dot z' >* 4.0) ? ( lift (x,y,i)
                       , lift (A.fst z', A.snd z', i+1) )
     where z' = A.curry (next c) x y


mkinit :: Acc ComplexPlane -> Acc (Array DIM2 (F,F,Int))
mkinit cs = A.map (lift1 f) cs
  where f :: (Exp F, Exp F) -> (Exp F, Exp F, Exp Int)
        f (x,y) = (x,y,0)


-- Rendering -------------------------------------------------------------------

type RGBA = Word32

prettyRGBA :: Exp Int -> Exp (F, F, Int) -> Exp RGBA
prettyRGBA lIMIT s' = r + g + b + a
  where
    (_, _, s)   = unlift s' :: (Exp F, Exp F, Exp Int)
    t           = A.fromIntegral $ ((lIMIT - s) * 255) `quot` lIMIT
    r           = (t     `mod` 128 + 64) * 0x1000000
    g           = (t * 2 `mod` 128 + 64) * 0x10000
    b           = (t * 3 `mod` 256     ) * 0x100
    a           = 0xFF


makePicture :: Options -> Int -> Acc (Array DIM2 (F, F, Int)) -> G.Picture
makePicture opt limit zs = pic
  where
    arrPixels   = run opt $ A.map (prettyRGBA (constant limit)) zs
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
  = do  (config, nops) <- processArgs =<< getArgs
        let size        = get optSize config
            limit       = get optLimit config
            --
            x           = -0.25         -- should get this from command line as well
            y           = -1.0
            x'          =  0.0
            y'          = -0.75
            --
            force arr   = indexArray arr (Z:.0:.0) `seq` arr
            image       = makePicture config limit
                        $ mandelbrot x y x' y' size size limit

        void $ evaluate image

        if get optBench config
           then withArgs nops $ defaultMain
                    [ bench "mandelbrot" $
                      whnf (force . run config . mandelbrot x y x' y' size size) limit ]

#ifndef ACCELERATE_ENABLE_GUI
           else return ()

#elif MIN_VERSION_gloss(1,6,0)
           else G.display
                    (G.InWindow "Mandelbrot" (size, size) (10, 10))
                    G.black
                    image
#else
           else G.displayInWindow
                    "Mandelbrot"
                    (size, size)
                    (10, 10)
                    G.black
                    image
#endif

