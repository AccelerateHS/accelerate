{-# LANGUAGE CPP #-}
--
-- A Mandelbrot set generator.
-- Originally submitted by Simon Marlow as part of Issue #49.
--

import World
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


-- Main ------------------------------------------------------------------------

makePicture :: World -> G.Picture
makePicture world = pic
  where
    arrPixels   = renderWorld world
    (Z:.h:.w)   = arrayShape arrPixels

    {-# NOINLINE rawData #-}
    rawData     = let (Array _ adata)   = arrPixels
                      ((), ptr)         = ptrsOfArrayData adata
                  in
                  unsafePerformIO       $ newForeignPtr_ (castPtr ptr)

    pic         = G.bitmapOfForeignPtr h w rawData False


main :: IO ()
main
  = do
        (config, critConf, nops) <- processArgs =<< getArgs

        let world       = initialWorld config view
            fps         = get optFramerate config
            size        = get optSize config

            view        = (-0.25, -1.0, 0.0, -0.75)

            force arr   = indexArray arr (Z:.0:.0) `seq` arr

        unless (P.null nops) $
          putStrLn $ "Warning: unrecognized options: " ++ show nops

        void $ evaluate (force $ renderWorld world)

        if get optBench config
           then withArgs nops $ defaultMainWith critConf (return ())
                    [ bench "mandelbrot" $
                      whnf (force . renderWorld) world ]

#ifndef ACCELERATE_ENABLE_GUI
           else return ()
#else
           else G.play
                    (G.InWindow "Mandelbrot" (size, size) (10, 10))
                    G.black
                    fps
                    world
                    makePicture
                    (react config)
                    (const refocus)
#endif

