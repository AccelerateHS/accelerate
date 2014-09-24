{-# LANGUAGE CPP #-}

module Main where

-- friends
import Config
import ParseArgs
import Monitoring
import Scene.State
import Gloss.Draw
import Gloss.Event
import Ray.Trace

-- frenemies
import Data.Label
import Data.Array.Accelerate                                    as A
import Graphics.Gloss.Accelerate.Data.Color.RGB
import qualified Graphics.Gloss.Accelerate.Raster.Field         as G

-- library
import Criterion.Main                                           ( defaultMainWith, bench, whnf )
import System.Environment                                       ( getArgs, withArgs )


main :: IO ()
main = do
  beginMonitoring
  argv                  <- getArgs
  (conf, cconf, rest)   <- parseArgs optHelp optBackend options defaults header footer argv

  let width     = get optWidth conf
      height    = get optHeight conf
      zoom      = get optZoom conf
      fov       = get optFieldOfView conf
      bounces   = get optBounces conf
      backend   = get optBackend conf
      fps       = get optFramerate conf
      state     = initState 0
      ambient   = rawColor 0.3 0.3 0.3

      scene st
        = let eye               = constant (get stateEyePos state)
              eyeDir            = castViewRays width height fov eye
              eyePos            = fill (constant (Z :. height :. width)) eye
              (objects, lights) = unlift st
          in
          A.zipWith (traceRay bounces objects lights ambient) eyePos eyeDir

  if get optBench conf
     then withArgs rest $ defaultMainWith cconf
            [ bench "ray" $ whnf (run1 backend scene)
                                 (get stateObjects state, get stateLights state)
            ]

#ifndef ACCELERATE_ENABLE_GUI
     else return ()
#else
     else G.playFieldWith
            (run1 backend)
            (G.InWindow "Ray" (width, height) (10, 10))
            (zoom, zoom)
            fps
            state
            prepareState
            (tracePixel width height fov bounces ambient)
            handleEvent
            advanceState
#endif

