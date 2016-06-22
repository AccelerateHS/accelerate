
module Main where

-- Ray
import Config
import Scene.Object                                             ( Objects )
import Scene.Light                                              ( Lights )
import Scene.State
import Gloss.Draw
import Gloss.Event
import Ray.Trace

-- Friends
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Examples.Internal
import Data.Array.Accelerate.Data.Colour.RGB
import qualified Graphics.Gloss.Accelerate.Raster.Field         as G

-- Enemies
import Data.Label


main :: IO ()
main = do
  beginMonitoring
  (conf, opts, rest)    <- parseArgs options defaults header footer

  let width     = get configWidth conf
      height    = get configHeight conf
      zoom      = get configZoom conf
      fov       = get configFieldOfView conf
      bounces   = get configBounces conf
      fps       = get configFramerate conf
      backend   = get optBackend opts
      state     = initState 0
      ambient   = rgb 0.3 0.3 0.3

      scene :: Acc (Objects,Lights) -> Acc (Array DIM2 Colour)
      scene st
        = let eye               = constant (get stateEyePos state)
              eyeDir            = castViewRays width height fov eye
              eyePos            = fill (constant (Z :. height :. width)) eye
              (objects, lights) = unlift st
          in
          A.zipWith (traceRay bounces objects lights ambient) eyePos eyeDir

  runBenchmarks opts rest
    [ bench "ray"
    $ whnf (run1 backend scene)
           (get stateObjects state, get stateLights state) ]

  runInteractive opts rest
    $ G.playFieldWith
          (run1 backend)
          (G.InWindow "Ray" (width, height) (10, 10))
          (zoom, zoom)
          fps
          state
          prepareState
          (tracePixel width height fov bounces ambient)
          handleEvent
          advanceState

