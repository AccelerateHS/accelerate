
module Main where

-- friends
import Vec3
import Config
import ParseArgs
import Monitoring
import Scene.World
import Ray.Trace

-- frenemies
import Data.Label
import Data.Array.Accelerate                    as A
import Graphics.Gloss.Accelerate.Data.Color
import Graphics.Gloss.Accelerate.Data.Picture

-- library
import System.Environment                       ( getArgs )
import qualified Graphics.Gloss                 as G


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

      ambient   = rawColor 0.3 0.3 0.3 1.0

      eye       = makeVec3 50 (-100) (-700)
      eyePos    = A.fill (constant (Z :. height :. width)) eye
      eyeDir    = castViewRays width height fov eye

      time      = 1
      objects   = use $ makeObjects time
      lights    = use $ makeLights  time

      scene     = run backend
                $ A.map packRGBA
                $ traceRays bounces ambient objects lights eyePos eyeDir

  G.display
      (G.InWindow "Ray" (width, height) (10, 10))
      G.black
      (bitmapOfArray scene True)

