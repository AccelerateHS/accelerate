
module Main where

-- friends
import Config
import ParseArgs
import Monitoring
import Scene.State
import Gloss.Draw
import Gloss.Event

-- frenemies
import Data.Label
import Data.Array.Accelerate                                    as A
import qualified Graphics.Gloss.Accelerate.Raster.Field         as G

-- library
import System.Environment                                       ( getArgs )


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

  G.playFieldWith
      (run1 backend)
      (G.InWindow "Ray" (width, height) (10, 10))
      (zoom, zoom)
      fps
      (initState 0)
      prepareState
      (tracePixel width height fov bounces)
      handleEvent
      advanceState

