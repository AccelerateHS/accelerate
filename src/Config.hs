{-# LANGUAGE DeriveDataTypeable #-}
--
-- Configuration parameters
--

module Config (Config(..), processArgs) where

import Data.Version
import System.Console.CmdArgs
import Paths_accelerate_fluid

data Config = Config
  {
    -- simulation
      timestep         :: Float
    , viscosity        :: Float
    , diffusion        :: Float
    , simulationWidth  :: Int
    , simulationHeight :: Int

    -- visualisation
    , displayScale     :: Int
    , displayFramerate :: Int
  }
  deriving (Show, Data, Typeable)

processArgs :: IO Config
processArgs =
  cmdArgs defaultConfig

defaultConfig :: Config
defaultConfig =  Config
  {
    timestep = 0.5
      &= explicit
      &= name "t"
      &= name "timestep"
      &= help "size of a simulation time step"
      &= typ "FLOAT"

  , viscosity = 0
      &= explicit
      &= name "u"
      &= name "viscosity"
      &= help "viscosity for velocity dampening"
      &= typ "FLOAT"

  , diffusion = 0
      &= explicit
      &= name "d"
      &= name "diffusion"
      &= help "diffusion rate for mass dispersion"
      &= typ "FLOAT"

  , simulationWidth = 100
      &= explicit
      &= name "w"
      &= name "width"
      &= help "grid width for simulation"
      &= typ "INT"

  , simulationHeight = 100
      &= explicit
      &= name "h"
      &= name "height"
      &= help "grid height for simulation"
      &= typ "INT"

  , displayScale = 4
      &= explicit
      &= name "s"
      &= name "scale"
      &= help "feature size of visualisation"
      &= typ "FLOAT"

  , displayFramerate = 10
      &= explicit
      &= name "r"
      &= name "framerate"
      &= help "frame rate for visualisation"
      &= typ "INT"

  }
  &= program "accelerate-fluid"
  &= summary "accelerate-fluid (c) 2011 Ben Lambert-Smith & Trevor L. McDonell"
  &= versionArg [summary $ "accelerate-fluid-" ++ showVersion version]
  &= verbosityArgs
       [explicit, name "verbose", help "Print more output"]
       [explicit, name "quiet",   help "Print less output"]

