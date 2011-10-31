{-# LANGUAGE CPP, DeriveDataTypeable #-}
--
-- Configuration parameters
--

module Config (Config(..), run, processArgs) where

import           Data.Version
import           System.Console.CmdArgs
import           Paths_accelerate_fluid
import           Data.Array.Accelerate                  (Acc, Arrays)

import qualified Data.Array.Accelerate.Interpreter      as I
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif

data Backend
  = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
  | CUDA
#endif
  deriving (Show, Data, Typeable)


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

    -- execution
    , backend          :: Backend
  }
  deriving (Show, Data, Typeable)


-- Execute an Accelerate expression using the selected backend
--
run :: Arrays a => Config -> Acc a -> a
run cfg = case backend cfg of
  Interpreter   -> I.run
#ifdef ACCELERATE_CUDA_BACKEND
  CUDA          -> CUDA.run
#endif

-- Process command line arguments
--
processArgs :: IO Config
processArgs =  cmdArgs defaultConfig

defaultConfig :: Config
defaultConfig =  Config
  {
    backend = enum
    [ Interpreter
        &= help "Reference implementation (sequential)"
#ifdef ACCELERATE_CUDA_BACKEND
    , CUDA
        &= explicit
        &= name "cuda"
        &= help "Implementation for NVIDIA GPUs (parallel)"
#endif
    ]

  , timestep = 0.5
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
      &= typ "INT"

  , displayFramerate = 10
      &= explicit
      &= name "r"
      &= name "framerate"
      &= help "frame rate for visualisation"
      &= typ "INT"
  }
  &= program "accelerate-fluid"
  &= summary "accelerate-fluid (c) 2011 Trevor L. McDonell"
  &= versionArg [summary $ "accelerate-fluid-" ++ showVersion version]
  &= verbosityArgs
       [explicit, name "verbose", help "Print more output"]
       [explicit, name "quiet",   help "Print less output"]

