{-# LANGUAGE TemplateHaskell #-}

module Gloss.Simulate (

  Mode(..),
  Simulate, sigmoidMode, mixingMode,
  initialise,

) where

import Config                                             ( Config )
import Data.Label


data Mode = Hard | Linear | Hermite | Sine | Smooth | Atan | Atancos | Overshoot
  deriving (Eq, Show)

data Simulate = Simulate
  {
    -- Parameters to control the simulation
    _sigmoidMode        :: Mode
  , _mixingMode         :: Mode
  }

$(mkLabels [''Simulate])


-- Initial simulation state
--
initialise :: Config -> Simulate
initialise _
  = Simulate
  {
    _sigmoidMode        = Smooth
  , _mixingMode         = Smooth
  }

