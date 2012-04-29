module Density
   ( densitySteps )
   where

import Data.Array.Repa

import Model
import Stages
import Constants

-- Runs the stages for processing the density field in one time step
densitySteps :: DensityField -> Maybe (Source Float) -> VelocityField -> DensityField
densitySteps df ds vf = df'
   where
      df'@(Array _ [Region RangeAll GenManifest{}]) = advection vf df2
      df2@(Array _ [Region RangeAll GenManifest{}]) = diffusion df1 diff
      df1@(Array _ [Region RangeAll GenManifest{}]) = addSources ds df
