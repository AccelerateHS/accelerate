{-# LANGUAGE FlexibleInstances #-}
--
-- Operations for different field types
--
module Field where

import Data.Array.Accelerate

type Timestep      = Float
type Viscosity     = Float
type Diffusion     = Float
type Point         = DIM2
type Density       = Float
type Velocity      = (Float, Float)

type Field a       = Array DIM2 a
type DensityField  = Field Density
type VelocityField = Field Velocity
type IndexField    = Field Point

