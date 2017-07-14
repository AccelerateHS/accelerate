{-# LANGUAGE FlexibleInstances #-}
--
-- Types used throughout the simulation
--

module Type (

  Timestep, Viscosity, Diffusion, Index, Density, Velocity,
  Field, FieldElt(..), DensityField, VelocityField, DensitySource, VelocitySource,

  RGBA32, Image

) where

import Data.Word
import Data.Array.Accelerate

type Timestep           = Float
type Viscosity          = Float
type Diffusion          = Float
type Index              = DIM2
type Density            = Float
type Velocity           = (Float, Float)

type Field a            = Array DIM2 a
type DensityField       = Field Density
type VelocityField      = Field Velocity

type Source a           = (Vector Index, Vector a)
type DensitySource      = Source Density
type VelocitySource     = Source Velocity

type RGBA32             = Word32
type Image a            = Array DIM2 a


infixl 6 .+.
infixl 6 .-.
infixl 7 .*.
infixl 7 ./.

class Elt e => FieldElt e where
  zero  :: Exp e
  (.+.) :: Exp e -> Exp e -> Exp e
  (.-.) :: Exp e -> Exp e -> Exp e
  (.*.) :: Exp Float -> Exp e -> Exp e
  (./.) :: Exp e -> Exp Float -> Exp e

instance FieldElt Density where
  zero  = 0
  (.+.) = (+)
  (.-.) = (-)
  (.*.) = (*)
  (./.) = (/)

instance FieldElt Velocity where
  zero  = constant (0, 0)
  (.+.) = app2 (+)
  (.-.) = app2 (-)
  c  .*. xy = let (x,y) = unlift xy in lift (c*x, c*y)
  xy ./. c  = let (x,y) = unlift xy in lift (x/c, y/c)

app2 :: Elt e => (Exp e -> Exp e -> Exp e) -> Exp (e,e) -> Exp (e,e) -> Exp (e,e)
app2 f xu yv = let (x,u) = unlift xu
                   (y,v) = unlift yv
               in  lift (f x y, f u v)

