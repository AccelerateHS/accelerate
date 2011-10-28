{-# LANGUAGE FlexibleInstances #-}
--
-- Types used throughout the simulation
--

module Type (
  Timestep, Viscosity, Diffusion, Point, Density, Velocity,
  Field, FieldElt(..), DensityField, VelocityField, IndexField
) where

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


infixl 6 .+.
infixl 6 .-.
infixl 7 .*.
infixl 7 ./.

class Elt e => FieldElt e where
  zero  :: e
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
  zero  = (0, 0)
  (.+.) = app2 (+)
  (.-.) = app2 (-)
  c  .*. xy = let (x,y) = unlift xy in lift (c*x, c*y)
  xy ./. c  = let (x,y) = unlift xy in lift (x/c, y/c)

app2 :: Elt e => (Exp e -> Exp e -> Exp e) -> Exp (e,e) -> Exp (e,e) -> Exp (e,e)
app2 f xu yv = let (x,u) = unlift xu
                   (y,v) = unlift yv
               in  lift (f x y, f u v)

