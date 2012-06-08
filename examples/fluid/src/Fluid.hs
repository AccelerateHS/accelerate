--
-- Fluid simulation
--

module Fluid (

  Simulation, fluid

) where

import Type
import Prelude                  as P
import Data.Array.Accelerate    as A


type Simulation
  =  Acc ( DensitySource                -- locations to add density sources
         , VelocitySource               -- locations to add velocity sources
         , DensityField                 -- the current density field
         , VelocityField )              -- the current velocity field
  -> Acc ( DensityField, VelocityField )


-- A fluid simulation
--
fluid :: Int -> Timestep -> Viscosity -> Diffusion -> Simulation
fluid steps dt dp dn inputs =
  let (ds, vs, df, vf)  = A.unlift inputs
      vf'               = velocity steps dt dp vs vf
      df'               = density  steps dt dn ds vf' df
  in
  A.lift (df', vf')

-- The velocity over a timestep evolves due to three causes:
--   1. the addition of forces
--   2. viscous diffusion
--   3. self-advection
--
velocity
    :: Int
    -> Timestep
    -> Viscosity
    -> Acc VelocitySource
    -> Acc VelocityField
    -> Acc VelocityField
velocity steps dt dp vs vf0
  = project steps
  . advect dt vf0
  . project steps
  . diffuse steps dt dp
  $ inject vs vf0


-- Ensure the velocity field conserves mass
--
project :: Int -> Acc VelocityField -> Acc VelocityField
project steps vf = A.stencil2 poisson (A.Constant zero) vf (A.Constant zero) p
  where
    grad        = A.stencil divF (A.Constant zero) vf
    p1          = A.stencil2 pF (A.Constant zero) grad (A.Constant zero)
    p           = foldl1 (.) (P.replicate steps p1) grad

    poisson :: A.Stencil3x3 Velocity -> A.Stencil3x3 Float -> Exp Velocity
    poisson (_,(_,uv,_),_) ((_,t,_), (l,_,r), (_,b,_)) = uv .-. 0.5 .*. A.lift (r-l, t-b)

    divF :: A.Stencil3x3 Velocity -> Exp Float
    divF ((_,t,_), (l,_,r), (_,b,_)) = -0.5 * (A.fst r - A.fst l + A.snd t - A.snd b)

    pF :: A.Stencil3x3 Float -> A.Stencil3x3 Float -> Exp Float
    pF (_,(_,x,_),_) ((_,t,_), (l,_,r), (_,b,_)) = 0.25 * (x + l + t + r + b)



-- The density over a timestep evolves due to three causes:
--   1. the addition of source particles
--   2. self-diffusion
--   3. motion through the velocity field
--
density
    :: Int
    -> Timestep
    -> Diffusion
    -> Acc DensitySource
    -> Acc VelocityField
    -> Acc DensityField
    -> Acc DensityField
density steps dt dn ds vf
  = advect dt vf
  . diffuse steps dt dn
  . inject ds


-- Inject sources into the field
--
-- TLM: sources should be a vector of (index, value) pairs, but no fusion means
--   that extracting the components for permute (via unzip) is extra work.
--
inject
    :: FieldElt e
    => Acc (Vector Index, Vector e)
    -> Acc (Field e)
    -> Acc (Field e)
inject source field =
  let (is, ps) = A.unlift source
  in A.size ps ==* 0
       ?| ( field, A.permute (.+.) field (is A.!) ps )


diffuse
    :: FieldElt e
    => Int
    -> Timestep
    -> Diffusion
    -> Acc (Field e)
    -> Acc (Field e)
diffuse steps dt dn df0 =
  a ==* 0
    ?| ( df0 , foldl1 (.) (P.replicate steps diffuse1) df0 )
  where
    a           = A.constant dt * A.constant dn * (A.fromIntegral (A.size df0))
    c           = 1 + 4*a

    diffuse1 df = A.stencil2 relax (A.Constant zero) df0 (A.Constant zero) df

    relax :: FieldElt e => A.Stencil3x3 e -> A.Stencil3x3 e -> Exp e
    relax (_,(_,x0,_),_) ((_,t,_), (l,_,r), (_,b,_)) = (x0 .+. a .*. (l.+.t.+.r.+.b)) ./. c


advect
    :: FieldElt e
    => Timestep
    -> Acc VelocityField
    -> Acc (Field e)
    -> Acc (Field e)
advect dt vf df = A.generate sh backtrace
  where
    sh          = A.shape vf
    Z :. h :. w = A.unlift sh

    backtrace ix = s0.*.(t0.*.d00 .+. t1.*.d10) .+. s1.*.(t0.*.d01 .+. t1.*.d11)
      where
        Z:.j:.i = A.unlift ix
        (u, v)  = A.unlift (vf A.! ix)

        -- backtrack densities based on velocity field
        clamp z = A.max 0.5 . A.min (A.fromIntegral z - 1.5)
        x       = w `clamp` (A.fromIntegral i - dt * u)
        y       = h `clamp` (A.fromIntegral j - dt * v)

        -- discrete locations surrounding point
        i0      = A.truncate x
        j0      = A.truncate y
        i1      = i0 + 1
        j1      = j0 + 1

        -- weighting based on location between the discrete points
        s1      = x - A.fromIntegral i0
        t1      = y - A.fromIntegral j0
        s0      = 1 - s1
        t0      = 1 - t1

        -- read the density values surrounding the calculated advection point
        d00     = df A.! A.lift (Z :. j0 :. i0)
        d10     = df A.! A.lift (Z :. j1 :. i0)
        d01     = df A.! A.lift (Z :. j0 :. i1)
        d11     = df A.! A.lift (Z :. j1 :. i1)

