--
-- Fluid simulation
--

module Fluid (simulate) where

import Type
import World
import Config

import Data.Array.Accelerate            ( Z(..), (:.)(..), Exp, Acc, Vector, (?|), (==*) )
import qualified Data.Array.Accelerate  as A


-- A simulation step
--
simulate :: Options -> Viscosity -> Diffusion -> Timestep -> World -> World
simulate opt dp dn dt world =
  world { densityField  = df', densitySource  = []
        , velocityField = vf', velocitySource = []
        }
  where
    vf          = velocity world dt dp    $ A.use (velocityField world)
    df          = density  world dt dn vf $ A.use (densityField  world)
    (vf', df')  = run opt (A.pairA vf df)


-- The velocity over a timestep evolves due to three causes:
--   1. the addition of forces
--   2. viscous diffusion
--   3. self-advection
--
velocity
    :: World
    -> Timestep
    -> Viscosity
    -> Acc VelocityField
    -> Acc VelocityField
velocity world dt dp vf0
  = project
  . advect dt vf0
  . project
  . diffuse dt dp
  $ inject ix' vs' vf0
  where
    (ix, vs)    = unzip $ velocitySource world
    ix'         = A.use $ A.fromList (Z :. length ix) ix
    vs'         = A.use $ A.fromList (Z :. length vs) vs


-- Ensure the velocity field conserves mass
--
project :: Acc VelocityField -> Acc VelocityField
project vf = A.stencil2 poisson A.Mirror vf A.Mirror p
  where
    steps       = 10 --20
    grad        = A.stencil divF A.Mirror vf
    p1          = A.stencil2 pF (A.Constant 0) grad A.Mirror
    p           = foldl1 (.) (replicate steps p1) grad

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
    :: World
    -> Timestep
    -> Diffusion
    -> Acc VelocityField
    -> Acc DensityField
    -> Acc DensityField
density world dt dn vf
  = advect dt vf
  . diffuse dt dn
  . inject ix' ds'
  where
    (ix, ds)    = unzip $ densitySource world
    ix'         = A.use $ A.fromList (Z :. length ix) ix
    ds'         = A.use $ A.fromList (Z :. length ds) ds


-- Inject sources into the field
--
-- TLM: sources should be a vector of (index, value) pairs, but no fusion means
--   that extracting the components for permute (via unzip) is extra work.
--
inject
    :: FieldElt e
    => Acc (Vector Index) -> Acc (Vector e)
    -> Acc (Field e)
    -> Acc (Field e)
inject is ps df =
  A.size ps ==* 0
    ?| ( df, A.permute (.+.) df (is A.!) ps )


diffuse
    :: FieldElt e
    => Timestep
    -> Diffusion
    -> Acc (Field e)
    -> Acc (Field e)
diffuse dt dn df0 =
  a ==* 0
    ?| ( df0 , foldl1 (.) (replicate steps diffuse1) df0 )
  where
    steps       = 10 --20
    a           = A.constant (dt * dn) * (A.fromIntegral (A.size df0))
    c           = 1 + 4*a

    diffuse1 df = A.stencil2 relax (A.Constant zero) df0 A.Mirror df

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
        x       = w `clamp` (A.fromIntegral i - A.constant dt * u)
        y       = h `clamp` (A.fromIntegral j - A.constant dt * v)

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

