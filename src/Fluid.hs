--
-- Fluid simulation
--

module Fluid (simulate) where

import           Type
import           World
import           Data.Array.Accelerate          ( Z(..), (:.)(..), Acc, Exp )
import qualified Data.Array.Accelerate          as A


-- A simulation step
--
simulate :: Viscosity -> Diffusion -> Timestep -> World -> World
simulate dp dn dt world = world { densityField = df', velocityField = vf' }
  where
    vf' = velocity dt dp $ world
    df' = density  dt dn $ world { velocityField = vf' }


-- The velocity over a timestep evolves due to three causes:
--   1. the addition of forces
--   2. viscous diffusion
--   3. self-advection
--
velocity :: Timestep -> Viscosity -> World -> Acc VelocityField
velocity dt dp world
  = project
  . advect dt ix vf0
  . project
  $ diffuse dt dp vf0 vf
  where
    ix  = indexField world
    vf  = velocityField world
    vf0 = inject (velocitySource world) vf


-- Ensure the velocity field conserves mass
--
project :: Acc VelocityField -> Acc VelocityField
project vf = A.stencil2 poisson A.Mirror vf A.Mirror p
  where
    steps   = 20
    grad    = A.stencil divF A.Mirror vf
    p1      = A.stencil2 pF (A.Constant 0) grad A.Mirror
    p       = foldl1 (.) (replicate steps p1) grad

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
density :: Timestep -> Diffusion -> World -> Acc DensityField
density dt dn world
  = advect  dt ix vf
  $ diffuse dt dn (inject (densitySource world) df) df
  where
    ix  = indexField world
    vf  = velocityField world
    df  = densityField world


-- Inject sources into the field
--
inject :: FieldElt e => [(Point, e)] -> Acc (Field e) -> Acc (Field e)
inject []  df = df
inject src df =
  let n     = length src
      (i,d) = unzip src
      is    = A.use $ A.fromList (Z:.n) i
      ps    = A.use $ A.fromList (Z:.n) d
  in
  A.permute (.+.) df (is A.!) ps


diffuse :: FieldElt e
        => Timestep -> Diffusion -> Acc (Field e) -> Acc (Field e) -> Acc (Field e)
diffuse dt dn df0 = foldl1 (.) (replicate steps diffuse1)
  where
    steps = 20
    a     = A.constant (dt * dn) * (A.fromIntegral (A.size df0))
    c     = 1 + 4*a

    diffuse1 df = A.stencil2 relax (A.Constant zero) df0 A.Mirror df

    relax :: FieldElt e => A.Stencil3x3 e -> A.Stencil3x3 e -> Exp e
    relax (_,(_,x0,_),_) ((_,t,_), (l,_,r), (_,b,_)) = (x0 .+. a .*. (l.+.t.+.r.+.b)) ./. c


advect :: FieldElt e
       => Timestep -> Acc IndexField -> Acc VelocityField -> Acc (Field e) -> Acc (Field e)
advect dt ixf vf df = imap backtrace vf
  where
    Z:.h:.w = A.unlift $ A.shape vf
    imap f  = A.zipWith f ixf

    backtrace ix vu = s0.*.(t0.*.d00 .+. t1.*.d10) .+. s1.*.(t0.*.d01 .+. t1.*.d11)
      where
        Z:.j:.i = A.unlift ix
        (v,u)   = A.unlift vu

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

