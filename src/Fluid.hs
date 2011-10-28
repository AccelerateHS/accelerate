--
-- Fluid simulation
--

module Fluid where

import           Field
import           World
import           Data.Array.Accelerate          ( Z(..), (:.)(..), Acc, Exp )
import qualified Data.Array.Accelerate          as A


-- The velocity over a timestep evolves due to three causes:
--   1. the addition of forces
--   2. viscous diffusion
--   3. self-advection
--
velocity :: World -> VelocityField
velocity = error "TODO: velocity"


-- Ensure the velocity field conserves mass.
--
--project = error "TODO: project"



-- The density over a timestep evolves due to three causes:
--   1. the addition of source particles
--   2. self-diffusion
--   3. motion through the velocity field
--
density :: Timestep -> Diffusion -> World -> Acc DensityField
density dt dn world = advect  dt ix vf
                    $ diffuse dt dn (inject (densitySource world) df) df
  where
    ix  = indexField world
    vf  = velocityField world
    df  = densityField world


-- Inject sources into the density field
--
inject :: [(Point, Density)] -> Acc DensityField -> Acc DensityField
inject []  df = df
inject src df =
  let n     = length src
      (i,d) = unzip src
      is    = A.use $ A.fromList (Z:.n) i
      ps    = A.use $ A.fromList (Z:.n) d
  in
  A.permute (+) df (is A.!) ps


diffuse :: Timestep -> Diffusion -> Acc DensityField -> Acc DensityField -> Acc DensityField
diffuse dt dn df0 = foldl1 (.) (replicate steps diffuse1)
  where
    steps = 20
    a     = A.constant (dt * dn) * (A.fromIntegral (A.size df0))
    c     = 1 + 4*a

    diffuse1 :: Acc DensityField -> Acc DensityField
    diffuse1 df = A.stencil2 relax (A.Constant 0) df0 A.Mirror df

    relax :: A.Stencil3x3 Density -> A.Stencil3x3 Density -> Exp Density
    relax (_,(_,x0,_),_) ((_,t,_), (l,_,r), (_,b,_)) = (x0 + a * (l+t+r+b)) / c


advect :: Timestep -> Acc IndexField -> Acc VelocityField -> Acc DensityField -> Acc DensityField
advect dt ixf vf df = imap backtrace vf
  where
    Z:.h:.w = A.unlift $ A.shape vf
    imap f  = A.zipWith f ixf

    backtrace :: Exp Point -> Exp Velocity -> Exp Density
    backtrace ix vu = s0*(t0*d00 + t1*d10) + s1*(t0*d01 + t1*d11)
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

