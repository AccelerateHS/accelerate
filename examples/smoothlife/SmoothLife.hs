{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
--
-- A cellular automata simulation over a smooth domain.
--
--  http://arxiv.org/abs/1111.1567
--  http://sourceforge.net/projects/smoothlife/
--

module SmoothLife
  where

import Config

import Prelude                                  as P
import Data.Label
import Data.Array.Accelerate                    as A hiding ( size )
import Data.Array.Accelerate.Math.FFT
import Data.Array.Accelerate.Math.DFT.Centre
import Data.Array.Accelerate.Math.Complex


-- Smooth life
-- ~~~~~~~~~~~

smoothlife
    :: Config
    -> Acc (Matrix R)
    -> Acc (Matrix R)
smoothlife conf aa = snm sn sm b1 b2 d1 d2 n m
  where
    -- A simulation step
    --
    aaf         = fft2D' Forward size size (complex aa)
    nf          = A.zipWith (*) aaf (use krf')
    mf          = A.zipWith (*) aaf (use kdf')
    n           = A.map (\x -> real x / kflr'') (fft2D' Inverse size size nf)
    m           = A.map (\x -> real x / kfld'') (fft2D' Inverse size size mf)

    -- simulation parameters
    --
    b           = get1 configRim conf
    (ri,ra)     = get2 configDiscRadius conf
    (b1,b2)     = get2 configBirthInterval conf
    (d1,d2)     = get2 configDeathInterval conf
    (sn,sm)     = get2 configStep conf          -- aka. alpha_n alpha_m

    size        = get configWindowSize conf
    sh          = constant (Z:.size:.size)

    -- initial state
    --
    kflr        = A.sum kr
    kfld        = A.sum kd
    krf         = fft2D' Forward size size (centre2D (complex kr))
    kdf         = fft2D' Forward size size (centre2D (complex kd))

    kd          = A.generate sh (\ix -> 1 - linear (radius ix) ri b)
    kr          = A.generate sh (\ix -> let r = radius ix
                                        in  linear r ri b * (1 - linear r ra b))

    kflr''      = constant (kflr' `indexArray` Z)
    kfld''      = constant (kfld' `indexArray` Z)
    (kflr', kfld', krf', kdf')
                = run conf $ lift (kflr, kfld, krf, kdf)

    -- Auxiliary
    --
    get1 f c    = constant  $ get f c
    get2 f c    = let (x,y) = get f c in (constant x, constant y)

    complex     = A.map (\x -> lift (x, constant 0))

    radius ix   =
      let Z:.y':.x'   = unlift ix     :: Z :. Exp Int :. Exp Int
          x           = A.fromIntegral $ x' - constant (size `div` 2)
          y           = A.fromIntegral $ y' - constant (size `div` 2)
      in
      sqrt (x*x + y*y)

    linear x l u
      = x <* l-u/2 ? ( 0.0
      , x >* l+u/2 ? ( 1.0
      , (x - l + u / 2) / u ))


-- Equation 6: s(n,m)
-- ~~~~~~~~~~~~~~~~~~

-- Also a few additional modes as discovered from the source code
--
snm :: Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Acc (Matrix R) -> Acc (Matrix R) -> Acc (Matrix R)
snm en em b1 b2 d1 d2
  = A.zipWith (\n m -> sigmoid_ab sigmode n (sigmoid_mix mixmode b1 d1 m em)
                                            (sigmoid_mix mixmode b2 d2 m em) en en)
  where
    -- TODO: make runtime configurable
    --
    sigmode     = _smooth
    mixmode     = _smooth

    sigmoid_ab :: (Exp R -> Exp R -> Exp R -> Exp R) -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R
    sigmoid_ab f x a b ea eb
      = f x a ea * (1 - f x b eb)

    sigmoid_mix :: (Exp R -> Exp R -> Exp R -> Exp R) -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R
    sigmoid_mix f x y m sm
      = x * (1 - f m 0.5 sm) + y * f m 0.5 sm

    -- available modes
    _hard x a _         = x >=* a ? (1, 0)
    _smooth x a ea      = 1 / (1 + exp (-(x-a) * 4.0 / ea))
    _atan x a ea        = atan ((x-a) * pi/ea) / pi + 0.5
    _atancos x a ea     = 0.5 * (0.5 * atan ((x-a) / ea) / pi * cos ((x-a) * 1.4) * 1.1 + 1.0)
    _overshoot x a ea   = 0.5 + (1.0 / (1.0 + exp (-(x-a)*4.0/ea)) - 0.5) * (1.0 + exp(-(x-a)*(x-a)/ea/ea))
    _linear             = bounded $ \x a ea -> (x-a)/ea + 0.5
    _hermite            = bounded $ \x a ea -> let v = (x - (a-ea/2.0))/ea in v * v * (3.0-2.0*v)
    _sin                = bounded $ \x a ea -> sin (pi * (x-a)/ea) * 0.5 + 0.5

    bounded :: (Exp R -> Exp R -> Exp R -> Exp R) -> Exp R -> Exp R -> Exp R -> Exp R
    bounded f x a ea
      = x <* a-ea/2.0 ? ( 0.0
      , x >* a+ea/2.0 ? ( 1.0
      , f x a ea ))

