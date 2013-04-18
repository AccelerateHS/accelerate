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

import Data.Array.Accelerate.CUDA.Foreign

-- Smooth life
-- ~~~~~~~~~~~

smoothlife
    :: Config
    -> Acc (Matrix R)
    -> Acc (Matrix R)
smoothlife conf aa
  = aa''
  where
    -- A simulation step
    --
    aaf         = fft2D' Forward size size (complex aa)
    nf          = A.zipWith (*) aaf (use krf')
    mf          = A.zipWith (*) aaf (use kdf')
    n           = A.map (\x -> real x / kflr'') (fft2D' Inverse size size nf)
    m           = A.map (\x -> real x / kfld'') (fft2D' Inverse size size mf)
    aa'         = snm conf sn sm b1 b2 d1 d2 n m
    aa''        = clamp $ A.zipWith timestepMode aa' aa

    -- simulation parameters
    --
    b           = get1 configRim conf
    (ri,ra)     = get2 configDiscRadius conf
    (b1,b2)     = get2 configBirthInterval conf
    (d1,d2)     = get2 configDeathInterval conf
    (sn,sm)     = get2 configStep conf          -- aka. alpha_n alpha_m
    dt          = get1 configTimestep conf

    timestepMode f g = timestepModes f g P.!! get configTimestepMode conf

    size        = get configWindowSize conf
    sh          = constant (Z:.size:.size)

    -- initial state
    --
    kflr        = A.sum kr
    kfld        = A.sum kd
    krf         = fft2D' Forward size size (fftShift2D (complex kr))
    kdf         = fft2D' Forward size size (fftShift2D (complex kd))

    kd          = A.generate sh (\ix -> 1 - linear (radius ix) ri b)
    kr          = A.generate sh (\ix -> let r = radius ix
                                        in  linear r ri b * (1 - linear r ra b))

    kflr''      = constant (kflr' `A.indexArray` Z)
    kfld''      = constant (kfld' `A.indexArray` Z)
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

    clamp = A.map
          (\x -> A.min (A.max x 0.0) 1.0)

    timestepModes f g
      = [ f
        , g + dt*(2.0*f-1.0)
        , g + dt*(f-g) ]


-- Equation 6: s(n,m)
-- ~~~~~~~~~~~~~~~~~~

-- Also a few additional modes as discovered from the source code
--
snm :: Config -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Exp R -> Acc (Matrix R) -> Acc (Matrix R) -> Acc (Matrix R)
snm conf sn sm b1 b2 d1 d2
  = A.zipWith sigmode
  where
    sigtype     = getSigmoidFunction (get configSigtype conf)
    mixtype     = getSigmoidFunction (get configMixtype conf)
    sigmode n m = sigmodes n m P.!! (get configSigmode conf - 1)

    sigmoid_ab :: Exp R -> Exp R -> Exp R -> Exp R
    sigmoid_ab x a b
      = sigtype x a sn * (1.0 - sigtype x b sn)

    sigmoid_mix :: Exp R -> Exp R -> Exp R -> Exp R
    sigmoid_mix x y m
      = x * (1 - mixtype m 0.5 sm) + y * mixtype m 0.5 sm

    mix :: Exp R -> Exp R -> Exp R -> Exp R
    mix x y a = x * (1 - a) + y*a

    -- available sigmodes
    sigmodes n m
      = [ mix         (sigmoid_ab n b1 b2)
                      (sigmoid_ab n d1 d2) m
        , sigmoid_mix (sigmoid_ab n b1 b2)
                      (sigmoid_ab n d1 d2) m
        , sigmoid_ab n (mix b1 d1 m)
                       (mix b2 d2 m)
        , sigmoid_ab n (sigmoid_mix b1 d1 m)
                       (sigmoid_mix b2 d2 m)]


getSigmoidFunction :: SigmoidFunction -> Exp R -> Exp R -> Exp R -> Exp R
getSigmoidFunction f x a ea
  = let
      -- __expf is CUDA's faster but less precise version of exp.
      cexp = foreignExp (cudaExp "math_functions.h __expf") exp
    in
    case f of
      Hard      -> x >=* a ? (1, 0)
      Smooth    -> 1.0/(1.0+cexp(-(x-a)*4.0/ea))
      Atan      -> atan ((x-a) * pi/ea) / pi + 0.5
      Atancos   -> 0.5 * (0.5 * atan ((x-a) / ea) / pi * cos ((x-a) * 1.4) * 1.1 + 1.0)
      Overshoot -> 0.5 + (1.0 / (1.0 + exp (-(x-a)*4.0/ea)) - 0.5) * (1.0 + exp(-(x-a)*(x-a)/ea/ea))
      Linear    -> bounded (\x' a' ea' -> (x'-a')/ea' + 0.5) x a ea
      Hermite   -> bounded (\x' a' ea' -> let v = (x' - (a'-ea'/2.0))/ea' in v * v * (3.0-2.0*v)) x a ea
      Sin       -> bounded (\x' a' ea' -> sin (pi * (x'-a')/ea') * 0.5 + 0.5) x a ea

  where
    bounded :: (Exp R -> Exp R -> Exp R -> Exp R) -> Exp R -> Exp R -> Exp R -> Exp R
    bounded f' x' a' ea'
      = x' <* a'-ea'/2.0 ? ( 0.0
      , x' >* a'+ea'/2.0 ? ( 1.0
      , f' x' a' ea' ))
