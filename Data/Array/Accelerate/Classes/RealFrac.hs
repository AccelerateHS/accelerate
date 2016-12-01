{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.RealFrac
-- Copyright   : [2016] Manuel M T Chakravarty, Gabriele Keller
--               [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.RealFrac (

  RealFrac(..),
  div', mod', divMod',

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Floating
import Data.Array.Accelerate.Classes.Fractional
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Real
import Data.Array.Accelerate.Classes.ToFloating

import Text.Printf
import Prelude                                                      ( ($), String, error )
import qualified Prelude                                            as P


-- | Generalisation of 'P.div' to any instance of 'RealFrac'
--
div' :: (RealFrac a, Elt b, IsIntegral b) => Exp a -> Exp a -> Exp b
div' n d = floor (n / d)

-- | Generalisation of 'P.mod' to any instance of 'RealFrac'
--
mod' :: (Floating a, RealFrac a, ToFloating Int a) => Exp a -> Exp a -> Exp a
mod' n d = n - (toFloating f) * d
  where
    f :: Exp Int
    f = div' n d

-- | Generalisation of 'P.divMod' to any instance of 'RealFrac'
--
divMod'
    :: (Floating a, RealFrac a, Num b, IsIntegral b, ToFloating b a)
    => Exp a
    -> Exp a
    -> (Exp b, Exp a)
divMod' n d = (f, n - (toFloating f) * d)
  where
    f = div' n d


-- | Extracting components of fractions.
--
class (Real a, Fractional a) => RealFrac a where
  -- The function 'properFraction' takes a real fractional number @x@ and
  -- returns a pair @(n,f)@ such that @x = n+f@, and:
  --
  -- * @n@ is an integral number with the same sign as @x@; and
  --
  -- * @f@ is a fraction with the same type and sign as @x@,
  --   and with absolute value less than @1@.
  --
  -- The default definitions of the 'ceiling', 'floor', 'truncate'
  -- and 'round' functions are in terms of 'properFraction'.
  properFraction :: (Num b, ToFloating b a, IsIntegral b) => Exp a -> (Exp b, Exp a)

  -- The function 'splitFraction' takes a real fractional number @x@ and
  -- returns a pair @(n,f)@ such that @x = n+f@, and:
  --
  -- * @n@ is an integral number with the same sign as @x@; and
  --
  -- * @f@ is a fraction with the same type as @x@ in the range [0,1). Note that
  -- this differs from 'Prelude.properFraction'.
  --
  -- splitFraction :: (Elt b, IsIntegral b) => Exp a -> (Exp b, Exp a)

  -- @fraction x@ returns @x@ with the integer part removed.
  -- fraction       :: Exp a -> Exp a

  -- properFraction is part of the standard Haskell'98 RealFrac type classes
  -- splitFraction / fraction are from numeric-prelude Algebra.RealRing

  -- | @truncate x@ returns the integer nearest @x@ between zero and @x@
  truncate       :: (Elt b, IsIntegral b) => Exp a -> Exp b

  -- | @'round' x@ returns the nearest integer to @x@; the even integer if @x@
  -- is equidistant between two integers
  round          :: (Elt b, IsIntegral b) => Exp a -> Exp b

  -- | @'ceiling' x@ returns the least integer not less than @x@
  ceiling        :: (Elt b, IsIntegral b) => Exp a -> Exp b

  -- | @'floor' x@ returns the greatest integer not greater than @x@
  floor          :: (Elt b, IsIntegral b) => Exp a -> Exp b


instance RealFrac Float where
  properFraction  = defaultProperFraction
  truncate        = mkTruncate
  round           = mkRound
  ceiling         = mkCeiling
  floor           = mkFloor

instance RealFrac Double where
  properFraction  = defaultProperFraction
  truncate        = mkTruncate
  round           = mkRound
  ceiling         = mkCeiling
  floor           = mkFloor

instance RealFrac CFloat where
  properFraction  = defaultProperFraction
  truncate        = mkTruncate
  round           = mkRound
  ceiling         = mkCeiling
  floor           = mkFloor

instance RealFrac CDouble where
  properFraction  = defaultProperFraction
  truncate        = mkTruncate
  round           = mkRound
  ceiling         = mkCeiling
  floor           = mkFloor


-- Must test for ±0.0 to avoid returning -0.0 in the second component of the
-- pair. Unfortunately the branching costs a lot of performance.
--
defaultProperFraction
    :: (ToFloating a b, RealFrac b, IsIntegral a, Num a, Floating b)
    => Exp b
    -> (Exp a, Exp b)
defaultProperFraction x =
  untup2 $ Exp
         $ Cond (x == 0) (tup2 (0, 0))
                         (tup2 (n, f))
  where
    n = truncate x
    f = x - toFloating n


-- To satisfy superclass constraints
--
instance RealFrac a => P.RealFrac (Exp a) where
  properFraction = preludeError "properFraction"
  truncate       = preludeError "truncate"
  round          = preludeError "round"
  ceiling        = preludeError "ceiling"
  floor          = preludeError "floor"

preludeError :: String -> a
preludeError x = error (printf "Prelude.%s applied to EDSL types: use Data.Array.Accelerate.%s instead" x)

