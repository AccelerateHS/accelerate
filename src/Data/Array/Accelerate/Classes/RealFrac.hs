{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.RealFrac
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.RealFrac (

  RealFrac(..),
  div', mod', divMod',

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Language                               ( (^), cond, even )
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Classes.Floating
import Data.Array.Accelerate.Classes.Fractional
import Data.Array.Accelerate.Classes.FromIntegral
import Data.Array.Accelerate.Classes.Integral
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.ToFloating
import {-# SOURCE #-} Data.Array.Accelerate.Classes.RealFloat       -- defaultProperFraction

import Data.Typeable
import Data.Maybe
import Text.Printf
import Prelude                                                      ( ($), String, error, unlines, otherwise )
import qualified Prelude                                            as P


-- | Generalisation of 'P.div' to any instance of 'RealFrac'
--
div' :: (RealFrac a, FromIntegral Int64 b, Integral b) => Exp a -> Exp a -> Exp b
div' n d = floor (n / d)

-- | Generalisation of 'P.mod' to any instance of 'RealFrac'
--
mod' :: (Floating a, RealFrac a, ToFloating Int64 a) => Exp a -> Exp a -> Exp a
mod' n d = n - (toFloating f) * d
  where
    f :: Exp Int64
    f = div' n d

-- | Generalisation of 'P.divMod' to any instance of 'RealFrac'
--
divMod'
    :: (Floating a, RealFrac a, Integral b, FromIntegral Int64 b, ToFloating b a)
    => Exp a
    -> Exp a
    -> (Exp b, Exp a)
divMod' n d = (f, n - (toFloating f) * d)
  where
    f = div' n d


-- | Extracting components of fractions.
--
class (Ord a, Fractional a) => RealFrac a where
  -- | The function 'properFraction' takes a real fractional number @x@ and
  -- returns a pair @(n,f)@ such that @x = n+f@, and:
  --
  -- * @n@ is an integral number with the same sign as @x@; and
  --
  -- * @f@ is a fraction with the same type and sign as @x@,
  --   and with absolute value less than @1@.
  --
  -- The default definitions of the 'ceiling', 'floor', 'truncate'
  -- and 'round' functions are in terms of 'properFraction'.
  properFraction :: (Integral b, FromIntegral Int64 b) => Exp a -> (Exp b, Exp a)

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
  truncate :: (Integral b, FromIntegral Int64 b) => Exp a -> Exp b
  truncate = defaultTruncate

  -- | @'round' x@ returns the nearest integer to @x@; the even integer if @x@
  -- is equidistant between two integers
  round    :: (Integral b, FromIntegral Int64 b) => Exp a -> Exp b
  round    = defaultRound

  -- | @'ceiling' x@ returns the least integer not less than @x@
  ceiling  :: (Integral b, FromIntegral Int64 b) => Exp a -> Exp b
  ceiling  = defaultCeiling

  -- | @'floor' x@ returns the greatest integer not greater than @x@
  floor    :: (Integral b, FromIntegral Int64 b) => Exp a -> Exp b
  floor    = defaultFloor

instance RealFrac Half where
  properFraction  = defaultProperFraction

instance RealFrac Float where
  properFraction  = defaultProperFraction

instance RealFrac Double where
  properFraction  = defaultProperFraction

instance RealFrac CFloat where
  properFraction  = defaultProperFraction
  truncate        = lift1 defaultTruncate
  round           = lift1 defaultRound
  ceiling         = lift1 defaultCeiling
  floor           = lift1 defaultFloor

instance RealFrac CDouble where
  properFraction  = defaultProperFraction
  truncate        = lift1 defaultTruncate
  round           = lift1 defaultRound
  ceiling         = lift1 defaultCeiling
  floor           = lift1 defaultFloor


-- Must test for ±0.0 to avoid returning -0.0 in the second component of the
-- pair. Unfortunately the branching costs a lot of performance.
--
-- defaultProperFraction
--     :: (ToFloating b a, RealFrac a, IsIntegral b, Num b, Floating a)
--     => Exp a
--     -> (Exp b, Exp a)
-- defaultProperFraction x =
--   untup2 $ Exp
--          $ Cond (x == 0) (tup2 (0, 0))
--                          (tup2 (n, f))
--   where
--     n = truncate x
--     f = x - toFloating n

defaultProperFraction
    :: (RealFloat a, FromIntegral Int64 b, Integral b)
    => Exp a
    -> (Exp b, Exp a)
defaultProperFraction x
  = untup2
  $ cond (n >= 0)
      (tup2 (fromIntegral m * (2 ^ n), 0.0))
      (tup2 (fromIntegral q, encodeFloat r n))
  where
    (m, n) = decodeFloat x
    (q, r) = quotRem m (2 ^ (negate n))

defaultTruncate :: forall a b. (RealFrac a, Integral b, FromIntegral Int64 b) => Exp a -> Exp b
defaultTruncate x
  | Just IsFloatingDict <- isFloating @a
  , Just IsIntegralDict <- isIntegral @b
  = mkTruncate x
  --
  | otherwise
  = let (n, _) = properFraction x in n

defaultCeiling :: forall a b. (RealFrac a, Integral b, FromIntegral Int64 b) => Exp a -> Exp b
defaultCeiling x
  | Just IsFloatingDict <- isFloating @a
  , Just IsIntegralDict <- isIntegral @b
  = mkCeiling x
  --
  | otherwise
  = let (n, r) = properFraction x in cond (r > 0) (n+1) n

defaultFloor :: forall a b. (RealFrac a, Integral b, FromIntegral Int64 b) => Exp a -> Exp b
defaultFloor x
  | Just IsFloatingDict <- isFloating @a
  , Just IsIntegralDict <- isIntegral @b
  = mkFloor x
  --
  | otherwise
  = let (n, r) = properFraction x in cond (r < 0) (n-1) n

defaultRound :: forall a b. (RealFrac a, Integral b, FromIntegral Int64 b) => Exp a -> Exp b
defaultRound x
  | Just IsFloatingDict <- isFloating @a
  , Just IsIntegralDict <- isIntegral @b
  = mkRound x
  --
  | otherwise
  = let (n, r)    = properFraction x
        m         = cond (r < 0.0) (n-1) (n+1)
        half_down = abs r - 0.5
        p         = compare half_down 0.0
    in
    cond (constant LT == p) n                   $
    cond (constant EQ == p) (cond (even n) n m) $
            {- otherwise -} m


data IsFloatingDict a where
  IsFloatingDict :: IsFloating a => IsFloatingDict a

data IsIntegralDict a where
  IsIntegralDict :: IsIntegral a => IsIntegralDict a

isFloating :: forall a. Elt a => Maybe (IsFloatingDict a)
isFloating
  | Just Refl          <- eqT @a @(EltRepr a)
  , TypeRscalar t      <- eltType @a
  , SingleScalarType s <- t
  , NumSingleType n    <- s
  , FloatingNumType f  <- n
  = case f of
      TypeHalf{}   -> Just IsFloatingDict
      TypeFloat{}  -> Just IsFloatingDict
      TypeDouble{} -> Just IsFloatingDict
  --
  | otherwise
  = Nothing

isIntegral :: forall a. Elt a => Maybe (IsIntegralDict a)
isIntegral
  | Just Refl          <- eqT @a @(EltRepr a)
  , TypeRscalar t      <- eltType @a
  , SingleScalarType s <- t
  , NumSingleType n    <- s
  , IntegralNumType i  <- n
  = case i of
      TypeInt{}    -> Just IsIntegralDict
      TypeInt8{}   -> Just IsIntegralDict
      TypeInt16{}  -> Just IsIntegralDict
      TypeInt32{}  -> Just IsIntegralDict
      TypeInt64{}  -> Just IsIntegralDict
      TypeWord{}   -> Just IsIntegralDict
      TypeWord8{}  -> Just IsIntegralDict
      TypeWord16{} -> Just IsIntegralDict
      TypeWord32{} -> Just IsIntegralDict
      TypeWord64{} -> Just IsIntegralDict
  --
  | otherwise
  = Nothing


-- To satisfy superclass constraints
--
instance RealFrac a => P.RealFrac (Exp a) where
  properFraction = preludeError "properFraction"
  truncate       = preludeError "truncate"
  round          = preludeError "round"
  ceiling        = preludeError "ceiling"
  floor          = preludeError "floor"

preludeError :: String -> a
preludeError x
  = error
  $ unlines [ printf "Prelude.%s applied to EDSL types: use Data.Array.Accelerate.%s instead" x x
            , ""
            , "These Prelude.RealFrac instances are present only to fulfil superclass"
            , "constraints for subsequent classes in the standard Haskell numeric hierarchy."
            ]

lift1 :: (Elt a, Elt b, Elt c, IsScalar b, b ~ EltRepr a)
      => (Exp b -> Exp c)
      -> Exp a
      -> Exp c
lift1 f x = f (mkUnsafeCoerce x)

