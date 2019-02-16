{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Floating
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Floating (

  Floating,
  P.pi,
  P.sin, P.cos, P.tan,
  P.asin, P.acos, P.atan,
  P.sinh, P.cosh, P.tanh,
  P.asinh, P.acosh, P.atanh,
  P.exp,
  P.sqrt,
  P.log,
  (P.**),
  P.logBase,

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Fractional

import qualified Prelude                                            as P


-- | Trigonometric and hyperbolic functions and related functions
--
type Floating a = (Fractional a, P.Floating (Exp a))


instance P.Floating (Exp Half) where
  pi      = mkPi
  sin     = mkSin
  cos     = mkCos
  tan     = mkTan
  asin    = mkAsin
  acos    = mkAcos
  atan    = mkAtan
  sinh    = mkSinh
  cosh    = mkCosh
  tanh    = mkTanh
  asinh   = mkAsinh
  acosh   = mkAcosh
  atanh   = mkAtanh
  exp     = mkExpFloating
  sqrt    = mkSqrt
  log     = mkLog
  (**)    = mkFPow
  logBase = mkLogBase

instance P.Floating (Exp Float) where
  pi      = mkPi
  sin     = mkSin
  cos     = mkCos
  tan     = mkTan
  asin    = mkAsin
  acos    = mkAcos
  atan    = mkAtan
  sinh    = mkSinh
  cosh    = mkCosh
  tanh    = mkTanh
  asinh   = mkAsinh
  acosh   = mkAcosh
  atanh   = mkAtanh
  exp     = mkExpFloating
  sqrt    = mkSqrt
  log     = mkLog
  (**)    = mkFPow
  logBase = mkLogBase

instance P.Floating (Exp Double) where
  pi      = mkPi
  sin     = mkSin
  cos     = mkCos
  tan     = mkTan
  asin    = mkAsin
  acos    = mkAcos
  atan    = mkAtan
  sinh    = mkSinh
  cosh    = mkCosh
  tanh    = mkTanh
  asinh   = mkAsinh
  acosh   = mkAcosh
  atanh   = mkAtanh
  exp     = mkExpFloating
  sqrt    = mkSqrt
  log     = mkLog
  (**)    = mkFPow
  logBase = mkLogBase

instance P.Floating (Exp CFloat) where
  pi      = mkBitcast (mkPi @Float)
  sin     = lift1 mkSin
  cos     = lift1 mkCos
  tan     = lift1 mkTan
  asin    = lift1 mkAsin
  acos    = lift1 mkAcos
  atan    = lift1 mkAtan
  sinh    = lift1 mkSinh
  cosh    = lift1 mkCosh
  tanh    = lift1 mkTanh
  asinh   = lift1 mkAsinh
  acosh   = lift1 mkAcosh
  atanh   = lift1 mkAtanh
  exp     = lift1 mkExpFloating
  sqrt    = lift1 mkSqrt
  log     = lift1 mkLog
  (**)    = lift2 mkFPow
  logBase = lift2 mkLogBase

instance P.Floating (Exp CDouble) where
  pi      = mkBitcast (mkPi @Double)
  sin     = lift1 mkSin
  cos     = lift1 mkCos
  tan     = lift1 mkTan
  asin    = lift1 mkAsin
  acos    = lift1 mkAcos
  atan    = lift1 mkAtan
  sinh    = lift1 mkSinh
  cosh    = lift1 mkCosh
  tanh    = lift1 mkTanh
  asinh   = lift1 mkAsinh
  acosh   = lift1 mkAcosh
  atanh   = lift1 mkAtanh
  exp     = lift1 mkExpFloating
  sqrt    = lift1 mkSqrt
  log     = lift1 mkLog
  (**)    = lift2 mkFPow
  logBase = lift2 mkLogBase

lift1 :: (Elt a, Elt b, IsScalar b, b ~ EltRepr a)
      => (Exp b -> Exp b)
      -> Exp a
      -> Exp a
lift1 f x = mkUnsafeCoerce (f (mkUnsafeCoerce x))

lift2 :: (Elt a, Elt b, IsScalar b, b ~ EltRepr a)
      => (Exp b -> Exp b -> Exp b)
      -> Exp a
      -> Exp a
      -> Exp a
lift2 f x y = mkUnsafeCoerce (f (mkUnsafeCoerce x) (mkUnsafeCoerce y))

