{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Floating
-- Copyright   : [2016..2017] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Fractional

import qualified Prelude                                            as P


-- | Trigonometric and hyperbolic functions and related functions
--
type Floating a = (Fractional a, P.Floating (Exp a))


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

instance P.Floating (Exp CDouble) where
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

