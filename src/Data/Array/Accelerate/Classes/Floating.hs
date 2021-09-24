{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Floating
-- Copyright   : [2016..2020] The Accelerate Team
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

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Fractional

import Prelude                                                      ( ($) )
import qualified Prelude                                            as P


-- | Trigonometric and hyperbolic functions and related functions
--
type Floating a = (Fractional a, P.Floating (Exp a))


instance P.Floating (Exp Half) where
  pi      = sourceMapRuntime mkPi
  sin     = sourceMapRuntime mkSin
  cos     = sourceMapRuntime mkCos
  tan     = sourceMapRuntime mkTan
  asin    = sourceMapRuntime mkAsin
  acos    = sourceMapRuntime mkAcos
  atan    = sourceMapRuntime mkAtan
  sinh    = sourceMapRuntime mkSinh
  cosh    = sourceMapRuntime mkCosh
  tanh    = sourceMapRuntime mkTanh
  asinh   = sourceMapRuntime mkAsinh
  acosh   = sourceMapRuntime mkAcosh
  atanh   = sourceMapRuntime mkAtanh
  exp     = sourceMapRuntime mkExpFloating
  sqrt    = sourceMapRuntime mkSqrt
  log     = sourceMapRuntime mkLog
  (**)    = sourceMapRuntime mkFPow
  logBase = sourceMapRuntime mkLogBase

instance P.Floating (Exp Float) where
  pi      = sourceMapRuntime mkPi
  sin     = sourceMapRuntime mkSin
  cos     = sourceMapRuntime mkCos
  tan     = sourceMapRuntime mkTan
  asin    = sourceMapRuntime mkAsin
  acos    = sourceMapRuntime mkAcos
  atan    = sourceMapRuntime mkAtan
  sinh    = sourceMapRuntime mkSinh
  cosh    = sourceMapRuntime mkCosh
  tanh    = sourceMapRuntime mkTanh
  asinh   = sourceMapRuntime mkAsinh
  acosh   = sourceMapRuntime mkAcosh
  atanh   = sourceMapRuntime mkAtanh
  exp     = sourceMapRuntime mkExpFloating
  sqrt    = sourceMapRuntime mkSqrt
  log     = sourceMapRuntime mkLog
  (**)    = sourceMapRuntime mkFPow
  logBase = sourceMapRuntime mkLogBase

instance P.Floating (Exp Double) where
  pi      = sourceMapRuntime mkPi
  sin     = sourceMapRuntime mkSin
  cos     = sourceMapRuntime mkCos
  tan     = sourceMapRuntime mkTan
  asin    = sourceMapRuntime mkAsin
  acos    = sourceMapRuntime mkAcos
  atan    = sourceMapRuntime mkAtan
  sinh    = sourceMapRuntime mkSinh
  cosh    = sourceMapRuntime mkCosh
  tanh    = sourceMapRuntime mkTanh
  asinh   = sourceMapRuntime mkAsinh
  acosh   = sourceMapRuntime mkAcosh
  atanh   = sourceMapRuntime mkAtanh
  exp     = sourceMapRuntime mkExpFloating
  sqrt    = sourceMapRuntime mkSqrt
  log     = sourceMapRuntime mkLog
  (**)    = sourceMapRuntime mkFPow
  logBase = sourceMapRuntime mkLogBase

instance P.Floating (Exp CFloat) where
  pi      = sourceMapRuntime $ mkBitcast (mkPi @Float)
  sin     = sourceMapRuntime mkSin
  cos     = sourceMapRuntime mkCos
  tan     = sourceMapRuntime mkTan
  asin    = sourceMapRuntime mkAsin
  acos    = sourceMapRuntime mkAcos
  atan    = sourceMapRuntime mkAtan
  sinh    = sourceMapRuntime mkSinh
  cosh    = sourceMapRuntime mkCosh
  tanh    = sourceMapRuntime mkTanh
  asinh   = sourceMapRuntime mkAsinh
  acosh   = sourceMapRuntime mkAcosh
  atanh   = sourceMapRuntime mkAtanh
  exp     = sourceMapRuntime mkExpFloating
  sqrt    = sourceMapRuntime mkSqrt
  log     = sourceMapRuntime mkLog
  (**)    = sourceMapRuntime mkFPow
  logBase = sourceMapRuntime mkLogBase

instance P.Floating (Exp CDouble) where
  pi      = sourceMapRuntime $ mkBitcast (mkPi @Double)
  sin     = sourceMapRuntime mkSin
  cos     = sourceMapRuntime mkCos
  tan     = sourceMapRuntime mkTan
  asin    = sourceMapRuntime mkAsin
  acos    = sourceMapRuntime mkAcos
  atan    = sourceMapRuntime mkAtan
  sinh    = sourceMapRuntime mkSinh
  cosh    = sourceMapRuntime mkCosh
  tanh    = sourceMapRuntime mkTanh
  asinh   = sourceMapRuntime mkAsinh
  acosh   = sourceMapRuntime mkAcosh
  atanh   = sourceMapRuntime mkAtanh
  exp     = sourceMapRuntime mkExpFloating
  sqrt    = sourceMapRuntime mkSqrt
  log     = sourceMapRuntime mkLog
  (**)    = sourceMapRuntime mkFPow
  logBase = sourceMapRuntime mkLogBase
