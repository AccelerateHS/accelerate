{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}
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

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type
import qualified Data.Primitive.Vec                                 as Prim

import Data.Array.Accelerate.Classes.Fractional

import Language.Haskell.TH                                          hiding ( Exp )
import Prelude                                                      hiding ( Fractional, Floating )
import qualified Prelude                                            as P


-- | Trigonometric and hyperbolic functions and related functions
--
type Floating a = (Fractional a, P.Floating (Exp a))

runQ $
  let
      floatingTypes :: [Name]
      floatingTypes =
        [ ''Half
        , ''Float
        , ''Double
        , ''Float128
        ]

      thFloating :: Name -> Q [Dec]
      thFloating a =
        [d| instance P.Floating (Exp $(conT a)) where
              pi      = constant pi
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

            instance KnownNat n => P.Floating (Exp (Vec n $(conT a))) where
              pi      = constant (Vec (Prim.splat pi))
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
          |]
  in
  concat <$> mapM thFloating floatingTypes

