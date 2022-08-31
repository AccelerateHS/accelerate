{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Integral
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Integral (

  Integral,
  P.quot,
  P.rem,
  P.div,
  P.mod,
  P.quotRem,
  P.divMod,

) where

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Enum
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Classes.Real                           ()

import Control.Monad
import Language.Haskell.TH                                          hiding ( Exp )
import Prelude                                                      hiding ( Enum, Ord, Num, Integral )
import qualified Prelude                                            as P


-- | Integral numbers, supporting integral division
--
type Integral a = (Enum a, Ord a, Num a, P.Integral (Exp a))

runQ $
  let
      integralTypes :: [Name]
      integralTypes =
        [ ''Int
        , ''Int8
        , ''Int16
        , ''Int32
        , ''Int64
        , ''Int128
        , ''Word
        , ''Word8
        , ''Word16
        , ''Word32
        , ''Word64
        , ''Word128
        ]

      mkIntegral :: Name -> Q [Dec]
      mkIntegral a =
        [d| instance P.Integral (Exp $(conT a)) where
              quot      = mkQuot
              rem       = mkRem
              div       = mkIDiv
              mod       = mkMod
              quotRem   = mkQuotRem
              divMod    = mkDivMod
              toInteger = P.error "Prelude.toInteger not supported for Accelerate types"

            instance KnownNat n => P.Integral (Exp (Vec n $(conT a))) where
              quot      = mkQuot
              rem       = mkRem
              div       = mkIDiv
              mod       = mkMod
              quotRem   = mkQuotRem
              divMod    = mkDivMod
              toInteger = P.error "Prelude.toInteger not supported for Accelerate types"
          |]
  in
  concat <$> mapM mkIntegral integralTypes

