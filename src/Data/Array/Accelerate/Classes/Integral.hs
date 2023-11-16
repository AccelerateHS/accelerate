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

import Data.Array.Accelerate.AST                                    ( PrimFun(..) )
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Sugar.Elt
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

mkQuotRem :: IsIntegral (EltR t) => Exp t -> Exp t -> (Exp t, Exp t)
mkQuotRem (Exp x) (Exp y) =
  let r = SmartExp $ PrimQuotRem integralType `PrimApp` SmartExp (Pair x y)
   in ( mkExp $ Prj PairIdxLeft  r
      , mkExp $ Prj PairIdxRight r)

mkDivMod :: IsIntegral (EltR t) => Exp t -> Exp t -> (Exp t, Exp t)
mkDivMod (Exp x) (Exp y) =
  let r = SmartExp $ PrimDivMod integralType `PrimApp` SmartExp (Pair x y)
   in ( mkExp $ Prj PairIdxLeft  r
      , mkExp $ Prj PairIdxRight r)

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
              quot      = mkPrimBinary $ PrimQuot integralType
              rem       = mkPrimBinary $ PrimRem integralType
              div       = mkPrimBinary $ PrimIDiv integralType
              mod       = mkPrimBinary $ PrimMod integralType
              quotRem   = mkQuotRem
              divMod    = mkDivMod
              toInteger = P.error "Prelude.toInteger not supported for Accelerate types"

            instance KnownNat n => P.Integral (Exp (Vec n $(conT a))) where
              quot      = mkPrimBinary $ PrimQuot integralType
              rem       = mkPrimBinary $ PrimRem integralType
              div       = mkPrimBinary $ PrimIDiv integralType
              mod       = mkPrimBinary $ PrimMod integralType
              quotRem   = mkQuotRem
              divMod    = mkDivMod
              toInteger = P.error "Prelude.toInteger not supported for Accelerate types"
          |]
  in
  concat <$> mapM mkIntegral integralTypes

