{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.FromBool
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.FromBool (

  FromBool(..),

) where

import Data.Array.Accelerate.AST                                    ( PrimFun(..) )
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Integral

import Language.Haskell.TH                                          hiding ( Exp )
import Prelude                                                      hiding ( Integral )


-- | Convert from Bool to integral types
--
-- @since 1.4.0.0
--
class FromBool a b where
  fromBool :: Integral b => Exp a -> Exp b


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

      thFromBool :: Name -> Q [Dec]
      thFromBool b =
          [d| instance FromBool Bool $(conT b) where
                fromBool = mkPrimUnary $ PrimFromBool bitType integralType

              instance KnownNat n => FromBool (Vec n Bool) (Vec n $(conT b)) where
                fromBool = mkPrimUnary $ PrimFromBool bitType integralType
            |]
  in
  concat <$> mapM thFromBool integralTypes


