{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TemplateHaskell       #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.VNum
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.VNum (

  VNum(..),

) where

import Data.Array.Accelerate.AST                                    ( PrimFun(..) )
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Smart

import Language.Haskell.TH.Extra                                    hiding ( Type, Exp )


-- | The 'VNum' class defines numeric operations over SIMD vectors.
--
-- @since 1.4.0.0
--
class SIMD n a => VNum n a where
  -- | Horizontal reduction of a vector with addition. This operation is not
  -- guaranteed to preserve the associativity of an equivalent scalarised
  -- counterpart.
  vadd :: Exp (Vec n a) -> Exp a

  -- | Horizontal reduction of a vector with multiplication. This operation is
  -- not guaranteed to preserve the associativity of an equivalent scalarised
  -- counterpart.
  vmul :: Exp (Vec n a) -> Exp a


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

      floatingTypes :: [Name]
      floatingTypes =
        [ ''Half
        , ''Float
        , ''Double
        , ''Float128
        ]

      numTypes :: [Name]
      numTypes = integralTypes ++ floatingTypes

      thVNum :: Name -> Q [Dec]
      thVNum name =
        [d| instance KnownNat n => VNum n $(conT name) where
              vadd = mkPrimUnary $ PrimVAdd numType
              vmul = mkPrimUnary $ PrimVMul numType
          |]
  in
  concat <$> mapM thVNum numTypes

