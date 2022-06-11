{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.ToFloating
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.ToFloating (

  ToFloating(..),

) where

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Floating
import Data.Array.Accelerate.Classes.Num

import Language.Haskell.TH                                          hiding ( Exp )
import Control.Monad
import Prelude                                                      hiding ( Num, Floating )


-- | Accelerate lacks an arbitrary-precision 'Prelude.Rational' type, which the
-- standard 'Prelude.realToFrac' uses as an intermediate value when coercing
-- to floating-point types. Instead, we use this class to capture a direct
-- coercion between two types.
--
class ToFloating a b where
  -- | General coercion to floating types
  toFloating :: (Num a, Floating b) => Exp a -> Exp b

-- instance (Elt a, Elt b, IsNum a, IsFloating b) => ToFloating a b where
--   toFloating = mkToFloating


-- Generate standard instances explicitly. See also: 'FromIntegral'.
--
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

      thToFloating :: Name -> Name -> Q [Dec]
      thToFloating a b =
        [d| instance ToFloating $(conT a) $(conT b) where
              toFloating = $(varE $ if a == b then 'id else 'mkToFloating)

            instance KnownNat n => ToFloating (Vec n $(conT a)) (Vec n $(conT b)) where
              toFloating = $(varE $ if a == b then 'id else 'mkToFloating)
          |]
  in
  concat <$> sequence [ thToFloating from to | from <- numTypes, to <- floatingTypes ]

