{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.FromIntegral
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.FromIntegral (

  FromIntegral(..),

) where

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Integral

import Language.Haskell.TH                                          hiding ( Exp )
import Prelude                                                      hiding ( Integral )


-- | Accelerate lacks a most-general lossless 'Prelude.Integer' type, which the
-- standard 'Prelude.fromIntegral' function uses as an intermediate value when
-- coercing from integral types. Instead, we use this class to capture a direct
-- coercion between two types.
--
class FromIntegral a b where
  -- | General coercion from integral types
  fromIntegral :: Integral a => Exp a -> Exp b

-- instance {-# OVERLAPPABLE #-} (Elt a, Elt b, IsIntegral a, IsNum b) => FromIntegral a b where
--   fromIntegral = mkFromIntegral

-- Reify in ghci:
--
-- $( stringE . show =<< reify ''Thing )

-- Generate all the standard instances explicitly. This gives us sensible error
-- messages when we don't have an instance available, rather than a "can not
-- deduce IsNum..." style error (which the user can do nothing about).
--
-- > -- Get all the types that our dictionaries reify
-- > digItOut :: Name -> Q [Name]
-- > digItOut name = do
-- >   TyConI (DataD _ _ _ _ cons _) <- reify name
-- >   let
-- >     -- This is what a constructor such as IntegralNumType will be reified
-- >     -- as prior to GHC 8.4...
-- >     dig (NormalC _ [(_, AppT (ConT n) (VarT _))])               = digItOut n
-- >     -- ...but this is what IntegralNumType will be reified as on GHC 8.4
-- >     -- and later, after the changes described in
-- >     -- https://ghc.haskell.org/trac/ghc/wiki/Migration/8.4#TemplateHaskellreificationchangesforGADTs
-- >     dig (ForallC _ _ (GadtC _ [(_, AppT (ConT n) (VarT _))] _)) = digItOut n
-- >     dig (GadtC _ _ (AppT (ConT _) (ConT n)))                    = return [n]
-- >     dig _ = error "Unexpected case generating FromIntegral instances"
-- >     --
-- >   concat `fmap` mapM dig cons
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

      thFromIntegral :: Name -> Name -> Q [Dec]
      thFromIntegral a b =
        [d| instance FromIntegral $(conT a) $(conT b) where
              fromIntegral = $(varE $ if a == b then 'id else 'mkFromIntegral )

            instance KnownNat n => FromIntegral (Vec n $(conT a)) (Vec n $(conT b)) where
              fromIntegral = $(varE $ if a == b then 'id else 'mkFromIntegral )
          |]
  in
  concat <$> sequence [ thFromIntegral from to | from <- integralTypes, to <- numTypes ]

