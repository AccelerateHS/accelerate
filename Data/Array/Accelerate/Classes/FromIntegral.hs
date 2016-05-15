{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.FromIntegral
-- Copyright   : [2016] Manuel M T Chakravarty, Gabriele Keller
--               [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.FromIntegral (

  FromIntegral(..),

) where

import Data.Array.Accelerate.Smart
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

-- Generate all the standard instances explicitly. This gives us sensible error
-- messages when we don't have an instance available, rather than a "can not
-- deduce IsNum..." style error (which the user can do nothing about).
--
$(runQ $ do
    let
        -- Get all the types that our dictionaries reify
        digItOut :: Name -> Q [Name]
        digItOut name = do
          TyConI (DataD _ _ _ cons _) <- reify name
          let
            dig (ForallC _ _ (NormalC _ [(_, AppT (ConT _) (ConT n))])) = return [n]
            dig (NormalC _ [(_, AppT (ConT n) (VarT _))])               = digItOut n
            dig _ = error "Unexpected case generating FromIntegral instances"
            --
          concat `fmap` mapM dig cons

        thFromIntegral :: Name -> Name -> Dec
        thFromIntegral a b =
          InstanceD [] (AppT (AppT (ConT (mkName "FromIntegral")) (ConT a)) (ConT b))
            [ ValD (VarP (mkName "fromIntegral")) (NormalB (VarE (mkName "mkFromIntegral"))) []
            ]
    --
    as <- digItOut ''IntegralType
    bs <- digItOut ''NumType
    return [ thFromIntegral a b | a <- as, b <- bs ]
 )

