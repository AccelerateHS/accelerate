{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.FromIntegral
-- Copyright   : [2016..2017] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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


-- Reify in ghci:
--
-- $( stringE . show =<< reify ''Thing )

-- Generate all the standard instances explicitly. This gives us sensible error
-- messages when we don't have an instance available, rather than a "can not
-- deduce IsNum..." style error (which the user can do nothing about).
--
$(runQ $ do
    let
        -- Get all the types that our dictionaries reify
        digItOut :: Name -> Q [Name]
        digItOut name = do
#if __GLASGOW_HASKELL__ < 800
          TyConI (DataD _ _ _   cons _) <- reify name
#else
          TyConI (DataD _ _ _ _ cons _) <- reify name
#endif
          let
            dig (NormalC _ [(_, AppT (ConT n) (VarT _))])               = digItOut n
#if __GLASGOW_HASKELL__ < 800
            dig (ForallC _ _ (NormalC _ [(_, AppT (ConT _) (ConT n))])) = return [n]
#else
            dig (GadtC _ _ (AppT (ConT _) (ConT n)))                    = return [n]
#endif
            dig _ = error "Unexpected case generating FromIntegral instances"
            --
          concat `fmap` mapM dig cons

        thFromIntegral :: Name -> Name -> Q Dec
        thFromIntegral a b =
          let
              ty  = AppT (AppT (ConT (mkName "FromIntegral")) (ConT a)) (ConT b)
              dec = ValD (VarP (mkName "fromIntegral")) (NormalB (VarE (mkName "mkFromIntegral"))) []
          in
          instanceD (return []) (return ty) [return dec]
    --
    as <- digItOut ''IntegralType
    bs <- digItOut ''NumType
    sequence [ thFromIntegral a b | a <- as, b <- bs ]
 )

