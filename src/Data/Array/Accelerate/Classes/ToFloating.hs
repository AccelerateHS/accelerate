{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.ToFloating
-- Copyright   : [2016..2019] The Accelerate Team
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
            -- This is what a constructor such as IntegralNumType will be reified
            -- as prior to GHC 8.4...
            dig (NormalC _ [(_, AppT (ConT n) (VarT _))])               = digItOut n
#if __GLASGOW_HASKELL__ < 800
            dig (ForallC _ _ (NormalC _ [(_, AppT (ConT _) (ConT n))])) = return [n]
#else
            -- ...but this is what IntegralNumType will be reified as on GHC 8.4
            -- and later, after the changes described in
            -- https://ghc.haskell.org/trac/ghc/wiki/Migration/8.4#TemplateHaskellreificationchangesforGADTs
            dig (ForallC _ _ (GadtC _ [(_, AppT (ConT n) (VarT _))] _)) = digItOut n
            dig (GadtC _ _ (AppT (ConT _) (ConT n)))                    = return [n]
#endif
            dig _ = error "Unexpected case generating ToFloating instances"
            --
          concat `fmap` mapM dig cons

        thToFloating :: Name -> Name -> Q Dec
        thToFloating a b =
          let
              ty  = AppT (AppT (ConT (mkName "ToFloating")) (ConT a)) (ConT b)
              dec = ValD (VarP (mkName "toFloating")) (NormalB (VarE (mkName f))) []
              f | a == b    = "id"
                | otherwise = "mkToFloating"
          in
          instanceD (return []) (return ty) [return dec]
    --
    as <- digItOut ''NumType
    bs <- digItOut ''FloatingType
    sequence [ thToFloating a b | a <- as, b <- bs ]
 )

