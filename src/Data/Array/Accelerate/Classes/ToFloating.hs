{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.ToFloating
-- Copyright   : [2016..2017] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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
import Prelude                                                      ( ($), error, concat )


-- | Accelerate lacks an arbitrary-precision 'Prelude.Rational' type, which the
-- standard 'Prelude.realToFrac' uses as an intermediate value when coercing
-- to floating-point types. Instead, we use this class to capture a direct
-- coercion between to types.
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
            dig (NormalC _ [(_, AppT (ConT n) (VarT _))])               = digItOut n
#if __GLASGOW_HASKELL__ < 800
            dig (ForallC _ _ (NormalC _ [(_, AppT (ConT _) (ConT n))])) = return [n]
#else
            dig (GadtC _ _ (AppT (ConT _) (ConT n)))                    = return [n]
#endif
            dig _ = error "Unexpected case generating ToFloating instances"
            --
          concat `fmap` mapM dig cons

        thToFloating :: Name -> Name -> Q Dec
        thToFloating a b =
          let
              ty  = AppT (AppT (ConT (mkName "ToFloating")) (ConT a)) (ConT b)
              dec = ValD (VarP (mkName "toFloating")) (NormalB (VarE (mkName "mkToFloating"))) []
          in
          instanceD (return []) (return ty) [return dec]
    --
    as <- digItOut ''NumType
    bs <- digItOut ''FloatingType
    sequence [ thToFloating a b | a <- as, b <- bs ]
 )

