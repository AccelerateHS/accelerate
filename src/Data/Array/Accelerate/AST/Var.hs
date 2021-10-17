{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.AST.Var
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.AST.Var
  where

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.AST.Idx

import Language.Haskell.TH.Extra


data Var  s env t = Var Ann (s t) (Idx env t)
type Vars s env   = TupR (Var s env)

varsType :: Vars s env t -> TupR s t
varsType TupRunit                 = TupRunit
varsType (TupRsingle (Var _ t _)) = TupRsingle t
varsType (TupRpair a b)           = TupRpair (varsType a) (varsType b)


rnfVar :: (forall b. s b -> ()) -> Var s env t -> ()
rnfVar f (Var ann t idx) = rnfAnn ann `seq` f t `seq` rnfIdx idx

rnfVars :: (forall b. s b -> ()) -> Vars s env t -> ()
rnfVars f = rnfTupR (rnfVar f)

liftVar :: (forall b. s b -> CodeQ (s b)) -> Var s env t -> CodeQ (Var s env t)
liftVar f (Var ann s idx) = [|| Var $$(liftAnn ann) $$(f s) $$(liftIdx idx) ||]

liftVars :: (forall b. s b -> CodeQ (s b)) -> Vars s env t -> CodeQ (Vars s env t)
liftVars f = liftTupR (liftVar f)

