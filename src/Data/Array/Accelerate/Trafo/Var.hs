{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Var
-- Copyright   : [2012..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Var
  where

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.AST.Environment
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Type


data DeclareVars s t aenv where
  DeclareVars :: LeftHandSide s t env env'
              -> (env :> env')
              -> (forall env''. env' :> env'' -> Vars s env'' t)
              -> DeclareVars s t env

declareVars :: TupR s t -> DeclareVars s t env
declareVars TupRunit
  = DeclareVars LeftHandSideUnit weakenId $ const $ TupRunit
declareVars (TupRsingle s)
  = DeclareVars (LeftHandSideSingle s) (weakenSucc weakenId) $ \k -> TupRsingle $ Var s $ k >:> ZeroIdx
declareVars (TupRpair r1 r2)
  | DeclareVars lhs1 subst1 a1 <- declareVars r1
  , DeclareVars lhs2 subst2 a2 <- declareVars r2
  = DeclareVars (LeftHandSidePair lhs1 lhs2) (subst2 .> subst1) $ \k -> a1 (k .> subst2) `TupRpair` a2 k


type InjectAcc  acc = forall env t. PreOpenAcc acc env t -> acc env t
type ExtractAcc acc = forall env t. acc env t -> Maybe (PreOpenAcc acc env t)

-- TODO: Where should we get the annotations (for source mapping) belonging to
--       the referenced variables from?
avarIn :: InjectAcc acc
       -> ArrayVar aenv a
       -> acc aenv a
avarIn inject v@(Var ArrayR{} _) = inject (Avar mkDummyAnn v)

-- TODO: Same as the above
avarsIn :: forall acc aenv arrs.
           InjectAcc acc
        -> ArrayVars aenv arrs
        -> acc aenv arrs
avarsIn inject = go
  where
    go :: ArrayVars aenv t -> acc aenv t
    go TupRunit       = inject (Anil mkDummyAnn)
    go (TupRsingle v) = avarIn inject v
    go (TupRpair a b) = inject (Apair mkDummyAnn (go a) (go b))

avarsOut
    :: ExtractAcc acc
    -> PreOpenAcc acc aenv a
    -> Maybe (ArrayVars aenv a)
avarsOut extract = \case
  Anil _   -> Just $ TupRunit
  Avar _ v -> Just $ TupRsingle v
  Apair _ al ar
    | Just pl <- extract al
    , Just pr <- extract ar
    , Just as <- avarsOut extract pl
    , Just bs <- avarsOut extract pr
    -> Just (TupRpair as bs)
  _ -> Nothing

