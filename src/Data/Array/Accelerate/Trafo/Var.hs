{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
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

module Data.Array.Accelerate.Trafo.Var (

  FreeAnn(..), AnnR, HasAnnR(..), annRfromTup,

  DeclareVars(..), InjectAcc, ExtractAcc,
  declareVars, avarIn, avarsIn, avarsOut,

) where

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.AST.Environment
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Type
import qualified Data.Array.Accelerate.Smart as S


newtype FreeAnn a = FreeAnn Ann
type AnnR = TupR FreeAnn

class HasAnnR s where
  type ShapeRepr s a
  annR :: s a -> ShapeRepr s a -> AnnR a

instance HasAnnR S.SmartAcc where
  type ShapeRepr S.SmartAcc a = ArraysR a
  annR = annR'
    where
      annR' :: S.SmartAcc arrs -> ArraysR arrs -> AnnR arrs
      annR' _                              TupRunit         = TupRunit
      annR' v                              (TupRsingle _)   = TupRsingle (FreeAnn $ extractAnn v)
      annR' (S.SmartAcc (S.Apair _ v1 v2)) (TupRpair r1 r2) = TupRpair (annR' v1 r1) (annR' v2 r2)
      -- For non-tuple types we can only duplicate the existing annotation
      annR' v                              (TupRpair r1 r2) = TupRpair (annRfromTup (extractAnn v) r1) (annRfromTup (extractAnn v) r2)

instance HasAnnR (OpenAcc aenv) where
  type ShapeRepr (OpenAcc aenv) a = ArraysR a
  annR = annR'
    where
      annR' :: OpenAcc aenv arrs -> ArraysR arrs -> AnnR arrs
      annR' _                         TupRunit         = TupRunit
      annR' v                         (TupRsingle _)   = TupRsingle (FreeAnn $ extractAnn v)
      annR' (OpenAcc (Apair _ v1 v2)) (TupRpair r1 r2) = TupRpair (annR' v1 r1) (annR' v2 r2)
      annR' v                         (TupRpair r1 r2) = TupRpair (annRfromTup (extractAnn v) r1) (annRfromTup (extractAnn v) r2)

instance HasAnnR S.SmartExp where
  type ShapeRepr S.SmartExp a = TypeR a
  annR = annR'
    where
      annR' :: S.SmartExp t -> TypeR t -> AnnR t
      annR' _                             TupRunit         = TupRunit
      annR' v                             (TupRsingle _)   = TupRsingle (FreeAnn $ extractAnn v)
      annR' (S.SmartExp (S.Pair _ v1 v2)) (TupRpair r1 r2) = TupRpair (annR' v1 r1) (annR' v2 r2)
      annR' v                             (TupRpair r1 r2) = TupRpair (annRfromTup (extractAnn v) r1) (annRfromTup (extractAnn v) r2)

instance HasAnnR (OpenExp env aenv) where
  type ShapeRepr (OpenExp env aenv) a = TypeR a
  annR = annR'
    where
      annR' :: OpenExp env aenv t -> TypeR t -> AnnR t
      annR' _              TupRunit         = TupRunit
      annR' v              (TupRsingle _)   = TupRsingle (FreeAnn $ extractAnn v)
      annR' (Pair _ v1 v2) (TupRpair r1 r2) = TupRpair (annR' v1 r1) (annR' v2 r2)
      annR' v              (TupRpair r1 r2) = TupRpair (annRfromTup (extractAnn v) r1) (annRfromTup (extractAnn v) r2)

-- | Used for function parameters where we don't have a concrete type yet.
annRfromTup :: Ann -> TupR s t -> AnnR t
annRfromTup _   TupRunit         = TupRunit
annRfromTup ann (TupRsingle _ )  = TupRsingle (FreeAnn ann)
annRfromTup ann (TupRpair t1 t2) = TupRpair (annRfromTup ann t1) (annRfromTup ann t2)


data DeclareVars s t aenv where
  DeclareVars :: LeftHandSide s t env env'
              -> (env :> env')
              -> (forall env''. env' :> env'' -> Vars s env'' t)
              -> DeclareVars s t env

-- TODO: Can this just be a single 'Ann' instead of this 'AnnR'? Ergo, is there
--       a situation where we need to declare variables for a group of values
--       that don't all come from the same source (and have the exact same
--       annotations)?
declareVars :: AnnR t -> TupR s t -> DeclareVars s t env
declareVars TupRunit TupRunit
  = DeclareVars LeftHandSideUnit weakenId $ const $ TupRunit
declareVars (TupRsingle (FreeAnn ann)) (TupRsingle s)
  = DeclareVars (LeftHandSideSingle ann s) (weakenSucc weakenId) $ \k -> TupRsingle $ Var ann s $ k >:> ZeroIdx
declareVars (TupRpair ann1 ann2) (TupRpair r1 r2)
  | DeclareVars lhs1 subst1 a1 <- declareVars ann1 r1
  , DeclareVars lhs2 subst2 a2 <- declareVars ann2 r2
  = DeclareVars (LeftHandSidePair lhs1 lhs2) (subst2 .> subst1) $ \k -> a1 (k .> subst2) `TupRpair` a2 k
-- TODO: Is there a cleaner way to handle this? The patterns technically aren't
--       exhaustive because type parameter @t@ used in 'TupRsingle' can also be
--       a tuple itself, but in normal situations this should never be hit.
declareVars _ _ = error "Is it unsafeCoerce time again?"


type InjectAcc  acc = forall env t. PreOpenAcc acc env t -> acc env t
type ExtractAcc acc = forall env t. acc env t -> Maybe (PreOpenAcc acc env t)

avarIn :: InjectAcc acc
       -> ArrayVar aenv a
       -> acc aenv a
avarIn inject v@(Var _ ArrayR{} _) = inject (Avar v)

-- TODO: Decide for this (and the other places where we're creating pairs of
--       Vars) whether the pair itself should have source annotations. Right now
--       we inconsistently give pairs no annotations, propagate some other
--       annotation, or do @extractAnn a <> extractAnn b@.
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
  Anil _ -> Just TupRunit
  Avar v -> Just $ TupRsingle v
  Apair _ al ar
    | Just pl <- extract al
    , Just pr <- extract ar
    , Just as <- avarsOut extract pl
    , Just bs <- avarsOut extract pr
    -> Just (TupRpair as bs)
  _ -> Nothing

