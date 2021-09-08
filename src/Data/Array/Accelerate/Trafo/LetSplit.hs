{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs      #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.LetSplit
-- Copyright   : [2012..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.LetSplit (

  convertAfun,
  convertAcc,

) where

import Prelude                                          hiding ( exp )
import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Environment
import Data.Array.Accelerate.Trafo.Substitution


convertAfun :: PreOpenAfun OpenAcc aenv f -> PreOpenAfun OpenAcc aenv f
convertAfun (Alam lhs f) = Alam lhs (convertAfun f)
convertAfun (Abody a)    = Abody (convertAcc a)

convertAcc :: OpenAcc aenv a -> OpenAcc aenv a
convertAcc (OpenAcc pacc) = OpenAcc (convertPreOpenAcc pacc)

convertPreOpenAcc :: PreOpenAcc OpenAcc aenv a -> PreOpenAcc OpenAcc aenv a
convertPreOpenAcc = \case
  -- FIXME: We're completely ignoring source mapping information post-sharing
  --        recovery at the moment
  Alet _ lhs bnd body                 -> convertLHS lhs (convertAcc bnd) (convertAcc body)
  Avar ann var                        -> Avar ann var
  Apair ann a1 a2                     -> Apair ann (convertAcc a1) (convertAcc a2)
  Anil ann                            -> Anil ann
  Atrace ann msg as bs                -> Atrace ann msg (convertAcc as) (convertAcc bs)
  Apply ann repr f a                  -> Apply ann repr (convertAfun f) (convertAcc a)
  Aforeign ann repr asm f a           -> Aforeign ann repr asm (convertAfun f) (convertAcc a)
  Acond ann e a1 a2                   -> Acond ann e (convertAcc a1) (convertAcc a2)
  Awhile ann c f a                    -> Awhile ann (convertAfun c) (convertAfun f) (convertAcc a)
  Use ann repr arr                    -> Use ann repr arr
  Unit ann tp e                       -> Unit ann tp e
  Reshape ann shr e a                 -> Reshape ann shr e a
  Generate ann repr e f               -> Generate ann repr e f
  Transform ann repr sh f g a         -> Transform ann repr sh f g (convertAcc a)
  Replicate ann slix sl a             -> Replicate ann slix sl (convertAcc a)
  Slice ann slix a sl                 -> Slice ann slix (convertAcc a) sl
  Map ann tp f a                      -> Map ann tp f (convertAcc a)
  ZipWith ann tp f a1 a2              -> ZipWith ann tp f (convertAcc a1) (convertAcc a2)
  Fold ann f e a                      -> Fold ann f e (convertAcc a)
  FoldSeg ann i f e a s               -> FoldSeg ann i f e (convertAcc a) (convertAcc s)
  Scan  ann d f e a                   -> Scan  ann d f e (convertAcc a)
  Scan' ann d f e a                   -> Scan' ann d f e (convertAcc a)
  Permute ann f a1 g a2               -> Permute ann f (convertAcc a1) g (convertAcc a2)
  Backpermute ann shr sh f a          -> Backpermute ann shr sh f (convertAcc a)
  Stencil ann s tp f b a              -> Stencil ann s tp f b (convertAcc a)
  Stencil2 ann s1 s2 tp f b1 a1 b2 a2 -> Stencil2 ann s1 s2 tp f b1 (convertAcc a1) b2 (convertAcc a2)

-- TODO: Propagate source mapping annotations during sharing recovery
convertLHS
    :: ALeftHandSide bnd aenv aenv'
    -> OpenAcc aenv bnd
    -> OpenAcc aenv' a
    -> PreOpenAcc OpenAcc aenv a
convertLHS lhs bnd@(OpenAcc pbnd) a@(OpenAcc pa) =
  case lhs of
    LeftHandSideWildcard{} -> pa
    LeftHandSideSingle{}   -> Alet mkDummyAnn lhs bnd a
    LeftHandSidePair l1 l2 ->
      case pbnd of
        Apair _ b1 b2 -> convertLHS l1 b1 (OpenAcc (convertLHS l2 (weaken (weakenWithLHS l1) b2) a))
        _             -> Alet mkDummyAnn lhs bnd a
