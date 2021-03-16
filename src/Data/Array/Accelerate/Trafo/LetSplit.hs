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
  Alet lhs bnd body               -> convertLHS lhs (convertAcc bnd) (convertAcc body)
  Avar var                        -> Avar var
  Apair a1 a2                     -> Apair (convertAcc a1) (convertAcc a2)
  Anil                            -> Anil
  Atrace msg as bs                -> Atrace msg (convertAcc as) (convertAcc bs)
  Apply repr f a                  -> Apply repr (convertAfun f) (convertAcc a)
  Aforeign repr asm f a           -> Aforeign repr asm (convertAfun f) (convertAcc a)
  Acond e a1 a2                   -> Acond e (convertAcc a1) (convertAcc a2)
  Awhile c f a                    -> Awhile (convertAfun c) (convertAfun f) (convertAcc a)
  Use repr arr                    -> Use repr arr
  Unit tp e                       -> Unit tp e
  Reshape shr e a                 -> Reshape shr e a
  Generate repr e f               -> Generate repr e f
  Transform repr sh f g a         -> Transform repr sh f g (convertAcc a)
  Replicate slix sl a             -> Replicate slix sl (convertAcc a)
  Slice slix a sl                 -> Slice slix (convertAcc a) sl
  Map ann tp f a                  -> Map ann tp f (convertAcc a)
  ZipWith tp f a1 a2              -> ZipWith tp f (convertAcc a1) (convertAcc a2)
  Fold ann f e a                  -> Fold ann f e (convertAcc a)
  FoldSeg i f e a s               -> FoldSeg i f e (convertAcc a) (convertAcc s)
  Scan d f e a                    -> Scan d f e (convertAcc a)
  Scan' d f e a                   -> Scan' d f e (convertAcc a)
  Permute f a1 g a2               -> Permute f (convertAcc a1) g (convertAcc a2)
  Backpermute shr sh f a          -> Backpermute shr sh f (convertAcc a)
  Stencil s tp f b a              -> Stencil s tp f b (convertAcc a)
  Stencil2 s1 s2 tp f b1 a1 b2 a2 -> Stencil2 s1 s2 tp f b1 (convertAcc a1) b2 (convertAcc a2)

convertLHS
    :: ALeftHandSide bnd aenv aenv'
    -> OpenAcc aenv bnd
    -> OpenAcc aenv' a
    -> PreOpenAcc OpenAcc aenv a
convertLHS lhs bnd@(OpenAcc pbnd) a@(OpenAcc pa) =
  case lhs of
    LeftHandSideWildcard{} -> pa
    LeftHandSideSingle{}   -> Alet lhs bnd a
    LeftHandSidePair l1 l2 ->
      case pbnd of
        Apair b1 b2 -> convertLHS l1 b1 (OpenAcc (convertLHS l2 (weaken (weakenWithLHS l1) b2) a))
        _           -> Alet lhs bnd a

