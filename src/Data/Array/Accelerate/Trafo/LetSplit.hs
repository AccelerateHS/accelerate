{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.LetSplit
-- Copyright   : [2012..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.LetSplit (

  convertAcc, convertAfun

) where

import Prelude                                          hiding ( exp )
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Trafo.Base

convertAcc :: Kit acc => acc aenv a -> acc aenv a
convertAcc acc = case extract acc of
  Just a  -> travA a
  Nothing -> acc

travA :: Kit acc => PreOpenAcc acc aenv a -> acc aenv a
travA (Alet lhs bnd body)               = travBinding lhs (convertAcc bnd) (convertAcc body)
travA (Avar var)                        = inject $ Avar var
travA (Apair a1 a2)                     = inject $ Apair (convertAcc a1) (convertAcc a2)
travA Anil                              = inject $ Anil
travA (Apply repr f a)                  = inject $ Apply repr (convertAfun f) (convertAcc a)
travA (Aforeign repr asm f a)           = inject $ Aforeign repr asm (convertAfun f) (convertAcc a)
travA (Acond e a1 a2)                   = inject $ Acond (travE e) (convertAcc a1) (convertAcc a2)
travA (Awhile c f a)                    = inject $ Awhile (convertAfun c) (convertAfun f) (convertAcc a)
travA (Use repr arr)                    = inject $ Use repr arr
travA (Unit tp e)                       = inject $ Unit tp (travE e)
travA (Reshape shr e a)                 = inject $ Reshape shr (travE e) a
travA (Generate repr e f)               = inject $ Generate repr (travE e) (travF f)
travA (Transform repr sh f g a)         = inject $ Transform repr (travE sh) (travF f) (travF g) (convertAcc a)
travA (Replicate slix sl a)             = inject $ Replicate slix (travE sl) (convertAcc a)
travA (Slice slix a sl)                 = inject $ Slice slix (convertAcc a) (travE sl)
travA (Map tp f a)                      = inject $ Map tp (travF f) (convertAcc a)
travA (ZipWith tp f a1 a2)              = inject $ ZipWith tp (travF f) (convertAcc a1) (convertAcc a2)
travA (Fold f e a)                      = inject $ Fold (travF f) (travE e) (convertAcc a)
travA (Fold1 f a)                       = inject $ Fold1 (travF f) (convertAcc a)
travA (FoldSeg i f e a s)               = inject $ FoldSeg i (travF f) (travE e) (convertAcc a) (convertAcc s)
travA (Fold1Seg i f a s)                = inject $ Fold1Seg i (travF f) (convertAcc a) (convertAcc s)
travA (Scanl f e a)                     = inject $ Scanl (travF f) (travE e) (convertAcc a)
travA (Scanl' f e a)                    = inject $ Scanl' (travF f) (travE e) (convertAcc a)
travA (Scanl1 f a)                      = inject $ Scanl1 (travF f) (convertAcc a)
travA (Scanr f e a)                     = inject $ Scanr (travF f) (travE e) (convertAcc a)
travA (Scanr' f e a)                    = inject $ Scanr' (travF f) (travE e) (convertAcc a)
travA (Scanr1 f a)                      = inject $ Scanr1 (travF f) (convertAcc a)
travA (Permute f a1 g a2)               = inject $ Permute (travF f) (convertAcc a1) (travF g) (convertAcc a2)
travA (Backpermute shr sh f a)          = inject $ Backpermute shr (travE sh) (travF f) (convertAcc a)
travA (Stencil s tp f b a)              = inject $ Stencil s tp (travF f) (travB b) (convertAcc a)
travA (Stencil2 s1 s2 tp f b1 a1 b2 a2) = inject $ Stencil2 s1 s2 tp (travF f) (travB b1) (convertAcc a1) (travB b2) (convertAcc a2)

travBinding :: Kit acc => ALeftHandSide bnd aenv aenv' -> acc aenv bnd -> acc aenv' a -> acc aenv a
travBinding     (LeftHandSideWildcard _) _   a = a
travBinding lhs@(LeftHandSideSingle _)   bnd a = inject $ Alet lhs bnd a
travBinding lhs@(LeftHandSidePair l1 l2) bnd a = case extract bnd of
  Just (Apair b1 b2) -> travBinding l1 b1 $ travBinding l2 (weaken (weakenWithLHS l1) b2) a
  _                  -> inject $ Alet lhs bnd a

-- XXX: We assume that any Acc contained in an expression is Avar.
-- We thus do not have to descend into expressions.
-- This isn't yet enforced using the types however.
travE :: PreExp acc aenv t -> PreExp acc aenv t
travE = id

travF :: PreFun acc aenv t -> PreFun acc aenv t
travF = id

travB :: PreBoundary acc aenv (Array sh e) -> PreBoundary acc aenv (Array sh e)
travB = id

convertAfun :: Kit acc => PreOpenAfun acc aenv f -> PreOpenAfun acc aenv f
convertAfun (Alam lhs f) = Alam lhs $ convertAfun f
convertAfun (Abody a) = Abody $ convertAcc a
