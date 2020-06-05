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
travA (Acond e a1 a2)                   = inject $ Acond e (convertAcc a1) (convertAcc a2)
travA (Awhile c f a)                    = inject $ Awhile (convertAfun c) (convertAfun f) (convertAcc a)
travA (Use repr arr)                    = inject $ Use repr arr
travA (Unit tp e)                       = inject $ Unit tp e
travA (Reshape shr e a)                 = inject $ Reshape shr e a
travA (Generate repr e f)               = inject $ Generate repr e f
travA (Transform repr sh f g a)         = inject $ Transform repr sh f g (convertAcc a)
travA (Replicate slix sl a)             = inject $ Replicate slix sl (convertAcc a)
travA (Slice slix a sl)                 = inject $ Slice slix (convertAcc a) sl
travA (Map tp f a)                      = inject $ Map tp f (convertAcc a)
travA (ZipWith tp f a1 a2)              = inject $ ZipWith tp f (convertAcc a1) (convertAcc a2)
travA (Fold f e a)                      = inject $ Fold f e (convertAcc a)
travA (Fold1 f a)                       = inject $ Fold1 f (convertAcc a)
travA (FoldSeg i f e a s)               = inject $ FoldSeg i f e (convertAcc a) (convertAcc s)
travA (Fold1Seg i f a s)                = inject $ Fold1Seg i f (convertAcc a) (convertAcc s)
travA (Scanl f e a)                     = inject $ Scanl f e (convertAcc a)
travA (Scanl' f e a)                    = inject $ Scanl' f e (convertAcc a)
travA (Scanl1 f a)                      = inject $ Scanl1 f (convertAcc a)
travA (Scanr f e a)                     = inject $ Scanr f e (convertAcc a)
travA (Scanr' f e a)                    = inject $ Scanr' f e (convertAcc a)
travA (Scanr1 f a)                      = inject $ Scanr1 f (convertAcc a)
travA (Permute f a1 g a2)               = inject $ Permute f (convertAcc a1) g (convertAcc a2)
travA (Backpermute shr sh f a)          = inject $ Backpermute shr sh f (convertAcc a)
travA (Stencil s tp f b a)              = inject $ Stencil s tp f b (convertAcc a)
travA (Stencil2 s1 s2 tp f b1 a1 b2 a2) = inject $ Stencil2 s1 s2 tp f b1 (convertAcc a1) b2 (convertAcc a2)

travBinding :: Kit acc => ALeftHandSide bnd aenv aenv' -> acc aenv bnd -> acc aenv' a -> acc aenv a
travBinding     (LeftHandSideWildcard _) _   a = a
travBinding lhs@(LeftHandSideSingle _)   bnd a = inject $ Alet lhs bnd a
travBinding lhs@(LeftHandSidePair l1 l2) bnd a = case extract bnd of
  Just (Apair b1 b2) -> travBinding l1 b1 $ travBinding l2 (weaken (weakenWithLHS l1) b2) a
  _                  -> inject $ Alet lhs bnd a

convertAfun :: Kit acc => PreOpenAfun acc aenv f -> PreOpenAfun acc aenv f
convertAfun (Alam lhs f) = Alam lhs $ convertAfun f
convertAfun (Abody a) = Abody $ convertAcc a

