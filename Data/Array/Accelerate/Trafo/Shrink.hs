{-# LANGUAGE GADTs         #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes    #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Shrink
-- Copyright   : [2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The shrinking substitution arises as a restriction of beta-reduction to cases
-- where the bound variable is used zero (dead-code elimination) or one (linear
-- inlining) times.
--
-- TODO: replace with a linear shrinking algorithm; e.g.
--
--   * Andrew Appel & Trevor Jim, "Shrinking lambda expressions in linear time".
--
--   * Nick Benton, Andrew Kennedy, Sam Lindley and Claudio Russo, "Shrinking
--     Reductions in SML.NET"
--

module Data.Array.Accelerate.Trafo.Shrink (

  shrinkE, shrinkFE,
  shrinkA, shrinkAfun, shrinkOpenAcc,

) where

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar                ( Arrays )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo.Substitution

-- standard library
import Prelude                                          hiding ( exp )


-- Scalar expressions
-- ==================

shrinkE :: PreOpenExp acc env aenv t -> PreOpenExp acc env aenv t
shrinkE exp =
  case exp of
    Let bnd body
      | Var _ <- bnd                    -> shrinkE (inline body  bnd)
      | usesOfE ZeroIdx body' <= lIMIT  -> shrinkE (inline body' bnd')
      | otherwise                       -> Let bnd' body'
      where
        bnd'    = shrinkE bnd
        body'   = shrinkE body

        -- Allow both inlining and dead-code elimination
        --
        lIMIT   = 1
    --
    Var idx             -> Var idx
    Const c             -> Const c
    Tuple t             -> Tuple (shrinkTE t)
    Prj tup e           -> Prj tup (shrinkE e)
    IndexNil            -> IndexNil
    IndexCons sl sz     -> IndexCons (shrinkE sl) (shrinkE sz)
    IndexHead sh        -> IndexHead (shrinkE sh)
    IndexTail sh        -> IndexTail (shrinkE sh)
    IndexSlice x ix sh  -> IndexSlice x (shrinkE ix) (shrinkE sh)
    IndexFull x ix sl   -> IndexFull x (shrinkE ix) (shrinkE sl)
    IndexAny            -> IndexAny
    ToIndex sh ix       -> ToIndex (shrinkE sh) (shrinkE ix)
    FromIndex sh i      -> FromIndex (shrinkE sh) (shrinkE i)
    Cond p t e          -> Cond (shrinkE p) (shrinkE t) (shrinkE e)
    Iterate n f x       -> Iterate (shrinkE n) (shrinkE f) (shrinkE x)
    PrimConst c         -> PrimConst c
    PrimApp f x         -> PrimApp f (shrinkE x)
    Index a sh          -> Index a (shrinkE sh)
    LinearIndex a i     -> LinearIndex a (shrinkE i)
    Shape a             -> Shape a
    ShapeSize sh        -> ShapeSize (shrinkE sh)
    Intersect sh sz     -> Intersect (shrinkE sh) (shrinkE sz)

shrinkFE
    :: PreOpenFun acc env aenv f
    -> PreOpenFun acc env aenv f
shrinkFE fun =
  case fun of
    Body e      -> Body (shrinkE e)
    Lam f       -> Lam (shrinkFE f)

shrinkTE
    :: Tuple (PreOpenExp acc env aenv) t
    -> Tuple (PreOpenExp acc env aenv) t
shrinkTE tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> SnocTup (shrinkTE t) (shrinkE e)


usesOfE :: forall acc env aenv s t. Idx env s -> PreOpenExp acc env aenv t -> Int
usesOfE idx exp =
  case exp of
    Let bnd body        -> usesOfE idx bnd + usesOfE (SuccIdx idx) body
    Var idx'
      | Just REFL <- matchIdx idx idx'  -> 1
      | otherwise                       -> 0
    Const _             -> 0
    Tuple t             -> usesOfTE idx t
    Prj _ e             -> usesOfE idx e
    IndexNil            -> 0
    IndexCons sl sz     -> usesOfE idx sl + usesOfE idx sz
    IndexHead sh        -> usesOfE idx sh
    IndexTail sh        -> usesOfE idx sh
    IndexSlice _ ix sh  -> usesOfE idx ix + usesOfE idx sh
    IndexFull _ ix sl   -> usesOfE idx ix + usesOfE idx sl
    IndexAny            -> 0
    ToIndex sh ix       -> usesOfE idx sh + usesOfE idx ix
    FromIndex sh i      -> usesOfE idx sh + usesOfE idx i
    Cond p t e          -> usesOfE idx p  + usesOfE idx t + usesOfE idx e
    Iterate n f x       -> usesOfE idx n  + usesOfE idx x + usesOfE (SuccIdx idx) f
    PrimConst _         -> 0
    PrimApp _ x         -> usesOfE idx x
    Index _ sh          -> usesOfE idx sh
    LinearIndex _ i     -> usesOfE idx i
    Shape _             -> 0
    ShapeSize sh        -> usesOfE idx sh
    Intersect sh sz     -> usesOfE idx sh + usesOfE idx sz

usesOfTE :: Idx env s -> Tuple (PreOpenExp acc env aenv) t -> Int
usesOfTE idx tup =
  case tup of
    NilTup      -> 0
    SnocTup t e -> usesOfTE idx t + usesOfE idx e


-- Array expressions
-- =================

type UsesOfAcc acc = forall aenv s t. Idx aenv s -> acc aenv t -> Int
type ShrinkAcc acc = forall aenv a.   acc aenv a -> acc aenv a


shrinkOpenAcc :: OpenAcc aenv a -> OpenAcc aenv a
shrinkOpenAcc (OpenAcc pacc) =
  OpenAcc (shrinkA rebuildOpenAcc shrinkOpenAcc usesOfOpenAcc pacc)


-- TLM: Shrinking of array expressions is currently specialised to OpenAcc
--      because we need to unwrap terms to do further substitution and
--      shrinking at the Alet case.
--
shrinkA
    :: RebuildAcc OpenAcc
    -> ShrinkAcc OpenAcc
    -> UsesOfAcc OpenAcc
    -> PreOpenAcc OpenAcc aenv t
    -> PreOpenAcc OpenAcc aenv t
shrinkA k s u pacc =
  let subTop :: Arrays t => PreOpenAcc acc aenv s -> Idx (aenv,s) t -> PreOpenAcc acc aenv t
      subTop t ZeroIdx          = t
      subTop _ (SuccIdx idx)    = Avar idx
  in
  case pacc of
    Alet bnd@(OpenAcc pbnd) body@(OpenAcc pbody)
      | Avar _ <- pbnd           -> shrinkA k s u (rebuildA k (subTop pbnd)  pbody)
      | u ZeroIdx body' <= lIMIT -> shrinkA k s u (rebuildA k (subTop pbnd') pbody')
      | otherwise                -> Alet bnd' body'
      where
        bnd'@(OpenAcc pbnd')    = s bnd
        body'@(OpenAcc pbody')  = s body

        -- Allow only dead code elimination, otherwise we might inline array
        -- computations directly into scalar expressions, and later stages rely
        -- on there being only variables embedded in scalar expressions.
        --
        lIMIT   = 0
    --
    Avar ix             -> Avar ix
    Atuple tup          -> Atuple (shrinkATA s tup)
    Aprj tup a          -> Aprj tup (s a)
    Apply f a           -> Apply f (s a)
    Acond p t e         -> Acond (shrinkEA s p) (s t) (s e)
    Use a               -> Use a
    Unit e              -> Unit (shrinkEA s e)
    Reshape e a         -> Reshape (shrinkEA s e) (s a)
    Generate e f        -> Generate (shrinkEA s e) (shrinkFA s f)
    Transform sh ix f a -> Transform (shrinkEA s sh) (shrinkFA s ix) (shrinkFA s f) (s a)
    Replicate sl slix a -> Replicate sl (shrinkEA s slix) (s a)
    Slice sl a slix     -> Slice sl (s a) (shrinkEA s slix)
    Map f a             -> Map (shrinkFA s f) (s a)
    ZipWith f a1 a2     -> ZipWith (shrinkFA s f) (s a1) (s a2)
    Fold f z a          -> Fold (shrinkFA s f) (shrinkEA s z) (s a)
    Fold1 f a           -> Fold1 (shrinkFA s f) (s a)
    FoldSeg f z a b     -> FoldSeg (shrinkFA s f) (shrinkEA s z) (s a) (s b)
    Fold1Seg f a b      -> Fold1Seg (shrinkFA s f) (s a) (s b)
    Scanl f z a         -> Scanl (shrinkFA s f) (shrinkEA s z) (s a)
    Scanl' f z a        -> Scanl' (shrinkFA s f) (shrinkEA s z) (s a)
    Scanl1 f a          -> Scanl1 (shrinkFA s f) (s a)
    Scanr f z a         -> Scanr (shrinkFA s f) (shrinkEA s z) (s a)
    Scanr' f z a        -> Scanr' (shrinkFA s f) (shrinkEA s z) (s a)
    Scanr1 f a          -> Scanr1 (shrinkFA s f) (s a)
    Permute f1 a1 f2 a2 -> Permute (shrinkFA s f1) (s a1) (shrinkFA s f2) (s a2)
    Backpermute sh f a  -> Backpermute (shrinkEA s sh) (shrinkFA s f) (s a)
    Stencil f b a       -> Stencil (shrinkFA s f) b (s a)
    Stencil2 f b1 a1 b2 a2
                        -> Stencil2 (shrinkFA s f) b1 (s a1) b2 (s a2)


shrinkAfun :: ShrinkAcc acc -> PreOpenAfun acc aenv t -> PreOpenAfun acc aenv t
shrinkAfun s afun =
  case afun of
    Abody b     -> Abody (s b)
    Alam f      -> Alam (shrinkAfun s f)

shrinkATA :: ShrinkAcc acc -> Atuple (acc aenv) t -> Atuple (acc aenv) t
shrinkATA s atup =
  case atup of
    NilAtup      -> NilAtup
    SnocAtup t a -> shrinkATA s t `SnocAtup` s a

shrinkFA :: ShrinkAcc acc -> PreOpenFun acc env aenv t -> PreOpenFun acc env aenv t
shrinkFA s fun =
  case fun of
    Body b      -> Body (shrinkEA s b)
    Lam f       -> Lam (shrinkFA s f)

shrinkEA :: ShrinkAcc acc -> PreOpenExp acc env aenv t -> PreOpenExp acc env aenv t
shrinkEA s exp =
  case exp of
    Let bnd body        -> Let (shrinkEA s bnd) (shrinkEA s body)
    Var idx             -> Var idx
    Const c             -> Const c
    Tuple t             -> Tuple (shrinkTA s t)
    Prj tup e           -> Prj tup (shrinkEA s e)
    IndexNil            -> IndexNil
    IndexCons sl sz     -> IndexCons (shrinkEA s sl) (shrinkEA s sz)
    IndexHead sh        -> IndexHead (shrinkEA s sh)
    IndexTail sh        -> IndexTail (shrinkEA s sh)
    IndexSlice x ix sh  -> IndexSlice x (shrinkEA s ix) (shrinkEA s sh)
    IndexFull x ix sl   -> IndexFull x (shrinkEA s ix) (shrinkEA s sl)
    IndexAny            -> IndexAny
    ToIndex sh ix       -> ToIndex (shrinkEA s sh) (shrinkEA s ix)
    FromIndex sh i      -> FromIndex (shrinkEA s sh) (shrinkEA s i)
    Cond p t e          -> Cond (shrinkEA s p) (shrinkEA s t) (shrinkEA s e)
    Iterate n f x       -> Iterate (shrinkEA s n) (shrinkEA s f) (shrinkEA s x)
    PrimConst c         -> PrimConst c
    PrimApp f x         -> PrimApp f (shrinkEA s x)
    Index a sh          -> Index (s a) (shrinkEA s sh)
    LinearIndex a i     -> LinearIndex (s a) (shrinkEA s i)
    Shape a             -> Shape (s a)
    ShapeSize sh        -> ShapeSize (shrinkEA s sh)
    Intersect sh sz     -> Intersect (shrinkEA s sh) (shrinkEA s sz)

shrinkTA :: ShrinkAcc acc -> Tuple (PreOpenExp acc env aenv) t -> Tuple (PreOpenExp acc env aenv) t
shrinkTA s tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> shrinkTA s t `SnocTup` shrinkEA s e


usesOfOpenAcc :: Idx aenv s -> OpenAcc aenv t -> Int
usesOfOpenAcc idx (OpenAcc acc) = usesOfA usesOfOpenAcc idx acc

usesOfA :: UsesOfAcc acc -> Idx aenv s -> PreOpenAcc acc aenv t -> Int
usesOfA u idx acc =
  case acc of
    Alet bnd body       -> u idx bnd + u (SuccIdx idx) body
    Avar idx'
      | Just REFL <- matchIdx idx idx'  -> 1
      | otherwise                       -> 0
    Atuple tup          -> usesOfATA u idx tup
    Aprj _ a            -> u idx a
    Apply _ a           -> u idx a
    Acond p t e         -> usesOfEA u idx p + u idx t + u idx e
    Use _               -> 0
    Unit e              -> usesOfEA u idx e
    Reshape e a         -> usesOfEA u idx e + u idx a
    Generate e f        -> usesOfEA u idx e + usesOfFA u idx f
    Transform sh ix f a -> usesOfEA u idx sh + usesOfFA u idx ix + usesOfFA u idx f + u idx a
    Replicate _ slix a  -> usesOfEA u idx slix + u idx a
    Slice _ a slix      -> usesOfEA u idx slix + u idx a
    Map f a             -> usesOfFA u idx f + u idx a
    ZipWith f a1 a2     -> usesOfFA u idx f + u idx a1 + u idx a2
    Fold f z a          -> usesOfFA u idx f + usesOfEA u idx z + u idx a
    Fold1 f a           -> usesOfFA u idx f + u idx a
    FoldSeg f z a s     -> usesOfFA u idx f + usesOfEA u idx z + u idx a + u idx s
    Fold1Seg f a s      -> usesOfFA u idx f + u idx a + u idx s
    Scanl f z a         -> usesOfFA u idx f + usesOfEA u idx z + u idx a
    Scanl' f z a        -> usesOfFA u idx f + usesOfEA u idx z + u idx a
    Scanl1 f a          -> usesOfFA u idx f + u idx a
    Scanr f z a         -> usesOfFA u idx f + usesOfEA u idx z + u idx a
    Scanr' f z a        -> usesOfFA u idx f + usesOfEA u idx z + u idx a
    Scanr1 f a          -> usesOfFA u idx f + u idx a
    Permute f1 a1 f2 a2 -> usesOfFA u idx f1 + u idx a1 + usesOfFA u idx f2 + u idx a2
    Backpermute sh f a  -> usesOfEA u idx sh + usesOfFA u idx f + u idx a
    Stencil f _ a       -> usesOfFA u idx f + u idx a
    Stencil2 f _ a1 _ a2-> usesOfFA u idx f + u idx a1 + u idx a2

usesOfATA :: UsesOfAcc acc -> Idx aenv s -> Atuple (acc aenv) t -> Int
usesOfATA s idx atup =
  case atup of
    NilAtup      -> 0
    SnocAtup t a -> usesOfATA s idx t + s idx a

usesOfEA :: UsesOfAcc acc -> Idx aenv a -> PreOpenExp acc env aenv t -> Int
usesOfEA s idx exp =
  case exp of
    Let bnd body        -> usesOfEA s idx bnd + usesOfEA s idx body
    Var _               -> 0
    Const _             -> 0
    Tuple t             -> usesOfTA s idx t
    Prj _ e             -> usesOfEA s idx e
    IndexNil            -> 0
    IndexCons sl sz     -> usesOfEA s idx sl + usesOfEA s idx sz
    IndexHead sh        -> usesOfEA s idx sh
    IndexTail sh        -> usesOfEA s idx sh
    IndexSlice _ ix sh  -> usesOfEA s idx ix + usesOfEA s idx sh
    IndexFull _ ix sl   -> usesOfEA s idx ix + usesOfEA s idx sl
    IndexAny            -> 0
    ToIndex sh ix       -> usesOfEA s idx sh + usesOfEA s idx ix
    FromIndex sh i      -> usesOfEA s idx sh + usesOfEA s idx i
    Cond p t e          -> usesOfEA s idx p  + usesOfEA s idx t  + usesOfEA s idx e
    Iterate n f x       -> usesOfEA s idx n  + usesOfEA s idx f  + usesOfEA s idx x
    PrimConst _         -> 0
    PrimApp _ x         -> usesOfEA s idx x
    Index a sh          -> s idx a + usesOfEA s idx sh
    LinearIndex a i     -> s idx a + usesOfEA s idx i
    Shape a             -> s idx a
    ShapeSize sh        -> usesOfEA s idx sh
    Intersect sh sz     -> usesOfEA s idx sh + usesOfEA s idx sz

usesOfTA :: UsesOfAcc acc -> Idx aenv a -> Tuple (PreOpenExp acc env aenv) t -> Int
usesOfTA s idx tup =
  case tup of
    NilTup      -> 0
    SnocTup t e -> usesOfTA s idx t + usesOfEA s idx e

usesOfFA :: UsesOfAcc acc -> Idx aenv a -> PreOpenFun acc env aenv f -> Int
usesOfFA s idx fun =
  case fun of
    Body e      -> usesOfEA s idx e
    Lam f       -> usesOfFA s idx f

