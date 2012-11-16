{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Substitution
-- Copyright   : [2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Substitution (

  -- * Renaming & Substitution
  inline, substitute, compose,

  -- * Weakening
  weakenA, weakenEA, weakenFA,
  weakenE, weakenFE,

  weakenByA, weakenByEA, weakenByFA,
  weakenByE, weakenByFE,

  -- * Shrinking
  shrinkE, shrinkFE,
  shrinkA, shrinkAfun, shrinkOpenAcc,

  -- * Rebuilding
  rebuildA, rebuildAfun, rebuildOpenAcc,
  rebuildE, rebuildEA,
  rebuildFA,

) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar        ( Elt, Arrays )
import Prelude                                  hiding ( exp )


-- NOTE: [Renaming and Substitution]
--
-- To do things like renaming and substitution, we need some operation on
-- variables that we push structurally through terms, applying to each variable.
-- We have a type preserving but environment changing operation:
--
--   v :: forall t. Idx env t -> f env' aenv t
--
-- The crafty bit is that 'f' might represent variables (for renaming) or terms
-- (for substitutions). The demonic forall, --- which is to say that the
-- quantifier is in a position which gives us obligation, not opportunity ---
-- forces us to respect type: when pattern matching detects the variable we care
-- about, happily we discover that it has the type we must respect. The demon is
-- not so free to mess with us as one might fear at first.
--
-- We then lift this to an operation which traverses terms and rebuild them
-- after applying 'v' to the variables:
--
--   rebuild v :: OpenExp env aenv t -> OpenExp env' aenv t
--
-- The Syntactic class tells us what we need to know about 'f' if we want to be
-- able to rebuild terms. In essence, the crucial functionality is to propagate
-- a class of operations on variables that is closed under shifting.
--
infixr `compose`
infixr `substitute`

-- | Replace the first variable with the given expression. The environment
-- shrinks.
--
inline :: Elt t
       => PreOpenExp acc (env, s) aenv t
       -> PreOpenExp acc env      aenv s
       -> PreOpenExp acc env      aenv t
inline f g = rebuildE (subTop g) f
  where
    subTop :: Elt t => PreOpenExp acc env aenv s -> Idx (env, s) t -> PreOpenExp acc env aenv t
    subTop s ZeroIdx      = s
    subTop _ (SuccIdx ix) = Var ix

-- | Replace an expression that uses the top environment variable with another.
-- The result of the first is let bound into the second.
--
substitute :: (Elt b, Elt c)
           => PreOpenExp acc (env, b) aenv c
           -> PreOpenExp acc (env, a) aenv b
           -> PreOpenExp acc (env, a) aenv c
substitute f g
  | Var ZeroIdx <- g    = f     -- don't rebind an identity function
  | otherwise           = Let g $ rebuildE split f
  where
    split :: Elt c => Idx (env,b) c -> PreOpenExp acc ((env,a),b) aenv c
    split ZeroIdx       = Var ZeroIdx
    split (SuccIdx ix)  = Var (SuccIdx (SuccIdx ix))


-- | Composition of unary functions.
--
compose :: Elt c
        => PreOpenFun acc env aenv (b -> c)
        -> PreOpenFun acc env aenv (a -> b)
        -> PreOpenFun acc env aenv (a -> c)
compose (Lam (Body f)) (Lam (Body g)) = Lam . Body $ substitute f g
compose _              _              = error "compose: impossible evaluation"


-- NOTE: [Weakening]
--
-- Weakening is something we usually take for granted: every time you learn a
-- new word, old sentences still make sense. If a conclusion is justified by a
-- hypothesis, it is still justified if you add more hypotheses. Similarly, a
-- term remains in scope if you bind more (fresh) variables. Weakening is the
-- operation of shifting things from one scope to a larger scope in which new
-- things have become meaningful, but no old things have vanished.
--
-- When we use a named representation (or HOAS) we get weakening for free. But
-- in the de Bruijn representation weakening takes work: you have to shift all
-- variable references to make room for the new bindings.
--

-- Functions to increase the scope of scalar or array environments of OpenAcc
-- tied expressions by a single index.
--
weakenA :: OpenAcc aenv t -> OpenAcc (aenv, s) t
weakenA = weakenByA SuccIdx

weakenE :: OpenExp env aenv t -> OpenExp (env, s) aenv t
weakenE = weakenByE SuccIdx

weakenEA :: OpenExp env aenv t -> OpenExp env (aenv,s) t
weakenEA = weakenByEA SuccIdx

weakenFA :: OpenFun env aenv t -> OpenFun env (aenv,s) t
weakenFA = weakenByFA SuccIdx

weakenFE :: OpenFun env aenv t -> OpenFun (env,s) aenv t
weakenFE = weakenByFE SuccIdx


-- Weakening functions parameterised by an index manipulation
--
weakenByA :: (forall t'. Idx aenv t' -> Idx aenv' t') -> OpenAcc aenv t -> OpenAcc aenv' t
weakenByA k = rebuildOpenAcc (Avar . k)

weakenByE :: (forall t'. Idx env t' -> Idx env' t') -> OpenExp env aenv t -> OpenExp env' aenv t
weakenByE k = rebuildE (Var . k)

weakenByEA :: (forall t'. Idx aenv t' -> Idx aenv' t') -> OpenExp env aenv t -> OpenExp env aenv' t
weakenByEA k = rebuildEA rebuildOpenAcc (Avar . k)

weakenByFA :: (forall t'. Idx aenv t' -> Idx aenv' t') -> OpenFun env aenv t -> OpenFun env aenv' t
weakenByFA k = rebuildFA rebuildOpenAcc (Avar . k)

weakenByFE :: (forall t'. Idx env t' -> Idx env' t') -> OpenFun env aenv t -> OpenFun env' aenv t
weakenByFE k = rebuildFE (Var . k)


-- Simultaneous Substitution ===================================================
--

-- Scalar expressions
-- ------------------

-- SEE: [Renaming and Substitution]
-- SEE: [Weakening]
--
class SyntacticExp f where
  varIn         :: Elt t => Idx env t        -> f acc env aenv t
  expOut        :: Elt t => f acc env aenv t -> PreOpenExp acc env aenv t
  weakenExp     :: Elt t => f acc env aenv t -> f acc (env, s) aenv t

newtype IdxE (acc :: * -> * -> *) env aenv t = IE { unIE :: Idx env t }

instance SyntacticExp IdxE where
  varIn         = IE
  expOut        = Var . unIE
  weakenExp     = IE . SuccIdx . unIE

instance SyntacticExp PreOpenExp where
  varIn         = Var
  expOut        = id
  weakenExp     = rebuildE (weakenExp . IE)


shiftE
    :: (SyntacticExp f, Elt t)
    => (forall t'. Elt t' => Idx env t' -> f acc env' aenv t')
    -> Idx     (env,  s)      t
    -> f   acc (env', s) aenv t
shiftE _ ZeroIdx      = varIn ZeroIdx
shiftE v (SuccIdx ix) = weakenExp (v ix)

rebuildE
    :: SyntacticExp f
    => (forall t'. Elt t' => Idx env t' -> f acc env' aenv t')
    -> PreOpenExp acc env  aenv t
    -> PreOpenExp acc env' aenv t
rebuildE v exp =
  case exp of
    Let a b             -> Let (rebuildE v a) (rebuildE (shiftE v) b)
    Var ix              -> expOut (v ix)
    Const c             -> Const c
    Tuple tup           -> Tuple (rebuildTE v tup)
    Prj tup e           -> Prj tup (rebuildE v e)
    IndexNil            -> IndexNil
    IndexCons sh sz     -> IndexCons (rebuildE v sh) (rebuildE v sz)
    IndexHead sh        -> IndexHead (rebuildE v sh)
    IndexTail sh        -> IndexTail (rebuildE v sh)
    IndexAny            -> IndexAny
    IndexSlice x ix sh  -> IndexSlice x (rebuildE v ix) (rebuildE v sh)
    IndexFull x ix sl   -> IndexFull x (rebuildE v ix) (rebuildE v sl)
    ToIndex sh ix       -> ToIndex (rebuildE v sh) (rebuildE v ix)
    FromIndex sh ix     -> FromIndex (rebuildE v sh) (rebuildE v ix)
    Cond p t e          -> Cond (rebuildE v p) (rebuildE v t) (rebuildE v e)
    Iterate n f x       -> Iterate n (rebuildFE v f) (rebuildE v x)
    PrimConst c         -> PrimConst c
    PrimApp f x         -> PrimApp f (rebuildE v x)
    Index a sh          -> Index a (rebuildE v sh)
    LinearIndex a i     -> LinearIndex a (rebuildE v i)
    Shape a             -> Shape a
    ShapeSize sh        -> ShapeSize (rebuildE v sh)
    Intersect s t       -> Intersect (rebuildE v s) (rebuildE v t)

rebuildTE
    :: SyntacticExp f
    => (forall t'. Elt t' => Idx env t' -> f acc env' aenv t')
    -> Tuple (PreOpenExp acc env  aenv) t
    -> Tuple (PreOpenExp acc env' aenv) t
rebuildTE v tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> rebuildTE v t `SnocTup` rebuildE v e

rebuildFE
    :: SyntacticExp f
    => (forall t'. Elt t' => Idx env t' -> f acc env' aenv t')
    -> PreOpenFun acc env  aenv t
    -> PreOpenFun acc env' aenv t
rebuildFE v fun =
  case fun of
    Body e      -> Body (rebuildE v e)
    Lam f       -> Lam (rebuildFE (shiftE v) f)


-- Array expressions
-- -----------------

type RebuildAcc acc =
  forall aenv aenv' f a. SyntacticAcc f
    => (forall a'. Arrays a' => Idx aenv a' -> f acc aenv' a')
    -> acc aenv  a
    -> acc aenv' a

class SyntacticAcc f where
  avarIn        :: Arrays t => Idx aenv t     -> f acc aenv t
  accOut        :: Arrays t => f acc aenv t   -> PreOpenAcc acc aenv t
  weakenAcc     :: Arrays t => RebuildAcc acc -> f acc aenv t -> f acc (aenv, s) t

newtype IdxA (acc :: * -> * -> *) aenv t = IA { unIA :: Idx aenv t }

instance SyntacticAcc IdxA where
  avarIn        = IA
  accOut        = Avar . unIA
  weakenAcc _   = IA . SuccIdx . unIA

instance SyntacticAcc PreOpenAcc where
  avarIn        = Avar
  accOut        = id
  weakenAcc k   = rebuildA k (weakenAcc k . IA)


rebuildOpenAcc
    :: SyntacticAcc f
    => (forall t'. Arrays t' => Idx aenv t' -> f OpenAcc aenv' t')
    -> OpenAcc aenv  t
    -> OpenAcc aenv' t
rebuildOpenAcc v (OpenAcc acc) = OpenAcc (rebuildA rebuildOpenAcc v acc)


shiftA
    :: (SyntacticAcc f, Arrays t)
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f acc aenv' t')
    -> Idx     (aenv,  s) t
    -> f   acc (aenv', s) t
shiftA _ _ ZeroIdx      = avarIn ZeroIdx
shiftA k v (SuccIdx ix) = weakenAcc k (v ix)

rebuildA
    :: SyntacticAcc f
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f acc aenv' t')
    -> PreOpenAcc acc aenv  t
    -> PreOpenAcc acc aenv' t
rebuildA rebuild v acc =
  case acc of
    Alet a b            -> Alet (rebuild v a) (rebuild (shiftA rebuild v) b)
    Avar ix             -> accOut (v ix)
    Atuple tup          -> Atuple (rebuildATA rebuild v tup)
    Aprj tup a          -> Aprj tup (rebuild v a)
    Apply f a           -> Apply f (rebuild v a)
    Acond p t e         -> Acond (rebuildEA rebuild v p) (rebuild v t) (rebuild v e)
    Use a               -> Use a
    Unit e              -> Unit (rebuildEA rebuild v e)
    Reshape e a         -> Reshape (rebuildEA rebuild v e) (rebuild v a)
    Generate e f        -> Generate (rebuildEA rebuild v e) (rebuildFA rebuild v f)
    Transform sh ix f a -> Transform (rebuildEA rebuild v sh) (rebuildFA rebuild v ix) (rebuildFA rebuild v f) (rebuild v a)
    Replicate sl slix a -> Replicate sl (rebuildEA rebuild v slix) (rebuild v a)
    Slice sl a slix     -> Slice sl (rebuild v a) (rebuildEA rebuild v slix)
    Map f a             -> Map (rebuildFA rebuild v f) (rebuild v a)
    ZipWith f a1 a2     -> ZipWith (rebuildFA rebuild v f) (rebuild v a1) (rebuild v a2)
    Fold f z a          -> Fold (rebuildFA rebuild v f) (rebuildEA rebuild v z) (rebuild v a)
    Fold1 f a           -> Fold1 (rebuildFA rebuild v f) (rebuild v a)
    FoldSeg f z a s     -> FoldSeg (rebuildFA rebuild v f) (rebuildEA rebuild v z) (rebuild v a) (rebuild v s)
    Fold1Seg f a s      -> Fold1Seg (rebuildFA rebuild v f) (rebuild v a) (rebuild v s)
    Scanl f z a         -> Scanl (rebuildFA rebuild v f) (rebuildEA rebuild v z) (rebuild v a)
    Scanl' f z a        -> Scanl' (rebuildFA rebuild v f) (rebuildEA rebuild v z) (rebuild v a)
    Scanl1 f a          -> Scanl1 (rebuildFA rebuild v f) (rebuild v a)
    Scanr f z a         -> Scanr (rebuildFA rebuild v f) (rebuildEA rebuild v z) (rebuild v a)
    Scanr' f z a        -> Scanr' (rebuildFA rebuild v f) (rebuildEA rebuild v z) (rebuild v a)
    Scanr1 f a          -> Scanr1 (rebuildFA rebuild v f) (rebuild v a)
    Permute f1 a1 f2 a2 -> Permute (rebuildFA rebuild v f1) (rebuild v a1) (rebuildFA rebuild v f2) (rebuild v a2)
    Backpermute sh f a  -> Backpermute (rebuildEA rebuild v sh) (rebuildFA rebuild v f) (rebuild v a)
    Stencil f b a       -> Stencil (rebuildFA rebuild v f) b (rebuild v a)
    Stencil2 f b1 a1 b2 a2
                        -> Stencil2 (rebuildFA rebuild v f) b1 (rebuild v a1) b2 (rebuild v a2)


-- Rebuilding array computations
--

rebuildAfun
    :: SyntacticAcc f
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f acc aenv' t')
    -> PreOpenAfun acc aenv  t
    -> PreOpenAfun acc aenv' t
rebuildAfun k v afun =
  case afun of
    Abody b     -> Abody (k v b)
    Alam f      -> Alam (rebuildAfun k (shiftA k v) f)

rebuildATA
    :: SyntacticAcc f
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f acc aenv' t')
    -> Atuple (acc aenv)  t
    -> Atuple (acc aenv') t
rebuildATA k v atup =
  case atup of
    NilAtup      -> NilAtup
    SnocAtup t a -> rebuildATA k v t `SnocAtup` k v a


-- Rebuilding scalar expressions
--

rebuildEA
    :: SyntacticAcc f
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f acc aenv' t')
    -> PreOpenExp acc env aenv  t
    -> PreOpenExp acc env aenv' t
rebuildEA k v exp =
  case exp of
    Let a b             -> Let (rebuildEA k v a) (rebuildEA k v b)
    Var ix              -> Var ix
    Const c             -> Const c
    Tuple tup           -> Tuple (rebuildTA k v tup)
    Prj tup e           -> Prj tup (rebuildEA k v e)
    IndexNil            -> IndexNil
    IndexCons sh sz     -> IndexCons (rebuildEA k v sh) (rebuildEA k v sz)
    IndexHead sh        -> IndexHead (rebuildEA k v sh)
    IndexTail sh        -> IndexTail (rebuildEA k v sh)
    IndexAny            -> IndexAny
    IndexSlice x ix sh  -> IndexSlice x (rebuildEA k v ix) (rebuildEA k v sh)
    IndexFull x ix sl   -> IndexFull x (rebuildEA k v ix) (rebuildEA k v sl)
    ToIndex sh ix       -> ToIndex (rebuildEA k v sh) (rebuildEA k v ix)
    FromIndex sh ix     -> FromIndex (rebuildEA k v sh) (rebuildEA k v ix)
    Cond p t e          -> Cond (rebuildEA k v p) (rebuildEA k v t) (rebuildEA k v e)
    Iterate n f x       -> Iterate n (rebuildFA k v f) (rebuildEA k v x)
    PrimConst c         -> PrimConst c
    PrimApp f x         -> PrimApp f (rebuildEA k v x)
    Index a sh          -> Index (k v a) (rebuildEA k v sh)
    LinearIndex a i     -> LinearIndex (k v a) (rebuildEA k v i)
    Shape a             -> Shape (k v a)
    ShapeSize sh        -> ShapeSize (rebuildEA k v sh)
    Intersect s t       -> Intersect (rebuildEA k v s) (rebuildEA k v t)

rebuildTA
    :: SyntacticAcc f
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f acc aenv' t')
    -> Tuple (PreOpenExp acc env aenv)  t
    -> Tuple (PreOpenExp acc env aenv') t
rebuildTA k v tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> rebuildTA k v t `SnocTup` rebuildEA k v e

rebuildFA
    :: SyntacticAcc f
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f acc aenv' t')
    -> PreOpenFun acc env aenv  t
    -> PreOpenFun acc env aenv' t
rebuildFA k v fun =
  case fun of
    Body e      -> Body (rebuildEA k v e)
    Lam f       -> Lam  (rebuildFA k v f)


-- Shrinking ===================================================================
--
-- The shrinking substitution arises as a restriction of beta-reduction to cases
-- where the bound variable is used zero (dead-code elimination) or one (linear
-- inlining) times.
--

-- Scalar expressions
-- ------------------

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

        -- Allow inlining and dead-code elimination
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
    Iterate n f x       -> Iterate n (shrinkFE f) (shrinkE x)
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
    Cond p t e          -> usesOfE idx p  + usesOfE idx t  + usesOfE idx e
    Iterate _ f x       -> usesOfFE idx f + usesOfE idx x
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

usesOfFE :: Idx env s -> PreOpenFun acc env aenv f -> Int
usesOfFE idx fun =
  case fun of
    Body e      -> usesOfE  idx           e
    Lam f       -> usesOfFE (SuccIdx idx) f



-- Array expressions
-- -----------------

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
    Apply f a           -> Apply (shrinkAfun s f) (s a)
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
    Iterate n f x       -> Iterate n (shrinkFA s f) (shrinkEA s x)
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
    Iterate _ f x       -> usesOfFA s idx f  + usesOfEA s idx x
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

