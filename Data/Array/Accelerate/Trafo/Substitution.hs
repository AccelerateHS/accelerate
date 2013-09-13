{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards  #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
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

  -- ** Renaming & Substitution
  inline, substitute, compose,
  subTop, subAtop,

  -- ** Weakening
  (:>),
  weakenA, weakenEA, weakenFA,
  weakenE, weakenFE,

  -- ** Rebuilding terms
  RebuildAcc,
  rebuildA, rebuildAfun, rebuildOpenAcc,
  rebuildE, rebuildEA,
  rebuildFA, rebuildFE,

) where

import Prelude                                  hiding ( exp )

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar        ( Elt, Arrays )

import qualified Data.Array.Accelerate.Debug    as Stats


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
inline f g = Stats.substitution "inline" $ rebuildE (subTop g) f

-- | Replace an expression that uses the top environment variable with another.
-- The result of the first is let bound into the second.
--
substitute :: (Elt b, Elt c)
           => PreOpenExp acc (env, b) aenv c
           -> PreOpenExp acc (env, a) aenv b
           -> PreOpenExp acc (env, a) aenv c
substitute f g
  | Stats.substitution "substitute" False = undefined

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
compose (Lam (Body f)) (Lam (Body g)) = Stats.substitution "compose" . Lam . Body $ substitute f g
compose _              _              = error "compose: impossible evaluation"


subTop :: Elt t => PreOpenExp acc env aenv s -> Idx (env, s) t -> PreOpenExp acc env aenv t
subTop s ZeroIdx      = s
subTop _ (SuccIdx ix) = Var ix

subAtop :: Arrays t => PreOpenAcc acc aenv s -> Idx (aenv, s) t -> PreOpenAcc acc aenv t
subAtop t ZeroIdx       = t
subAtop _ (SuccIdx idx) = Avar idx


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

-- The type of shifting terms from one context into another
--
type env :> env' = forall t'. Idx env t' -> Idx env' t'

weakenA :: RebuildAcc acc -> aenv :> aenv' -> PreOpenAcc acc aenv a -> PreOpenAcc acc aenv' a
weakenA k v = Stats.substitution "weakenA" . rebuildA k (Avar . v)

weakenEA :: RebuildAcc acc -> aenv :> aenv' -> PreOpenExp acc env aenv t -> PreOpenExp acc env aenv' t
weakenEA k v = Stats.substitution "weakenEA" . rebuildEA k (Avar . v)

weakenFA :: RebuildAcc acc -> aenv :> aenv' -> PreOpenFun acc env aenv f -> PreOpenFun acc env aenv' f
weakenFA k v = Stats.substitution "weakenFA" . rebuildFA k (Avar . v)


weakenE :: env :> env' -> PreOpenExp acc env aenv t -> PreOpenExp acc env' aenv t
weakenE v = Stats.substitution "weakenE" . rebuildE (Var . v)

weakenFE :: env :> env' -> PreOpenFun acc env aenv f -> PreOpenFun acc env' aenv f
weakenFE v = Stats.substitution "weakenFE" . rebuildFE (Var . v)


{-# RULES
"weakenA/weakenA" forall a (k :: RebuildAcc acc) (v1 :: env' :> env'') (v2 :: env :> env').
    weakenA k v1 (weakenA k v2 a) = weakenA k (v1 . v2) a

"weakenEA/weakenEA" forall a (k :: RebuildAcc acc) (v1 :: env' :> env'') (v2 :: env :> env').
    weakenEA k v1 (weakenEA k v2 a) = weakenEA k (v1 . v2) a

"weakenFA/weakenFA" forall a (k :: RebuildAcc acc) (v1 :: env' :> env'') (v2 :: env :> env').
    weakenFA k v1 (weakenFA k v2 a) = weakenFA k (v1 . v2) a

"weakenE/weakenE" forall e (v1 :: env' :> env'') (v2 :: env :> env').
    weakenE v1 (weakenE v2 e) = weakenE (v1 . v2) e

"weakenFE/weakenFE" forall e (v1 :: env' :> env'') (v2 :: env :> env').
    weakenFE v1 (weakenFE v2 e) = weakenFE (v1 . v2) e
 #-}

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
    While p f x         -> While (rebuildFE v p) (rebuildFE v f) (rebuildE v x)
    PrimConst c         -> PrimConst c
    PrimApp f x         -> PrimApp f (rebuildE v x)
    Index a sh          -> Index a (rebuildE v sh)
    LinearIndex a i     -> LinearIndex a (rebuildE v i)
    Shape a             -> Shape a
    ShapeSize sh        -> ShapeSize (rebuildE v sh)
    Intersect s t       -> Intersect (rebuildE v s) (rebuildE v t)
    Foreign ff f e      -> Foreign ff f (rebuildE v e)

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
    Apply f a           -> Apply (rebuildAfun rebuild v f) (rebuild v a)
    Aforeign ff afun as -> Aforeign ff afun (rebuild v as)
    Acond p t e         -> Acond (rebuildEA rebuild v p) (rebuild v t) (rebuild v e)
    Awhile p f a        -> Awhile (rebuildAfun rebuild v p) (rebuildAfun rebuild v f) (rebuild v a)
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
    While p f x         -> While (rebuildFA k v p) (rebuildFA k v f) (rebuildEA k v x)
    PrimConst c         -> PrimConst c
    PrimApp f x         -> PrimApp f (rebuildEA k v x)
    Index a sh          -> Index (k v a) (rebuildEA k v sh)
    LinearIndex a i     -> LinearIndex (k v a) (rebuildEA k v i)
    Shape a             -> Shape (k v a)
    ShapeSize sh        -> ShapeSize (rebuildEA k v sh)
    Intersect s t       -> Intersect (rebuildEA k v s) (rebuildEA k v t)
    Foreign ff f e      -> Foreign ff f (rebuildEA k v e)

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

