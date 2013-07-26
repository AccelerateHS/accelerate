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
  inline, substitute,

  -- ** Weakening
  (:>),
  weakenA, weakenEA, weakenFA,
  weakenE, weakenFE, weakenAE,

  -- ** Rebuilding terms
  RebuildAcc,

  -- *** With respect to both environments
  rebuildPreOpenExp, rebuildPreOpenAcc, rebuildFun,
  rebuildAfun,

  -- *** With respect to the scalar environment
  rebuildE, rebuildFE, rebuildAE,

  -- *** With respect to the array environment
  rebuildA, rebuildFA, rebuildEA,

  -- *** For OpenAcc
  rebuildOpenAcc,

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
infixr `substitute`

-- | Replace the first variable with the given expression. The environment
-- shrinks.
--
inline :: Elt t
       => RebuildAcc acc
       -> PreOpenExp acc (env, s) aenv t
       -> PreOpenExp acc env      aenv s
       -> PreOpenExp acc env      aenv t
inline k f g = Stats.substitution "inline" $ rebuildE k (subTop g) f
  where
    subTop :: Elt t => PreOpenExp acc env aenv s -> Idx (env, s) t -> PreOpenExp acc env aenv t
    subTop s ZeroIdx      = s
    subTop _ (SuccIdx ix) = Var ix

-- | Replace an expression that uses the top environment variable with another.
-- The result of the first is let bound into the second.
--
substitute :: (Elt b, Elt c)
           => RebuildAcc acc
           -> PreOpenExp acc (env, b) aenv c
           -> PreOpenExp acc (env, a) aenv b
           -> PreOpenExp acc (env, a) aenv c
substitute k f g
  | Stats.substitution "substitute" False = undefined

  | Var ZeroIdx <- g    = f     -- don't rebind an identity function
  | otherwise           = Let g $ rebuildE k split f
  where
    split :: Elt c => Idx (env,b) c -> PreOpenExp acc ((env,a),b) aenv c
    split ZeroIdx       = Var ZeroIdx
    split (SuccIdx ix)  = Var (SuccIdx (SuccIdx ix))

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

weakenA :: RebuildAcc acc -> aenv :> aenv' -> PreOpenAcc acc env aenv a -> PreOpenAcc acc env aenv' a
weakenA k v = Stats.substitution "weakenA" . rebuildA k (Avar . v)

weakenEA :: RebuildAcc acc -> aenv :> aenv' -> PreOpenExp acc env aenv t -> PreOpenExp acc env aenv' t
weakenEA k v = Stats.substitution "weakenEA" . rebuildEA k (Avar . v)

weakenFA :: RebuildAcc acc -> aenv :> aenv' -> PreOpenFun acc env aenv f -> PreOpenFun acc env aenv' f
weakenFA k v = Stats.substitution "weakenFA" . rebuildFA k (Avar . v)


weakenE :: RebuildAcc acc -> env :> env' -> PreOpenExp acc env aenv t -> PreOpenExp acc env' aenv t
weakenE k v = Stats.substitution "weakenE" . rebuildE k (Var . v)

weakenFE :: RebuildAcc acc -> env :> env' -> PreOpenFun acc env aenv f -> PreOpenFun acc env' aenv f
weakenFE k v = Stats.substitution "weakenFE" . rebuildFE k (Var . v)

weakenAE :: RebuildAcc acc -> env :> env' -> PreOpenAcc acc env aenv t -> PreOpenAcc acc env' aenv t
weakenAE k v = Stats.substitution "weakenAE" . rebuildAE k (Var . v)

{-# RULES
"weakenA/weakenA" forall a (k :: RebuildAcc acc) (v1 :: env' :> env'') (v2 :: env :> env').
    weakenA k v1 (weakenA k v2 a) = weakenA k (v1 . v2) a

"weakenEA/weakenEA" forall a (k :: RebuildAcc acc) (v1 :: env' :> env'') (v2 :: env :> env').
    weakenEA k v1 (weakenEA k v2 a) = weakenEA k (v1 . v2) a

"weakenFA/weakenFA" forall a (k :: RebuildAcc acc) (v1 :: env' :> env'') (v2 :: env :> env').
    weakenFA k v1 (weakenFA k v2 a) = weakenFA k (v1 . v2) a

"weakenE/weakenE" forall e (k :: RebuildAcc acc) (v1 :: env' :> env'') (v2 :: env :> env').
    weakenE k v1 (weakenE k v2 e) = weakenE k (v1 . v2) e

"weakenFE/weakenFE" forall e (k :: RebuildAcc acc) (v1 :: env' :> env'') (v2 :: env :> env').
    weakenFE k v1 (weakenFE k v2 e) = weakenFE k (v1 . v2) e
 #-}

-- Simultaneous Substitution ===================================================
--

-- The scalar environment
-- ------------------

-- SEE: [Renaming and Substitution]
-- SEE: [Weakening]
--
class SyntacticExp f where
  varIn         :: Elt t => Idx env t        -> f acc env aenv t
  expOut        :: Elt t => f acc env aenv t -> PreOpenExp acc env aenv t
  weakenExp     :: Elt t => RebuildAcc acc -> f acc env aenv t -> f acc (env, s) aenv t
  weakenExpAcc  :: Elt t => RebuildAcc acc -> f acc env aenv t -> f acc env (aenv, s) t

newtype IdxE (acc :: * -> * -> * -> *) env aenv t = IE { unIE :: Idx env t }

instance SyntacticExp IdxE where
  varIn         = IE
  expOut        = Var . unIE
  weakenExp _   = IE . SuccIdx . unIE
  weakenExpAcc _ = IE . unIE

instance SyntacticExp PreOpenExp where
  varIn         = Var
  expOut        = id
  weakenExp k   = rebuildE k (weakenExp k . IE)
  weakenExpAcc k = rebuildEA k (weakenAcc k . IA)

shiftE
    :: (SyntacticExp f, Elt t)
    => RebuildAcc acc
    -> (forall t'. Elt t' => Idx env t' -> f acc env' aenv t')
    -> Idx     (env,  s)      t
    -> f   acc (env', s) aenv t
shiftE _ _ ZeroIdx      = varIn ZeroIdx
shiftE k v (SuccIdx ix) = weakenExp k (v ix)

rebuildPreOpenExp
    :: (SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> fe acc env' aenv' t')
    -> (forall t'. Arrays t' => Idx aenv t' -> fa acc env' aenv' t')
    -> PreOpenExp acc env  aenv t
    -> PreOpenExp acc env' aenv' t
rebuildPreOpenExp k v av exp =
  case exp of
    Let a b             -> Let (rebuildPreOpenExp k v av a) (rebuildPreOpenExp k (shiftE k v) (weakenAccExp k . av) b)
    Var ix              -> expOut (v ix)
    Const c             -> Const c
    Tuple tup           -> Tuple (rebuildTup k v av tup)
    Prj tup e           -> Prj tup (rebuildPreOpenExp k v av e)
    IndexNil            -> IndexNil
    IndexCons sh sz     -> IndexCons (rebuildPreOpenExp k v av sh) (rebuildPreOpenExp k v av sz)
    IndexHead sh        -> IndexHead (rebuildPreOpenExp k v av sh)
    IndexTail sh        -> IndexTail (rebuildPreOpenExp k v av sh)
    IndexAny            -> IndexAny
    IndexSlice x ix sh  -> IndexSlice x (rebuildPreOpenExp k v av ix) (rebuildPreOpenExp k v av sh)
    IndexFull x ix sl   -> IndexFull x (rebuildPreOpenExp k v av ix) (rebuildPreOpenExp k v av sl)
    ToIndex sh ix       -> ToIndex (rebuildPreOpenExp k v av sh) (rebuildPreOpenExp k v av ix)
    FromIndex sh ix     -> FromIndex (rebuildPreOpenExp k v av sh) (rebuildPreOpenExp k v av ix)
    Cond p t e          -> Cond (rebuildPreOpenExp k v av p) (rebuildPreOpenExp k v av t) (rebuildPreOpenExp k v av e)
    Iterate n f x       -> Iterate (rebuildPreOpenExp k v av n) (rebuildPreOpenExp k (shiftE k v) (weakenAccExp k . av) f) (rebuildPreOpenExp k v av x)
    PrimConst c         -> PrimConst c
    PrimApp f x         -> PrimApp f (rebuildPreOpenExp k v av x)
    Index a sh          -> Index (k v av a) (rebuildPreOpenExp k v av sh)
    LinearIndex a i     -> LinearIndex (k v av a) (rebuildPreOpenExp k v av i)
    Shape a             -> Shape (k v av a)
    ShapeSize sh        -> ShapeSize (rebuildPreOpenExp k v av sh)
    Intersect s t       -> Intersect (rebuildPreOpenExp k v av s) (rebuildPreOpenExp k v av t)
    Foreign ff f e      -> Foreign ff f (rebuildPreOpenExp k v av e)

rebuildTup
    :: (SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> fe acc env' aenv' t')
    -> (forall t'. Arrays t' => Idx aenv t' -> fa acc env' aenv' t')
    -> Tuple (PreOpenExp acc env  aenv)  t
    -> Tuple (PreOpenExp acc env' aenv') t
rebuildTup k v av tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> rebuildTup k v av t `SnocTup` rebuildPreOpenExp k v av e

rebuildFun
    :: (SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> fe acc env' aenv' t')
    -> (forall t'. Arrays t' => Idx aenv t' -> fa acc env' aenv' t')
    -> PreOpenFun acc env  aenv  t
    -> PreOpenFun acc env' aenv' t
rebuildFun k v av fun =
  case fun of
    Body e      -> Body (rebuildPreOpenExp k v av e)
    Lam f       -> Lam (rebuildFun k (shiftE k v) (weakenAccExp k . av) f)

-- The array environment
-- -----------------

type RebuildAcc acc =
  forall env env' aenv aenv' fa fe a. (SyntacticAcc fa, SyntacticExp fe)
    => (forall e'. Elt e' => Idx env e' -> fe acc env' aenv' e')
    -> (forall a'. Arrays a' => Idx aenv a' -> fa acc env' aenv' a')
    -> acc env  aenv  a
    -> acc env' aenv' a

class SyntacticAcc f where
  avarIn        :: Arrays t => Idx aenv t     -> f acc env aenv t
  accOut        :: Arrays t => f acc env aenv t   -> PreOpenAcc acc env aenv t
  weakenAcc     :: Arrays t => RebuildAcc acc -> f acc env aenv t -> f acc env (aenv, s) t
  weakenAccExp  :: Arrays t => RebuildAcc acc -> f acc env aenv t -> f acc (env,s) aenv t

newtype IdxA (acc :: * -> * -> * -> *) env aenv t = IA { unIA :: Idx aenv t }

instance SyntacticAcc IdxA where
  avarIn         = IA
  accOut         = Avar . unIA
  weakenAcc _    = IA . SuccIdx . unIA
  weakenAccExp _ = IA . unIA

instance SyntacticAcc PreOpenAcc where
  avarIn        = Avar
  accOut        = id
  weakenAcc k   = rebuildA k (weakenAcc k . IA)
  weakenAccExp k = rebuildAE k (weakenExp k . IE)

shiftA
    :: (SyntacticAcc f, Arrays t)
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f acc env aenv' t')
    -> Idx         (aenv,  s) t
    -> f   acc env (aenv', s) t
shiftA _ _ ZeroIdx      = avarIn ZeroIdx
shiftA k v (SuccIdx ix) = weakenAcc k (v ix)

rebuildPreOpenAcc
    :: (SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> fe acc env' aenv' t')
    -> (forall t'. Arrays t' => Idx aenv t' -> fa acc env' aenv' t')
    -> PreOpenAcc acc env  aenv  t
    -> PreOpenAcc acc env' aenv' t
rebuildPreOpenAcc k v av acc =
  case acc of
    Alet a b            -> Alet (k v av a) (k (weakenExpAcc k . v) (shiftA k av) b)
    Avar ix             -> accOut (av ix)
    Atuple tup          -> Atuple (rebuildAtup k v av tup)
    Aprj tup a          -> Aprj tup (k v av a)
    Apply f a           -> Apply (rebuildAfun k v av f) (k v av a)
    Aforeign ff afun as -> Aforeign ff afun (k v av as)
    Acond p t e         -> Acond (rebuildPreOpenExp k v av p) (k v av t) (k v av e)
    Use a               -> Use a
    Unit e              -> Unit (rebuildPreOpenExp k v av e)
    Reshape e a         -> Reshape (rebuildPreOpenExp k v av e) (k v av a)
    Generate e f        -> Generate (rebuildPreOpenExp k v av e) (rebuildFun k v av f)
    Transform sh ix f a -> Transform (rebuildPreOpenExp k v av sh) (rebuildFun k v av ix) (rebuildFun k v av f) (k v av a)
    Replicate sl slix a -> Replicate sl (rebuildPreOpenExp k v av slix) (k v av a)
    Slice sl a slix     -> Slice sl (k v av a) (rebuildPreOpenExp k v av slix)
    Map f a             -> Map (rebuildFun k v av f) (k v av a)
    ZipWith f a1 a2     -> ZipWith (rebuildFun k v av f) (k v av a1) (k v av a2)
    Fold f z a          -> Fold (rebuildFun k v av f) (rebuildPreOpenExp k v av z) (k v av a)
    Fold1 f a           -> Fold1 (rebuildFun k v av f) (k v av a)
    FoldSeg f z a s     -> FoldSeg (rebuildFun k v av f) (rebuildPreOpenExp k v av z) (k v av a) (k v av s)
    Fold1Seg f a s      -> Fold1Seg (rebuildFun k v av f) (k v av a) (k v av s)
    Scanl f z a         -> Scanl (rebuildFun k v av f) (rebuildPreOpenExp k v av z) (k v av a)
    Scanl' f z a        -> Scanl' (rebuildFun k v av f) (rebuildPreOpenExp k v av z) (k v av a)
    Scanl1 f a          -> Scanl1 (rebuildFun k v av f) (k v av a)
    Scanr f z a         -> Scanr (rebuildFun k v av f) (rebuildPreOpenExp k v av z) (k v av a)
    Scanr' f z a        -> Scanr' (rebuildFun k v av f) (rebuildPreOpenExp k v av z) (k v av a)
    Scanr1 f a          -> Scanr1 (rebuildFun k v av f) (k v av a)
    Permute f1 a1 f2 a2 -> Permute (rebuildFun k v av f1) (k v av a1) (rebuildFun k v av f2) (k v av a2)
    Backpermute sh f a  -> Backpermute (rebuildPreOpenExp k v av sh) (rebuildFun k v av f) (k v av a)
    Stencil f b a       -> Stencil (rebuildFun k v av f) b (k v av a)
    Stencil2 f b1 a1 b2 a2
                        -> Stencil2 (rebuildFun k v av f) b1 (k v av a1) b2 (k v av a2)
rebuildAfun
    :: (SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> fe acc env' aenv' t')
    -> (forall t'. Arrays t' => Idx aenv t' -> fa acc env' aenv' t')
    -> PreOpenAfun acc env  aenv  t
    -> PreOpenAfun acc env' aenv' t
rebuildAfun k v av afun =
  case afun of
    Abody b     -> Abody (k v av b)
    Alam f      -> Alam (rebuildAfun k (weakenExpAcc k . v) (shiftA k av) f)

rebuildAtup
    :: (SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> fe acc env' aenv' t')
    -> (forall t'. Arrays t' => Idx aenv t' -> fa acc env' aenv' t')
    -> Atuple (acc env  aenv)  t
    -> Atuple (acc env' aenv') t
rebuildAtup k v av atup =
  case atup of
    NilAtup      -> NilAtup
    SnocAtup t a -> rebuildAtup k v av t `SnocAtup` k v av a

-- With respect to the scalar environment
--

rebuildE
    :: SyntacticExp f
    => RebuildAcc acc
    -> (forall t'. Elt t' => Idx env t' -> f acc env' aenv t')
    -> PreOpenExp acc env  aenv t
    -> PreOpenExp acc env' aenv t
rebuildE k v = rebuildPreOpenExp k v (avarIn :: Arrays t => Idx aenv t -> IdxA acc env aenv t)

rebuildAE
    :: SyntacticExp f
    => RebuildAcc acc
    -> (forall t'. Elt t' => Idx env t' -> f acc env' aenv t')
    -> PreOpenAcc acc env  aenv t
    -> PreOpenAcc acc env' aenv t
rebuildAE k v = rebuildPreOpenAcc k v (avarIn :: Arrays t => Idx aenv t -> IdxA acc env aenv t)

rebuildFE
    :: SyntacticExp f
    => RebuildAcc acc
    -> (forall t'. Elt t' => Idx env t' -> f acc env' aenv t')
    -> PreOpenFun acc env  aenv t
    -> PreOpenFun acc env' aenv t
rebuildFE k v = rebuildFun k v (avarIn :: Arrays t => Idx aenv t -> IdxA acc env aenv t)

-- With respect to the array environment
--

rebuildA
    :: SyntacticAcc f
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f acc env aenv' t')
    -> PreOpenAcc acc env aenv  t
    -> PreOpenAcc acc env aenv' t
rebuildA k = rebuildPreOpenAcc k (varIn :: Elt t => Idx env t -> IdxE acc env aenv t)

rebuildEA
    :: SyntacticAcc f
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f acc env aenv' t')
    -> PreOpenExp acc env aenv  t
    -> PreOpenExp acc env aenv' t
rebuildEA k = rebuildPreOpenExp k (varIn :: Elt t => Idx env t -> IdxE acc env aenv t)

rebuildFA
    :: SyntacticAcc f
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f acc env aenv' t')
    -> PreOpenFun acc env aenv  t
    -> PreOpenFun acc env aenv' t
rebuildFA k = rebuildFun k (varIn :: Elt t => Idx env t -> IdxE acc env aenv t)

-- For OpenAcc

rebuildOpenAcc
    :: (SyntacticExp fe, SyntacticAcc fa)
    => (forall t'. Elt t'    => Idx env t'  -> fe OpenAcc env' aenv' t')
    -> (forall t'. Arrays t' => Idx aenv t' -> fa OpenAcc env' aenv' t')
    -> OpenAcc env  aenv  t
    -> OpenAcc env' aenv' t
rebuildOpenAcc v av (OpenAcc acc) = OpenAcc (rebuildPreOpenAcc rebuildOpenAcc v av acc)
