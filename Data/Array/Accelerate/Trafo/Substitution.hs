{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards  #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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

  -- ** Substitution
  substitute, subTop, subAtop,

  -- ** Weakening
  (:>), (:?>), Sink(..),

  -- ** Rebuilding terms
  RebuildAcc, SyntacticExp, SyntacticAcc,
  IdxA(..), IdxE(..), Rebuildable(..),
  RebuildableAcc, RebuildTup(..),

  -- *** With respect to both environments
  rebuildPreOpenExp, rebuildPreOpenAcc, rebuildFun,
  rebuildAfun, rebuildTup,

  -- *** For OpenAcc
  rebuildOpenAcc,

) where

import Prelude                                  hiding ( exp )
import Control.Applicative                      hiding ( Const )
import Data.Functor.Identity

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

-- | Replace an expression that uses the top environment variable with another.
-- The result of the first is let bound into the second.
--
substitute :: (RebuildableAcc acc, Elt b, Elt c)
           => PreOpenExp acc (env, b) aenv c
           -> PreOpenExp acc (env, a) aenv b
           -> PreOpenExp acc (env, a) aenv c
substitute f g
  | Stats.substitution "substitute" False = undefined

  | Var ZeroIdx <- g    = f     -- don't rebind an identity function
  | otherwise           = Let g $ rebuildPure split Avar f
  where
    split :: Elt c => Idx (env,b) c -> PreOpenExp acc ((env,a),b) aenv c
    split ZeroIdx       = Var ZeroIdx
    split (SuccIdx ix)  = Var (SuccIdx (SuccIdx ix))


subTop :: Elt t => PreOpenExp acc env aenv s -> Idx (env, s) t -> PreOpenExp acc env aenv t
subTop s ZeroIdx      = s
subTop _ (SuccIdx ix) = Var ix

subAtop :: Arrays t => PreOpenAcc acc env aenv s -> Idx (aenv, s) t -> PreOpenAcc acc env aenv t
subAtop t ZeroIdx       = t
subAtop _ (SuccIdx idx) = Avar idx

-- A class for rebuilding terms.
--
-- Minimal complete definition is 'AccClo' and rebuild'.
--
class Rebuildable f where

  type AccClo f :: (* -> * -> * -> *)

  rebuild :: (Applicative f', SyntacticExp fe, SyntacticAcc fa)
          => (forall e'. Elt e' => Idx env e' -> f' (fe (AccClo f) env' aenv' e'))
          -> (forall a'. Arrays a' => Idx aenv a' -> f' (fa (AccClo f) env' aenv' a'))
          -> f env  aenv  a
          -> f' (f env' aenv' a)

  rebuildPure :: (SyntacticExp fe, SyntacticAcc fa)
              => (forall e'. Elt e' => Idx env e' -> fe (AccClo f) env' aenv' e')
              -> (forall a'. Arrays a' => Idx aenv a' -> fa (AccClo f) env' aenv' a')
              -> f env  aenv  a
              -> f env' aenv' a
  rebuildPure v av = runIdentity . rebuild (Identity . v) (Identity . av)

  strengthenE :: env  :?> env'  -> f env aenv t -> Maybe (f env' aenv  t)
  strengthenE k = Stats.substitution "strengthenE" . rebuild (liftA Var . k) (Just . Avar)

  strengthenA :: aenv :?> aenv' -> f env aenv t -> Maybe (f env  aenv' t)
  strengthenA k = Stats.substitution "strengthenA" . rebuild (Just . Var) (liftA Avar . k)

  -- | Replace the first variable with the given expression. The environment
  -- shrinks.
  --
  inlineE :: f (env,s) aenv t -> PreOpenExp (AccClo f) env aenv s -> f env aenv t
  inlineE f g = Stats.substitution "inlineE" $ rebuildPure (subTop g) Avar f

  -- | Replace the first variable with the given array expression. The environment
  -- shrinks.
  --
  inlineA :: f env (aenv,s) t -> PreOpenAcc (AccClo f) env aenv s -> f env aenv t
  inlineA f g = Stats.substitution "inlineA" $ rebuildPure Var (subAtop g) f

-- Terms that are rebuildable and also recursive closures
--
-- RCE: Perhaps this should just be replaced with a constraint kind?
--
class (Rebuildable acc, AccClo acc ~ acc) => RebuildableAcc acc where

instance (Rebuildable acc, AccClo acc ~ acc) => RebuildableAcc acc

-- We can use the same plumbing to rebuild all the things we want to rebuild.
--
instance RebuildableAcc acc => Rebuildable (PreOpenExp acc) where
  type AccClo (PreOpenExp acc) = acc
  rebuild = rebuildPreOpenExp rebuild

instance RebuildableAcc acc => Rebuildable (PreOpenFun acc) where
  type AccClo (PreOpenFun acc) = acc
  rebuild = rebuildFun rebuild

instance RebuildableAcc acc => Rebuildable (PreOpenAcc acc) where
  type AccClo (PreOpenAcc acc) = acc
  rebuild = rebuildPreOpenAcc rebuild

instance RebuildableAcc acc => Rebuildable (PreOpenAfun acc) where
  type AccClo (PreOpenAfun acc) = acc
  rebuild = rebuildAfun rebuild

-- Tuples have to be handled specially.
newtype RebuildTup acc env aenv t = RebuildTup { unRTup :: Tuple (PreOpenExp acc env aenv) t }

instance RebuildableAcc acc => Rebuildable (RebuildTup acc) where
  type AccClo (RebuildTup acc) = acc
  rebuild v av t = RebuildTup <$> rebuildTup rebuild v av (unRTup t)

instance Sink (IdxA acc) where
  weakenE _ (IA ix) = Stats.substitution "weakenE" $ IA ix
  weakenA k = Stats.substitution "weakenA" . IA . k . unIA

instance Sink (IdxE acc) where
  weakenE k = Stats.substitution "weakenE" . IE . k . unIE
  weakenA _ (IE ix) = Stats.substitution "weakenA" $ IE ix

instance Rebuildable OpenAcc where
  type AccClo OpenAcc = OpenAcc
  rebuild = rebuildOpenAcc

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

-- Not all things we want to weaken are rebuildable, so we separate out weakening into Sink.
--
class Sink f where
  weakenE :: env  :> env'  -> f env aenv t -> f env' aenv  t
  weakenA :: aenv :> aenv' -> f env aenv t -> f env  aenv' t

instance Rebuildable f => Sink f where -- undecidable, incoherent
  weakenE k = Stats.substitution "weakenE" . rebuildPure (Var . k) Avar
  weakenA k = Stats.substitution "weakenA" . rebuildPure Var (Avar . k)

{-# RULES
"weakenA/weakenA" forall a (v1 :: env' :> env'') (v2 :: env :> env').
    weakenA v1 (weakenA v2 a) = weakenA (v1 . v2) a

"weakenE/weakenE" forall e (v1 :: env' :> env'') (v2 :: env :> env').
    weakenE v1 (weakenE v2 e) = weakenE (v1 . v2) e
 #-}

-- NOTE: [Strengthening]
--
-- Strengthening is the dual of weakening. Shifting terms from one scope to a
-- smaller scope. Of course this is not always possible. If the term contains
-- any variables not in the new environment, then it cannot be strengthened.
-- This partial behaviour is captured with 'Maybe'.
--

-- The type of partially shifting terms from one context into another.
type env :?> env' = forall t'. Idx env t' -> Maybe (Idx env' t')


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
  weakenExp k   = runIdentity . rebuildPreOpenExp k (Identity . weakenExp k . IE) (Identity . IA)
  weakenExpAcc k = runIdentity . rebuildPreOpenExp k (Identity . IE) (Identity . weakenAcc k . IA)

shiftE
    :: (Applicative f, SyntacticExp fe, Elt t)
    => RebuildAcc acc
    -> (forall t'. Elt t' => Idx env t' -> f (fe acc env' aenv t'))
    -> Idx     (env,  s)      t
    -> f (fe  acc (env', s) aenv t)
shiftE _ _ ZeroIdx      = pure $ varIn ZeroIdx
shiftE k v (SuccIdx ix) = weakenExp k <$> (v ix)

rebuildPreOpenExp
    :: (Applicative f, SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> f (fe acc env' aenv' t'))
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc env' aenv' t'))
    -> PreOpenExp acc env  aenv t
    -> f (PreOpenExp acc env' aenv' t)
rebuildPreOpenExp k v av exp =
  case exp of
    Let a b             -> Let <$> rebuildPreOpenExp k v av a <*> rebuildPreOpenExp k (shiftE k v) (liftA (weakenAccExp k) . av) b
    Var ix              -> expOut <$> v ix
    Const c             -> pure $ Const c
    Tuple tup           -> Tuple <$> rebuildTup k v av tup
    Prj tup e           -> Prj tup <$> rebuildPreOpenExp k v av e
    IndexNil            -> pure IndexNil
    IndexCons sh sz     -> IndexCons <$> rebuildPreOpenExp k v av sh <*> rebuildPreOpenExp k v av sz
    IndexHead sh        -> IndexHead <$> rebuildPreOpenExp k v av sh
    IndexTail sh        -> IndexTail <$> rebuildPreOpenExp k v av sh
    IndexAny            -> pure IndexAny
    IndexSlice x ix sh  -> IndexSlice x <$> rebuildPreOpenExp k v av ix <*> rebuildPreOpenExp k v av sh
    IndexFull x ix sl   -> IndexFull x <$> rebuildPreOpenExp k v av ix <*> rebuildPreOpenExp k v av sl
    ToIndex sh ix       -> ToIndex <$> rebuildPreOpenExp k v av sh <*> rebuildPreOpenExp k v av ix
    FromIndex sh ix     -> FromIndex <$> rebuildPreOpenExp k v av sh <*> rebuildPreOpenExp k v av ix
    Cond p t e          -> Cond <$> rebuildPreOpenExp k v av p <*> rebuildPreOpenExp k v av t <*> rebuildPreOpenExp k v av e
    While p f x         -> While <$> rebuildFun k v av p <*> rebuildFun k v av f <*> rebuildPreOpenExp k v av x
    PrimConst c         -> pure $ PrimConst c
    PrimApp f x         -> PrimApp f <$> rebuildPreOpenExp k v av x
    Index a sh          -> Index <$> k v av a <*> rebuildPreOpenExp k v av sh
    LinearIndex a i     -> LinearIndex <$> k v av a <*> rebuildPreOpenExp k v av i
    Shape a             -> Shape <$> k v av a
    ShapeSize sh        -> ShapeSize <$> rebuildPreOpenExp k v av sh
    Intersect s t       -> Intersect <$> rebuildPreOpenExp k v av s <*> rebuildPreOpenExp k v av t
    Foreign ff f e      -> Foreign ff f <$> rebuildPreOpenExp k v av e

rebuildTup
    :: (Applicative f, SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> f (fe acc env' aenv' t'))
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc env' aenv' t'))
    -> Tuple (PreOpenExp acc env  aenv)  t
    -> f (Tuple (PreOpenExp acc env' aenv') t)
rebuildTup k v av tup =
  case tup of
    NilTup      -> pure NilTup
    SnocTup t e -> SnocTup <$> rebuildTup k v av t <*> rebuildPreOpenExp k v av e

rebuildFun
    :: (Applicative f, SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> f (fe acc env' aenv' t'))
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc env' aenv' t'))
    -> PreOpenFun acc env  aenv  t
    -> f (PreOpenFun acc env' aenv' t)
rebuildFun k v av fun =
  case fun of
    Body e      -> Body <$> rebuildPreOpenExp k v av e
    Lam f       -> Lam  <$> rebuildFun k (shiftE k v) (liftA (weakenAccExp k) . av) f

-- The array environment
-- -----------------

type RebuildAcc acc =
  forall env env' aenv aenv' f fa fe a. (Applicative f, SyntacticAcc fa, SyntacticExp fe)
    => (forall e'. Elt e' => Idx env e' -> f (fe acc env' aenv' e'))
    -> (forall a'. Arrays a' => Idx aenv a' -> f (fa acc env' aenv' a'))
    -> acc env  aenv  a
    -> f (acc env' aenv' a)

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
  weakenAcc k   = runIdentity . rebuildPreOpenAcc k (Identity . Var) (Identity . weakenAcc k . IA)
  weakenAccExp k = runIdentity . rebuildPreOpenAcc k (Identity . weakenExp k . IE) (Identity . Avar)

shiftA
    :: (Applicative f, SyntacticAcc fa, Arrays t)
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc env aenv' t'))
    -> Idx         (aenv,  s) t
    -> f (fa   acc env (aenv', s) t)
shiftA _ _ ZeroIdx      = pure $ avarIn ZeroIdx
shiftA k v (SuccIdx ix) = weakenAcc k <$> v ix

rebuildPreOpenAcc
    :: (Applicative f, SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> f (fe acc env' aenv' t'))
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc env' aenv' t'))
    -> PreOpenAcc acc env  aenv  t
    -> f (PreOpenAcc acc env' aenv' t)
rebuildPreOpenAcc k v av acc =
  case acc of
    Alet a b            -> Alet <$> k v av a <*> k (liftA (weakenExpAcc k) . v) (shiftA k av) b
    Elet e a            -> Elet <$> rebuildPreOpenExp k v av e <*> k (shiftE k v) (liftA (weakenAccExp k) . av) a
    Avar ix             -> accOut <$> av ix
    Atuple tup          -> Atuple <$> rebuildAtup k v av tup
    Aprj tup a          -> Aprj tup <$> k v av a
    Apply f a           -> Apply <$> rebuildAfun k v av f <*> k v av a
    Aforeign ff afun as -> Aforeign ff afun <$> k v av as
    Acond p t e         -> Acond <$> rebuildPreOpenExp k v av p <*> k v av t <*> k v av e
    Awhile p f a        -> Awhile <$> rebuildAfun k v av p <*> rebuildAfun k v av f <*> k v av a
    Use a               -> pure $ Use a
    Unit e              -> Unit <$> rebuildPreOpenExp k v av e
    Reshape e a         -> Reshape <$> rebuildPreOpenExp k v av e <*> k v av a
    Generate e f        -> Generate <$> rebuildPreOpenExp k v av e <*> rebuildFun k v av f
    Transform sh ix f a -> Transform <$> rebuildPreOpenExp k v av sh <*> rebuildFun k v av ix <*> rebuildFun k v av f <*> k v av a
    Replicate sl slix a -> Replicate sl <$> rebuildPreOpenExp k v av slix <*> k v av a
    Slice sl a slix     -> Slice sl <$> k v av a <*> rebuildPreOpenExp k v av slix
    Map f a             -> Map <$> rebuildFun k v av f <*> k v av a
    ZipWith f a1 a2     -> ZipWith <$> rebuildFun k v av f <*> k v av a1 <*> k v av a2
    Fold f z a          -> Fold <$> rebuildFun k v av f <*> rebuildPreOpenExp k v av z <*> k v av a
    Fold1 f a           -> Fold1 <$> rebuildFun k v av f <*> k v av a
    FoldSeg f z a s     -> FoldSeg <$> rebuildFun k v av f <*> rebuildPreOpenExp k v av z <*> k v av a <*> k v av s
    Fold1Seg f a s      -> Fold1Seg <$> rebuildFun k v av f <*> k v av a <*> k v av s
    Scanl f z a         -> Scanl <$> rebuildFun k v av f <*> rebuildPreOpenExp k v av z <*> k v av a
    Scanl' f z a        -> Scanl' <$> rebuildFun k v av f <*> rebuildPreOpenExp k v av z <*> k v av a
    Scanl1 f a          -> Scanl1 <$> rebuildFun k v av f <*> k v av a
    Scanr f z a         -> Scanr <$> rebuildFun k v av f <*> rebuildPreOpenExp k v av z <*> k v av a
    Scanr' f z a        -> Scanr' <$> rebuildFun k v av f <*> rebuildPreOpenExp k v av z <*> k v av a
    Scanr1 f a          -> Scanr1 <$> rebuildFun k v av f <*> k v av a
    Permute f1 a1 f2 a2 -> Permute <$> rebuildFun k v av f1 <*> k v av a1 <*> rebuildFun k v av f2 <*> k v av a2
    Backpermute sh f a  -> Backpermute <$> rebuildPreOpenExp k v av sh <*> rebuildFun k v av f <*> k v av a
    Stencil f b a       -> Stencil <$> rebuildFun k v av f <*> pure b <*> k v av a
    Stencil2 f b1 a1 b2 a2
                        -> Stencil2 <$> rebuildFun k v av f <*> pure b1 <*> k v av a1 <*> pure b2 <*> k v av a2
rebuildAfun
    :: (Applicative f, SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> f (fe acc env' aenv' t'))
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc env' aenv' t'))
    -> PreOpenAfun acc env  aenv  t
    -> f (PreOpenAfun acc env' aenv' t)
rebuildAfun k v av afun =
  case afun of
    Abody b     -> Abody <$> k v av b
    Alam f      -> Alam  <$> rebuildAfun k (liftA (weakenExpAcc k) . v) (shiftA k av) f

rebuildAtup
    :: (Applicative f, SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> f (fe acc env' aenv' t'))
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc env' aenv' t'))
    -> Atuple (acc env  aenv)  t
    -> f (Atuple (acc env' aenv') t)
rebuildAtup k v av atup =
  case atup of
    NilAtup      -> pure NilAtup
    SnocAtup t a -> SnocAtup <$> rebuildAtup k v av t <*> k v av a

-- For OpenAcc

rebuildOpenAcc
    :: (Applicative f, SyntacticExp fe, SyntacticAcc fa)
    => (forall t'. Elt t'    => Idx env t'  -> f (fe OpenAcc env' aenv' t'))
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa OpenAcc env' aenv' t'))
    -> OpenAcc env  aenv  t
    -> f (OpenAcc env' aenv' t)
rebuildOpenAcc v av (OpenAcc acc) = OpenAcc <$> rebuildPreOpenAcc rebuildOpenAcc v av acc
