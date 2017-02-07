{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Substitution
-- Copyright   : [2012..2014] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
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
  (:>), Sink(..), SinkExp(..),

  -- ** Strengthening
  (:?>), strengthen, strengthenE,

  -- ** Rebuilding terms
  RebuildAcc, Rebuildable(..), RebuildableAcc,
  RebuildableExp(..), RebuildTup(..), RebuildAtup(..),

) where

import Control.Applicative                              hiding ( Const )
#if __GLASGOW_HASKELL__ <= 708
import Data.Traversable                                 ( traverse, sequenceA )
#endif
import Prelude                                          hiding ( exp, seq )

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar                ( Elt, Arrays, Tuple(..), Atuple(..) )
import qualified Data.Array.Accelerate.Debug.Stats      as Stats


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
--   rebuildPartial v :: OpenExp env aenv t -> OpenExp env' aenv t
--
-- The Syntactic class tells us what we need to know about 'f' if we want to be
-- able to rebuildPartial terms. In essence, the crucial functionality is to propagate
-- a class of operations on variables that is closed under shifting.
--
infixr `compose`
infixr `substitute`

-- | Replace the first variable with the given expression. The environment
-- shrinks.
--
inline :: (RebuildableExp f, AccCloE f ~ acc)
       => f (env, s) aenv t
       -> PreOpenExp acc env      aenv s
       -> f env      aenv t
inline f g = Stats.substitution "inline" $ rebuildE (subTop g) f

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
  | otherwise           = Let g $ rebuildE split f
  where
    split :: Elt c => Idx (env,b) c -> PreOpenExp acc ((env,a),b) aenv c
    split ZeroIdx       = Var ZeroIdx
    split (SuccIdx ix)  = Var (SuccIdx (SuccIdx ix))


-- | Composition of unary functions.
--
compose :: (RebuildableAcc acc, Elt c)
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

data Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  Identity f <*> Identity a = Identity (f a)
  pure a                    = Identity a

-- A class for rebuilding terms.
--
class Rebuildable f where
  {-# MINIMAL rebuildPartial #-}
  type AccClo f :: (* -> * -> *)

  rebuildPartial :: (Applicative f', SyntacticAcc fa)
                 => (forall a'. Arrays a' => Idx aenv a' -> f' (fa (AccClo f) aenv' a'))
                 -> f aenv  a
                 -> f' (f aenv' a)

  {-# INLINEABLE rebuildA #-}
  rebuildA :: (SyntacticAcc fa)
              => (forall a'. Arrays a' => Idx aenv a' -> fa (AccClo f) aenv' a')
              -> f aenv  a
              -> f aenv' a
  rebuildA av = runIdentity . rebuildPartial (Identity . av)

-- A class for rebuilding scalar terms.
--
class RebuildableExp f where
  type AccCloE (f :: * -> * -> * -> *) :: * -> * -> *

  {-# MINIMAL rebuildPartialE #-}
  rebuildPartialE :: (Applicative f', SyntacticExp fe)
                  => (forall e'. Elt e' => Idx env e' -> f' (fe (AccCloE f) env' aenv e'))
                  -> f env aenv  e
                  -> f' (f env' aenv e)

  {-# INLINABLE rebuildE #-}
  rebuildE :: SyntacticExp fe
           => (forall e'. Elt e' => Idx env e' -> fe (AccCloE f) env' aenv e')
           -> f env aenv  e
           -> f env' aenv e
  rebuildE v = runIdentity . rebuildPartialE (Identity . v)

-- Terms that are rebuildable and also recursive closures
--
type RebuildableAcc acc = (Rebuildable acc, AccClo acc ~ acc)

-- We can use the same plumbing to rebuildPartial all the things we want to rebuild.
--
instance RebuildableAcc acc => Rebuildable (PreOpenExp acc env) where
  type AccClo (PreOpenExp acc env) = acc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial = rebuildPreOpenExp rebuildPartial (pure . IE)

instance RebuildableAcc acc => Rebuildable (PreOpenFun acc env) where
  type AccClo (PreOpenFun acc env) = acc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial = rebuildFun rebuildPartial (pure . IE)

instance RebuildableAcc acc => Rebuildable (PreOpenAcc acc) where
  type AccClo (PreOpenAcc acc) = acc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial = rebuildPreOpenAcc rebuildPartial

instance RebuildableAcc acc => Rebuildable (PreOpenAfun acc) where
  type AccClo (PreOpenAfun acc) = acc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial = rebuildAfun rebuildPartial

instance RebuildableAcc acc => Rebuildable (PreOpenSeq index acc) where
  type AccClo (PreOpenSeq index acc) = acc
  rebuildPartial = rebuildSeq rebuildPartial

instance RebuildableAcc acc => Rebuildable (Producer index acc) where
  type AccClo (Producer index acc) = acc
  rebuildPartial = rebuildP rebuildPartial

instance RebuildableAcc acc => Rebuildable (Consumer index acc) where
  type AccClo (Consumer index acc) = acc
  rebuildPartial = rebuildC rebuildPartial

-- Tuples have to be handled specially.
newtype RebuildTup acc env aenv t = RebuildTup { unRTup :: Tuple (PreOpenExp acc env aenv) t }
newtype RebuildAtup acc aenv t = RebuildAtup { unRAtup :: Atuple (acc aenv) t }

instance RebuildableAcc acc => Rebuildable (RebuildTup acc env) where
  type AccClo (RebuildTup acc env) = acc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial v t = RebuildTup <$> rebuildTup rebuildPartial (pure . IE) v (unRTup t)

instance RebuildableAcc acc => Rebuildable (RebuildAtup acc) where
  type AccClo (RebuildAtup acc) = acc
  rebuildPartial v t = RebuildAtup <$> rebuildAtup rebuildPartial v (unRAtup t)

instance Rebuildable OpenAcc where
  type AccClo OpenAcc = OpenAcc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial = rebuildOpenAcc

instance RebuildableAcc acc => RebuildableExp (PreOpenExp acc) where
  type AccCloE (PreOpenExp acc) = acc
  {-# INLINEABLE rebuildPartialE #-}
  rebuildPartialE v = rebuildPreOpenExp rebuildPartial v (pure . IA)

instance RebuildableAcc acc => RebuildableExp (PreOpenFun acc) where
  type AccCloE (PreOpenFun acc) = acc
  {-# INLINEABLE rebuildPartialE #-}
  rebuildPartialE v = rebuildFun rebuildPartial v (pure . IA)

instance RebuildableAcc acc => RebuildableExp (RebuildTup acc) where
  type AccCloE (RebuildTup acc) = acc
  rebuildPartialE v = (RebuildTup <$>) . rebuildTup rebuildPartial v (pure . IA) . unRTup

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

class Sink f where
  weaken :: env :> env' -> f env t -> f env' t

  -- TLM: We can't use this default instance because it doesn't lead to
  --      specialised code. Perhaps the INLINEABLE pragma is ignored: GHC bug?
  --
  -- {-# INLINEABLE weaken #-}
  -- default weaken :: Rebuildable f => env :> env' -> f env t -> f env' t
  -- weaken k = Stats.substitution "weaken" . rebuildA (Avar . k)

--instance Rebuildable f => Sink f where -- undecidable, incoherent
--  weaken k = Stats.substitution "weaken" . rebuildA (Avar . k)

instance Sink Idx where
  {-# INLINEABLE weaken #-}
  weaken k = k

instance RebuildableAcc acc => Sink (PreOpenAcc acc) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (Avar . k)

instance RebuildableAcc acc => Sink (PreOpenAfun acc) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (Avar . k)

instance RebuildableAcc acc => Sink (RebuildAtup acc) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (Avar . k)

instance RebuildableAcc acc => Sink (PreOpenExp acc env) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (Avar . k)

instance RebuildableAcc acc => Sink (PreOpenFun acc env) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (Avar . k)

instance RebuildableAcc acc => Sink (RebuildTup acc env) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (Avar . k)

instance RebuildableAcc acc => Sink (PreOpenSeq index acc) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (Avar . k)

instance RebuildableAcc acc => Sink (Producer index acc) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (Avar . k)

instance RebuildableAcc acc => Sink (Consumer index acc) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (Avar . k)

instance Sink OpenAcc where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (Avar . k)

-- This rewrite rule is disabled because 'weaken' is now part of a type class.
-- As such, we cannot attach a NOINLINE pragma because it has many definitions.
-- {-# RULES
-- "weaken/weaken" forall a (v1 :: env' :> env'') (v2 :: env :> env').
--     weaken v1 (weaken v2 a) = weaken (v1 . v2) a
--  #-}

class SinkExp f where
  weakenE :: env :> env' -> f env aenv t -> f env' aenv t

  -- See comment in 'weaken'
  --
  -- {-# INLINEABLE weakenE #-}
  -- default weakenE :: RebuildableExp f => env :> env' -> f env aenv t -> f env' aenv t
  -- weakenE v = Stats.substitution "weakenE" . rebuildE (IE . v)

instance RebuildableAcc acc => SinkExp (PreOpenExp acc) where
  {-# INLINEABLE weakenE #-}
  weakenE v = Stats.substitution "weakenE" . rebuildE (IE . v)

instance RebuildableAcc acc => SinkExp (PreOpenFun acc) where
  {-# INLINEABLE weakenE #-}
  weakenE v = Stats.substitution "weakenE" . rebuildE (IE . v)

instance RebuildableAcc acc => SinkExp (RebuildTup acc) where
  {-# INLINEABLE weakenE #-}
  weakenE v = Stats.substitution "weakenE" . rebuildE (IE . v)

-- See above for why this is disabled.
-- {-# RULES
-- "weakenE/weakenE" forall a (v1 :: env' :> env'') (v2 :: env :> env').
--    weakenE v1 (weakenE v2 a) = weakenE (v1 . v2) a
--  #-}

-- NOTE: [Strengthening]
--
-- Strengthening is the dual of weakening. Shifting terms from one scope to a
-- smaller scope. Of course this is not always possible. If the term contains
-- any variables not in the new environment, then it cannot be strengthened.
-- This partial behaviour is captured with 'Maybe'.
--

-- The type of partially shifting terms from one context into another.
type env :?> env' = forall t'. Idx env t' -> Maybe (Idx env' t')

{-# INLINEABLE strengthen #-}
strengthen :: Rebuildable f => env :?> env' -> f env t -> Maybe (f env' t)
strengthen k = rebuildPartial (fmap IA . k)

{-# INLINEABLE strengthenE #-}
strengthenE :: RebuildableExp f => env :?> env' -> f env aenv t -> Maybe (f env' aenv t)
strengthenE k = rebuildPartialE (fmap IE . k)

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

newtype IdxE (acc :: * -> * -> *) env aenv t = IE { unIE :: Idx env t }

instance SyntacticExp IdxE where
  varIn          = IE
  expOut         = Var . unIE
  weakenExp _    = IE . SuccIdx . unIE
  weakenExpAcc _ = IE . unIE

instance SyntacticExp PreOpenExp where
  varIn          = Var
  expOut         = id
  weakenExp k    = runIdentity . rebuildPreOpenExp k (Identity . weakenExp k . IE) (Identity . IA)
  weakenExpAcc k = runIdentity . rebuildPreOpenExp k (Identity . IE) (Identity . weakenAcc k . IA)

{-# INLINEABLE shiftE #-}
shiftE
    :: (Applicative f, SyntacticExp fe, Elt t)
    => RebuildAcc acc
    -> (forall t'. Elt t' => Idx env t' -> f (fe acc env' aenv t'))
    -> Idx       (env,  s)      t
    -> f (fe acc (env', s) aenv t)
shiftE _ _ ZeroIdx      = pure $ varIn ZeroIdx
shiftE k v (SuccIdx ix) = weakenExp k <$> (v ix)

{-# INLINEABLE rebuildPreOpenExp #-}
rebuildPreOpenExp
    :: (Applicative f, SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> f (fe acc env' aenv' t'))
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc aenv' t'))
    -> PreOpenExp acc env  aenv t
    -> f (PreOpenExp acc env' aenv' t)
rebuildPreOpenExp k v av exp =
  case exp of
    Const c             -> pure (Const c)
    PrimConst c         -> pure (PrimConst c)
    IndexNil            -> pure IndexNil
    IndexAny            -> pure IndexAny
    Var ix              -> expOut          <$> v ix
    Let a b             -> Let             <$> rebuildPreOpenExp k v av a  <*> rebuildPreOpenExp k (shiftE k v) av b
    Tuple tup           -> Tuple           <$> rebuildTup k v av tup
    Prj tup e           -> Prj tup         <$> rebuildPreOpenExp k v av e
    IndexCons sh sz     -> IndexCons       <$> rebuildPreOpenExp k v av sh <*> rebuildPreOpenExp k v av sz
    IndexHead sh        -> IndexHead       <$> rebuildPreOpenExp k v av sh
    IndexTail sh        -> IndexTail       <$> rebuildPreOpenExp k v av sh
    IndexSlice x ix sh  -> IndexSlice x ix <$> rebuildPreOpenExp k v av sh
    IndexFull x ix sl   -> IndexFull x     <$> rebuildPreOpenExp k v av ix <*> rebuildPreOpenExp k v av sl
    ToIndex sh ix       -> ToIndex         <$> rebuildPreOpenExp k v av sh <*> rebuildPreOpenExp k v av ix
    FromIndex sh ix     -> FromIndex       <$> rebuildPreOpenExp k v av sh <*> rebuildPreOpenExp k v av ix
    ToSlice x sh ix     -> ToSlice x       <$> rebuildPreOpenExp k v av sh <*> rebuildPreOpenExp k v av ix
    IndexTrans sh       -> IndexTrans      <$> rebuildPreOpenExp k v av sh
    Cond p t e          -> Cond            <$> rebuildPreOpenExp k v av p  <*> rebuildPreOpenExp k v av t  <*> rebuildPreOpenExp k v av e
    While p f x         -> While           <$> rebuildFun k v av p         <*> rebuildFun k v av f         <*> rebuildPreOpenExp k v av x
    PrimApp f x         -> PrimApp f       <$> rebuildPreOpenExp k v av x
    Index a sh          -> Index           <$> k av a                      <*> rebuildPreOpenExp k v av sh
    LinearIndex a i     -> LinearIndex     <$> k av a                      <*> rebuildPreOpenExp k v av i
    Shape a             -> Shape           <$> k av a
    ShapeSize sh        -> ShapeSize       <$> rebuildPreOpenExp k v av sh
    Intersect s t       -> Intersect       <$> rebuildPreOpenExp k v av s  <*> rebuildPreOpenExp k v av t
    Union s t           -> Union           <$> rebuildPreOpenExp k v av s  <*> rebuildPreOpenExp k v av t
    Foreign ff f e      -> Foreign ff f    <$> rebuildPreOpenExp k v av e

{-# INLINEABLE rebuildTup #-}
rebuildTup
    :: (Applicative f, SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> f (fe acc env' aenv' t'))
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc aenv' t'))
    -> Tuple (PreOpenExp acc env  aenv)  t
    -> f (Tuple (PreOpenExp acc env' aenv') t)
rebuildTup k v av tup =
  case tup of
    NilTup      -> pure NilTup
    SnocTup t e -> SnocTup <$> rebuildTup k v av t <*> rebuildPreOpenExp k v av e

{-# INLINEABLE rebuildFun #-}
rebuildFun
    :: (Applicative f, SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> f (fe acc env' aenv' t'))
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc aenv' t'))
    -> PreOpenFun acc env  aenv  t
    -> f (PreOpenFun acc env' aenv' t)
rebuildFun k v av fun =
  case fun of
    Body e      -> Body <$> rebuildPreOpenExp k v av e
    Lam f       -> Lam  <$> rebuildFun k (shiftE k v) av f

-- The array environment
-- -----------------

type RebuildAcc acc =
  forall aenv aenv' f fa a. (Applicative f, SyntacticAcc fa)
    => (forall a'. Arrays a' => Idx aenv a' -> f (fa acc aenv' a'))
    -> acc aenv  a
    -> f (acc aenv' a)

class SyntacticAcc f where
  avarIn        :: Arrays t => Idx aenv t     -> f acc aenv t
  accOut        :: Arrays t => f acc aenv t   -> PreOpenAcc acc aenv t
  weakenAcc     :: Arrays t => RebuildAcc acc -> f acc aenv t -> f acc (aenv, s) t

newtype IdxA (acc :: * -> * -> *) aenv t = IA { unIA :: Idx aenv t }

instance SyntacticAcc IdxA where
  avarIn         = IA
  accOut         = Avar . unIA
  weakenAcc _    = IA . SuccIdx . unIA

instance SyntacticAcc PreOpenAcc where
  avarIn        = Avar
  accOut        = id
  weakenAcc k   = runIdentity . rebuildPreOpenAcc k (Identity . weakenAcc k . IA)

{-# INLINEABLE shiftA #-}
shiftA
    :: (Applicative f, SyntacticAcc fa, Arrays t)
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc aenv' t'))
    -> Idx         (aenv,  s) t
    -> f (fa   acc (aenv', s) t)
shiftA _ _ ZeroIdx      = pure $ avarIn ZeroIdx
shiftA k v (SuccIdx ix) = weakenAcc k <$> v ix

{-# INLINEABLE rebuildPreOpenAcc #-}
rebuildPreOpenAcc
    :: (Applicative f, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc aenv' t'))
    -> PreOpenAcc acc aenv  t
    -> f (PreOpenAcc acc aenv' t)
rebuildPreOpenAcc k av acc =
  case acc of
    Use a                   -> pure (Use a)
    Alet a b                -> Alet         <$> k av a <*> k (shiftA k av) b
    Avar ix                 -> accOut       <$> av ix
    Atuple tup              -> Atuple       <$> rebuildAtup k av tup
    Aprj tup a              -> Aprj tup     <$> k av a
    Apply f a               -> Apply        <$> rebuildAfun k av f <*> k av a
    Acond p t e             -> Acond        <$> rebuildPreOpenExp k (pure . IE) av p <*> k av t <*> k av e
    Awhile p f a            -> Awhile       <$> rebuildAfun k av p <*> rebuildAfun k av f <*> k av a
    Subarray ix sh a        -> Subarray     <$> rebuildPreOpenExp k (pure . IE) av ix <*> rebuildPreOpenExp k (pure . IE) av sh <*> pure a
    Unit e                  -> Unit         <$> rebuildPreOpenExp k (pure . IE) av e
    Reshape e a             -> Reshape      <$> rebuildPreOpenExp k (pure . IE) av e <*> k av a
    Generate e f            -> Generate     <$> rebuildPreOpenExp k (pure . IE) av e <*> rebuildFun k (pure . IE) av f
    Transform sh ix f a     -> Transform    <$> rebuildPreOpenExp k (pure . IE) av sh <*> rebuildFun k (pure . IE) av ix <*> rebuildFun k (pure . IE) av f <*> k av a
    Replicate sl slix a     -> Replicate sl <$> rebuildPreOpenExp k (pure . IE) av slix <*> k av a
    Slice sl a slix         -> Slice sl     <$> k av a <*> rebuildPreOpenExp k (pure . IE) av slix
    Map f a                 -> Map          <$> rebuildFun k (pure . IE) av f <*> k av a
    ZipWith f a1 a2         -> ZipWith      <$> rebuildFun k (pure . IE) av f <*> k av a1 <*> k av a2
    Fold f z a              -> Fold         <$> rebuildFun k (pure . IE) av f <*> rebuildPreOpenExp k (pure . IE) av z <*> k av a
    Fold1 f a               -> Fold1        <$> rebuildFun k (pure . IE) av f <*> k av a
    FoldSeg f z a s         -> FoldSeg      <$> rebuildFun k (pure . IE) av f <*> rebuildPreOpenExp k (pure . IE) av z <*> k av a <*> k av s
    Fold1Seg f a s          -> Fold1Seg     <$> rebuildFun k (pure . IE) av f <*> k av a <*> k av s
    Scanl f z a             -> Scanl        <$> rebuildFun k (pure . IE) av f <*> rebuildPreOpenExp k (pure . IE) av z <*> k av a
    Scanl' f z a            -> Scanl'       <$> rebuildFun k (pure . IE) av f <*> rebuildPreOpenExp k (pure . IE) av z <*> k av a
    Scanl1 f a              -> Scanl1       <$> rebuildFun k (pure . IE) av f <*> k av a
    Scanr f z a             -> Scanr        <$> rebuildFun k (pure . IE) av f <*> rebuildPreOpenExp k (pure . IE) av z <*> k av a
    Scanr' f z a            -> Scanr'       <$> rebuildFun k (pure . IE) av f <*> rebuildPreOpenExp k (pure . IE) av z <*> k av a
    Scanr1 f a              -> Scanr1       <$> rebuildFun k (pure . IE) av f <*> k av a
    Permute f1 a1 f2 a2     -> Permute      <$> rebuildFun k (pure . IE) av f1 <*> k av a1 <*> rebuildFun k (pure . IE) av f2 <*> k av a2
    Backpermute sh f a      -> Backpermute  <$> rebuildPreOpenExp k (pure . IE) av sh <*> rebuildFun k (pure . IE) av f <*> k av a
    Stencil f b a           -> Stencil      <$> rebuildFun k (pure . IE) av f <*> pure b <*> k av a
    Stencil2 f b1 a1 b2 a2  -> Stencil2     <$> rebuildFun k (pure . IE) av f <*> pure b1 <*> k av a1 <*> pure b2 <*> k av a2
    Collect min max i s     -> Collect      <$> rebuildPreOpenExp k (pure . IE) av min <*> traverse (rebuildPreOpenExp k (pure . IE) av) max <*> traverse (rebuildPreOpenExp k (pure . IE) av) i <*> rebuildSeq k av s
    Aforeign ff afun as     -> Aforeign ff afun <$> k av as

{-# INLINEABLE rebuildAfun #-}
rebuildAfun
    :: (Applicative f, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc aenv' t'))
    -> PreOpenAfun acc aenv  t
    -> f (PreOpenAfun acc aenv' t)
rebuildAfun k av afun =
  case afun of
    Abody b     -> Abody <$> k av b
    Alam f      -> Alam  <$> rebuildAfun k (shiftA k av) f

{-# INLINEABLE rebuildAtup #-}
rebuildAtup
    :: (Applicative f, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc aenv' t'))
    -> Atuple (acc aenv)  t
    -> f (Atuple (acc aenv') t)
rebuildAtup k av atup =
  case atup of
    NilAtup      -> pure NilAtup
    SnocAtup t a -> SnocAtup <$> rebuildAtup k av t <*> k av a

{-# INLINEABLE rebuildSeq #-}
rebuildSeq
    :: (SyntacticAcc fa, Applicative f)
    => RebuildAcc acc
    -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc aenv' t'))
    -> PreOpenSeq idx acc aenv t
    -> f (PreOpenSeq idx acc aenv' t)
rebuildSeq k v seq =
  case seq of
    Producer p s -> Producer <$> rebuildP k v p <*> rebuildSeq k (shiftA k v) s
    Consumer c   -> Consumer <$> rebuildC k v c
    Reify ty a   -> Reify ty <$> k v a

{-# INLINEABLE rebuildP #-}
rebuildP :: (SyntacticAcc fa, Applicative f)
         => RebuildAcc acc
         -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc aenv' t'))
         -> Producer idx acc aenv a
         -> f (Producer idx acc aenv' a)
rebuildP k v p =
  case p of
    Pull arrs           -> pure (Pull arrs)
    Subarrays sh arr    -> Subarrays <$> rebuildPreOpenExp k (pure . IE) v sh <*> pure arr
    FromSegs s n vs     -> FromSegs <$> k v s <*> rebuildPreOpenExp k (pure . IE) v n <*> k v vs
    Produce l f         -> Produce <$> sequenceA (rebuildPreOpenExp k (pure . IE) v <$> l) <*> rebuildAfun k v f
    -- MapBatch f c c' a x -> MapBatch <$> rebuildAfun k v f <*> rebuildAfun k v c <*> rebuildAfun k v c' <*> k v a <*> k v x
    ProduceAccum l f a  -> ProduceAccum <$> sequenceA (rebuildPreOpenExp k (pure . IE) v <$> l) <*> rebuildAfun k v f <*> k v a

{-# INLINEABLE rebuildC #-}
rebuildC :: forall idx acc fa f aenv aenv' a. (SyntacticAcc fa, Applicative f)
         => RebuildAcc acc
         -> (forall t'. Arrays t' => Idx aenv t' -> f (fa acc aenv' t'))
         -> Consumer idx acc aenv a
         -> f (Consumer idx acc aenv' a)
rebuildC k v c =
  case c of
    FoldBatch f a x -> FoldBatch <$> rebuildAfun k v f <*> k v a <*> k v x
    Stuple t        -> Stuple <$> rebuildT t
    Last a d        -> Last <$> k v a <*> k v d
    Elements x      -> Elements <$> k v x
    Tabulate x      -> Tabulate <$> k v x
  where
    rebuildT :: Atuple (PreOpenSeq idx acc aenv) t -> f (Atuple (PreOpenSeq idx acc aenv') t)
    rebuildT NilAtup        = pure NilAtup
    rebuildT (SnocAtup t s) = SnocAtup <$> rebuildT t <*> rebuildSeq k v s

-- For OpenAcc

{-# INLINEABLE rebuildOpenAcc #-}
rebuildOpenAcc
    :: (Applicative f, SyntacticAcc fa)
    => (forall t'. Arrays t' => Idx aenv t' -> f (fa OpenAcc aenv' t'))
    -> OpenAcc aenv  t
    -> f (OpenAcc aenv' t)
rebuildOpenAcc av (OpenAcc acc) = OpenAcc <$> rebuildPreOpenAcc rebuildOpenAcc av acc
