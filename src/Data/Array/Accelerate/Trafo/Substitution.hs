{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Substitution
-- Copyright   : [2012..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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
  RebuildableExp(..), RebuildTup(..), rebuildWeakenVar

) where

import Data.Kind
import Control.Applicative                              hiding ( Const )
import Prelude                                          hiding ( exp, seq )

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar                ( Elt, Tuple(..), Array )
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
inline :: RebuildableAcc acc
       => PreOpenExp acc (env, s) aenv t
       -> PreOpenExp acc env      aenv s
       -> PreOpenExp acc env      aenv t
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

subAtop :: PreOpenAcc acc aenv t -> ArrayVar (aenv, t) (Array sh2 e2) -> PreOpenAcc acc aenv (Array sh2 e2)
subAtop t (ArrayVar ZeroIdx      ) = t
subAtop _ (ArrayVar (SuccIdx idx)) = Avar $ ArrayVar idx

data Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  {-# INLINE fmap #-}
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  {-# INLINE (<*>) #-}
  {-# INLINE pure  #-}
  Identity f <*> Identity a = Identity (f a)
  pure a                    = Identity a

-- A class for rebuilding terms.
--
class Rebuildable f where
  {-# MINIMAL rebuildPartial #-}
  type AccClo f :: Type -> Type -> Type

  rebuildPartial :: (Applicative f', SyntacticAcc fa)
                 => (forall sh e. ArrayVar aenv (Array sh e) -> f' (fa (AccClo f) aenv' (Array sh e)))
                 -> f aenv  a
                 -> f' (f aenv' a)

  {-# INLINEABLE rebuildA #-}
  rebuildA :: (SyntacticAcc fa)
           => (forall sh e. ArrayVar aenv (Array sh e) -> fa (AccClo f) aenv' (Array sh e))
           -> f aenv  a
           -> f aenv' a
  rebuildA av = runIdentity . rebuildPartial (Identity . av)

-- A class for rebuilding scalar terms.
--
class RebuildableExp f where
  {-# MINIMAL rebuildPartialE #-}
  rebuildPartialE :: (Applicative f', SyntacticExp fe)
                  => (forall e'. Elt e' => Idx env e' -> f' (fe (AccClo (f env)) env' aenv e'))
                  -> f env aenv  e
                  -> f' (f env' aenv e)

  {-# INLINEABLE rebuildE #-}
  rebuildE :: SyntacticExp fe
           => (forall e'. Elt e' => Idx env e' -> fe (AccClo (f env)) env' aenv e')
           -> f env  aenv e
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
  rebuildPartial x = Stats.substitution "rebuild" $ rebuildPreOpenExp rebuildPartial (pure . IE) x

instance RebuildableAcc acc => Rebuildable (PreOpenFun acc env) where
  type AccClo (PreOpenFun acc env) = acc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial x = Stats.substitution "rebuild" $ rebuildFun rebuildPartial (pure . IE) x

instance RebuildableAcc acc => Rebuildable (PreOpenAcc acc) where
  type AccClo (PreOpenAcc acc) = acc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial x = Stats.substitution "rebuild" $ rebuildPreOpenAcc rebuildPartial x

instance RebuildableAcc acc => Rebuildable (PreOpenAfun acc) where
  type AccClo (PreOpenAfun acc) = acc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial x = Stats.substitution "rebuild" $ rebuildAfun rebuildPartial x

-- Tuples have to be handled specially.
newtype RebuildTup acc env aenv t = RebuildTup { unRTup :: Tuple (PreOpenExp acc env aenv) t }

instance RebuildableAcc acc => Rebuildable (RebuildTup acc env) where
  type AccClo (RebuildTup acc env) = acc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial v t = Stats.substitution "rebuild" . RebuildTup <$> rebuildTup rebuildPartial (pure . IE) v (unRTup t)

instance Rebuildable OpenAcc where
  type AccClo OpenAcc = OpenAcc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial x = Stats.substitution "rebuild" $ rebuildOpenAcc x

instance RebuildableAcc acc => RebuildableExp (PreOpenExp acc) where
  {-# INLINEABLE rebuildPartialE #-}
  rebuildPartialE v x = Stats.substitution "rebuild" $ rebuildPreOpenExp rebuildPartial v (pure . IA) x

instance RebuildableAcc acc => RebuildableExp (PreOpenFun acc) where
  {-# INLINEABLE rebuildPartialE #-}
  rebuildPartialE v x = Stats.substitution "rebuild" $ rebuildFun rebuildPartial v (pure . IA) x

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

class Sink f where
  weaken :: env :> env' -> f env t -> f env' t

  -- TLM: We can't use this default instance because it doesn't lead to
  --      specialised code. Perhaps the INLINEABLE pragma is ignored: GHC bug?
  --
  -- {-# INLINEABLE weaken #-}
  -- default weaken :: Rebuildable f => env :> env' -> f env t -> f env' t
  -- weaken k = Stats.substitution "weaken" . rebuildA rebuildWeakenVar

--instance Rebuildable f => Sink f where -- undecidable, incoherent
--  weaken k = Stats.substitution "weaken" . rebuildA rebuildWeakenVar

instance Sink Idx where
  {-# INLINEABLE weaken #-}
  weaken k = k

instance Sink ArrayVar where
  {-# INLINEABLE weaken #-}
  weaken k (ArrayVar ix) = ArrayVar (k ix)

instance Sink ArrayVars where
  {-# INLINEABLE weaken #-}
  weaken _  ArrayVarsNil       = ArrayVarsNil
  weaken k (ArrayVarsArray v)  = ArrayVarsArray $ weaken k v
  weaken k (ArrayVarsPair v w) = ArrayVarsPair (weaken k v) (weaken k w)

rebuildWeakenVar :: env :> env' -> ArrayVar env (Array sh e) -> PreOpenAcc acc env' (Array sh e)
rebuildWeakenVar k (ArrayVar idx) = Avar $ ArrayVar $ k idx

instance RebuildableAcc acc => Sink (PreOpenAcc acc) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (rebuildWeakenVar k)

instance RebuildableAcc acc => Sink (PreOpenAfun acc) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (rebuildWeakenVar k)

instance RebuildableAcc acc => Sink (PreOpenExp acc env) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (rebuildWeakenVar k)

instance RebuildableAcc acc => Sink (PreOpenFun acc env) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (rebuildWeakenVar k)

instance RebuildableAcc acc => Sink (RebuildTup acc env) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (rebuildWeakenVar k)

instance RebuildableAcc acc => Sink (PreBoundary acc) where
  {-# INLINEABLE weaken #-}
  weaken k bndy =
    case bndy of
      Clamp      -> Clamp
      Mirror     -> Mirror
      Wrap       -> Wrap
      Constant c -> Constant c
      Function f -> Function (weaken k f)

instance Sink OpenAcc where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (rebuildWeakenVar k)

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
strengthen :: forall f env env' t. Rebuildable f => env :?> env' -> f env t -> Maybe (f env' t)
strengthen k x = Stats.substitution "strengthen" $ rebuildPartial @f @Maybe @IdxA (\(ArrayVar idx) -> fmap (IA . ArrayVar) $ k idx) x -- (\(ArrayVar idx) -> fmap (IA . ArrayVar) $ k idx)

{-# INLINEABLE strengthenE #-}
strengthenE :: RebuildableExp f => env :?> env' -> f env aenv t -> Maybe (f env' aenv t)
strengthenE k x = Stats.substitution "strengthenE" $ rebuildPartialE (fmap IE . k) x

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
  -- weakenExpAcc  :: Elt t => RebuildAcc acc -> f acc env aenv t -> f acc env (aenv, s) t

newtype IdxE (acc :: Type -> Type -> Type) env aenv t = IE { unIE :: Idx env t }

instance SyntacticExp IdxE where
  varIn          = IE
  expOut         = Var . unIE
  weakenExp _    = IE . SuccIdx . unIE
  -- weakenExpAcc _ = IE . unIE

instance SyntacticExp PreOpenExp where
  varIn          = Var
  expOut         = id
  weakenExp k    = runIdentity . rebuildPreOpenExp k (Identity . weakenExp k . IE) (Identity . IA)
  -- weakenExpAcc k = runIdentity . rebuildPreOpenExp k (Identity . IE) (Identity . weakenAcc k . IA)

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
    -> RebuildAvar f fa acc aenv aenv'
    -> PreOpenExp acc env  aenv t
    -> f (PreOpenExp acc env' aenv' t)
rebuildPreOpenExp k v av exp =
  case exp of
    Const c             -> pure (Const c)
    PrimConst c         -> pure (PrimConst c)
    Undef               -> pure Undef
    IndexNil            -> pure IndexNil
    IndexAny            -> pure IndexAny
    Var ix              -> expOut       <$> v ix
    Let a b             -> Let          <$> rebuildPreOpenExp k v av a  <*> rebuildPreOpenExp k (shiftE k v) av b
    Tuple tup           -> Tuple        <$> rebuildTup k v av tup
    Prj tup e           -> Prj tup      <$> rebuildPreOpenExp k v av e
    IndexCons sh sz     -> IndexCons    <$> rebuildPreOpenExp k v av sh <*> rebuildPreOpenExp k v av sz
    IndexHead sh        -> IndexHead    <$> rebuildPreOpenExp k v av sh
    IndexTail sh        -> IndexTail    <$> rebuildPreOpenExp k v av sh
    IndexSlice x ix sh  -> IndexSlice x <$> rebuildPreOpenExp k v av ix <*> rebuildPreOpenExp k v av sh
    IndexFull x ix sl   -> IndexFull x  <$> rebuildPreOpenExp k v av ix <*> rebuildPreOpenExp k v av sl
    ToIndex sh ix       -> ToIndex      <$> rebuildPreOpenExp k v av sh <*> rebuildPreOpenExp k v av ix
    FromIndex sh ix     -> FromIndex    <$> rebuildPreOpenExp k v av sh <*> rebuildPreOpenExp k v av ix
    Cond p t e          -> Cond         <$> rebuildPreOpenExp k v av p  <*> rebuildPreOpenExp k v av t  <*> rebuildPreOpenExp k v av e
    While p f x         -> While        <$> rebuildFun k v av p         <*> rebuildFun k v av f         <*> rebuildPreOpenExp k v av x
    PrimApp f x         -> PrimApp f    <$> rebuildPreOpenExp k v av x
    Index a sh          -> Index        <$> k av a                      <*> rebuildPreOpenExp k v av sh
    LinearIndex a i     -> LinearIndex  <$> k av a                      <*> rebuildPreOpenExp k v av i
    Shape a             -> Shape        <$> k av a
    ShapeSize sh        -> ShapeSize    <$> rebuildPreOpenExp k v av sh
    Intersect s t       -> Intersect    <$> rebuildPreOpenExp k v av s  <*> rebuildPreOpenExp k v av t
    Union s t           -> Union        <$> rebuildPreOpenExp k v av s  <*> rebuildPreOpenExp k v av t
    Foreign ff f e      -> Foreign ff f <$> rebuildPreOpenExp k v av e
    Coerce e            -> Coerce       <$> rebuildPreOpenExp k v av e

{-# INLINEABLE rebuildTup #-}
rebuildTup
    :: (Applicative f, SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> (forall t'. Elt t'    => Idx env t'  -> f (fe acc env' aenv' t'))
    -> RebuildAvar f fa acc aenv aenv'
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
    -> RebuildAvar f fa acc aenv aenv'
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
    => RebuildAvar f fa acc aenv aenv'
    -> acc aenv a
    -> f (acc aenv' a)

newtype IdxA (acc :: Type -> Type -> Type) aenv t = IA { unIA :: ArrayVar aenv t }

class SyntacticAcc f where
  avarIn        :: ArrayVar aenv (Array sh e) -> f acc aenv (Array sh e)
  accOut        :: f acc aenv (Array sh e) -> PreOpenAcc acc aenv (Array sh e)
  weakenAcc     :: RebuildAcc acc -> f acc aenv (Array sh e) -> f acc (aenv, s) (Array sh e)

instance SyntacticAcc IdxA where
  avarIn                          = IA
  accOut                          = Avar . unIA
  weakenAcc _ (IA (ArrayVar idx)) = IA $ ArrayVar $ SuccIdx idx

instance SyntacticAcc PreOpenAcc where
  avarIn        = Avar
  accOut        = id
  weakenAcc k   = runIdentity . rebuildPreOpenAcc k (Identity . weakenAcc k . IA)

type RebuildAvar f (fa :: (* -> * -> *) -> * -> * -> *) acc aenv aenv' =
  forall sh e. ArrayVar aenv (Array sh e) -> f (fa acc aenv' (Array sh e))

{-# INLINEABLE shiftA #-}
shiftA
    :: (Applicative f, SyntacticAcc fa)
    => RebuildAcc acc
    -> RebuildAvar f fa acc aenv aenv'
    -> ArrayVar    (aenv,  s) (Array sh e)
    -> f (fa   acc (aenv', s) (Array sh e))
shiftA _ _ (ArrayVar ZeroIdx)      = pure $ avarIn $ ArrayVar ZeroIdx
shiftA k v (ArrayVar (SuccIdx ix)) = weakenAcc k <$> v (ArrayVar ix)

shiftA'
    :: (Applicative f, SyntacticAcc fa)
    => LeftHandSide t aenv1 aenv1'
    -> LeftHandSide t aenv2 aenv2'
    -> RebuildAcc acc
    -> RebuildAvar f fa acc aenv1  aenv2
    -> RebuildAvar f fa acc aenv1' aenv2'
shiftA' (LeftHandSideWildcard _) (LeftHandSideWildcard _) _ v = v
shiftA'  LeftHandSideArray        LeftHandSideArray       k v = shiftA k v
shiftA' (LeftHandSidePair a1 b1) (LeftHandSidePair a2 b2) k v = shiftA' b1 b2 k $ shiftA' a1 a2 k v
shiftA' _ _ _ _ = error "Substitution: left hand sides do not match"

{-# INLINEABLE rebuildOpenAcc #-}
rebuildOpenAcc
    :: (Applicative f, SyntacticAcc fa)
    => (forall sh e. ArrayVar aenv (Array sh e) -> f (fa OpenAcc aenv' (Array sh e)))
    -> OpenAcc aenv  t
    -> f (OpenAcc aenv' t)
rebuildOpenAcc av (OpenAcc acc) = OpenAcc <$> rebuildPreOpenAcc rebuildOpenAcc av acc

{-# INLINEABLE rebuildPreOpenAcc #-}
rebuildPreOpenAcc
    :: (Applicative f, SyntacticAcc fa)
    => RebuildAcc acc
    -> RebuildAvar f fa acc aenv aenv'
    -> PreOpenAcc acc aenv  t
    -> f (PreOpenAcc acc aenv' t)
rebuildPreOpenAcc k av acc =
  case acc of
    Use repr a              -> pure (Use repr a)
    Alet lhs a b            -> rebuildAlet k av lhs a b
    Avar ix                 -> accOut       <$> av ix
    Apair as bs             -> Apair        <$> k av as <*> k av bs
    Anil                    -> pure Anil
    Apply f a               -> Apply        <$> rebuildAfun k av f <*> k av a
    Acond p t e             -> Acond        <$> rebuildPreOpenExp k (pure . IE) av p <*> k av t <*> k av e
    Awhile p f a            -> Awhile       <$> rebuildAfun k av p <*> rebuildAfun k av f <*> k av a
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
    Stencil f b a           -> Stencil      <$> rebuildFun k (pure . IE) av f <*> rebuildBoundary k av b  <*> k av a
    Stencil2 f b1 a1 b2 a2  -> Stencil2     <$> rebuildFun k (pure . IE) av f <*> rebuildBoundary k av b1 <*> k av a1 <*> rebuildBoundary k av b2 <*> k av a2
    -- Collect seq             -> Collect      <$> rebuildSeq k av seq
    Aforeign ff afun as     -> Aforeign ff afun <$> k av as

{-# INLINEABLE rebuildAfun #-}
rebuildAfun
    :: (Applicative f, SyntacticAcc fa)
    => RebuildAcc acc
    -> RebuildAvar f fa acc aenv aenv'
    -> PreOpenAfun acc aenv  t
    -> f (PreOpenAfun acc aenv' t)
rebuildAfun k av afun =
  case afun of
    Abody b -> Abody <$> k av b
    Alam lhs1 f -> case rebuildLHS lhs1 of
      Exists lhs2 -> Alam lhs2 <$> rebuildAfun k (shiftA' lhs1 lhs2 k av) f

rebuildAlet
    :: forall f fa acc aenv1 aenv1' aenv2 bndArrs arrs. (Applicative f, SyntacticAcc fa)
    => RebuildAcc acc
    -> RebuildAvar f fa acc aenv1 aenv2
    -> LeftHandSide bndArrs aenv1 aenv1'
    -> acc aenv1  bndArrs
    -> acc aenv1' arrs
    -> f (PreOpenAcc acc aenv2 arrs)
rebuildAlet k av lhs1 bind1 body1 = case rebuildLHS lhs1 of
  Exists lhs2 -> Alet lhs2 <$> k av bind1 <*> k (shiftA' lhs1 lhs2 k av) body1

{-# INLINEABLE rebuildLHS #-}
rebuildLHS :: LeftHandSide arr aenv1 aenv1' -> Exists (LeftHandSide arr aenv2)
rebuildLHS (LeftHandSideWildcard r) = Exists $ LeftHandSideWildcard r
rebuildLHS LeftHandSideArray = Exists $ LeftHandSideArray
rebuildLHS (LeftHandSidePair as bs) = case rebuildLHS as of
  Exists as' -> case rebuildLHS bs of
    Exists bs' -> Exists $ LeftHandSidePair as' bs'

{-# INLINEABLE rebuildBoundary #-}
rebuildBoundary
    :: (Applicative f, SyntacticAcc fa)
    => RebuildAcc acc
    -> RebuildAvar f fa acc aenv aenv'
    -> PreBoundary acc aenv t
    -> f (PreBoundary acc aenv' t)
rebuildBoundary k av bndy =
  case bndy of
    Clamp       -> pure Clamp
    Mirror      -> pure Mirror
    Wrap        -> pure Wrap
    Constant v  -> pure (Constant v)
    Function f  -> Function <$> rebuildFun k (pure . IE) av f

{--
{-# INLINEABLE rebuildSeq #-}
rebuildSeq
    :: (SyntacticAcc fa, Applicative f)
    => RebuildAcc acc
    -> RebuildAvar f fa acc aenv aenv'
    -> PreOpenSeq acc aenv senv t
    -> f (PreOpenSeq acc aenv' senv t)
rebuildSeq k v seq =
  case seq of
    Producer p s -> Producer <$> (rebuildP k v p) <*> (rebuildSeq k v s)
    Consumer c   -> Consumer <$> (rebuildC k v c)
    Reify ix     -> pure $ Reify ix

{-# INLINEABLE rebuildP #-}
rebuildP :: (SyntacticAcc fa, Applicative f)
         => RebuildAcc acc
         -> RebuildAvar f fa acc aenv aenv'
         -> Producer acc aenv senv a
         -> f (Producer acc aenv' senv a)
rebuildP k v p =
  case p of
    StreamIn arrs        -> pure (StreamIn arrs)
    ToSeq sl slix acc    -> ToSeq sl slix <$> k v acc
    MapSeq f x           -> MapSeq <$> rebuildAfun k v f <*> pure x
    ChunkedMapSeq f x    -> ChunkedMapSeq <$> rebuildAfun k v f <*> pure x
    ZipWithSeq f x y     -> ZipWithSeq <$> rebuildAfun k v f <*> pure x <*> pure y
    ScanSeq f e x        -> ScanSeq <$> rebuildFun k (pure . IE) v f <*> rebuildPreOpenExp k (pure . IE) v e <*> pure x

{-# INLINEABLE rebuildC #-}
rebuildC :: forall acc fa f aenv aenv' senv a. (SyntacticAcc fa, Applicative f)
         => RebuildAcc acc
         -> RebuildAvar f fa acc aenv aenv'
         -> Consumer acc aenv senv a
         -> f (Consumer acc aenv' senv a)
rebuildC k v c =
  case c of
    FoldSeq f e x          -> FoldSeq <$> rebuildFun k (pure . IE) v f <*> rebuildPreOpenExp k (pure . IE) v e <*> pure x
    FoldSeqFlatten f acc x -> FoldSeqFlatten <$> rebuildAfun k v f <*> k v acc <*> pure x
    Stuple t               -> Stuple <$> rebuildT t
  where
    rebuildT :: Atuple (Consumer acc aenv senv) t -> f (Atuple (Consumer acc aenv' senv) t)
    rebuildT NilAtup        = pure NilAtup
    rebuildT (SnocAtup t s) = SnocAtup <$> (rebuildT t) <*> (rebuildC k v s)
--}

