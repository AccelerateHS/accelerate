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
{-# LANGUAGE ViewPatterns        #-}
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
  inline, inlineVars, compose,
  subTop, subAtop,

  -- ** Weakening
  (:>), Sink(..), SinkExp(..),

  -- ** Strengthening
  (:?>), strengthen, strengthenE,

  -- ** Rebuilding terms
  RebuildAcc, Rebuildable(..), RebuildableAcc,
  RebuildableExp(..), rebuildWeakenVar, rebuildLHS,

  -- ** Checks
  isIdentity, isIdentityIndexing, extractExpVars,
  bindingIsTrivial,

) where

import Data.Kind
import Control.Applicative                              hiding ( Const )
import Control.Monad
import Prelude                                          hiding ( exp, seq )

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Analysis.Match
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
-- infixr `substitute`

lhsFullVars :: forall s a env1 env2. LeftHandSide s a env1 env2 -> Maybe (Vars s env2 a)
lhsFullVars = fmap snd . go weakenId
  where
    go :: forall env env' b. (env' :> env2) -> LeftHandSide s b env env' -> Maybe (env :> env2, Vars s env2 b)
    go k (LeftHandSideWildcard TupRunit) = Just (k, VarsNil)
    go k (LeftHandSideSingle s) = Just $ (weakenSucc $ k, VarsSingle $ Var s $ k >:> ZeroIdx)
    go k (LeftHandSidePair l1 l2)
      | Just (k',  v2) <- go k  l2
      , Just (k'', v1) <- go k' l1 = Just (k'', VarsPair v1 v2)
    go _ _ = Nothing

bindingIsTrivial :: LeftHandSide s a env1 env2 -> Vars s env2 b -> Maybe (a :~: b)
bindingIsTrivial lhs vars
  | Just lhsVars <- lhsFullVars lhs
  , Just Refl <- matchVars vars lhsVars = Just Refl
bindingIsTrivial _ _ = Nothing

isIdentity :: PreOpenFun acc env aenv (a -> b) -> Maybe (a :~: b)
isIdentity (Lam lhs (Body (extractExpVars -> Just vars))) = bindingIsTrivial lhs vars
isIdentity _ = Nothing

-- Detects whether the function is of the form \ix -> a ! ix
isIdentityIndexing :: PreOpenFun acc env aenv (a -> b) -> Maybe (acc aenv (Array a b))
isIdentityIndexing (Lam lhs (Body body))
  | Index a ix <- body
  , Just vars  <- extractExpVars ix
  , Just Refl  <- bindingIsTrivial lhs vars = Just a
isIdentityIndexing _ = Nothing

-- | Replace the first variable with the given expression. The environment
-- shrinks.
--
inline :: RebuildableAcc acc
       => PreOpenExp acc (env, s) aenv t
       -> PreOpenExp acc env      aenv s
       -> PreOpenExp acc env      aenv t
inline f g = Stats.substitution "inline" $ rebuildE (subTop g) f

inlineVars :: forall acc env env' aenv t1 t2.
              RebuildableAcc acc
           => ELeftHandSide t1 env env'
           -> PreOpenExp acc env' aenv t2
           -> PreOpenExp acc env  aenv t1
           -> Maybe (PreOpenExp acc env  aenv t2)
inlineVars lhsBound expr bound
  | Just vars <- lhsFullVars lhsBound = substitute (strengthenWithLHS lhsBound) weakenId vars expr
  where
    substitute :: forall env1 env2 t.
               env1 :?> env2
            -> env :> env2
            -> ExpVars env1 t1
            -> PreOpenExp acc env1 aenv t
            -> Maybe (PreOpenExp acc env2 aenv t)
    substitute _ k2 vars (extractExpVars -> Just vars')
      | Just Refl <- matchVars vars vars' = Just $ weakenE k2 bound
    substitute k1 k2 vars e = case e of
      Let lhs e1 e2
        | Exists lhs' <- rebuildLHS lhs
                          -> Let lhs' <$> travE e1 <*> substitute (strengthenAfter lhs lhs' k1) (weakenWithLHS lhs' .> k2) (weakenWithLHS lhs `weaken` vars) e2
      Evar (Var t ix)     -> Evar . Var t <$> k1 ix
      Foreign asm f e1    -> Foreign asm f <$> travE e1
      Pair e1 e2          -> Pair <$> travE e1 <*> travE e2
      Nil                 -> Just Nil
      VecPack   vec e1    -> VecPack   vec <$> travE e1
      VecUnpack vec e1    -> VecUnpack vec <$> travE e1
      IndexSlice si e1 e2 -> IndexSlice si <$> travE e1 <*> travE e2
      IndexFull  si e1 e2 -> IndexFull  si <$> travE e1 <*> travE e2
      ToIndex   shr e1 e2 -> ToIndex   shr <$> travE e1 <*> travE e2
      FromIndex shr e1 e2 -> FromIndex shr <$> travE e1 <*> travE e2
      Cond e1 e2 e3       -> Cond <$> travE e1 <*> travE e2 <*> travE e3
      While f1 f2 e1      -> While <$> travF f1 <*> travF f2 <*> travE e1
      Const t c           -> Just $ Const t c
      PrimConst c         -> Just $ PrimConst c
      PrimApp p e1        -> PrimApp p <$> travE e1
      Index a e1          -> Index a <$> travE e1
      LinearIndex a e1    -> LinearIndex a <$> travE e1
      Shape a             -> Just $ Shape a
      ShapeSize shr e1    -> ShapeSize shr <$> travE e1
      Undef t             -> Just $ Undef t
      Coerce t1 t2 e1     -> Coerce t1 t2 <$> travE e1

      where
        travE :: PreOpenExp acc env1 aenv s -> Maybe (PreOpenExp acc env2 aenv s)
        travE = substitute k1 k2 vars

        travF :: PreOpenFun acc env1 aenv s -> Maybe (PreOpenFun acc env2 aenv s)
        travF = substituteF k1 k2 vars

    substituteF :: forall env1 env2 t.
               env1 :?> env2
            -> env :> env2
            -> ExpVars env1 t1
            -> PreOpenFun acc env1 aenv t
            -> Maybe (PreOpenFun acc env2 aenv t)
    substituteF k1 k2 vars (Body e) = Body <$> substitute k1 k2 vars e
    substituteF k1 k2 vars (Lam lhs f) 
      | Exists lhs' <- rebuildLHS lhs = Lam lhs' <$> substituteF (strengthenAfter lhs lhs' k1) (weakenWithLHS lhs' .> k2) (weakenWithLHS lhs `weaken` vars) f

inlineVars _ _ _ = Nothing


-- | Replace an expression that uses the top environment variable with another.
-- The result of the first is let bound into the second.
--
{- substitute' :: RebuildableAcc acc
            => PreOpenExp acc (env, b) aenv c
            -> PreOpenExp acc (env, a) aenv b
            -> PreOpenExp acc (env, a) aenv c
substitute' f g
  | Stats.substitution "substitute" False = undefined
  | isIdentity f = g -- don't rebind an identity function
  | isIdentity g = f
  | otherwise = Let g $ rebuildE split f
  where
    split :: Idx (env,b) c -> PreOpenExp acc ((env,a),b) aenv c
    split ZeroIdx       = Var ZeroIdx
    split (SuccIdx ix)  = Var (SuccIdx (SuccIdx ix))

substitute :: RebuildableAcc acc
           => LeftHandSide b env envb
           -> PreOpenExp acc envb c
           -> LeftHandSide a env enva
           -> PreOpenExp acc enva b
-}

-- | Composition of unary functions.
--
compose :: RebuildableAcc acc
        => PreOpenFun acc env aenv (b -> c)
        -> PreOpenFun acc env aenv (a -> b)
        -> PreOpenFun acc env aenv (a -> c)
compose f@(Lam lhsB (Body c)) g@(Lam lhsA (Body b))
  | Stats.substitution "compose" False = undefined
  | Just Refl <- isIdentity f = g -- don't rebind an identity function
  | Just Refl <- isIdentity g = f

  | Exists lhsB' <- rebuildLHS lhsB
   = Lam lhsA $ Body $ Let lhsB' b (weakenE (sinkWithLHS lhsB lhsB' $ weakenWithLHS lhsA) c)
  -- = Stats.substitution "compose" . Lam lhs2 . Body $ substitute' f g
compose _                   _                   = error "compose: impossible evaluation"

subTop :: PreOpenExp acc env aenv s -> ExpVar (env, s) t -> PreOpenExp acc env aenv t
subTop s (Var _  ZeroIdx     ) = s
subTop _ (Var tp (SuccIdx ix)) = Evar $ Var tp ix

subAtop :: PreOpenAcc acc aenv t -> ArrayVar (aenv, t) (Array sh2 e2) -> PreOpenAcc acc aenv (Array sh2 e2)
subAtop t (Var _    ZeroIdx      ) = t
subAtop _ (Var repr (SuccIdx idx)) = Avar $ Var repr idx

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
                  => (forall e'. ExpVar env e' -> f' (fe (AccClo (f env)) env' aenv e'))
                  -> f env aenv  e
                  -> f' (f env' aenv e)

  {-# INLINEABLE rebuildE #-}
  rebuildE :: SyntacticExp fe
           => (forall e'. ExpVar env e' -> fe (AccClo (f env)) env' aenv e')
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
  weaken = (>:>)

instance Sink (Var s) where
  {-# INLINEABLE weaken #-}
  weaken k (Var s ix) = Var s (k >:> ix)

instance Sink (Vars s) where
  {-# INLINEABLE weaken #-}
  weaken _  VarsNil       = VarsNil
  weaken k (VarsSingle v) = VarsSingle $ weaken k v
  weaken k (VarsPair v w) = VarsPair (weaken k v) (weaken k w)

rebuildWeakenVar :: env :> env' -> ArrayVar env (Array sh e) -> PreOpenAcc acc env' (Array sh e)
rebuildWeakenVar k (Var s idx) = Avar $ Var s $ k >:> idx

rebuildWeakenEvar :: env :> env' -> ExpVar env t -> PreOpenExp acc env' aenv t
rebuildWeakenEvar k (Var s idx) = Evar $ Var s $ k >:> idx

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
  weakenE v = Stats.substitution "weakenE" . rebuildE (rebuildWeakenEvar v)

instance RebuildableAcc acc => SinkExp (PreOpenFun acc) where
  {-# INLINEABLE weakenE #-}
  weakenE v = Stats.substitution "weakenE" . rebuildE (rebuildWeakenEvar v)

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
strengthen k x = Stats.substitution "strengthen" $ rebuildPartial @f @Maybe @IdxA (\(Var s ix) -> fmap (IA . Var s) $ k ix) x

{-# INLINEABLE strengthenE #-}
strengthenE :: forall f env env' aenv t. RebuildableExp f => env :?> env' -> f env aenv t -> Maybe (f env' aenv t)
strengthenE k x = Stats.substitution "strengthenE" $ rebuildPartialE @f @Maybe @IdxE (\(Var tp ix) -> fmap (IE . Var tp) $ k ix) x

strengthenWithLHS :: LeftHandSide s t env1 env2 -> env2 :?> env1
strengthenWithLHS (LeftHandSideWildcard _) = Just
strengthenWithLHS (LeftHandSideSingle _) = \ix -> case ix of
  ZeroIdx   -> Nothing
  SuccIdx i -> Just i
strengthenWithLHS (LeftHandSidePair l1 l2) = strengthenWithLHS l2 >=> strengthenWithLHS l1

strengthenAfter :: LeftHandSide s t env1 env2 -> LeftHandSide s t env1' env2' -> env1 :?> env1' -> env2 :?> env2'
strengthenAfter (LeftHandSideWildcard _) (LeftHandSideWildcard _) k = k
strengthenAfter (LeftHandSideSingle _) (LeftHandSideSingle _) k = \ix -> case ix of
  ZeroIdx   -> Just ZeroIdx
  SuccIdx i -> SuccIdx <$> k i
strengthenAfter (LeftHandSidePair l1 l2) (LeftHandSidePair l1' l2') k
  = strengthenAfter l2 l2' $ strengthenAfter l1 l1' k
strengthenAfter _ _ _ = error "Substitution.strengthenAfter: left hand sides do not match"

-- Simultaneous Substitution ===================================================
--

-- The scalar environment
-- ------------------

-- SEE: [Renaming and Substitution]
-- SEE: [Weakening]
--
class SyntacticExp f where
  varIn         :: ExpVar env t       -> f acc env aenv t
  expOut        :: f acc env aenv t -> PreOpenExp acc env aenv t
  weakenExp     :: RebuildAcc acc -> f acc env aenv t -> f acc (env, s) aenv t
  -- weakenExpAcc  :: RebuildAcc acc -> f acc env aenv t -> f acc env (aenv, s) t

newtype IdxE (acc :: Type -> Type -> Type) env aenv t = IE { unIE :: ExpVar env t }

instance SyntacticExp IdxE where
  varIn          = IE
  expOut         = Evar . unIE
  weakenExp _ (IE (Var tp ix)) = IE $ Var tp $ SuccIdx ix
  -- weakenExpAcc _ = IE . unIE

instance SyntacticExp PreOpenExp where
  varIn          = Evar
  expOut         = id
  weakenExp k    = runIdentity . rebuildPreOpenExp k (Identity . weakenExp k . IE) (Identity . IA)
  -- weakenExpAcc k = runIdentity . rebuildPreOpenExp k (Identity . IE) (Identity . weakenAcc k . IA)

{-# INLINEABLE shiftE #-}
shiftE
    :: (Applicative f, SyntacticExp fe)
    => RebuildAcc acc
    -> RebuildEvar f fe acc env      env'      aenv
    -> RebuildEvar f fe acc (env, s) (env', s) aenv
shiftE _ _ (Var tp ZeroIdx)      = pure $ varIn (Var tp ZeroIdx)
shiftE k v (Var tp (SuccIdx ix)) = weakenExp k <$> v (Var tp ix)

{-# INLINEABLE shiftE' #-}
shiftE'
    :: (Applicative f, SyntacticExp fa)
    => ELeftHandSide t env1 env1'
    -> ELeftHandSide t env2 env2'
    -> RebuildAcc acc
    -> RebuildEvar f fa acc env1  env2  aenv
    -> RebuildEvar f fa acc env1' env2' aenv
shiftE' (LeftHandSideWildcard _) (LeftHandSideWildcard _) _ v = v
shiftE' (LeftHandSideSingle _)   (LeftHandSideSingle _)   k v = shiftE k v
shiftE' (LeftHandSidePair a1 b1) (LeftHandSidePair a2 b2) k v = shiftE' b1 b2 k $ shiftE' a1 a2 k v
shiftE' _ _ _ _ = error "Substitution: left hand sides do not match"


{-# INLINEABLE rebuildPreOpenExp #-}
rebuildPreOpenExp
    :: (Applicative f, SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> RebuildEvar f fe acc env env' aenv'
    -> RebuildAvar f fa acc aenv aenv'
    -> PreOpenExp acc env  aenv t
    -> f (PreOpenExp acc env' aenv' t)
rebuildPreOpenExp k v av exp =
  case exp of
    Const t c           -> pure $ Const t c
    PrimConst c         -> pure $ PrimConst c
    Undef t             -> pure $ Undef t
    Evar var            -> expOut        <$> v var
    Let lhs a b
      | Exists lhs' <- rebuildLHS lhs
                        -> Let lhs'      <$> rebuildPreOpenExp k v av a  <*> rebuildPreOpenExp k (shiftE' lhs lhs' k v) av b
    Pair e1 e2          -> Pair          <$> rebuildPreOpenExp k v av e1 <*> rebuildPreOpenExp k v av e2
    Nil                 -> pure $ Nil
    VecPack   vec e     -> VecPack   vec <$> rebuildPreOpenExp k v av e
    VecUnpack vec e     -> VecUnpack vec <$> rebuildPreOpenExp k v av e
    IndexSlice x ix sh  -> IndexSlice x  <$> rebuildPreOpenExp k v av ix <*> rebuildPreOpenExp k v av sh
    IndexFull x ix sl   -> IndexFull x   <$> rebuildPreOpenExp k v av ix <*> rebuildPreOpenExp k v av sl
    ToIndex shr sh ix   -> ToIndex shr   <$> rebuildPreOpenExp k v av sh <*> rebuildPreOpenExp k v av ix
    FromIndex shr sh ix -> FromIndex shr <$> rebuildPreOpenExp k v av sh <*> rebuildPreOpenExp k v av ix
    Cond p t e          -> Cond          <$> rebuildPreOpenExp k v av p  <*> rebuildPreOpenExp k v av t  <*> rebuildPreOpenExp k v av e
    While p f x         -> While         <$> rebuildFun k v av p         <*> rebuildFun k v av f         <*> rebuildPreOpenExp k v av x
    PrimApp f x         -> PrimApp f     <$> rebuildPreOpenExp k v av x
    Index a sh          -> Index         <$> k av a                      <*> rebuildPreOpenExp k v av sh
    LinearIndex a i     -> LinearIndex   <$> k av a                      <*> rebuildPreOpenExp k v av i
    Shape a             -> Shape         <$> k av a
    ShapeSize shr sh    -> ShapeSize shr <$> rebuildPreOpenExp k v av sh
    Foreign ff f e      -> Foreign ff f  <$> rebuildPreOpenExp k v av e
    Coerce t1 t2 e      -> Coerce t1 t2  <$> rebuildPreOpenExp k v av e

{-# INLINEABLE rebuildFun #-}
rebuildFun
    :: (Applicative f, SyntacticExp fe, SyntacticAcc fa)
    => RebuildAcc acc
    -> RebuildEvar f fe acc env env' aenv'
    -> RebuildAvar f fa acc aenv aenv'
    -> PreOpenFun acc env  aenv  t
    -> f (PreOpenFun acc env' aenv' t)
rebuildFun k v av fun =
  case fun of
    Body e      -> Body <$> rebuildPreOpenExp k v av e
    Lam lhs f   
      | Exists lhs' <- rebuildLHS lhs
        -> Lam lhs' <$> rebuildFun k (shiftE' lhs lhs' k v) av f

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
  avarIn                       = IA
  accOut                       = Avar . unIA
  weakenAcc _ (IA (Var s idx)) = IA $ Var s $ SuccIdx idx

instance SyntacticAcc PreOpenAcc where
  avarIn        = Avar
  accOut        = id
  weakenAcc k   = runIdentity . rebuildPreOpenAcc k (Identity . weakenAcc k . IA)

type RebuildAvar f (fa :: (Type -> Type -> Type) -> Type -> Type -> Type) acc aenv aenv'
    = forall sh e. ArrayVar aenv (Array sh e) -> f (fa acc aenv' (Array sh e))

type RebuildEvar f fe (acc :: * -> * -> *) env env' aenv' =
  forall t'. ExpVar env t' -> f (fe acc env' aenv' t')

{-# INLINEABLE shiftA #-}
shiftA
    :: (Applicative f, SyntacticAcc fa)
    => RebuildAcc acc
    -> RebuildAvar f fa acc aenv aenv'
    -> ArrayVar    (aenv,  s) (Array sh e)
    -> f (fa   acc (aenv', s) (Array sh e))
shiftA _ _ (Var s ZeroIdx)      = pure $ avarIn $ Var s ZeroIdx
shiftA k v (Var s (SuccIdx ix)) = weakenAcc k <$> v (Var s ix)

shiftA'
    :: (Applicative f, SyntacticAcc fa)
    => ALeftHandSide t aenv1 aenv1'
    -> ALeftHandSide t aenv2 aenv2'
    -> RebuildAcc acc
    -> RebuildAvar f fa acc aenv1  aenv2
    -> RebuildAvar f fa acc aenv1' aenv2'
shiftA' (LeftHandSideWildcard _) (LeftHandSideWildcard _) _ v = v
shiftA' (LeftHandSideSingle _)   (LeftHandSideSingle _)   k v = shiftA k v
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
    Use repr a              -> pure $ Use repr a
    Alet lhs a b            -> rebuildAlet k av lhs a b
    Avar ix                 -> accOut       <$> av ix
    Apair as bs             -> Apair        <$> k av as <*> k av bs
    Anil                    -> pure Anil
    Apply repr f a          -> Apply repr   <$> rebuildAfun k av f <*> k av a
    Acond p t e             -> Acond        <$> rebuildPreOpenExp k (pure . IE) av p <*> k av t <*> k av e
    Awhile p f a            -> Awhile       <$> rebuildAfun k av p <*> rebuildAfun k av f <*> k av a
    Unit tp e               -> Unit tp      <$> rebuildPreOpenExp k (pure . IE) av e
    Reshape shr e a         -> Reshape shr  <$> rebuildPreOpenExp k (pure . IE) av e <*> k av a
    Generate repr e f       -> Generate repr <$> rebuildPreOpenExp k (pure . IE) av e <*> rebuildFun k (pure . IE) av f
    Transform repr sh ix f a -> Transform repr <$> rebuildPreOpenExp k (pure . IE) av sh <*> rebuildFun k (pure . IE) av ix <*> rebuildFun k (pure . IE) av f <*> k av a
    Replicate sl slix a     -> Replicate sl <$> rebuildPreOpenExp k (pure . IE) av slix <*> k av a
    Slice sl a slix         -> Slice sl     <$> k av a <*> rebuildPreOpenExp k (pure . IE) av slix
    Map tp f a              -> Map tp       <$> rebuildFun k (pure . IE) av f <*> k av a
    ZipWith tp f a1 a2      -> ZipWith tp   <$> rebuildFun k (pure . IE) av f <*> k av a1 <*> k av a2
    Fold f z a              -> Fold         <$> rebuildFun k (pure . IE) av f <*> rebuildPreOpenExp k (pure . IE) av z <*> k av a
    Fold1 f a               -> Fold1        <$> rebuildFun k (pure . IE) av f <*> k av a
    FoldSeg itp f z a s     -> FoldSeg itp  <$> rebuildFun k (pure . IE) av f <*> rebuildPreOpenExp k (pure . IE) av z <*> k av a <*> k av s
    Fold1Seg itp f a s      -> Fold1Seg itp <$> rebuildFun k (pure . IE) av f <*> k av a <*> k av s
    Scanl f z a             -> Scanl        <$> rebuildFun k (pure . IE) av f <*> rebuildPreOpenExp k (pure . IE) av z <*> k av a
    Scanl' f z a            -> Scanl'       <$> rebuildFun k (pure . IE) av f <*> rebuildPreOpenExp k (pure . IE) av z <*> k av a
    Scanl1 f a              -> Scanl1       <$> rebuildFun k (pure . IE) av f <*> k av a
    Scanr f z a             -> Scanr        <$> rebuildFun k (pure . IE) av f <*> rebuildPreOpenExp k (pure . IE) av z <*> k av a
    Scanr' f z a            -> Scanr'       <$> rebuildFun k (pure . IE) av f <*> rebuildPreOpenExp k (pure . IE) av z <*> k av a
    Scanr1 f a              -> Scanr1       <$> rebuildFun k (pure . IE) av f <*> k av a
    Permute f1 a1 f2 a2     -> Permute      <$> rebuildFun k (pure . IE) av f1 <*> k av a1 <*> rebuildFun k (pure . IE) av f2 <*> k av a2
    Backpermute shr sh f a  -> Backpermute shr <$> rebuildPreOpenExp k (pure . IE) av sh <*> rebuildFun k (pure . IE) av f <*> k av a
    Stencil sr tp f b a     -> Stencil sr tp <$> rebuildFun k (pure . IE) av f <*> rebuildBoundary k av b  <*> k av a
    Stencil2 s1 s2 tp f b1 a1 b2 a2 -> Stencil2 s1 s2 tp <$> rebuildFun k (pure . IE) av f <*> rebuildBoundary k av b1 <*> k av a1 <*> rebuildBoundary k av b2 <*> k av a2
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
    -> ALeftHandSide bndArrs aenv1 aenv1'
    -> acc aenv1  bndArrs
    -> acc aenv1' arrs
    -> f (PreOpenAcc acc aenv2 arrs)
rebuildAlet k av lhs1 bind1 body1 = case rebuildLHS lhs1 of
  Exists lhs2 -> Alet lhs2 <$> k av bind1 <*> k (shiftA' lhs1 lhs2 k av) body1

{-# INLINEABLE rebuildLHS #-}
rebuildLHS :: LeftHandSide s t aenv1 aenv1' -> Exists (LeftHandSide s t aenv2)
rebuildLHS (LeftHandSideWildcard r) = Exists $ LeftHandSideWildcard r
rebuildLHS (LeftHandSideSingle s)   = Exists $ LeftHandSideSingle s
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

extractExpVars :: PreOpenExp acc env aenv a -> Maybe (ExpVars env a)
extractExpVars Nil          = Just VarsNil
extractExpVars (Pair e1 e2) = VarsPair <$> extractExpVars e1 <*> extractExpVars e2
extractExpVars (Evar v)     = Just $ VarsSingle v
extractExpVars _            = Nothing
