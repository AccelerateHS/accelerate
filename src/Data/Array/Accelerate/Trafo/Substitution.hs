{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Substitution
-- Copyright   : [2012..2020] The Accelerate Team
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
  (:>), Sink(..), SinkExp(..), weakenVars,

  -- ** Strengthening
  (:?>), strengthen, strengthenE,

  -- ** Rebuilding terms
  RebuildAcc, Rebuildable(..), RebuildableAcc,
  RebuildableExp(..), rebuildWeakenVar, rebuildLHS,
  OpenAccFun(..), OpenAccExp(..),

  -- ** Checks
  isIdentity, isIdentityIndexing, extractExpVars,
  bindingIsTrivial,

) where

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.Environment
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Representation.Array
import qualified Data.Array.Accelerate.Debug.Internal.Stats         as Stats

import Data.Kind
import Control.Applicative                                          hiding ( Const )
import Control.Monad
import Prelude                                                      hiding ( exp, seq )


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
    go k (LeftHandSideWildcard TupRunit) = Just (k, TupRunit)
    go k (LeftHandSideSingle s) = Just $ (weakenSucc $ k, TupRsingle $ Var s $ k >:> ZeroIdx)
    go k (LeftHandSidePair l1 l2)
      | Just (k',  v2) <- go k  l2
      , Just (k'', v1) <- go k' l1 = Just (k'', TupRpair v1 v2)
    go _ _ = Nothing

bindingIsTrivial :: LeftHandSide s a env1 env2 -> Vars s env2 b -> Maybe (a :~: b)
bindingIsTrivial lhs vars
  | Just lhsVars <- lhsFullVars lhs
  , Just Refl    <- matchVars vars lhsVars
  = Just Refl
bindingIsTrivial _ _ = Nothing

isIdentity :: OpenFun env aenv (a -> b) -> Maybe (a :~: b)
isIdentity (Lam lhs (Body (extractExpVars -> Just vars))) = bindingIsTrivial lhs vars
isIdentity _ = Nothing

-- Detects whether the function is of the form \ix -> a ! ix
isIdentityIndexing :: OpenFun env aenv (a -> b) -> Maybe (ArrayVar aenv (Array a b))
isIdentityIndexing (Lam lhs (Body body))
  | Index avar ix <- body
  , Just vars     <- extractExpVars ix
  , Just Refl     <- bindingIsTrivial lhs vars
  = Just avar
isIdentityIndexing _ = Nothing

-- | Replace the first variable with the given expression. The environment
-- shrinks.
--
inline :: OpenExp (env, s) aenv t
       -> OpenExp env      aenv s
       -> OpenExp env      aenv t
inline f g = Stats.substitution "inline" $ rebuildE (subTop g) f

inlineVars :: forall env env' aenv t1 t2.
              ELeftHandSide t1 env env'
           ->        OpenExp env' aenv t2
           ->        OpenExp env  aenv t1
           -> Maybe (OpenExp env  aenv t2)
inlineVars lhsBound expr bound
  | Just vars <- lhsFullVars lhsBound = substitute (strengthenWithLHS lhsBound) weakenId vars expr
  where
    substitute
        :: forall env1 env2 t.
           env1 :?> env2
        -> env :> env2
        -> ExpVars env1 t1
        -> OpenExp env1 aenv t
        -> Maybe (OpenExp env2 aenv t)
    substitute _ k2 vars (extractExpVars -> Just vars')
      | Just Refl <- matchVars vars vars' = Just $ weakenE k2 bound
    substitute k1 k2 vars topExp = case topExp of
      Let ann lhs e1 e2
        | Exists lhs' <- rebuildLHS lhs
                          -> Let ann lhs' <$> travE e1 <*> substitute (strengthenAfter lhs lhs' k1) (weakenWithLHS lhs' .> k2) (weakenWithLHS lhs `weakenVars` vars) e2
      Evar (Var t ix)     -> Evar . Var t <$> k1 ix
      Foreign tp asm f e1 -> Foreign tp asm f <$> travE e1
      Pair ann e1 e2      -> Pair ann <$> travE e1 <*> travE e2
      Nil ann             -> Just (Nil ann)
      VecPack   vec e1    -> VecPack   vec <$> travE e1
      VecUnpack vec e1    -> VecUnpack vec <$> travE e1
      IndexSlice si e1 e2 -> IndexSlice si <$> travE e1 <*> travE e2
      IndexFull  si e1 e2 -> IndexFull  si <$> travE e1 <*> travE e2
      ToIndex   shr e1 e2 -> ToIndex   shr <$> travE e1 <*> travE e2
      FromIndex shr e1 e2 -> FromIndex shr <$> travE e1 <*> travE e2
      Case e1 rhs def     -> Case <$> travE e1 <*> mapM (\(t,c) -> (t,) <$> travE c) rhs <*> travMaybeE def
      Cond e1 e2 e3       -> Cond <$> travE e1 <*> travE e2 <*> travE e3
      While f1 f2 e1      -> While <$> travF f1 <*> travF f2 <*> travE e1
      Const ann t c       -> Just $ Const ann t c
      PrimConst c         -> Just $ PrimConst c
      PrimApp p e1        -> PrimApp p <$> travE e1
      Index a e1          -> Index a <$> travE e1
      LinearIndex a e1    -> LinearIndex a <$> travE e1
      Shape a             -> Just $ Shape a
      ShapeSize shr e1    -> ShapeSize shr <$> travE e1
      Undef t             -> Just $ Undef t
      Coerce t1 t2 e1     -> Coerce t1 t2 <$> travE e1

      where
        travE :: OpenExp env1 aenv s -> Maybe (OpenExp env2 aenv s)
        travE = substitute k1 k2 vars

        travF :: OpenFun env1 aenv s -> Maybe (OpenFun env2 aenv s)
        travF = substituteF k1 k2 vars

        travMaybeE :: Maybe (OpenExp env1 aenv s) -> Maybe (Maybe (OpenExp env2 aenv s))
        travMaybeE Nothing  = pure Nothing
        travMaybeE (Just x) = Just <$> travE x

    substituteF :: forall env1 env2 t.
               env1 :?> env2
            -> env :> env2
            -> ExpVars env1 t1
            -> OpenFun env1 aenv t
            -> Maybe (OpenFun env2 aenv t)
    substituteF k1 k2 vars (Body e) = Body <$> substitute k1 k2 vars e
    substituteF k1 k2 vars (Lam lhs f)
      | Exists lhs' <- rebuildLHS lhs = Lam lhs' <$> substituteF (strengthenAfter lhs lhs' k1) (weakenWithLHS lhs' .> k2) (weakenWithLHS lhs `weakenVars` vars) f

inlineVars _ _ _ = Nothing


-- | Replace an expression that uses the top environment variable with another.
-- The result of the first is let bound into the second.
--
{- substitute' :: OpenExp (env, b) aenv c
            -> OpenExp (env, a) aenv b
            -> OpenExp (env, a) aenv c
substitute' f g
  | Stats.substitution "substitute" False = undefined
  | isIdentity f = g -- don't rebind an identity function
  | isIdentity g = f
  | otherwise = Let g $ rebuildE split f
  where
    split :: Idx (env,b) c -> OpenExp ((env,a),b) aenv c
    split ZeroIdx       = Var ZeroIdx
    split (SuccIdx ix)  = Var (SuccIdx (SuccIdx ix))

substitute :: LeftHandSide b env envb
           -> OpenExp envb c
           -> LeftHandSide a env enva
           -> OpenExp enva b
-}

-- | Composition of unary functions.
--
compose :: HasCallStack
        => OpenFun env aenv (b -> c)
        -> OpenFun env aenv (a -> b)
        -> OpenFun env aenv (a -> c)
compose f@(Lam lhsB (Body c)) g@(Lam lhsA (Body b))
  | Stats.substitution "compose" False = undefined
  | Just Refl <- isIdentity f = g -- don't rebind an identity function
  | Just Refl <- isIdentity g = f

  | Exists lhsB' <- rebuildLHS lhsB
  = Lam lhsA
  $ Body
  $ Let mkDummyAnn lhsB' b
  $ weakenE (sinkWithLHS lhsB lhsB' $ weakenWithLHS lhsA) c
  -- = Stats.substitution "compose" . Lam lhs2 . Body $ substitute' f g
compose _
  _ = error "compose: impossible evaluation"

subTop :: OpenExp env aenv s -> ExpVar (env, s) t -> OpenExp env aenv t
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
                  => (forall e'. ExpVar env e' -> f' (fe env' aenv e'))
                  -> f env aenv e
                  -> f' (f env' aenv e)

  {-# INLINEABLE rebuildE #-}
  rebuildE :: SyntacticExp fe
           => (forall e'. ExpVar env e' -> fe env' aenv e')
           -> f env  aenv e
           -> f env' aenv e
  rebuildE v = runIdentity . rebuildPartialE (Identity . v)

-- Terms that are rebuildable and also recursive closures
--
type RebuildableAcc acc = (Rebuildable acc, AccClo acc ~ acc)

-- Wrappers which add the 'acc' type argument
--
data OpenAccExp (acc :: Type -> Type -> Type) env aenv a where
  OpenAccExp :: { unOpenAccExp :: OpenExp env aenv a } -> OpenAccExp acc env aenv a

data OpenAccFun (acc :: Type -> Type -> Type) env aenv a where
  OpenAccFun :: { unOpenAccFun :: OpenFun env aenv a } -> OpenAccFun acc env aenv a

-- We can use the same plumbing to rebuildPartial all the things we want to rebuild.
--
instance Rebuildable (OpenAccExp acc env) where
  type AccClo (OpenAccExp acc env) = acc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial v (OpenAccExp e) = OpenAccExp <$> Stats.substitution "rebuild" (rebuildOpenExp (pure . IE) (reindexAvar v) e)

instance Rebuildable (OpenAccFun acc env) where
  type AccClo (OpenAccFun acc env) = acc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial v (OpenAccFun f) = OpenAccFun <$> Stats.substitution "rebuild" (rebuildFun (pure . IE) (reindexAvar v) f)

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

instance RebuildableExp OpenExp where
  {-# INLINEABLE rebuildPartialE #-}
  rebuildPartialE v x = Stats.substitution "rebuild" $ rebuildOpenExp v (ReindexAvar pure) x

instance RebuildableExp OpenFun where
  {-# INLINEABLE rebuildPartialE #-}
  rebuildPartialE v x = Stats.substitution "rebuild" $ rebuildFun v (ReindexAvar pure) x

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
  weaken :: env :> env' -> forall t. f env t -> f env' t

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

weakenVars :: env :> env' -> Vars s env t -> Vars s env' t
weakenVars _  TupRunit      = TupRunit
weakenVars k (TupRsingle v) = TupRsingle $ weaken k v
weakenVars k (TupRpair v w) = TupRpair (weakenVars k v) (weakenVars k w)

rebuildWeakenVar :: env :> env' -> ArrayVar env (Array sh e) -> PreOpenAcc acc env' (Array sh e)
rebuildWeakenVar k (Var s idx) = Avar $ Var s $ k >:> idx

rebuildWeakenEvar :: env :> env' -> ExpVar env t -> OpenExp env' aenv t
rebuildWeakenEvar k (Var s idx) = Evar $ Var s $ k >:> idx

instance RebuildableAcc acc => Sink (PreOpenAcc acc) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (rebuildWeakenVar k)

instance RebuildableAcc acc => Sink (PreOpenAfun acc) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . rebuildA (rebuildWeakenVar k)

instance Sink (OpenExp env) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . runIdentity . rebuildOpenExp (Identity . Evar) (ReindexAvar (Identity . weaken k))

instance Sink (OpenFun env) where
  {-# INLINEABLE weaken #-}
  weaken k = Stats.substitution "weaken" . runIdentity . rebuildFun (Identity . Evar) (ReindexAvar (Identity . weaken k))

instance Sink Boundary where
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

instance SinkExp OpenExp where
  {-# INLINEABLE weakenE #-}
  weakenE v = Stats.substitution "weakenE" . rebuildE (rebuildWeakenEvar v)

instance SinkExp OpenFun where
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
strengthenWithLHS (LeftHandSideSingle _)   = \ix -> case ix of
  ZeroIdx   -> Nothing
  SuccIdx i -> Just i
strengthenWithLHS (LeftHandSidePair l1 l2) = strengthenWithLHS l2 >=> strengthenWithLHS l1

strengthenAfter :: LeftHandSide s t env1 env2 -> LeftHandSide s t env1' env2' -> env1 :?> env1' -> env2 :?> env2'
strengthenAfter (LeftHandSideWildcard _) (LeftHandSideWildcard _) k = k
strengthenAfter (LeftHandSideSingle _)   (LeftHandSideSingle _)   k = \ix -> case ix of
  ZeroIdx   -> Just ZeroIdx
  SuccIdx i -> SuccIdx <$> k i
strengthenAfter (LeftHandSidePair l1 l2) (LeftHandSidePair l1' l2') k =
  strengthenAfter l2 l2' $ strengthenAfter l1 l1' k
strengthenAfter _ _ _ = error "Substitution.strengthenAfter: left hand sides do not match"

-- Simultaneous Substitution ===================================================
--

-- The scalar environment
-- ------------------

-- SEE: [Renaming and Substitution]
-- SEE: [Weakening]
--
class SyntacticExp f where
  varIn         :: ExpVar env t -> f env aenv t
  expOut        :: f env aenv t -> OpenExp env aenv t
  weakenExp     :: f env aenv t -> f (env, s) aenv t

newtype IdxE env aenv t = IE { unIE :: ExpVar env t }

instance SyntacticExp IdxE where
  varIn          = IE
  expOut         = Evar . unIE
  weakenExp (IE (Var tp ix)) = IE $ Var tp $ SuccIdx ix

instance SyntacticExp OpenExp where
  varIn          = Evar
  expOut         = id
  weakenExp      = runIdentity . rebuildOpenExp (Identity . weakenExp . IE) (ReindexAvar Identity)

{-# INLINEABLE shiftE #-}
shiftE
    :: (Applicative f, SyntacticExp fe)
    => RebuildEvar f fe env      env'      aenv
    -> RebuildEvar f fe (env, s) (env', s) aenv
shiftE _ (Var tp ZeroIdx)      = pure $ varIn (Var tp ZeroIdx)
shiftE v (Var tp (SuccIdx ix)) = weakenExp <$> v (Var tp ix)

{-# INLINEABLE shiftE' #-}
shiftE'
    :: (Applicative f, SyntacticExp fa)
    => ELeftHandSide t env1 env1'
    -> ELeftHandSide t env2 env2'
    -> RebuildEvar f fa env1  env2  aenv
    -> RebuildEvar f fa env1' env2' aenv
shiftE' (LeftHandSideWildcard _) (LeftHandSideWildcard _) v = v
shiftE' (LeftHandSideSingle _)   (LeftHandSideSingle _)   v = shiftE v
shiftE' (LeftHandSidePair a1 b1) (LeftHandSidePair a2 b2) v = shiftE' b1 b2 $ shiftE' a1 a2 v
shiftE' _ _ _ = error "Substitution: left hand sides do not match"

{-# INLINEABLE rebuildMaybeExp #-}
rebuildMaybeExp
    :: (HasCallStack, Applicative f, SyntacticExp fe)
    => RebuildEvar f fe env env' aenv'
    -> ReindexAvar f aenv aenv'
    -> Maybe (OpenExp env  aenv t)
    -> f (Maybe (OpenExp env' aenv' t))
rebuildMaybeExp _ _  Nothing  = pure Nothing
rebuildMaybeExp v av (Just x) = Just <$> rebuildOpenExp v av x

{-# INLINEABLE rebuildOpenExp #-}
rebuildOpenExp
    :: (HasCallStack, Applicative f, SyntacticExp fe)
    => RebuildEvar f fe env env' aenv'
    -> ReindexAvar f aenv aenv'
    -> OpenExp env  aenv t
    -> f (OpenExp env' aenv' t)
rebuildOpenExp v av@(ReindexAvar reindex) exp =
  case exp of
    Const ann t c       -> pure $ Const ann t c
    PrimConst c         -> pure $ PrimConst c
    Undef t             -> pure $ Undef t
    Evar var            -> expOut          <$> v var
    Let ann lhs a b
      | Exists lhs' <- rebuildLHS lhs
                        -> Let ann lhs'    <$> rebuildOpenExp v av a  <*> rebuildOpenExp (shiftE' lhs lhs' v) av b
    Pair ann e1 e2      -> Pair ann        <$> rebuildOpenExp v av e1 <*> rebuildOpenExp v av e2
    Nil ann             -> pure (Nil ann)
    VecPack   vec e     -> VecPack   vec   <$> rebuildOpenExp v av e
    VecUnpack vec e     -> VecUnpack vec   <$> rebuildOpenExp v av e
    IndexSlice x ix sh  -> IndexSlice x    <$> rebuildOpenExp v av ix <*> rebuildOpenExp v av sh
    IndexFull x ix sl   -> IndexFull x     <$> rebuildOpenExp v av ix <*> rebuildOpenExp v av sl
    ToIndex shr sh ix   -> ToIndex shr     <$> rebuildOpenExp v av sh <*> rebuildOpenExp v av ix
    FromIndex shr sh ix -> FromIndex shr   <$> rebuildOpenExp v av sh <*> rebuildOpenExp v av ix
    Case e rhs def      -> Case            <$> rebuildOpenExp v av e  <*> sequenceA [ (t,) <$> rebuildOpenExp v av c | (t,c) <- rhs ] <*> rebuildMaybeExp v av def
    Cond p t e          -> Cond            <$> rebuildOpenExp v av p  <*> rebuildOpenExp v av t  <*> rebuildOpenExp v av e
    While p f x         -> While           <$> rebuildFun v av p      <*> rebuildFun v av f      <*> rebuildOpenExp v av x
    PrimApp f x         -> PrimApp f       <$> rebuildOpenExp v av x
    Index a sh          -> Index           <$> reindex a              <*> rebuildOpenExp v av sh
    LinearIndex a i     -> LinearIndex     <$> reindex a              <*> rebuildOpenExp v av i
    Shape a             -> Shape           <$> reindex a
    ShapeSize shr sh    -> ShapeSize shr   <$> rebuildOpenExp v av sh
    Foreign tp ff f e   -> Foreign tp ff f <$> rebuildOpenExp v av e
    Coerce t1 t2 e      -> Coerce t1 t2    <$> rebuildOpenExp v av e

{-# INLINEABLE rebuildFun #-}
rebuildFun
    :: (HasCallStack, Applicative f, SyntacticExp fe)
    => RebuildEvar f fe env env' aenv'
    -> ReindexAvar f aenv aenv'
    -> OpenFun env  aenv  t
    -> f (OpenFun env' aenv' t)
rebuildFun v av fun =
  case fun of
    Body e -> Body <$> rebuildOpenExp v av e
    Lam lhs f
      | Exists lhs' <- rebuildLHS lhs
        -> Lam lhs' <$> rebuildFun (shiftE' lhs lhs' v) av f

-- The array environment
-- -----------------

type RebuildAcc acc =
  forall aenv aenv' f fa a. (HasCallStack, Applicative f, SyntacticAcc fa)
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

type RebuildEvar f fe env env' aenv' =
  forall t'. ExpVar env t' -> f (fe env' aenv' t')

newtype ReindexAvar f aenv aenv' =
  ReindexAvar (forall sh e. ArrayVar aenv (Array sh e) -> f (ArrayVar aenv' (Array sh e)))

reindexAvar
    :: forall f fa acc aenv aenv'.
       (HasCallStack, Applicative f, SyntacticAcc fa)
    => RebuildAvar f fa acc aenv aenv'
    -> ReindexAvar f        aenv aenv'
reindexAvar v = ReindexAvar f where
  f :: forall sh e. ArrayVar aenv (Array sh e) -> f (ArrayVar aenv' (Array sh e))
  f var = g <$> v var

  g :: fa acc aenv' (Array sh e) -> ArrayVar aenv' (Array sh e)
  g fa = case accOut fa of
    Avar var' -> var'
    _ -> internalError "An Avar which was used in an Exp was mapped to an array term other than Avar. This mapping is invalid as an Exp can only contain array variables."


{-# INLINEABLE shiftA #-}
shiftA
    :: (HasCallStack, Applicative f, SyntacticAcc fa)
    => RebuildAcc acc
    -> RebuildAvar f fa acc aenv aenv'
    -> ArrayVar  (aenv,  s) (Array sh e)
    -> f (fa acc (aenv', s) (Array sh e))
shiftA _ _ (Var s ZeroIdx)      = pure $ avarIn $ Var s ZeroIdx
shiftA k v (Var s (SuccIdx ix)) = weakenAcc k <$> v (Var s ix)

shiftA'
    :: (HasCallStack, Applicative f, SyntacticAcc fa)
    => ALeftHandSide t aenv1 aenv1'
    -> ALeftHandSide t aenv2 aenv2'
    -> RebuildAcc acc
    -> RebuildAvar f fa acc aenv1  aenv2
    -> RebuildAvar f fa acc aenv1' aenv2'
shiftA' (LeftHandSideWildcard _) (LeftHandSideWildcard _) _ v = v
shiftA' (LeftHandSideSingle _)   (LeftHandSideSingle _)   k v = shiftA k v
shiftA' (LeftHandSidePair a1 b1) (LeftHandSidePair a2 b2) k v = shiftA' b1 b2 k $ shiftA' a1 a2 k v
shiftA' _ _ _ _ = internalError "left hand sides do not match"

{-# INLINEABLE rebuildOpenAcc #-}
rebuildOpenAcc
    :: (HasCallStack, Applicative f, SyntacticAcc fa)
    => (forall sh e. ArrayVar aenv (Array sh e) -> f (fa OpenAcc aenv' (Array sh e)))
    -> OpenAcc aenv  t
    -> f (OpenAcc aenv' t)
rebuildOpenAcc av (OpenAcc acc) = OpenAcc <$> rebuildPreOpenAcc rebuildOpenAcc av acc

{-# INLINEABLE rebuildPreOpenAcc #-}
rebuildPreOpenAcc
    :: (HasCallStack, Applicative f, SyntacticAcc fa)
    => RebuildAcc acc
    -> RebuildAvar f fa acc aenv aenv'
    -> PreOpenAcc acc aenv  t
    -> f (PreOpenAcc acc aenv' t)
rebuildPreOpenAcc k av acc =
  case acc of
    Use repr a                -> pure $ Use repr a
    Alet lhs a b              -> rebuildAlet k av lhs a b
    Avar ix                   -> accOut          <$> av ix
    Apair as bs               -> Apair           <$> k av as <*> k av bs
    Anil                      -> pure Anil
    Atrace msg as bs          -> Atrace msg      <$> k av as <*> k av bs
    Apply repr f a            -> Apply repr      <$> rebuildAfun k av f <*> k av a
    Acond p t e               -> Acond           <$> rebuildOpenExp (pure . IE) av' p <*> k av t <*> k av e
    Awhile p f a              -> Awhile          <$> rebuildAfun k av p <*> rebuildAfun k av f <*> k av a
    Unit tp e                 -> Unit tp         <$> rebuildOpenExp (pure . IE) av' e
    Reshape shr e a           -> Reshape shr     <$> rebuildOpenExp (pure . IE) av' e <*> k av a
    Generate repr e f         -> Generate repr   <$> rebuildOpenExp (pure . IE) av' e <*> rebuildFun (pure . IE) av' f
    Transform repr sh ix f a  -> Transform repr  <$> rebuildOpenExp (pure . IE) av' sh <*> rebuildFun (pure . IE) av' ix <*> rebuildFun (pure . IE) av' f <*> k av a
    Replicate sl slix a       -> Replicate sl    <$> rebuildOpenExp (pure . IE) av' slix <*> k av a
    Slice sl a slix           -> Slice sl        <$> k av a <*> rebuildOpenExp (pure . IE) av' slix
    Map ann tp f a            -> Map ann tp      <$> rebuildFun (pure . IE) av' f <*> k av a
    ZipWith tp f a1 a2        -> ZipWith tp      <$> rebuildFun (pure . IE) av' f <*> k av a1 <*> k av a2
    Fold ann f z a            -> Fold ann        <$> rebuildFun (pure . IE) av' f <*> rebuildMaybeExp (pure . IE) av' z <*> k av a
    FoldSeg itp f z a s       -> FoldSeg itp     <$> rebuildFun (pure . IE) av' f <*> rebuildMaybeExp (pure . IE) av' z <*> k av a <*> k av s
    Scan  d f z a             -> Scan  d         <$> rebuildFun (pure . IE) av' f <*> rebuildMaybeExp (pure . IE) av' z <*> k av a
    Scan' d f z a             -> Scan' d         <$> rebuildFun (pure . IE) av' f <*> rebuildOpenExp (pure . IE) av' z <*> k av a
    Permute f1 a1 f2 a2       -> Permute         <$> rebuildFun (pure . IE) av' f1 <*> k av a1 <*> rebuildFun (pure . IE) av' f2 <*> k av a2
    Backpermute shr sh f a    -> Backpermute shr <$> rebuildOpenExp (pure . IE) av' sh <*> rebuildFun (pure . IE) av' f <*> k av a
    Stencil sr tp f b a       -> Stencil sr tp   <$> rebuildFun (pure . IE) av' f <*> rebuildBoundary av' b  <*> k av a
    Stencil2 s1 s2 tp f b1 a1 b2 a2
                              -> Stencil2 s1 s2 tp <$> rebuildFun (pure . IE) av' f <*> rebuildBoundary av' b1 <*> k av a1 <*> rebuildBoundary av' b2 <*> k av a2
    Aforeign repr ff afun as  -> Aforeign repr ff afun <$> k av as
    -- Collect seq             -> Collect      <$> rebuildSeq k av seq
  where
    av' = reindexAvar av

{-# INLINEABLE rebuildAfun #-}
rebuildAfun
    :: (HasCallStack, Applicative f, SyntacticAcc fa)
    => RebuildAcc acc
    -> RebuildAvar f fa acc aenv aenv'
    -> PreOpenAfun acc aenv  t
    -> f (PreOpenAfun acc aenv' t)
rebuildAfun k av (Abody b) = Abody <$> k av b
rebuildAfun k av (Alam lhs1 f)
  | Exists lhs2 <- rebuildLHS lhs1
  = Alam lhs2 <$> rebuildAfun k (shiftA' lhs1 lhs2 k av) f

rebuildAlet
    :: forall f fa acc aenv1 aenv1' aenv2 bndArrs arrs. (HasCallStack, Applicative f, SyntacticAcc fa)
    => RebuildAcc acc
    -> RebuildAvar f fa acc aenv1 aenv2
    -> ALeftHandSide bndArrs aenv1 aenv1'
    -> acc aenv1  bndArrs
    -> acc aenv1' arrs
    -> f (PreOpenAcc acc aenv2 arrs)
rebuildAlet k av lhs1 bind1 body1
  | Exists lhs2 <- rebuildLHS lhs1
  = Alet lhs2 <$> k av bind1 <*> k (shiftA' lhs1 lhs2 k av) body1

{-# INLINEABLE rebuildLHS #-}
rebuildLHS :: LeftHandSide s t aenv1 aenv1' -> Exists (LeftHandSide s t aenv2)
rebuildLHS (LeftHandSideWildcard r) = Exists $ LeftHandSideWildcard r
rebuildLHS (LeftHandSideSingle s)   = Exists $ LeftHandSideSingle s
rebuildLHS (LeftHandSidePair as bs)
  | Exists as' <- rebuildLHS as
  , Exists bs' <- rebuildLHS bs
  = Exists $ LeftHandSidePair as' bs'

{-# INLINEABLE rebuildBoundary #-}
rebuildBoundary
    :: Applicative f
    => ReindexAvar f aenv aenv'
    -> Boundary aenv t
    -> f (Boundary aenv' t)
rebuildBoundary av bndy =
  case bndy of
    Clamp       -> pure Clamp
    Mirror      -> pure Mirror
    Wrap        -> pure Wrap
    Constant v  -> pure (Constant v)
    Function f  -> Function <$> rebuildFun (pure . IE) av f

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
    ScanSeq f e x        -> ScanSeq <$> rebuildFun (pure . IE) v f <*> rebuildOpenExp (pure . IE) v e <*> pure x

{-# INLINEABLE rebuildC #-}
rebuildC :: forall acc fa f aenv aenv' senv a. (SyntacticAcc fa, Applicative f)
         => RebuildAcc acc
         -> RebuildAvar f fa acc aenv aenv'
         -> Consumer acc aenv senv a
         -> f (Consumer acc aenv' senv a)
rebuildC k v c =
  case c of
    FoldSeq f e x          -> FoldSeq <$> rebuildFun (pure . IE) v f <*> rebuildOpenExp (pure . IE) v e <*> pure x
    FoldSeqFlatten f acc x -> FoldSeqFlatten <$> rebuildAfun k v f <*> k v acc <*> pure x
    Stuple t               -> Stuple <$> rebuildT t
  where
    rebuildT :: Atuple (Consumer acc aenv senv) t -> f (Atuple (Consumer acc aenv' senv) t)
    rebuildT NilAtup        = pure NilAtup
    rebuildT (SnocAtup t s) = SnocAtup <$> (rebuildT t) <*> (rebuildC k v s)
--}

extractExpVars :: OpenExp env aenv a -> Maybe (ExpVars env a)
extractExpVars (Nil  _)       = Just TupRunit
extractExpVars (Pair _ e1 e2) = TupRpair <$> extractExpVars e1 <*> extractExpVars e2
extractExpVars (Evar v)       = Just $ TupRsingle v
extractExpVars _              = Nothing
