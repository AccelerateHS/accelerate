{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.Trafo.Base
-- Copyright   : [2012..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Base (

  -- Toolkit
  Kit(..), Match(..), (:~:)(..),
  avarIn, avarsIn, kmap, extractArrayVars,

  -- Delayed Arrays
  DelayedAcc,  DelayedOpenAcc(..),
  DelayedAfun, DelayedOpenAfun,
  DelayedExp,  DelayedOpenExp,
  DelayedFun,  DelayedOpenFun,
  matchDelayedOpenAcc,
  encodeDelayedOpenAcc,

  -- Environments
  Gamma(..), incExp, prjExp, pushExp,
  Extend(..), pushArrayEnv, append, bind,
  Sink(..), sink, sink1,
  Supplement(..), bindExps,

  leftHandSideChangeEnv,

  -- Adding new variables to the environment
  declareArrays, DeclareArrays(..),

  aletBodyIsTrivial,
) where

-- standard library
import Control.Applicative
import Control.DeepSeq
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra
import Data.Maybe
import Data.Monoid
import Data.Type.Equality
import Prelude                                          hiding ( until )

-- friends
import Data.Array.Accelerate.AST                        hiding ( Val(..) )
import Data.Array.Accelerate.Analysis.Hash
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar                ( Array, Arrays, ArraysR(..), Shape, Elt )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Trafo.Substitution

import Data.Array.Accelerate.Debug.Stats                as Stats


-- Toolkit
-- =======

-- The bat utility belt of operations required to manipulate terms parameterised
-- by the recursive closure.
--
class (HasArraysRepr acc, RebuildableAcc acc, Sink acc) => Kit acc where
  inject        :: PreOpenAcc acc aenv a -> acc aenv a
  extract       :: acc aenv a -> Maybe (PreOpenAcc acc aenv a)
  --
  matchAcc      :: MatchAcc acc
  encodeAcc     :: EncodeAcc acc

instance Kit OpenAcc where
  {-# INLINEABLE encodeAcc #-}
  {-# INLINEABLE matchAcc  #-}
  inject                 = OpenAcc
  extract (OpenAcc pacc) = Just pacc
  encodeAcc              = encodeOpenAcc
  matchAcc               = matchOpenAcc

encodeOpenAcc :: EncodeAcc OpenAcc
encodeOpenAcc options (OpenAcc pacc) = encodePreOpenAcc options encodeAcc pacc

matchOpenAcc :: MatchAcc OpenAcc
matchOpenAcc (OpenAcc pacc1) (OpenAcc pacc2) = matchPreOpenAcc matchAcc encodeAcc pacc1 pacc2

avarIn :: forall acc aenv a. Kit acc => ArrayVar aenv a -> acc aenv a
avarIn v@ArrayVar{} = inject $ Avar v

avarsIn :: forall acc aenv arrs. Kit acc => ArrayVars aenv arrs -> acc aenv arrs
avarsIn ArrayVarsNil        = inject Anil
avarsIn (ArrayVarsArray v)  = avarIn v
avarsIn (ArrayVarsPair a b) = inject $ avarsIn a `Apair` avarsIn b

kmap :: Kit acc => (PreOpenAcc acc aenv a -> PreOpenAcc acc aenv b) -> acc aenv a -> acc aenv b
kmap f = inject . f . fromJust . extract

extractArrayVars :: Kit acc => acc aenv a -> Maybe (ArrayVars aenv a)
extractArrayVars (extract -> Just acc) = case acc of
  Apair (extractArrayVars -> Just a) (extractArrayVars -> Just b)
    -> Just $ ArrayVarsPair a b
  Anil
    -> Just ArrayVarsNil
  Avar v
    -> Just $ ArrayVarsArray v
  _ -> Nothing
extractArrayVars _ = Nothing

data DeclareArrays arrs aenv where
  DeclareArrays
    :: LeftHandSide arrs aenv aenv'
    -> (aenv :> aenv')
    -> (forall aenv''. aenv' :> aenv'' -> ArrayVars aenv'' arrs)
    -> DeclareArrays arrs aenv

declareArrays :: ArraysR arrs -> DeclareArrays arrs aenv
declareArrays ArraysRarray
  = DeclareArrays LeftHandSideArray SuccIdx $ \k -> ArrayVarsArray $ ArrayVar $ k ZeroIdx
declareArrays ArraysRunit
  = DeclareArrays (LeftHandSideWildcard ArraysRunit) id $ const $ ArrayVarsNil
declareArrays (ArraysRpair r1 r2) = case declareArrays r1 of
  DeclareArrays lhs1 subst1 a1 -> case declareArrays r2 of
    DeclareArrays lhs2 subst2 a2 ->
      DeclareArrays (LeftHandSidePair lhs1 lhs2) (subst2 . subst1) $ \k -> a1 (k . subst2) `ArrayVarsPair` a2 k



-- fromOpenAfun :: Kit acc => OpenAfun aenv f -> PreOpenAfun acc aenv f
-- fromOpenAfun (Abody a) = Abody $ fromOpenAcc a
-- fromOpenAfun (Alam f)  = Alam  $ fromOpenAfun f

-- A class for testing the equality of terms homogeneously, returning a witness
-- to the existentially quantified terms in the positive case.
--
class Match f where
  match :: f s -> f t -> Maybe (s :~: t)

instance Match (Idx env) where
  {-# INLINEABLE match #-}
  match = matchIdx

instance Match (ArrayVar env) where
  {-# INLINEABLE match #-}
  match (ArrayVar a) (ArrayVar b)
    | Just Refl <- match a b = Just Refl
    | otherwise              = Nothing

instance Kit acc => Match (PreOpenExp acc env aenv) where
  {-# INLINEABLE match #-}
  match = matchPreOpenExp matchAcc encodeAcc

instance Kit acc => Match (PreOpenFun acc env aenv) where
  {-# INLINEABLE match #-}
  match = matchPreOpenFun matchAcc encodeAcc

instance Kit acc => Match (PreOpenAcc acc aenv) where
  {-# INLINEABLE match #-}
  match = matchPreOpenAcc matchAcc encodeAcc

instance {-# INCOHERENT #-} Kit acc => Match (acc aenv) where
  {-# INLINEABLE match #-}
  match = matchAcc


-- Delayed Arrays
-- ==============

-- The type of delayed arrays. This representation is used to annotate the AST
-- in the recursive knot to distinguish standard AST terms from operand arrays
-- that should be embedded into their consumers.
--
type DelayedAcc         = DelayedOpenAcc ()
type DelayedAfun        = PreOpenAfun DelayedOpenAcc ()

type DelayedExp         = DelayedOpenExp ()
type DelayedFun         = DelayedOpenFun ()

-- data DelayedSeq t where
--   DelayedSeq :: Extend DelayedOpenAcc () aenv
--              -> DelayedOpenSeq aenv () t
--              -> DelayedSeq t

type DelayedOpenAfun    = PreOpenAfun DelayedOpenAcc
type DelayedOpenExp     = PreOpenExp DelayedOpenAcc
type DelayedOpenFun     = PreOpenFun DelayedOpenAcc
-- type DelayedOpenSeq     = PreOpenSeq DelayedOpenAcc

data DelayedOpenAcc aenv a where
  Manifest              :: PreOpenAcc DelayedOpenAcc aenv a -> DelayedOpenAcc aenv a

  Delayed               :: (Shape sh, Elt e) =>
    { extentD           :: PreExp DelayedOpenAcc aenv sh
    , indexD            :: PreFun DelayedOpenAcc aenv (sh  -> e)
    , linearIndexD      :: PreFun DelayedOpenAcc aenv (Int -> e)
    }                   -> DelayedOpenAcc aenv (Array sh e)

instance HasArraysRepr DelayedOpenAcc where
  arraysRepr (Manifest a) = arraysRepr a
  arraysRepr Delayed{}    = ArraysRarray

instance Rebuildable DelayedOpenAcc where
  type AccClo DelayedOpenAcc = DelayedOpenAcc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial v acc = case acc of
    Manifest pacc -> Manifest <$> rebuildPartial v pacc
    Delayed{..}   -> Delayed  <$> rebuildPartial v extentD
                              <*> rebuildPartial v indexD
                              <*> rebuildPartial v linearIndexD

instance Sink DelayedOpenAcc where
  weaken k = Stats.substitution "weaken" . rebuildA (rebuildWeakenVar k)

instance Kit DelayedOpenAcc where
  {-# INLINEABLE encodeAcc #-}
  {-# INLINEABLE matchAcc  #-}
  inject                  = Manifest
  extract (Manifest pacc) = Just pacc
  extract Delayed{}       = Nothing
  encodeAcc               = encodeDelayedOpenAcc
  matchAcc                = matchDelayedOpenAcc

instance NFData (DelayedOpenAfun aenv t) where
  rnf = rnfPreOpenAfun rnfDelayedOpenAcc

instance NFData (DelayedOpenAcc aenv t) where
  rnf = rnfDelayedOpenAcc

-- instance NFData (DelayedSeq t) where
--   rnf = rnfDelayedSeq

{-# INLINEABLE encodeDelayedOpenAcc #-}
encodeDelayedOpenAcc :: EncodeAcc DelayedOpenAcc
encodeDelayedOpenAcc options acc =
  let
      travE :: DelayedExp aenv sh -> Builder
      travE = encodePreOpenExp options encodeDelayedOpenAcc

      travF :: DelayedFun aenv f -> Builder
      travF = encodePreOpenFun options encodeDelayedOpenAcc

      travA :: PreOpenAcc DelayedOpenAcc aenv a -> Builder
      travA = encodePreOpenAcc options encodeDelayedOpenAcc

      deep :: Builder -> Builder
      deep x | perfect options = x
             | otherwise       = mempty
  in
  case acc of
    Manifest pacc   -> intHost $(hashQ ("Manifest" :: String)) <> deep (travA pacc)
    Delayed sh f g  -> intHost $(hashQ ("Delayed"  :: String)) <> travE sh <> travF f <> travF g

{-# INLINEABLE matchDelayedOpenAcc #-}
matchDelayedOpenAcc :: MatchAcc DelayedOpenAcc
matchDelayedOpenAcc (Manifest pacc1) (Manifest pacc2)
  = matchPreOpenAcc matchDelayedOpenAcc encodeDelayedOpenAcc pacc1 pacc2

matchDelayedOpenAcc (Delayed sh1 ix1 lx1) (Delayed sh2 ix2 lx2)
  | Just Refl <- matchPreOpenExp matchDelayedOpenAcc encodeDelayedOpenAcc sh1 sh2
  , Just Refl <- matchPreOpenFun matchDelayedOpenAcc encodeDelayedOpenAcc ix1 ix2
  , Just Refl <- matchPreOpenFun matchDelayedOpenAcc encodeDelayedOpenAcc lx1 lx2
  = Just Refl

matchDelayedOpenAcc _ _
  = Nothing

rnfDelayedOpenAcc :: DelayedOpenAcc aenv t -> ()
rnfDelayedOpenAcc (Manifest pacc)    = rnfPreOpenAcc rnfDelayedOpenAcc pacc
rnfDelayedOpenAcc (Delayed sh ix lx) = rnfPreOpenExp rnfDelayedOpenAcc sh
                                 `seq` rnfPreOpenFun rnfDelayedOpenAcc ix
                                 `seq` rnfPreOpenFun rnfDelayedOpenAcc lx

{--
rnfDelayedSeq :: DelayedSeq t -> ()
rnfDelayedSeq (DelayedSeq env s) = rnfExtend rnfDelayedOpenAcc env
                             `seq` rnfPreOpenSeq rnfDelayedOpenAcc s

rnfExtend :: NFDataAcc acc -> Extend acc aenv aenv' -> ()
rnfExtend _    BaseEnv         = ()
rnfExtend rnfA (PushEnv env a) = rnfExtend rnfA env `seq` rnfA a
--}


-- Environments
-- ============

-- An environment that holds let-bound scalar expressions. The second
-- environment variable env' is used to project out the corresponding
-- index when looking up in the environment congruent expressions.
--
data Gamma acc env env' aenv where
  EmptyExp :: Gamma acc env env' aenv

  PushExp  :: Elt t
           => Gamma acc env env' aenv
           -> WeakPreOpenExp acc env aenv t
           -> Gamma acc env (env', t) aenv

data WeakPreOpenExp acc env aenv t where
  Subst    :: env :> env'
           -> PreOpenExp     acc env  aenv t
           -> PreOpenExp     acc env' aenv t {- LAZY -}
           -> WeakPreOpenExp acc env' aenv t

-- XXX: The simplifier calls this function every time it moves under a let
-- binding. This means we have a number of calls to 'weakenE' exponential in the
-- depth of nested let bindings, which quickly causes problems.
--
-- We can improve the situation slightly by observing that weakening by a single
-- variable does no less work than weaking by multiple variables at once; both
-- require a deep copy of the AST. By exploiting laziness (or, an IORef) we can
-- queue up multiple weakenings to happen in a single step.
--
-- <https://github.com/AccelerateHS/accelerate-llvm/issues/20>
--
incExp
    :: Kit acc
    => Gamma acc env     env' aenv
    -> Gamma acc (env,s) env' aenv
incExp EmptyExp        = EmptyExp
incExp (PushExp env w) = incExp env `PushExp` subs w
  where
    subs :: forall acc env aenv s t. Kit acc => WeakPreOpenExp acc env aenv t -> WeakPreOpenExp acc (env,s) aenv t
    subs (Subst k (e :: PreOpenExp acc env_ aenv t) _) = Subst k' e (weakenE k' e)
      where
        k' :: env_ :> (env,s)
        k' = SuccIdx . k

prjExp :: Idx env' t -> Gamma acc env env' aenv -> PreOpenExp acc env aenv t
prjExp ZeroIdx      (PushExp _   (Subst _ _ e)) = e
prjExp (SuccIdx ix) (PushExp env _)             = prjExp ix env
prjExp _            _                           = $internalError "prjExp" "inconsistent valuation"

pushExp :: Elt t => Gamma acc env env' aenv -> PreOpenExp acc env aenv t -> Gamma acc env (env',t) aenv
pushExp env e = env `PushExp` Subst id e e

{--
lookupExp
    :: Kit acc
    => Gamma      acc env env' aenv
    -> PreOpenExp acc env      aenv t
    -> Maybe (Idx env' t)
lookupExp EmptyExp        _ = Nothing
lookupExp (PushExp env e) x
  | Just Refl <- match e x  = Just ZeroIdx
  | otherwise               = SuccIdx `fmap` lookupExp env x

weakenGamma1
    :: Kit acc
    => Gamma acc env env' aenv
    -> Gamma acc env env' (aenv,t)
weakenGamma1 EmptyExp        = EmptyExp
weakenGamma1 (PushExp env e) = PushExp (weakenGamma1 env) (weaken SuccIdx e)

sinkGamma
    :: Kit acc
    => Extend acc aenv aenv'
    -> Gamma acc env env' aenv
    -> Gamma acc env env' aenv'
sinkGamma _   EmptyExp        = EmptyExp
sinkGamma ext (PushExp env e) = PushExp (sinkGamma ext env) (sink ext e)
--}

-- As part of various transformations we often need to lift out array valued
-- inputs to be let-bound at a higher point.
--
-- The Extend type is a heterogeneous snoc-list of array terms that witnesses
-- how the array environment is extended by binding these additional terms.
--
data Extend acc aenv aenv' where
  BaseEnv :: Extend acc aenv aenv

  PushEnv :: Extend acc aenv aenv'
          -> LeftHandSide arrs aenv' aenv''
          -> acc aenv' arrs
          -> Extend acc aenv aenv''

pushArrayEnv :: (Shape sh, Elt e) => Extend acc aenv aenv' -> acc aenv' (Array sh e) -> Extend acc aenv (aenv', Array sh e)
pushArrayEnv env a = PushEnv env LeftHandSideArray a

-- Append two environment witnesses
--
append :: Extend acc env env' -> Extend acc env' env'' -> Extend acc env env''
append x BaseEnv           = x
append x (PushEnv e lhs a) = PushEnv (append x e) lhs a

-- Bring into scope all of the array terms in the Extend environment list. This
-- converts a term in the inner environment (aenv') into the outer (aenv).
--
bind :: (Kit acc, Arrays a)
     => Extend     acc aenv aenv'
     -> PreOpenAcc acc      aenv' a
     -> PreOpenAcc acc aenv       a
bind BaseEnv             = id
bind (PushEnv env lhs a) = bind env . Alet lhs a . inject

-- Sink a term from one array environment into another, where additional
-- bindings have come into scope according to the witness and no old things have
-- vanished.
--
sink :: Sink f => Extend acc env env' -> f env t -> f env' t
sink env = weaken (k env)
  where
    k :: Extend acc env env' -> Idx env t -> Idx env' t
    k BaseEnv           = Stats.substitution "sink" id
    k (PushEnv e (LeftHandSideWildcard _) _) = k e
    k (PushEnv e (LeftHandSideArray)      _) = SuccIdx . k e
    k (PushEnv e (LeftHandSidePair l1 l2) _) = k (PushEnv (PushEnv e l1 undefined) l2 undefined)

sink1 :: Sink f => Extend acc env env' -> f (env,s) t -> f (env',s) t
sink1 env = weaken (k env)
  where
    k :: Extend acc env env' -> Idx (env,s) t -> Idx (env',s) t
    k BaseEnv           = Stats.substitution "sink1" id
    k (PushEnv e (LeftHandSideWildcard _) _) = k e
    k (PushEnv e (LeftHandSideArray)      _) = split . k e
    k (PushEnv e (LeftHandSidePair l1 l2) _) = k (PushEnv (PushEnv e l1 undefined) l2 undefined)

    split :: Idx (env,s) t -> Idx ((env,u),s) t
    split ZeroIdx      = ZeroIdx
    split (SuccIdx ix) = SuccIdx (SuccIdx ix)


-- This is the same as Extend, but for the scalar environment.
--
data Supplement acc env env' aenv where
  BaseSup :: Supplement acc env env aenv

  PushSup :: Elt e
          => Supplement acc env env'      aenv
          -> PreOpenExp acc     env'      aenv e
          -> Supplement acc env (env', e) aenv

bindExps :: (Kit acc, Elt e)
         => Supplement acc env env' aenv
         -> PreOpenExp acc env' aenv e
         -> PreOpenExp acc env  aenv e
bindExps BaseSup       = id
bindExps (PushSup g b) = bindExps g . Let b

leftHandSideChangeEnv :: LeftHandSide arrs env1 env2 -> Exists (LeftHandSide arrs env3)
leftHandSideChangeEnv (LeftHandSideWildcard repr) = Exists $ LeftHandSideWildcard repr
leftHandSideChangeEnv LeftHandSideArray           = Exists $ LeftHandSideArray
leftHandSideChangeEnv (LeftHandSidePair l1 l2)    = case leftHandSideChangeEnv l1 of
  Exists l1' -> case leftHandSideChangeEnv l2 of
    Exists l2' -> Exists $ LeftHandSidePair l1' l2'

aletBodyIsTrivial :: forall acc aenv aenv' a b. Kit acc => LeftHandSide a aenv aenv' -> acc aenv' b -> Maybe (a :~: b)
aletBodyIsTrivial lhs rhs = case extractArrayVars rhs of
  Just vars -> case declareArrays @a @aenv (lhsToArraysR lhs) of
    DeclareArrays lhs' _ value
      | Just Refl <- matchLeftHandSide lhs lhs'
      , Just Refl <- matchArrayVars vars $ value id
        -> Just Refl
    _ -> Nothing
  Nothing -> Nothing
