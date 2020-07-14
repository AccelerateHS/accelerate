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
  matchDelayedOpenAcc,
  encodeDelayedOpenAcc,

  -- Environments
  Gamma(..), incExp, prjExp, pushExp,
  Extend(..), pushArrayEnv, append, bind,
  Sink(..), SinkExp(..), sinkA, sink1,
  OpenExp', bindExps,

  -- Adding new variables to the environment
  declareVars, DeclareVars(..),

  -- Checks
  isIdentity, isIdentityIndexing,

  -- Utilities
  mkIntersect, mkUnion,
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
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Analysis.Hash
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Representation
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
matchOpenAcc (OpenAcc pacc1) (OpenAcc pacc2) = matchPreOpenAcc matchAcc pacc1 pacc2

avarIn :: forall acc aenv a. Kit acc => ArrayVar aenv a -> acc aenv a
avarIn v@(Var ArrayR{} _) = inject $ Avar v

avarsIn :: forall acc aenv arrs. Kit acc => ArrayVars aenv arrs -> acc aenv arrs
avarsIn VarsNil        = inject Anil
avarsIn (VarsSingle v) = avarIn v
avarsIn (VarsPair a b) = inject $ avarsIn a `Apair` avarsIn b

kmap :: Kit acc => (PreOpenAcc acc aenv a -> PreOpenAcc acc aenv b) -> acc aenv a -> acc aenv b
kmap f = inject . f . fromJust . extract

extractArrayVars :: Kit acc => acc aenv a -> Maybe (ArrayVars aenv a)
extractArrayVars (extract -> Just acc) = case acc of
  Apair (extractArrayVars -> Just a) (extractArrayVars -> Just b)
    -> Just $ VarsPair a b
  Anil
    -> Just VarsNil
  Avar v
    -> Just $ VarsSingle v
  _ -> Nothing
extractArrayVars _ = Nothing

data DeclareVars s t aenv where
  DeclareVars
    :: LeftHandSide s t env env'
    -> (env :> env')
    -> (forall env''. env' :> env'' -> Vars s env'' t)
    -> DeclareVars s t env

declareVars :: TupR s t -> DeclareVars s t env
declareVars (TupRsingle s)
  = DeclareVars (LeftHandSideSingle s) (weakenSucc weakenId) $ \k -> VarsSingle $ Var s $ k >:> ZeroIdx
declareVars TupRunit
  = DeclareVars (LeftHandSideWildcard TupRunit) weakenId $ const $ VarsNil
declareVars (TupRpair r1 r2)
  | DeclareVars lhs1 subst1 a1 <- declareVars r1
  , DeclareVars lhs2 subst2 a2 <- declareVars r2
  = DeclareVars (LeftHandSidePair lhs1 lhs2) (subst2 .> subst1) $ \k -> a1 (k .> subst2) `VarsPair` a2 k


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

instance Match (Var s env) where
  {-# INLINEABLE match #-}
  match (Var _ a) (Var _ b)
    | Just Refl <- match a b = Just Refl
    | otherwise              = Nothing

instance Match ScalarType where
  match = matchScalarType

instance Match ArrayR where
  match = matchArrayR

instance Match a => Match (TupR a) where
  match = matchTupR match

instance Match (OpenExp env aenv) where
  {-# INLINEABLE match #-}
  match = matchOpenExp

instance Match (OpenFun env aenv) where
  {-# INLINEABLE match #-}
  match = matchOpenFun

instance Kit acc => Match (PreOpenAcc acc aenv) where
  {-# INLINEABLE match #-}
  match = matchPreOpenAcc matchAcc

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

-- data DelayedSeq t where
--   DelayedSeq :: Extend DelayedOpenAcc () aenv
--              -> DelayedOpenSeq aenv () t
--              -> DelayedSeq t

type DelayedOpenAfun    = PreOpenAfun DelayedOpenAcc
-- type DelayedOpenSeq     = PreOpenSeq DelayedOpenAcc

data DelayedOpenAcc aenv a where
  Manifest              :: PreOpenAcc DelayedOpenAcc aenv a -> DelayedOpenAcc aenv a

  Delayed               ::
    { reprD             :: ArrayR (Array sh e)
    , extentD           :: Exp aenv sh
    , indexD            :: Fun aenv (sh  -> e)
    , linearIndexD      :: Fun aenv (Int -> e)
    }                   -> DelayedOpenAcc aenv (Array sh e)

instance HasArraysRepr DelayedOpenAcc where
  arraysRepr (Manifest a) = arraysRepr a
  arraysRepr Delayed{..}  = TupRsingle reprD

instance Rebuildable DelayedOpenAcc where
  type AccClo DelayedOpenAcc = DelayedOpenAcc
  {-# INLINEABLE rebuildPartial #-}
  rebuildPartial v acc = case acc of
    Manifest pacc -> Manifest <$> rebuildPartial v pacc
    Delayed{..}   -> (\e i l -> Delayed reprD (unOpenAccExp e) (unOpenAccFun i) (unOpenAccFun l))
                              <$> rebuildPartial v (OpenAccExp extentD)
                              <*> rebuildPartial v (OpenAccFun indexD)
                              <*> rebuildPartial v (OpenAccFun linearIndexD)

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
      travE :: Exp aenv sh -> Builder
      travE = encodeOpenExp

      travF :: Fun aenv f -> Builder
      travF = encodeOpenFun

      travA :: PreOpenAcc DelayedOpenAcc aenv a -> Builder
      travA = encodePreOpenAcc options encodeDelayedOpenAcc

      deepA :: forall aenv' a. PreOpenAcc DelayedOpenAcc aenv' a -> Builder
      deepA | perfect options = travA
            | otherwise       = encodeArraysType . arraysRepr
  in
  case acc of
    Manifest pacc    -> intHost $(hashQ ("Manifest" :: String)) <> deepA pacc
    Delayed _ sh f g -> intHost $(hashQ ("Delayed"  :: String)) <> travE sh <> travF f <> travF g

{-# INLINEABLE matchDelayedOpenAcc #-}
matchDelayedOpenAcc :: MatchAcc DelayedOpenAcc
matchDelayedOpenAcc (Manifest pacc1) (Manifest pacc2)
  = matchPreOpenAcc matchDelayedOpenAcc pacc1 pacc2

matchDelayedOpenAcc (Delayed _ sh1 ix1 lx1) (Delayed _ sh2 ix2 lx2)
  | Just Refl <- matchOpenExp sh1 sh2
  , Just Refl <- matchOpenFun ix1 ix2
  , Just Refl <- matchOpenFun lx1 lx2
  = Just Refl

matchDelayedOpenAcc _ _
  = Nothing

rnfDelayedOpenAcc :: DelayedOpenAcc aenv t -> ()
rnfDelayedOpenAcc (Manifest pacc)         = rnfPreOpenAcc rnfDelayedOpenAcc pacc
rnfDelayedOpenAcc (Delayed repr sh ix lx) = rnfArrayR  repr
                                      `seq` rnfOpenExp sh
                                      `seq` rnfOpenFun ix
                                      `seq` rnfOpenFun lx

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
data Gamma env env' aenv where
  EmptyExp :: Gamma env env' aenv

  PushExp  :: Gamma env env' aenv
           -> WeakOpenExp env aenv t
           -> Gamma env (env', t) aenv

data WeakOpenExp env aenv t where
  Subst    :: env :> env'
           -> OpenExp     env  aenv t
           -> OpenExp     env' aenv t {- LAZY -}
           -> WeakOpenExp env' aenv t

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
    :: Gamma env     env' aenv
    -> Gamma (env,s) env' aenv
incExp EmptyExp        = EmptyExp
incExp (PushExp env w) = incExp env `PushExp` subs w
  where
    subs :: forall env aenv s t. WeakOpenExp env aenv t -> WeakOpenExp (env,s) aenv t
    subs (Subst k (e :: OpenExp env_ aenv t) _) = Subst (weakenSucc' k) e (weakenE (weakenSucc' k) e)

prjExp :: Idx env' t -> Gamma env env' aenv -> OpenExp env aenv t
prjExp ZeroIdx      (PushExp _   (Subst _ _ e)) = e
prjExp (SuccIdx ix) (PushExp env _)             = prjExp ix env
prjExp _            _                           = $internalError "prjExp" "inconsistent valuation"

pushExp :: Gamma env env' aenv -> OpenExp env aenv t -> Gamma env (env',t) aenv
pushExp env e = env `PushExp` Subst weakenId e e

{--
lookupExp
    :: Gamma      env env' aenv
    -> OpenExp env      aenv t
    -> Maybe (Idx env' t)
lookupExp EmptyExp        _ = Nothing
lookupExp (PushExp env e) x
  | Just Refl <- match e x  = Just ZeroIdx
  | otherwise               = SuccIdx `fmap` lookupExp env x

weakenGamma1
    :: Gamma env env' aenv
    -> Gamma env env' (aenv,t)
weakenGamma1 EmptyExp        = EmptyExp
weakenGamma1 (PushExp env e) = PushExp (weakenGamma1 env) (weaken SuccIdx e)

sinkGamma
    :: Kit acc
    => Extend acc aenv aenv'
    -> Gamma env env' aenv
    -> Gamma env env' aenv'
sinkGamma _   EmptyExp        = EmptyExp
sinkGamma ext (PushExp env e) = PushExp (sinkGamma ext env) (sinkA ext e)
--}

-- As part of various transformations we often need to lift out array valued
-- inputs to be let-bound at a higher point.
--
-- The Extend type is a heterogeneous snoc-list of array terms that witnesses
-- how the array environment is extended by binding these additional terms.
--
data Extend s f env env' where
  BaseEnv :: Extend s f env env

  PushEnv :: Extend s f env env'
          -> LeftHandSide s t env' env''
          -> f env' t
          -> Extend s f env env''

pushArrayEnv :: HasArraysRepr acc => Extend ArrayR acc aenv aenv' -> acc aenv' (Array sh e) -> Extend ArrayR acc aenv (aenv', Array sh e)
pushArrayEnv env a = PushEnv env (LeftHandSideSingle $ arrayRepr a) a


-- Append two environment witnesses
--
append :: Extend s acc env env' -> Extend s acc env' env'' -> Extend s acc env env''
append x BaseEnv           = x
append x (PushEnv e lhs a) = PushEnv (append x e) lhs a

-- Bring into scope all of the array terms in the Extend environment list. This
-- converts a term in the inner environment (aenv') into the outer (aenv).
--
bind :: Kit acc
     => Extend ArrayR  acc aenv aenv'
     -> PreOpenAcc acc      aenv' a
     -> PreOpenAcc acc aenv       a
bind BaseEnv           = id
bind (PushEnv g lhs a) = bind g . Alet lhs a . inject

-- Sink a term from one array environment into another, where additional
-- bindings have come into scope according to the witness and no old things have
-- vanished.
--
sinkA :: Sink f => Extend s acc env env' -> f env t -> f env' t
sinkA env = weaken (sinkWeaken env) -- TODO: Fix Stats sinkA  vs sink1

sinkWeaken :: Extend s acc env env' -> env :> env'
sinkWeaken BaseEnv = Stats.substitution "sink" weakenId
sinkWeaken (PushEnv e (LeftHandSideWildcard _) _) = sinkWeaken e
sinkWeaken (PushEnv e (LeftHandSideSingle _)   _) = weakenSucc' $ sinkWeaken e
sinkWeaken (PushEnv e (LeftHandSidePair l1 l2) _) = sinkWeaken (PushEnv (PushEnv e l1 undefined) l2 undefined)

sink1 :: Sink f => Extend s acc env env' -> f (env,t') t -> f (env',t') t
sink1 env = weaken $ sink $ sinkWeaken env

-- Wrapper around OpenExp, with the order of type arguments env and aenv flipped
newtype OpenExp' aenv env e = OpenExp' (OpenExp env aenv e)

bindExps :: Extend ScalarType (OpenExp' aenv) env env'
         -> OpenExp env' aenv e
         -> OpenExp env  aenv e
bindExps BaseEnv = id
bindExps (PushEnv g lhs (OpenExp' b)) = bindExps g . Let lhs b


-- Utilities for working with shapes
mkShapeBinary :: (forall env'. OpenExp env' aenv Int -> OpenExp env' aenv Int -> OpenExp env' aenv Int)
              -> ShapeR sh
              -> OpenExp env aenv sh
              -> OpenExp env aenv sh
              -> OpenExp env aenv sh
mkShapeBinary _ ShapeRz _ _ = Nil
mkShapeBinary f (ShapeRsnoc shr) (Pair as a) (Pair bs b) = mkShapeBinary f shr as bs `Pair` f a b
mkShapeBinary f shr (Let lhs bnd a) b = Let lhs bnd $ mkShapeBinary f shr a (weakenE (weakenWithLHS lhs) b)
mkShapeBinary f shr a (Let lhs bnd b) = Let lhs bnd $ mkShapeBinary f shr (weakenE (weakenWithLHS lhs) a) b
mkShapeBinary f shr a b@Pair{} -- `a` is not Pair
  | DeclareVars lhs k value <- declareVars $ shapeType shr
  = Let lhs a $ mkShapeBinary f shr (evars $ value weakenId) (weakenE k b)
mkShapeBinary f shr a b -- `b` is not a Pair
  | DeclareVars lhs k value <- declareVars $ shapeType shr
  = Let lhs b $ mkShapeBinary f shr (weakenE k a) (evars $ value weakenId)

mkIntersect :: ShapeR sh
            -> OpenExp env aenv sh
            -> OpenExp env aenv sh
            -> OpenExp env aenv sh
mkIntersect = mkShapeBinary f
  where
    f a b = PrimApp (PrimMin singleType) $ Pair a b

mkUnion :: ShapeR sh
        -> OpenExp env aenv sh
        -> OpenExp env aenv sh
        -> OpenExp env aenv sh
mkUnion = mkShapeBinary f
  where
    f a b = PrimApp (PrimMax singleType) $ Pair a b

