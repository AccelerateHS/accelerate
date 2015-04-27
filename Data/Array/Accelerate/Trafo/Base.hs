{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Base
-- Copyright   : [2012..2014] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Base (

  -- Toolkit
  Kit(..), Match(..), (:=:)(REFL),
  avarIn, kmap, fromOpenAfun,

  -- Delayed Arrays
  DelayedAcc,  DelayedOpenAcc(..),
  DelayedAfun, DelayedOpenAfun,
  DelayedExp, DelayedFun, DelayedOpenExp, DelayedOpenFun,
  DelayedSeq(..), DelayedOpenSeq,

  -- Environments
  Gamma(..), incExp, prjExp, lookupExp,
  Extend(..), append, bind, Sink(..), sink, sink1,
  weakenGamma1, sinkGamma,
  Supplement(..), bindExps,

  subApply, inlineA,

  -- Miscellaneous
  prettyDelayedSeq

) where

-- standard library
import Prelude                                          hiding ( until )
import Control.Applicative
import Data.Hashable
import Text.PrettyPrint

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar                ( Array, Arrays, Shape, Elt )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Pretty.Print
import Data.Array.Accelerate.Trafo.Substitution

import Data.Array.Accelerate.Debug                      as Stats


-- Toolkit
-- =======

-- The bat utility belt of operations required to manipulate terms parameterised
-- by the recursive closure.
--
class (RebuildableAcc acc, Sink acc) => Kit acc where
  inject        :: PreOpenAcc acc aenv a -> acc aenv a
  extract       :: acc aenv a -> PreOpenAcc acc aenv a
  fromOpenAcc   :: OpenAcc aenv a -> acc aenv a
  --
  matchAcc      :: MatchAcc acc
  hashAcc       :: HashAcc acc
  prettyAcc     :: PrettyAcc acc

instance Kit OpenAcc where
  inject                 = OpenAcc
  extract (OpenAcc pacc) = pacc
  fromOpenAcc            = id

  matchAcc      = matchOpenAcc
  hashAcc       = hashOpenAcc
  prettyAcc     = prettyOpenAcc

avarIn :: (Kit acc, Arrays arrs) => Idx aenv arrs -> acc aenv arrs
avarIn = inject  . Avar

kmap :: Kit acc => (PreOpenAcc acc aenv a -> PreOpenAcc acc aenv b) -> acc aenv a -> acc aenv b
kmap f = inject . f . extract

fromOpenAfun :: Kit acc => OpenAfun aenv f -> PreOpenAfun acc aenv f
fromOpenAfun (Abody a) = Abody $ fromOpenAcc a
fromOpenAfun (Alam f)  = Alam  $ fromOpenAfun f

-- A class for testing the equality of terms homogeneously, returning a witness
-- to the existentially quantified terms in the positive case.
--
class Match f where
  match :: f s -> f t -> Maybe (s :=: t)

instance Match (Idx env) where
  match = matchIdx

instance Kit acc => Match (PreOpenExp acc env aenv) where
  match = matchPreOpenExp matchAcc hashAcc

instance Kit acc => Match (PreOpenFun acc env aenv) where
  match = matchPreOpenFun matchAcc hashAcc

instance Kit acc => Match (PreOpenAcc acc aenv) where
  match = matchPreOpenAcc matchAcc hashAcc

instance Kit acc => Match (acc aenv) where      -- overlapping, undecidable, incoherent
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

data DelayedSeq t where
 DelayedSeq :: Extend DelayedOpenAcc () aenv -> DelayedOpenSeq aenv () t -> DelayedSeq t

type DelayedOpenAfun    = PreOpenAfun DelayedOpenAcc
type DelayedOpenExp     = PreOpenExp DelayedOpenAcc
type DelayedOpenFun     = PreOpenFun DelayedOpenAcc
type DelayedOpenSeq     = PreOpenSeq DelayedOpenAcc

data DelayedOpenAcc aenv a where
  Manifest              :: PreOpenAcc DelayedOpenAcc aenv a -> DelayedOpenAcc aenv a

  Delayed               :: (Shape sh, Elt e) =>
    { extentD           :: PreExp DelayedOpenAcc aenv sh
    , indexD            :: PreFun DelayedOpenAcc aenv (sh  -> e)
    , linearIndexD      :: PreFun DelayedOpenAcc aenv (Int -> e)
    }                   -> DelayedOpenAcc aenv (Array sh e)

instance Rebuildable DelayedOpenAcc where
  type AccClo DelayedOpenAcc = DelayedOpenAcc
  rebuildPartial v acc = case acc of
    Manifest pacc -> Manifest <$> (rebuildPartial v pacc)
    Delayed{..}   -> Delayed <$> (rebuildPartial v extentD)
                             <*> (rebuildPartial v indexD)
                             <*> (rebuildPartial v linearIndexD)

instance Sink DelayedOpenAcc where

instance Kit DelayedOpenAcc where
  inject        = Manifest
  extract       = error "DelayedAcc.extract"
  fromOpenAcc   = error "DelayedAcc.fromOpenAcc"
  --
  matchAcc      = matchDelayed
  hashAcc       = hashDelayed
  prettyAcc     = prettyDelayed


hashDelayed :: HashAcc DelayedOpenAcc
hashDelayed (Manifest pacc)     = hash "Manifest"       `hashWithSalt` hashPreOpenAcc hashAcc pacc
hashDelayed Delayed{..}         = hash "Delayed"        `hashE` extentD `hashF` indexD `hashF` linearIndexD
  where
    hashE salt = hashWithSalt salt . hashPreOpenExp hashAcc
    hashF salt = hashWithSalt salt . hashPreOpenFun hashAcc

matchDelayed :: MatchAcc DelayedOpenAcc
matchDelayed (Manifest pacc1) (Manifest pacc2)
  = matchPreOpenAcc matchAcc hashAcc pacc1 pacc2

matchDelayed (Delayed sh1 ix1 lx1) (Delayed sh2 ix2 lx2)
  | Just REFL   <- matchPreOpenExp matchAcc hashAcc sh1 sh2
  , Just REFL   <- matchPreOpenFun matchAcc hashAcc ix1 ix2
  , Just REFL   <- matchPreOpenFun matchAcc hashAcc lx1 lx2
  = Just REFL

matchDelayed _ _
  = Nothing


-- Note: If we detect that the delayed array is simply accessing an array
-- variable, then just print the variable name. That is:
--
-- > let a0 = <...> in map f (Delayed (shape a0) (\x0 -> a0!x0))
--
-- becomes
--
-- > let a0 = <...> in map f a0
--
prettyDelayed :: PrettyAcc DelayedOpenAcc
prettyDelayed alvl wrap acc = case acc of
  Manifest pacc         -> prettyPreAcc prettyDelayed alvl wrap pacc
  Delayed sh f _
    | Shape a           <- sh
    , Just REFL         <- match f (Lam (Body (Index a (Var ZeroIdx))))
    -> prettyDelayed alvl wrap a

    | otherwise
    -> wrap $ hang (text "Delayed") 2
            $ sep [ prettyPreExp prettyDelayed 0 alvl parens sh
                  , parens (prettyPreFun prettyDelayed alvl f)
                  ]

prettyDelayedSeq
    :: forall arrs.
       (Doc -> Doc)                             -- apply to compound expressions
    -> DelayedSeq arrs
    -> Doc
prettyDelayedSeq wrap (DelayedSeq env s)
  | (d, lvl) <- pp env 0
  =  wrap $   (hang (text "let") 2 $ sep $ punctuate (text ";") d)
          <+> (hang (text "in") 2  $ sep $ punctuate (text ";") $ prettySeq prettyAcc lvl 0 wrap s)
  where
    pp :: Extend DelayedOpenAcc aenv aenv' -> Int -> ([Doc], Int)
    pp BaseEnv          lvl = ([],lvl)
    pp (PushEnv env' a) lvl | (d', _) <- pp env' (lvl + 1)
                            = (prettyAcc lvl wrap a : d', lvl)


-- Environments
-- ============

-- An environment that holds let-bound scalar expressions. The second
-- environment variable env' is used to project out the corresponding
-- index when looking up in the environment congruent expressions.
--
data Gamma acc env env' aenv where
  EmptyExp :: Gamma      acc env env'      aenv

  PushExp  :: Gamma      acc env env'      aenv
           -> PreOpenExp acc env           aenv t
           -> Gamma      acc env (env', t) aenv

incExp :: RebuildableAcc acc => Gamma acc env env' aenv -> Gamma acc (env, s) env' aenv
incExp EmptyExp        = EmptyExp
incExp (PushExp env e) = incExp env `PushExp` weakenE SuccIdx e

prjExp :: Idx env' t -> Gamma acc env env' aenv -> PreOpenExp acc env aenv t
prjExp ZeroIdx      (PushExp _   v) = v
prjExp (SuccIdx ix) (PushExp env _) = prjExp ix env
prjExp _            _               = $internalError "prjExp" "inconsistent valuation"

weakenGamma1 :: Kit acc => Gamma acc env env' aenv -> Gamma acc env env' (aenv,t)
weakenGamma1 EmptyExp        = EmptyExp
weakenGamma1 (PushExp env e) = PushExp (weakenGamma1 env) (weaken SuccIdx e)

sinkGamma :: Kit acc => Extend acc aenv aenv' -> Gamma acc env env' aenv -> Gamma acc env env' aenv'
sinkGamma _   EmptyExp        = EmptyExp
sinkGamma ext (PushExp env e) = PushExp (sinkGamma ext env) (sink ext e)

lookupExp :: Kit acc => Gamma acc env env' aenv -> PreOpenExp acc env aenv t -> Maybe (Idx env' t)
lookupExp EmptyExp        _ = Nothing
lookupExp (PushExp env e) x
  | Just REFL <- match e x  = Just ZeroIdx
  | otherwise               = SuccIdx `fmap` lookupExp env x


-- As part of various transformations we often need to lift out array valued
-- inputs to be let-bound at a higher point.
--
-- The Extend type is a heterogeneous snoc-list of array terms that witnesses
-- how the array environment is extended by binding these additional terms.
--
data Extend acc aenv aenv' where
  BaseEnv :: Extend acc aenv aenv

  PushEnv :: Arrays a
          => Extend acc aenv aenv' -> acc aenv' a -> Extend acc aenv (aenv', a)

-- Append two environment witnesses
--
append :: Extend acc env env' -> Extend acc env' env'' -> Extend acc env env''
append x BaseEnv        = x
append x (PushEnv as a) = x `append` as `PushEnv` a

-- Bring into scope all of the array terms in the Extend environment list. This
-- converts a term in the inner environment (aenv') into the outer (aenv).
--
bind :: (Kit acc, Arrays a)
     => Extend acc aenv aenv'
     -> PreOpenAcc acc aenv' a
     -> PreOpenAcc acc aenv  a
bind BaseEnv         = id
bind (PushEnv env a) = bind env . Alet a . inject

-- Sink a term from one array environment into another, where additional
-- bindings have come into scope according to the witness and no old things have
-- vanished.
--
sink :: Sink f => Extend acc env env' -> f env t -> f env' t
sink env = weaken (k env)
  where
    k :: Extend acc env env' -> Idx env t -> Idx env' t
    k BaseEnv       = Stats.substitution "sink" id
    k (PushEnv e _) = SuccIdx . k e

sink1 :: Sink f => Extend acc env env' -> f (env,s) t -> f (env',s) t
sink1 env = weaken (k env)
  where
    k :: Extend acc env env' -> Idx (env,s) t -> Idx (env',s) t
    k BaseEnv       = Stats.substitution "sink1" id
    k (PushEnv e _) = split . k e
    --
    split :: Idx (env,s) t -> Idx ((env,u),s) t
    split ZeroIdx      = ZeroIdx
    split (SuccIdx ix) = SuccIdx (SuccIdx ix)

-- This is the same as above, however for the scalar environment.
--
-- RCE: This is much the same as `Gamma` above. The main difference being that the expressions
-- stored in a `Gamma` can not depend on each other, whereas in `Supplement` they can. We should
-- perhaps look at using `Supplement` wherever possible.
--
data Supplement acc env env' aenv where
  BaseSup :: Supplement acc env env aenv
  PushSup :: Elt e
          => Supplement acc env env' aenv
          -> PreOpenExp acc env' aenv e
          -> Supplement acc env (env', e) aenv

bindExps :: (Kit acc, Elt e)
         => Supplement acc env env' aenv
         -> PreOpenExp acc env' aenv e
         -> PreOpenExp acc env aenv e
bindExps BaseSup       = id
bindExps (PushSup g b) = bindExps g . Let b

-- Application via let binding.
subApply :: (RebuildableAcc acc, Arrays a)
         => PreOpenAfun acc aenv (a -> b)
         -> acc             aenv a
         -> PreOpenAcc  acc aenv b
subApply (Alam (Abody f)) a = Alet a f
subApply _                _ = error "subApply: inconsistent evaluation"

-- | Replace all occurences of the first variable with the given array expression. The environment
-- shrinks.
--
inlineA :: Rebuildable f => f (aenv,s) t -> PreOpenAcc (AccClo f) aenv s -> f aenv t
inlineA f g = Stats.substitution "inlineA" $ rebuildA (subAtop g) f
