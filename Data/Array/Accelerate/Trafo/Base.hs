{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  avarIn, kmap, fromOpenAfun, fromOpenExp, fromOpenFun,

  -- Delayed Arrays
  DelayedAcc,  DelayedOpenAcc(..),
  DelayedAfun, DelayedOpenAfun,
  DelayedExp, DelayedFun, DelayedOpenExp, DelayedOpenFun,
  DelayedSeq, DelayedOpenSeq, StreamSeq(..),
  prettyDelayedSeq,

  -- Environments
  Gamma(..), incExp, prjExp, lookupExp,
  Extend(..), append, bind, Sink(..), sink, sink1,
  weakenGamma1, sinkGamma,
  Supplement(..), bindExps,

  subApply, inlineA,

  -- Tuples
  FreeProd, IsAtupleRepr,

) where

-- standard library
import Control.Applicative                              hiding ( Const )
import Data.Hashable
import Data.Monoid
import Data.Typeable
import Text.PrettyPrint                                 hiding ( (<>) )
import Prelude                                          hiding ( until )

-- friends
import Data.Array.Accelerate.AST                        hiding ( Val(..) )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar                ( Array, Arrays(..), Shape, Elt, IsAtuple, ArrRepr, ArraysR(..), ArraysFlavour(..), Tuple(..), )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product                    ( ProdRepr, IsProduct(..), ProdR(..) )
import Data.Array.Accelerate.Pretty.Print
import Data.Array.Accelerate.Trafo.Dependency
import Data.Array.Accelerate.Trafo.Substitution

import Data.Array.Accelerate.Debug.Stats                as Stats


-- Toolkit
-- =======

-- The bat utility belt of operations required to manipulate terms parameterised
-- by the recursive closure.
--
class (RebuildableAcc acc, Sink acc) => Kit acc where
  inject          :: PreOpenAcc acc aenv a -> acc aenv a
  extract         :: acc aenv a -> PreOpenAcc acc aenv a
  fromOpenAcc     :: OpenAcc aenv a -> acc aenv a
  --
  matchAcc        :: MatchAcc acc
  hashAcc         :: HashAcc acc
  prettyAcc       :: PrettyAcc acc
  dependenciesAcc :: DependenciesAcc acc

instance Kit OpenAcc where
  inject                 = OpenAcc
  extract (OpenAcc pacc) = pacc
  fromOpenAcc            = id

  matchAcc        = matchOpenAcc
  hashAcc         = hashOpenAcc
  prettyAcc       = prettyOpenAcc
  dependenciesAcc = dependenciesOpenAcc

avarIn :: (Kit acc, Arrays arrs) => Idx aenv arrs -> acc aenv arrs
avarIn = inject  . Avar

kmap :: Kit acc => (PreOpenAcc acc aenv a -> PreOpenAcc acc aenv' b) -> acc aenv a -> acc aenv' b
kmap f = inject . f . extract

fromOpenAfun :: Kit acc => OpenAfun aenv f -> PreOpenAfun acc aenv f
fromOpenAfun (Abody a) = Abody $ fromOpenAcc a
fromOpenAfun (Alam f)  = Alam  $ fromOpenAfun f

fromOpenExp :: Kit acc => OpenExp env aenv e -> PreOpenExp acc env aenv e
fromOpenExp = cvtE
  where
    cvtA :: Kit acc => OpenAcc aenv t -> acc aenv t
    cvtA = fromOpenAcc

    cvtT :: Kit acc => Tuple (OpenExp env aenv) t -> Tuple (PreOpenExp acc env aenv) t
    cvtT tup = case tup of
      NilTup      -> NilTup
      SnocTup t a -> cvtT t `SnocTup` cvtE a

    cvtF :: Kit acc => OpenFun env aenv t -> PreOpenFun acc env aenv t
    cvtF = fromOpenFun

    cvtE :: Kit acc => OpenExp env aenv t -> PreOpenExp acc env aenv t
    cvtE exp =
      case exp of
        Let bnd body            -> Let (cvtE bnd) (cvtE body)
        Var ix                  -> Var ix
        Const c                 -> Const c
        Tuple tup               -> Tuple (cvtT tup)
        Prj tup t               -> Prj tup (cvtE t)
        IndexNil                -> IndexNil
        IndexCons sh sz         -> IndexCons (cvtE sh) (cvtE sz)
        IndexHead sh            -> IndexHead (cvtE sh)
        IndexTail sh            -> IndexTail (cvtE sh)
        IndexTrans sh           -> IndexTrans (cvtE sh)
        IndexAny                -> IndexAny
        IndexSlice x ix sh      -> IndexSlice x ix (cvtE sh)
        IndexFull x ix sl       -> IndexFull x (cvtE ix) (cvtE sl)
        ToIndex sh ix           -> ToIndex (cvtE sh) (cvtE ix)
        FromIndex sh ix         -> FromIndex (cvtE sh) (cvtE ix)
        ToSlice x sh i          -> ToSlice x (cvtE sh) (cvtE i)
        Cond p t e              -> Cond (cvtE p) (cvtE t) (cvtE e)
        While p f x             -> While (cvtF p) (cvtF f) (cvtE x)
        PrimConst c             -> PrimConst c
        PrimApp f x             -> PrimApp f (cvtE x)
        Index a sh              -> Index (cvtA a) (cvtE sh)
        LinearIndex a i         -> LinearIndex (cvtA a) (cvtE i)
        Shape a                 -> Shape (cvtA a)
        ShapeSize sh            -> ShapeSize (cvtE sh)
        Intersect s t           -> Intersect (cvtE s) (cvtE t)
        Union s t               -> Union (cvtE s) (cvtE t)
        Foreign ff f e          -> Foreign ff (cvtF f) (cvtE e)

fromOpenFun :: Kit acc
            => OpenFun env aenv t
            -> PreOpenFun acc env aenv t
fromOpenFun fun =
  case fun of
    Body b -> Body (fromOpenExp b)
    Lam f  -> Lam (fromOpenFun f)

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

data StreamSeq index acc t where
  StreamSeq :: Extend acc () aenv -> PreOpenSeq index acc aenv t -> StreamSeq index acc t

type DelayedOpenAfun      = PreOpenAfun DelayedOpenAcc
type DelayedOpenExp       = PreOpenExp DelayedOpenAcc
type DelayedOpenFun       = PreOpenFun DelayedOpenAcc
type DelayedOpenSeq index = PreOpenSeq index DelayedOpenAcc
type DelayedSeq index     = StreamSeq index DelayedOpenAcc

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
  inject                  = Manifest
  extract (Manifest pacc) = pacc
  extract Delayed{}       = error "DelayedAcc.extract"
  fromOpenAcc             = error "DelayedAcc.fromOpenAcc"
  --
  matchAcc                = matchDelayed
  hashAcc                 = hashDelayed
  prettyAcc               = prettyDelayed
  dependenciesAcc         = dependenciesDelayed


hashDelayed :: HashAcc DelayedOpenAcc
hashDelayed (Manifest pacc)     = hash "Manifest" `hashWithSalt` hashPreOpenAcc hashAcc pacc
hashDelayed Delayed{..}         = hash "Delayed"  `hashE` extentD `hashF` indexD `hashF` linearIndexD
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
prettyDelayed wrap aenv acc = case acc of
  Manifest pacc         -> prettyPreOpenAcc prettyDelayed wrap aenv pacc
  Delayed sh f _
    | Shape a           <- sh
    , Just REFL         <- match f (Lam (Body (Index a (Var ZeroIdx))))
    -> prettyDelayed wrap aenv a

    | otherwise
    -> wrap $ hang (text "Delayed") 2
            $ sep [ prettyPreExp prettyDelayed parens aenv sh
                  , parens (prettyPreFun prettyDelayed aenv f)
                  ]

dependenciesDelayed :: DependenciesAcc DelayedOpenAcc
dependenciesDelayed acc = case acc of
  Manifest pacc -> dependenciesPreAcc dependenciesDelayed pacc
  Delayed sh f _ -> dependenciesExp dependenciesDelayed sh <> dependenciesFun dependenciesDelayed f

-- Pretty print delayed sequences
--
-- TLM: What is going on with this sequence thing, why is it closed?
-- RCE: Not all sequence computations are embedded in array computations. For
-- example, if you want to stream the whole sequence out.
--
prettyDelayedSeq
    :: forall index arrs.
       (Doc -> Doc)                             -- apply to compound expressions
    -> DelayedSeq index arrs
    -> Doc
prettyDelayedSeq wrap (StreamSeq env s)
  | (d, aenv) <- pp env
  =  wrap $   (hang (text "let") 2 $ sep $ punctuate semi d)
          <+> (hang (text "in")  2 $ sep $ punctuate semi
                                         $ prettySeq prettyAcc wrap aenv s)
  where
    pp :: Extend DelayedOpenAcc () aenv' -> ([Doc], Val aenv')
    pp BaseEnv          = ([],Empty)
    pp (PushEnv env' a) | (d', aenv) <- pp env'
                        = (prettyAcc wrap aenv a : d', Push aenv (char 'a' <> int (sizeEnv aenv)))

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
     -> acc aenv' a
     -> acc aenv  a
bind BaseEnv         = id
bind (PushEnv env a) = bind env . inject . Alet a

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
--
subApply :: (RebuildableAcc acc, Arrays a)
         => PreOpenAfun acc aenv (a -> b)
         -> acc             aenv a
         -> PreOpenAcc  acc aenv b
subApply (Alam (Abody f)) a = Alet a f
subApply _                _ = error "subApply: inconsistent evaluation"

-- | Replace all occurrences of the first variable with the given array
-- expression. The environment shrinks.
--
inlineA :: Rebuildable f => f (aenv,s) t -> PreOpenAcc (AccClo f) aenv s -> f aenv t
inlineA f g = Stats.substitution "inlineA" $ rebuildA (subAtop g) f

-- Tuple manipulation
-- ==================

-- Note: [Tuple manipulation]
--
-- As a part of various transformations, we need to be able to transform tuples
-- and other product types. Unfortunately, due to the way product types are
-- represented in Accelerate, with a non injective relationship between surface
-- types and representation types, this causes problems. Supposing we have a
-- tuple like so
--
--   (a,b,c)
--
-- then suppose we want to pull b out of it, leaving us with (a,c). However,
-- the only way we can inspect the structure of a product is via its
-- representation type. That means we take
--
-- ((((),a),b),c)
--
-- and product (((),a),c). But what is the surface type corresponding to this
-- representation type?
--
-- FreeProd is a product type that gives a surface type for any product
-- representation type. That is, for all t, FreeProd (ProdRepr t) is a valid
-- product type. Additionally, for all t', ProdRepr (FreeProd t') ~ t'. This
-- gives us what we need in order to transform product types.
--

-- The free product. A surface product type for any given product representation
-- tyoe.
--
data FreeProd t where
  NilFreeProd  :: FreeProd ()
  SnocFreeProd :: Arrays s => FreeProd t -> s -> FreeProd (t,s)
  deriving ( Typeable )

instance IsProduct Arrays (FreeProd ()) where
  type ProdRepr (FreeProd ()) = ()
  fromProd _ _ = ()
  toProd _ _ = NilFreeProd
  prod _ _ = ProdRunit

instance (IsProduct Arrays (FreeProd t), Arrays s) => IsProduct Arrays (FreeProd (t,s)) where
  type ProdRepr (FreeProd (t,s)) = (ProdRepr (FreeProd t), s)
  fromProd cst (SnocFreeProd t s) = (fromProd cst t, s)
  toProd cst (t,s) = SnocFreeProd (toProd cst t) s
  prod cst _ = ProdRsnoc (prod cst (undefined :: FreeProd t))

type instance ArrRepr (FreeProd (t,a)) = (ArrRepr (FreeProd t), ArrRepr a)
type instance ArrRepr (FreeProd ())    = ((),())

instance (IsAtuple (FreeProd t), Typeable t, Arrays (FreeProd t), Arrays a) => Arrays (FreeProd (t,a)) where
  arrays  _ = arrays (undefined :: FreeProd t) `ArraysRpair` arrays (undefined :: a)
  flavour _ = ArraysFtuple
  --
  toArr (t,a) = SnocFreeProd (toArr t) (toArr a)
  fromArr (SnocFreeProd t a) = (fromArr t, fromArr a)

instance Arrays (FreeProd ()) where
  arrays  _ = ArraysRpair ArraysRunit ArraysRunit
  flavour _ = ArraysFtuple
  --
  toArr   _ = NilFreeProd
  fromArr _ = ((),())

-- Unofortunately, the properties that hold for all array tuple representations
-- GHCs typechecker cannot infer.
--
type IsAtupleRepr t = (Arrays (FreeProd t), Typeable t, IsAtuple (FreeProd t), t ~ ProdRepr (FreeProd t))
