{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Vectorise
-- Copyright   : [2012..2013] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell, Robert Clifton-Everest
-- License     : BSD3
--
-- Maintainer  : Robert Clifton-Everest <robertce@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Performs Blelloch's flattening transform on an embedded accelerate computation.
--

module Data.Array.Accelerate.Trafo.Vectorise (

  vectoriseSeq,
  vectoriseSeqAcc,
  vectoriseSeqAfun,
  reduceOpenSeq,
  reduceStreamSeq,

  liftOpenAfun1,
  liftOpenAfun2,
  Size,
  Strength(..),
  Context(..)

) where

import Prelude                                          hiding ( exp, replicate, concat )
import qualified Prelude                                as P
import Data.Typeable
import Control.Applicative                              hiding ( Const, empty )
import Control.Monad                                    ( (>=>) )
import Data.Maybe

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Lifted
import Data.Array.Accelerate.Array.Representation      ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar               hiding ( Segments )
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Trafo.Fusion
import Data.Array.Accelerate.Pretty                    ()
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar
import qualified Data.Array.Accelerate.Smart            as S
import qualified Data.Array.Accelerate.Prelude          as S
import qualified Data.Array.Accelerate.Language         as S
import qualified Data.Array.Accelerate.Trafo.Sharing    as S

import qualified Data.Array.Accelerate.Debug            as Debug
import Data.Array.Accelerate.Error

-- |Encodes the relationship between the old environments and the new environments during the
-- lifting transform
--
data Context env aenv env' aenv' where
  -- All environments are empty
  EmptyC     :: Context () () () ()

  -- An expression that has already been lifted
  PushLExpC :: Elt e
            => Context env aenv env' aenv'
            -> Context (env, e) aenv env' (aenv', Vector' e)

  -- An unlifted expression
  PushExpC  :: Elt e
            => Context env aenv env' aenv'
            -> Context (env, e) aenv (env',e) aenv'

  -- A lifted array expression
  PushLAccC :: Arrays t
            => Context env aenv env' aenv'
            -> Context env (aenv, t) env' (aenv', Nested t)

  -- An unlifted array expression
  PushAccC  :: Arrays t
            => Context env aenv env' aenv'
            -> Context env (aenv, t) env' (aenv', t)


data Strength = Aggressive | Conservative | HoistOnly | Nested deriving Show

type VectoriseAcc acc = forall aenv aenv' t.
                        Arrays t
                     => Strength
                     -> Context () aenv () aenv'
                     -> Size acc aenv' Int
                     -> acc aenv t
                     -> LiftedAcc acc aenv' t

data None sh = None sh
  deriving (Typeable, Show, Eq)

type instance EltRepr (None sh) = EltRepr sh

instance Shape sh => Elt (None sh) where
  eltType _         = eltType (undefined::sh)
  fromElt (None sh) = fromElt sh
  toElt sh          = None (toElt sh)

instance Shape sh => Slice (None sh) where
  type SliceShape   (None sh) = Z
  type CoSliceShape (None sh) = sh
  type FullShape    (None sh) = sh
  sliceIndex _ = sliceNoneIndex (undefined :: sh)
  toSlice sh i | AsSlice <- asSlice (Proxy :: Proxy sh)
                         = None (toSlice sh i)

instance Shape sh => IsProduct Elt (None sh) where
  type ProdRepr (None sh) = ((),sh)
  fromProd _ (None sh) = ((),sh)
  toProd _ ((),sh)     = None sh
  prod _ _ = ProdRsnoc ProdRunit

-- Lifting terms
-- ---------------
--

-- |The size parameter in the lifting transform.
--
data Size acc aenv e where
  Size :: Arrays a => acc aenv a -> PreExp acc (aenv,a) e -> Size acc aenv e

instance Kit acc => Sink (Size acc) where
  weaken k (Size b s) = Size (weaken k b) (weaken (newTop k) s)

data LiftedAcc acc aenv t = AvoidedAcc (acc aenv t)
                          | LiftedAcc (acc aenv (Nested t))

instance RebuildableAcc acc => Rebuildable (LiftedAcc acc) where
  type AccClo (LiftedAcc acc) = acc
  rebuildPartial v (AvoidedAcc a) = AvoidedAcc <$> rebuildPartial v a
  rebuildPartial v (LiftedAcc  a) = LiftedAcc <$> rebuildPartial v a

instance Sink acc => Sink (LiftedAcc acc) where
  weaken k (AvoidedAcc a) = AvoidedAcc (weaken k a)
  weaken k (LiftedAcc a)  = LiftedAcc (weaken k a)

data LiftedExp acc env aenv t where
  AvoidedExp :: Extend acc aenv aenv'
             -> PreOpenExp acc env aenv' t
             -> LiftedExp acc env aenv t
  LiftedExp  :: acc aenv (Vector' t)
             -> LiftedExp acc env aenv t

data AvoidedFun acc env aenv t where
  AvoidedFun :: Extend acc aenv aenv'
             -> PreOpenFun acc env aenv' t
             -> AvoidedFun acc env aenv t

type LiftedOpenAcc aenv t = LiftedAcc OpenAcc aenv t

over :: (acc aenv t          -> acc' aenv' t')
     -> (acc aenv (Nested t) -> acc' aenv' (Nested t'))
     -> LiftedAcc acc  aenv t
     -> LiftedAcc acc' aenv' t'
over f _  (AvoidedAcc a) = AvoidedAcc (f a)
over _ f' (LiftedAcc l)  = LiftedAcc (f' l)

injectL :: Kit acc => LiftedAcc (PreOpenAcc acc) aenv t -> LiftedAcc acc aenv t
injectL = over inject inject

vectoriseOpenAcc :: Arrays t
                 => Strength
                 -> Context () aenv () aenv'
                 -> Size OpenAcc aenv' Int
                 -> OpenAcc aenv t
                 -> LiftedOpenAcc aenv' t
vectoriseOpenAcc strength ctx size (OpenAcc a) = liftPreOpenAcc vectoriseOpenAcc strength ctx size a

liftedSize :: forall acc aenv t.
              (Kit acc, Arrays t)
           => acc aenv (Nested t)
           -> Size acc aenv Int
liftedSize a =
  case flavour (undefined :: t) of
    ArraysFunit  -> Size (inject $ Aprj ZeroTupIdx a) $ Index avar0 IndexNil
    ArraysFarray -> Size (inject $ Aprj ZeroTupIdx a) $ indexLastC (Shape avar0)
    ArraysFtuple -> fromTup $ prod (Proxy :: Proxy Arrays) (undefined :: t)
  where
    fromTup :: (ArrRepr t ~ (l,e), IsAtuple t) => ProdR Arrays (TupleRepr t) -> Size acc aenv Int
    fromTup ProdRunit     = error "Unreachable"
    fromTup (ProdRsnoc _) = liftedSize $^ Aprj tupIx0 a

-- |Lift a unary open array function
--
liftOpenAcc :: forall aenv aenv' a. Arrays a =>
                 Strength
              -> Context () aenv () aenv'
              -> OpenAcc aenv   a
              -> OpenAfun aenv' (Scalar Int -> Nested a)
liftOpenAcc strength ctx (weaken SuccIdx -> acc)
  | trace "liftOpenAcc" ("Starting " ++ show strength ++ " vectorisation") True
  = case vectoriseOpenAcc Conservative (PushAccC ctx) (simpleSize (the avar0)) acc of
      -- In the case that the body of the function does not depend on its argument,
      -- conservative vectorisation will return the unmodified body. In this,
      -- we just need to replicate the result.
      AvoidedAcc a' | Size b s <- simpleSize (the avar0)
                    -> Alam . Abody $ replicateC (inject $ Alet b $ inject $ Unit s) a'
      -- Otherwise, we have the lifted body.
      LiftedAcc  a' -> Alam . Abody $ a'
liftOpenAcc _ _ _
  = error "Unreachable"

-- |Lift a unary open array function
--
liftOpenAfun1 :: forall aenv aenv' a b.
                 Strength
              -> Context () aenv () aenv'
              -> OpenAfun aenv  (a -> b)
              -> OpenAfun aenv' (Nested a -> Nested b)
liftOpenAfun1 strength ctx (Alam (Abody f))
  | trace "liftOpenAfun1" ("Starting " ++ show strength ++ " vectorisation") True
  = case vectoriseOpenAcc Conservative (PushLAccC ctx) (liftedSize avar0) f of
      -- In the case that the body of the function does not depend on its argument,
      -- conservative vectorisation will return the unmodified body. In this,
      -- we just need to replicate the result.
      AvoidedAcc a' | Size b s <- liftedSize avar0
                    -> Alam . Abody $ replicateC (inject $ Alet b $ inject $ Unit s) a'
      -- Otherwise, we have the lifted body.
      LiftedAcc  a' -> Alam . Abody $ a'
liftOpenAfun1 _ _ _
  = error "Unreachable"

-- |Lift a binary open array function
--
liftOpenAfun2 :: forall aenv aenv' a b c.
                 Strength
              -> Context () aenv () aenv'
              -> OpenAfun aenv  (a -> b -> c)
              -> OpenAfun aenv' (Nested a -> Nested b -> Nested c)
liftOpenAfun2 strength ctx (Alam (Alam (Abody f)))
  | trace "liftOpenAfun2" ("Starting " ++ show strength ++ " vectorisation") True
  = case vectoriseOpenAcc Conservative (PushLAccC . PushLAccC $ ctx) (liftedSize avar0) f of
      -- In the case that the body of the function does not depend on its argument,
      -- conservative vectorisation will return the unmodified body. In this,
      -- we just need to replicate the result.
      AvoidedAcc a' | Size b s <- liftedSize avar0
                    -> Alam . Alam . Abody $ replicateC (inject $ Alet b $ inject $ Unit s) a'
      -- Otherwise, we have the lifted body.
      LiftedAcc  a' -> Alam . Alam . Abody $ a'
liftOpenAfun2 _ _ _
  = error "Unreachable"

-- |The core of the lifting transformation for array expression.
--
liftPreOpenAcc :: forall acc aenv aenv' t. (Kit acc, Arrays t)
               => VectoriseAcc acc
               -> Strength
               -> Context () aenv () aenv'
               -> Size acc aenv' Int
               -> PreOpenAcc acc aenv t
               -> LiftedAcc acc aenv' t
liftPreOpenAcc vectAcc strength ctx size acc
  = case acc of
    Alet a b            -> aletL a b
    Avar ix             -> avarL ix
    Atuple tup          -> atupleL tup
    Aprj tup a          -> aprjL tup a
    Apply f a           -> applyL f a
    Aforeign ff afun as -> foreignL ff afun as
    Acond p t e         -> acondL p t e
    Awhile p it i       -> awhileL p it i
    Use a               -> useL a
    Unit e              -> unitL e
    Reshape e a         -> reshapeL e a
    Generate e f        -> generateL e f
    -- Transform and Subarray only appear as part of subsequent optimsations.
    Transform {}        -> $internalError "liftPreOpenAcc" "Unable to vectorise Transform"
    Subarray {}         -> $internalError "liftPreOpenAcc" "Unable to vectorise Subarrays"
    Replicate sl slix a -> replicateL sl slix a
    Slice sl a slix     -> sliceL sl a slix
    Map f a             -> mapL f a
    ZipWith f a1 a2     -> zipWithL f a1 a2
    Fold f z a          -> foldL f z a
    Fold1 f a           -> fold1L f a
    FoldSeg f z a s     -> foldSegL f z a s
    Fold1Seg f a s      -> fold1SegL f a s
    Scanl f z a         -> scanlL f z a
    Scanl' f z a        -> scanl'L f z a
    Scanl1 f a          -> scanl1L f a
    Scanr f z a         -> scanrL f z a
    Scanr' f z a        -> scanr'L f z a
    Scanr1 f a          -> scanr1L f a
    Permute f1 a1 f2 a2 -> permuteL f1 a1 f2 a2
    Backpermute sh f a  -> backpermuteL sh f a
    Stencil f b a       -> stencilL f b a
    Stencil2 f b1 a1 b2 a2
                        -> stencil2L f b1 a1 b2 a2
    Collect _ _           -> error "Nested sequence"

  where
    nestedError :: String -> String -> String
    nestedError place op = "Unexpect nested parallelism in " ++ place ++ " argument to " ++ op

    hoistingOnlyError :: a
    hoistingOnlyError = error "The vectorisation strength is set to hoisting only, but nested parallelism has been encountered"

    avoidLifting | Conservative <- strength = True
                 | HoistOnly    <- strength = True
                 | otherwise                = False

    cvtA :: forall t. Arrays t => acc aenv t -> LiftedAcc acc aenv' t
    cvtA a = vectAcc strength ctx size a

    liftedAcc :: forall aenv t. acc aenv (Nested t) -> LiftedAcc acc aenv t
    liftedAcc | HoistOnly <- strength
              = hoistingOnlyError
              | otherwise
              = trace "liftPreOpenAcc" ("Lifting Acc term " ++ showPreAccOp acc) . LiftedAcc

    liftE :: forall env env' aenv aenv' e. (Elt e)
          => Context env aenv env' aenv'
          -> Size acc aenv' Int
          -> PreOpenExp acc env aenv e
          -> PreOpenAcc acc aenv' (Vector' e)
    liftE | HoistOnly <- strength
          = hoistingOnlyError
          | otherwise
          = liftExp vectAcc strength

    cvtE :: forall e. Elt e
         => PreExp acc aenv e
         -> LiftedExp acc () aenv' e
    cvtE e | avoidLifting
           , Avoided (b,e') <- avoidE e
           = AvoidedExp b e'
           | otherwise
           = trace "liftPreOpenAcc" ("Expression had to be lifted: " ++ showPreExpOp e)
           $ LiftedExp $ inject $ liftE ctx size e

    cvtT :: forall t.
            Atuple (acc aenv) t
         -> Atuple (LiftedAcc acc aenv') t
    cvtT NilAtup        = NilAtup
    cvtT (SnocAtup t a) = SnocAtup (cvtT t) (cvtA a)

    irregularTupleIdx :: forall t a. TupleIdx t a -> TupleIdx (NestedTupleRepr t) (Nested a)
    irregularTupleIdx ZeroTupIdx = ZeroTupIdx
    irregularTupleIdx (SuccTupIdx ix) = SuccTupIdx (irregularTupleIdx ix)

    liftAfun1 :: forall a b. (Arrays a, Arrays b)
              => PreOpenAfun acc aenv (a -> b)
              -> ( PreOpenAfun acc aenv' (Nested a -> Nested b)
                 , Maybe (PreOpenAfun acc aenv' (a -> b)))
    liftAfun1 (Alam (Abody b))
      = let
          lft = case vectAcc strength (PushLAccC ctx) (liftedSize avar0) b of
                  -- Result does not actually depend on the argument.
                  AvoidedAcc b' -> Alam (Abody (replicateA b' (liftedSize avar0)))
                  -- Result does depend on argument
                  LiftedAcc  b' -> Alam (Abody b')
          pln = case vectAcc strength (PushAccC ctx) (weakenA1 size) b of
                  AvoidedAcc b' -> Just $ Alam (Abody b')
                  LiftedAcc  _  -> Nothing
        in (lft,pln)
    liftAfun1 _ = error "Inconsistent"


    cvtF1 :: forall a b. (Elt a, Elt b)
          => PreFun  acc  aenv  (a -> b)
          -> ( PreOpenAfun acc aenv' (Vector a -> Vector b)
             , Maybe (AvoidedFun acc () aenv' (a -> b)))
    cvtF1 f@(Lam (Body e))
      = let l = Alam (Abody (inject $ liftE (PushLExpC ctx) (sizeOfVector' avar0) e))
        in case (avoidF f) of
             Avoided (b, Lam (Body e')) | avoidLifting
                                        -> (rewrap l, Just $ AvoidedFun b (Lam (Body e')))
             _                          -> trace "liftPreOpenAcc" "Function had to be lifted"
                                        $  (rewrap l, Nothing)
        where
          rewrap :: PreOpenAfun acc aenv' (Vector' a -> Vector' b) -> PreOpenAfun acc aenv' (Vector a -> Vector b)
          rewrap f = Alam . Abody . asVectorC $ weakenA1 f `apply` asVector'C avar0

    cvtF1 _              = $internalError "cvtF1" "Inconsistent valuation"

    cvtF2 :: forall a b c. (Elt a, Elt b, Elt c)
          => PreFun  acc  aenv  (a -> b -> c)
          -> ( PreOpenAfun acc aenv' (Vector a -> Vector b -> Vector c)
             , Maybe (AvoidedFun acc () aenv' (a -> b -> c)))
    cvtF2 f@(Lam (Lam (Body e)))
      = let l = Alam (Alam (Abody (inject $ liftE (PushLExpC (PushLExpC ctx))
                                                  (sizeOfVector' avar0)
                                                  e)))

        in case (avoidF f) of
             Avoided (b, Lam (Lam (Body e'))) | avoidLifting
                                              -> (rewrap l, Just $ AvoidedFun b (Lam (Lam (Body e'))))
             _                                -> trace "liftPreOpenAcc" "Function had to be lifted"
                                              $  (rewrap l, Nothing)
      where
        rewrap :: PreOpenAfun acc aenv' (Vector' a -> Vector' b -> Vector' c)
               -> PreOpenAfun acc aenv' (Vector a -> Vector b -> Vector c)
        rewrap f = Alam . Alam . Abody . asVectorC $ weakenA2 f `partApply` asVector'C avar1 `apply` asVector'C avar0
    cvtF2 _              = $internalError "cvtF2" "Inconsistent valuation"

    unzip :: forall aenv a b sh.
             (Elt a, Elt b, Shape sh)
          => acc aenv (Array sh (a,b))
          -> acc aenv (Array sh a, Array sh b)
    unzip = fromHOAS (S.lift . S.unzip)

    construct :: forall aenv e sh.
                 (Elt e, Shape sh)
              => acc aenv (Segments sh)
              -> acc aenv (Vector e)
              -> acc aenv (Nested (Array sh e))
    construct segs vals
      = inject
      $ Atuple (SnocAtup (SnocAtup NilAtup segs) vals)

    indexSnoc :: forall env aenv sh. (Shape sh, Slice sh)
              => PreOpenExp acc env aenv sh
              -> PreOpenExp acc env aenv Int
              -> PreOpenExp acc env aenv (sh:.Int)
    indexSnoc sh i = IndexTrans (IndexCons (IndexTrans sh) i)

    indexInit :: forall env aenv sh. (Shape sh, Slice sh)
              => PreOpenExp acc env aenv (sh:.Int)
              -> PreOpenExp acc env aenv sh
    indexInit = IndexTrans . IndexTail . IndexTrans

    nonEmpty :: forall env aenv sh. Shape sh
             => PreOpenExp acc env aenv sh
             -> PreOpenExp acc env aenv sh
    nonEmpty = Union (Const $ fromElt (listToShape (P.replicate (dim (undefined :: sh)) 1) :: sh))

    lifted :: forall t. Arrays t => LiftedAcc acc aenv' t -> acc aenv' (Nested t)
    lifted (AvoidedAcc a)   = replicateA a size
    lifted (LiftedAcc l)    = l

    liftedE :: forall t. Elt t => LiftedExp acc () aenv' t -> acc aenv' (Vector' t)
    liftedE (AvoidedExp b e) = inject . bind b . extract $ replicatedC (unitSize (sink b size)) (unit e)
    liftedE (LiftedExp  e)   = e

    -- Nested versions of combinators.
    -- ===================================

    aletL :: forall bnd. (Arrays bnd, Arrays t)
          => acc aenv bnd
          -> acc (aenv, bnd) t
          -> LiftedAcc acc aenv' t
    aletL bnd body = injectL
                   $ case (cvtA bnd) of
                       AvoidedAcc a -> over (Alet a) (Alet a)
                                    $ vectAcc strength (PushAccC ctx) (weakenA1 size) body
                       a            -> over (Alet (lifted a)) (Alet (lifted a))
                                    $ vectAcc strength (PushLAccC ctx) (weakenA1 size) body

    avarL :: Arrays t
          => Idx aenv t
          -> LiftedAcc acc aenv' t
    avarL = cvtIx ctx
      where
        cvtIx :: forall env aenv env' aenv'.
                 Context env aenv env' aenv'
              -> Idx aenv t
              -> LiftedAcc acc aenv' t
        cvtIx (PushLExpC d) ix             = weakenA1 (cvtIx d ix)
        --cvtIx (PushExpC  d)   ix           = weakenE1 (cvtIx d ix)
        cvtIx (PushLAccC _)   ZeroIdx      = liftedAcc $ avar0
        cvtIx (PushLAccC d)   (SuccIdx ix) = weakenA1 (cvtIx d ix)
        cvtIx (PushAccC  _)   ZeroIdx      = AvoidedAcc avar0
        cvtIx (PushAccC  d)   (SuccIdx ix) = weakenA1 (cvtIx d ix)
        cvtIx _               _            = $internalError "liftExp" "Inconsistent valuation"

    atupleL :: (Arrays t, IsAtuple t)
            => Atuple (acc aenv) (TupleRepr t)
            -> LiftedAcc acc aenv' t
    atupleL t = case atl (cvtT t) of
                  Left (a,_)  -> AvoidedAcc (inject $ Atuple a)
                  Right a     -> case flavour (undefined :: t) of
                                   ArraysFunit  -> liftedAcc $^ Atuple (SnocAtup NilAtup (unitSize size))
                                   ArraysFtuple -> liftedAcc $ inject $ Atuple a
                                   _            -> error "Absurd"
      where
        atl :: forall t.
               Atuple (LiftedAcc acc aenv') t
            -> Either (Atuple (acc aenv') t, Atuple (acc aenv') (NestedTupleRepr t))
                      (Atuple (acc aenv') (NestedTupleRepr t))
        atl NilAtup        = Left (NilAtup, NilAtup)
        atl (SnocAtup t a) = case atl t of
                               Left (av,li) | AvoidedAcc (a' :: acc aenv' a) <- a
                                            -> Left (SnocAtup av a', SnocAtup li (replicateA a' size))
                                            | LiftedAcc (a' :: acc aenv' (Nested a)) <- a
                                            -> Right (SnocAtup li a')
                               Right li     | LiftedAcc (a' :: acc aenv' (Nested a)) <- a
                                            -> Right (SnocAtup li a')
                                            | AvoidedAcc (a' :: acc aenv' a) <- a
                                            -> Right (SnocAtup li (replicateA a' size))
                               _            -> error "Unreachable code"


    aprjL :: forall a arrs. (Arrays a, Arrays arrs, IsAtuple arrs, Arrays (Nested a))
          => TupleIdx (TupleRepr arrs) a
          -> acc aenv arrs
          -> LiftedAcc acc aenv' a
    aprjL tup a | ArraysFtuple <- flavour (undefined :: arrs)
                = injectL $ over (Aprj tup) (Aprj (irregularTupleIdx tup)) (cvtA a)
                | otherwise = error "Absurd"

    applyL :: forall a1 a2.
              (Arrays a1, Arrays a2)
           => PreOpenAfun acc aenv (a1 -> a2)
           -> acc aenv a1
           -> LiftedAcc acc aenv' a2
    applyL f a1 | let (lft, pln) = liftAfun1 f
                = case cvtA a1 of
                    AvoidedAcc a1' |  avoidLifting
                                   ,  Just f' <- pln
                                   -> AvoidedAcc $ inject $ Apply f' a1'
                                   |  otherwise
                                   -> liftedAcc $ inject $ Apply lft (replicateA a1' size)
                    a1'            -> liftedAcc $ inject $ Apply lft (lifted a1')

    foreignL :: (Arrays arrs, Arrays t, Foreign f)
             => f arrs t
             -> PreAfun     acc            (arrs -> t)
             -> acc             aenv  arrs
             -> LiftedAcc   acc aenv' t
    foreignL ff afun (cvtA -> AvoidedAcc as)
      = AvoidedAcc $ inject $ Aforeign ff afun as
    foreignL _  _    _
      = error $ nestedError "first" "foreign"

    acondL :: Arrays t
           => PreExp acc aenv Bool
           -> acc aenv t
           -> acc aenv t
           -> LiftedAcc acc aenv' t
    acondL (cvtE -> p) (cvtA -> t) (cvtA -> e)
      | avoidLifting
      , AvoidedExp b p' <- p
      , AvoidedAcc t'   <- t
      , AvoidedAcc e'   <- e
      = AvoidedAcc $ inject $ bind b $ Acond p' (sink b t') (sink b e')
      | AvoidedExp b p' <- p
      = liftedAcc $ inject $ bind b $ Acond p' (sink b (lifted t)) (sink b (lifted e))
      | otherwise
      = liftedAcc $ liftedCondC (asVectorC (liftedE p)) (lifted t) (lifted e)

    -- TODO: Try to find a way to clean this up
    awhileL :: forall t. Arrays t
            => PreOpenAfun acc aenv (t -> Scalar Bool)
            -> PreOpenAfun acc aenv (t -> t)
            -> acc             aenv t
            -> LiftedAcc acc aenv' t
    awhileL (liftAfun1 -> (pred_l, pred_p)) (liftAfun1 -> (iter_l, iter_p)) (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a' <- a
      , Just pred_p'  <- pred_p
      , Just iter_p'  <- iter_p
      = AvoidedAcc $ inject $ Awhile pred_p' iter_p' a'
      | otherwise
      = liftedAcc
      $^ Alet (lifted a)
      $^ let
           init' = inject $ Alet (valuesC $ weakenA1 pred_l `apply` avar0)
                          $ atup3 avar1 avar0 (fromHOAS S.or avar0)

           pred' = Alam $ Abody $ inject $ Aprj ZeroTupIdx avar0

           iter' :: acc (aenv', s) (Nested t)
                 -> acc (aenv', s) (Vector Bool)
                 -> acc (aenv', s) (Scalar Bool)
                 -> acc (aenv', s) (Nested t, Vector Bool, Scalar Bool)
           iter' a f _ = let a' = liftedCondC f (weakenA1 iter_l `apply` a) a
                             f' = fromHOAS2 (S.zipWith (S.&&*)) f (valuesC $ weakenA1 pred_l `apply` a')
                             c' = fromHOAS S.or f'
                         in atup3 a' f' c'

           iter'' :: PreOpenAfun acc aenv' ((Nested t, Vector Bool, Scalar Bool)
                  -> (Nested t, Vector Bool, Scalar Bool))
           iter'' = Alam $ Abody $ iter' (inject $ Aprj (SuccTupIdx . SuccTupIdx $ ZeroTupIdx) avar0)
                                         (inject $ Aprj (SuccTupIdx ZeroTupIdx) avar0)
                                         (inject $ Aprj ZeroTupIdx avar0)

         in Aprj (SuccTupIdx . SuccTupIdx $ ZeroTupIdx)
         $^ Awhile pred'
                   (weakenA1 iter'')
                   init'

    useL :: Arrays a
         => ArrRepr a
         -> LiftedAcc acc aenv' a
    useL a = AvoidedAcc $ inject $ Use a

    unitL :: Elt e
          => PreExp acc aenv e
          -> LiftedAcc acc aenv' (Scalar e)
    unitL e = case cvtE e of
                AvoidedExp b e | avoidLifting
                               -> AvoidedAcc $ inject $ bind b $ Unit e
                a              -> liftedAcc
                               $ liftedScalarC (asVectorC (liftedE a))

    reshapeL :: forall sh sh' e.
                (Shape sh, Shape sh', Elt e)
             => PreExp acc aenv sh
             -> acc aenv (Array sh' e)
             -> LiftedAcc acc aenv' (Array sh e)
    reshapeL (cvtE -> sh) (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a'    <- a
      , AvoidedExp b sh' <- sh
      = AvoidedAcc $ inject $ (bind b $ Reshape sh' (sink b a'))
      | avoidLifting
      , AvoidedExp b sh' <- sh
      = liftedAcc
      $  regularC (unitSize size) (inject . bind b $ Unit sh') (valuesC (lifted a))
      | otherwise
      = liftedAcc $ liftedReshapeC (liftedE sh) (lifted a)


    generateL :: forall sh e. (Elt e, Shape sh)
              => PreExp acc aenv sh
              -> PreFun acc aenv (sh -> e)
              -> LiftedAcc  acc aenv' (Array sh e)
    generateL (cvtE -> e) (cvtF1 -> (f_l, f_a))
      | avoidLifting
      , Just (AvoidedFun b2 f) <- f_a
      , AvoidedExp b1 e' <- e
      = AvoidedAcc
      $^ bind b2
      $  Alet (sink b2 $ inject $ bind b1 $ Unit e')
      $^ Generate (the avar0) (weakenA1 f)
      | otherwise
      = liftedAcc
      $^ Alet (segmentsFromExp e)
      $ construct avar0
                  (inject $ weakenA1 f_l `subApply` (inject . Map (fun1 $ Prj tupIx0) $ enumSegC avar0))

    replicateL :: forall sh sl slix e co.
                  (Shape sh, Shape sl, Slice slix, Elt e)
               => SliceIndex (EltRepr slix)
                             (EltRepr sl)
                             co
                             (EltRepr sh)
               -> PreExp     acc aenv slix
               -> acc            aenv (Array sl e)
               -> LiftedAcc  acc aenv' (Array sh e)
    replicateL sl (cvtE -> slix) (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a'      <- a
      , AvoidedExp b slix' <- slix
      = AvoidedAcc
      $^ bind b
      $  Replicate sl slix' (sink b a')
    replicateL sl slix a
      = cvtA
      $^ Alet a
      $^ Backpermute (IndexFull sl (weakenA1 slix) (Shape avar0))
                     (Lam $ Body $ IndexSlice sl (Proxy :: Proxy slix) var0)
      $ avar0

    sliceL :: forall sh sl slix e co.
              (Shape sh, Shape sl, Slice slix, Elt e)
           => SliceIndex (EltRepr slix)
                         (EltRepr sl)
                         co
                         (EltRepr sh)
           -> acc            aenv (Array sh e)
           -> PreExp     acc aenv slix
           -> LiftedAcc  acc aenv' (Array sl e)
    sliceL sl (cvtA -> a) (cvtE -> slix)
      | avoidLifting
      , AvoidedAcc a'      <- a
      , AvoidedExp b slix' <- slix
      = AvoidedAcc
      $^ bind b
      $  Slice sl (sink b a') slix'
    sliceL sl a slix
      = cvtA
      $^ Alet a
      $^ Backpermute (IndexSlice sl (Proxy :: Proxy slix) (Shape avar0))
                     (Lam . Body $ IndexFull sl (weakenE1 (weakenA1 slix)) var0)
      $ avar0

    mapL :: forall sh e e'. (Elt e, Elt e', Shape sh)
         => PreFun    acc  aenv  (e -> e')
         -> acc            aenv  (Array sh e)
         -> LiftedAcc acc  aenv' (Array sh e')
    mapL (cvtF1 -> (f_l, f_a)) (cvtA -> a)
      | avoidLifting
      , Just (AvoidedFun b f) <- f_a
      , AvoidedAcc a'         <- a
      = AvoidedAcc
      $^ bind b
      $  Map f (sink b a')
      | avoidLifting
      , Just (AvoidedFun binds f) <- f_a
      = liftedAcc
      $^ bind binds
      $  Alet (sink binds $ lifted a)
      $  construct (segmentsC avar0)
      $^ Map (weakenA1 f) (valuesC avar0)
      | otherwise
      = liftedAcc
      $^ Alet (lifted a)
      $  construct (segmentsC avar0)
      $  weakenA1 f_l `apply` valuesC avar0


    zipWithL :: forall sh a b c. (Elt a, Elt b, Elt c, Shape sh)
             => PreFun     acc aenv  (a -> b -> c)
             -> acc            aenv  (Array sh a)
             -> acc            aenv  (Array sh b)
             -> LiftedAcc  acc aenv' (Array sh c)
    zipWithL (cvtF2 -> (f_l, f_a)) (cvtA -> a) (cvtA -> b)
      | avoidLifting
      , Just (AvoidedFun binds f) <- f_a
      , AvoidedAcc a'             <- a
      , AvoidedAcc b'             <- b
      = AvoidedAcc
      $^ bind binds
      $ ZipWith f (sink binds a') (sink binds b')
      | otherwise
      = liftedAcc
      $^ Alet (liftedZipC (lifted a) (lifted b))
      $^ Alet (unzip (valuesC avar0))
      $  construct (segmentsC avar1)
      $^ subApply2 (weakenA2 f_l) (fstA avar0) (sndA avar0)

    foldL :: forall sh e. (Elt e, Shape sh)
          => PreFun acc     aenv  (e -> e -> e)
          -> PreExp acc     aenv  e
          -> acc            aenv  (Array (sh:.Int) e)
          -> LiftedAcc  acc aenv' (Array sh e)
    foldL (cvtF2 -> (_, Just (AvoidedFun b1 f))) (cvtE -> AvoidedExp b2 z') (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a'    <- a
      = AvoidedAcc
      $^ bind b1
      $ Alet (sink b1 $^ bind b2 (Unit z'))
      $^ Fold (weakenA1 f) (the avar0) (weakenA1 $ sink b1 a')
      | AsSlice <- asSlice (Proxy :: Proxy sh)
      = liftedAcc
      $^ bind b1
      $  Alet (sink b1 (lifted a))
      $^ Alet (weakenA1 $ sink b1 $^ bind b2 $ Unit z')
      $^ Alet (isIrregularSegsC (segmentsC avar1))
      $^ Acond (the avar0) (inject
        -- The array is irregular
        $  Alet (makeFoldSegmentsC (segmentsC avar2))
        $  construct (sndA avar0)
        $^ FoldSeg (weakenA4 f) (the avar2) (valuesC avar3) (fstA avar0)
      ) (inject
        -- The array is regular
        $  Alet (regularSizeC (segmentsC avar2))
        $^ Alet (regularShapeC (segmentsC avar3))
        $^ Alet (inject $ Fold (weakenA5 f) (the avar3) $^ Reshape (indexSnoc (the avar0) (the avar1)) (valuesC avar4))
        $  construct (regularSegsC avar2 (unit . indexInit $ Shape avar0))
        $  flattenC avar0
        )
    foldL _ _ _
      = error $ nestedError "first or second" "fold"

    fold1L :: forall sh e. (Elt e, Shape sh)
           => PreFun acc  aenv  (e -> e -> e)
           -> acc            aenv  (Array (sh:.Int) e)
           -> LiftedAcc  acc aenv' (Array sh e)
    fold1L (cvtF2 -> (_, Just (AvoidedFun b1 f))) (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a'    <- a
      = AvoidedAcc
      $^ bind b1
      $  Fold1 f (sink b1 a')
      | AsSlice <- asSlice (Proxy :: Proxy sh)
      = liftedAcc
      $^ bind b1
      $  Alet (sink b1 (lifted a))
      $^ Alet (isIrregularSegsC (segmentsC avar0))
      $^ Acond (the avar0) (inject
        -- The array is irregular
        $  Alet (makeFoldSegmentsC (segmentsC avar1))
        $  construct (sndA avar0)
        $^ Fold1Seg (weakenA3 f) (valuesC avar2) (fstA avar0)
      ) (inject
        -- The array is regular
        $  Alet (weakenA2 (sink b1 (unitSize size)))
        $^ Alet (regularShapeC (segmentsC avar2))
        $^ Alet (inject $ Fold1 (weakenA4 f) $^ Reshape (indexSnoc (the avar0) (the avar1)) (valuesC avar3))
        $  construct (regularSegsC avar2 (unit . nonEmpty . indexInit $ the avar1))
        $  flattenC avar0
        )
    fold1L _ _
      = error $ nestedError "first or second" "fold1"

    foldSegL :: forall sh e i. (Elt e, Shape sh, IsIntegral i, Elt i)
             => PreFun acc aenv (e -> e -> e)
             -> PreExp acc aenv e
             -> acc            aenv (Array (sh:.Int) e)
             -> acc            aenv (Sugar.Segments i)
             -> LiftedAcc  acc aenv' (Array (sh:.Int) e)
    foldSegL (cvtF2 -> (_, Just (AvoidedFun b1 f))) (cvtE -> AvoidedExp b2 z) (cvtA -> a) (cvtA -> segs)
      | avoidLifting
      , AvoidedAcc a'    <- a
      , AvoidedAcc segs' <- segs
      = AvoidedAcc
      $^ bind b1
      $  Alet (sink b1 . inject . bind b2 $ Unit z)
      $^ FoldSeg (weakenA1 f) (the avar0) (weakenA1 $ sink b1 a') (weakenA1 $ sink b1 segs')
      | AsSlice <- asSlice (Proxy :: Proxy sh)
      = liftedAcc
      $^ bind b1
      $  Alet (sink b1 (lifted a))
      $^ Alet (weakenA1 . sink b1 $ lifted segs)
      $^ Alet (makeFoldSegSegmentsC (segmentsC avar1) avar0)
      $  construct (fstA avar0)
      $^ Alet (weakenA3 . sink b1 . inject . bind b2 $ Unit z)
      $^ FoldSeg (weakenA4 f) (the avar0) (valuesC avar3) (sndA avar1)
    foldSegL _ _ _ _
      = error $ nestedError "first or second" "foldSeg"

    fold1SegL :: forall sh e i. (Elt e, Shape sh, IsIntegral i, Elt i)
              => PreFun acc aenv (e -> e -> e)
              -> acc            aenv (Array (sh:.Int) e)
              -> acc            aenv (Sugar.Segments i)
              -> LiftedAcc  acc aenv' (Array (sh:.Int) e)
    fold1SegL (cvtF2 -> (_, Just (AvoidedFun b1 f))) (cvtA -> a) (cvtA -> segs)
      | avoidLifting
      , AvoidedAcc a'    <- a
      , AvoidedAcc segs' <- segs
      = AvoidedAcc
      $^ bind b1
      $  Fold1Seg f (sink b1 a') (sink b1 segs')
      | AsSlice <- asSlice (Proxy :: Proxy sh)
      = liftedAcc
      $^ bind b1
      $  Alet (sink b1 (lifted a))
      $^ Alet (weakenA1 . sink b1 $ lifted segs)
      $^ Alet (makeFoldSegSegmentsC (segmentsC avar1) avar0)
      $  construct (fstA avar0)
      $^ Fold1Seg (weakenA3 f) (valuesC avar2) (sndA avar0)
    fold1SegL _ _ _
      = error $ nestedError "first" "foldSeg"

    scanl1L :: forall e. Elt e
               => PreFun acc  aenv  (e -> e -> e)
               -> acc            aenv  (Vector e)
               -> LiftedAcc  acc aenv' (Vector e)
    scanl1L (cvtF2 -> (_, Just (AvoidedFun b1 f))) (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a' <- a
      = AvoidedAcc
      $^ bind b1
      $  Scanl1 f (sink b1 a')
      | otherwise
      = liftedAcc
      $^ bind b1
      $  extract $ scanl1Lift f (sink b1 (lifted a))
    scanl1L _ _
      = error $ nestedError "first" "scanl1"

    scanlL :: forall e. Elt e
               => PreFun acc  aenv  (e -> e -> e)
               -> PreExp acc  aenv  e
               -> acc            aenv  (Vector e)
               -> LiftedAcc  acc aenv' (Vector e)
    scanlL (cvtF2 -> (_, Just (AvoidedFun b1 f))) (cvtE -> AvoidedExp b2 z) (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a' <- a
      = AvoidedAcc
      $^ bind b1
      $  Alet (sink b1 $ inject $ bind b2 $ Unit z)
      $^ Scanl (weakenA1 f) (the avar0) (weakenA1 $ sink b1 a')
      | otherwise
      = liftedAcc
      $^ bind b1
      $  Alet (sink b1 $ inject $ bind b2 $ Unit z)
      $  scanlLift (weakenA1 f) (the avar0) (weakenA1 $ sink b1 $ lifted a)
    scanlL _ _ _
      = error $ nestedError "first or second" "scanl"

    scanl'L :: forall e. Elt e
               => PreFun acc  aenv  (e -> e -> e)
               -> PreExp acc  aenv  e
               -> acc            aenv  (Vector e)
               -> LiftedAcc  acc aenv' (Vector e, Scalar e)
    scanl'L (cvtF2 -> (_, Just (AvoidedFun b1 f))) (cvtE -> AvoidedExp b2 z) (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a' <- a
      = AvoidedAcc
      $^ bind b1
      $  Alet (sink b1 $ inject $ bind b2 $ Unit z)
      $^ Scanl' (weakenA1 f) (the avar0) (weakenA1 $ sink b1 a')
      | otherwise
      = liftedAcc
      $^ bind b1
      $  Alet (sink b1 $ lifted a)
      $^ Alet (S.map S.unindex1 `fromHOAS` shapesC (segmentsC avar0))
      $^ Alet (valuesC avar1)
      $^ Alet (weakenA3 $ sink b1 $ inject $ bind b2 $ Unit z)
      $^ Alet (valuesC $ scanlLift (weakenA4 f) (the avar0) avar3)
      $  fromHOAS3
            (\seg vec vec' ->
              let
                seg'        = S.map (+1) seg
                tails       = S.zipWith (+) seg . fst $ S.scanl' (+) 0 seg'
                sums        = S.backpermute (S.shape seg) (\ix -> S.index1 $ tails S.! ix) vec'

                offset      = S.scanl1 (+) seg
                inc         = S.scanl1 (+)
                            $ S.permute (+) (S.fill (S.index1 $ S.size vec + 1) 0)
                                          (\ix -> S.index1 $ offset S.! ix)
                                          (S.fill (S.shape seg) (1 :: S.Exp Int))

                body        = S.backpermute (S.shape vec)
                                          (\ix -> S.index1 $ S.unindex1 ix + inc S.! ix)
                                          vec'
              in S.Acc . S.Atuple
               $ SnocAtup (SnocAtup NilAtup (liftedArray (segmentsFromShapes (S.map S.index1 seg')) body))
                          (liftedScalar sums))
            avar3
            avar2
            avar0
    scanl'L _ _ _
      = error $ nestedError "first or second" "scanl"

    scanr1L :: forall e. Elt e
               => PreFun acc  aenv  (e -> e -> e)
               -> acc            aenv  (Vector e)
               -> LiftedAcc  acc aenv' (Vector e)
    scanr1L (cvtF2 -> (_, Just (AvoidedFun b1 f))) (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a' <- a
      = AvoidedAcc
      $^ bind b1
      $  Scanr1 f (sink b1 a')
      | otherwise
      = liftedAcc
      $^ bind b1
      $  extract $ scanr1Lift f (sink b1 (lifted a))
    scanr1L _ _
      = error $ nestedError "first" "scanr1"

    scanrL :: forall e. Elt e
               => PreFun acc  aenv  (e -> e -> e)
               -> PreExp acc  aenv  e
               -> acc            aenv  (Vector e)
               -> LiftedAcc  acc aenv' (Vector e)
    scanrL (cvtF2 -> (_, Just (AvoidedFun b1 f))) (cvtE -> AvoidedExp b2 z) (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a' <- a
      = AvoidedAcc
      $^ bind b1
      $  Alet (sink b1 $ inject $ bind b2 $ Unit z)
      $^ Scanr (weakenA1 f) (the avar0) (weakenA1 $ sink b1 a')
      | otherwise
      = liftedAcc
      $^ bind b1
      $  Alet (sink b1 $ inject $ bind b2 $ Unit z)
      $  scanrLift (weakenA1 f) (the avar0) (weakenA1 $ sink b1 (lifted a))
    scanrL _ _ _
      = error $ nestedError "first or second" "scanr"

    scanr'L :: forall e. Elt e
               => PreFun acc  aenv  (e -> e -> e)
               -> PreExp acc  aenv  e
               -> acc            aenv  (Vector e)
               -> LiftedAcc  acc aenv' (Vector e, Scalar e)
    scanr'L (cvtF2 -> (_, Just (AvoidedFun b1 f))) (cvtE -> AvoidedExp b2 z) (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a' <- a
      = AvoidedAcc
      $^ bind b1
      $  Alet (sink b1 $ inject $ bind b2 $ Unit z)
      $^ Scanr' (weakenA1 f) (the avar0) (weakenA1 $ sink b1 a')
      | otherwise
      = liftedAcc
      $^ bind b1
      $  Alet (sink b1 (lifted a))
      $^ Alet (segmentsC avar0)
      $^ Alet (valuesC avar1)
      $^ Alet (weakenA3 $ sink b1 $ inject $ bind b2 $ Unit z)
      $^ Alet (valuesC $ scanrLift (weakenA4 f) (the avar0) avar3)
      $  fromHOAS3
            (\seg vec vec' ->
              let
                -- reduction values
                seg'        = S.map (+1) $ S.map S.unindex1 (shapes seg)
                heads       = P.fst $ S.scanl' (+) 0 seg'
                sums        = S.backpermute (S.shape (shapes seg)) (\ix -> S.index1 $ heads S.! ix) vec'

                -- body segments
                inc         = S.scanl1 (+) $ mkHeadFlags seg
                body        = S.backpermute (S.shape vec)
                                            (\ix -> S.index1 $ S.unindex1 ix + inc S.! ix)
                                            vec'
              in S.Acc . S.Atuple
               $ SnocAtup (SnocAtup NilAtup (liftedArray (segmentsFromShapes (S.map S.index1 seg')) body))
                          (liftedScalar sums))
            avar3
            avar2
            avar0
    scanr'L _ _ _
      = error $ nestedError "first or second" "scanr'"

    backpermuteL :: (Shape sh, Shape sh', Elt e)
                 => PreExp acc  aenv  sh'
                 -> PreFun acc  aenv  (sh' -> sh)
                 -> acc            aenv  (Array sh e)
                 -> LiftedAcc  acc aenv' (Array sh' e)
    backpermuteL (cvtE -> sh) (cvtF1 -> (f_l, f_a)) (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a' <- a
      , AvoidedExp b1 sh' <- sh
      , Just (AvoidedFun b2 f) <- f_a
      =  AvoidedAcc
      $^ bind b2
      $  Alet (sink b2 $^ bind b1 (Unit sh'))
      $^ Backpermute (the avar0) (weakenA1 f) (weakenA1 $ sink b2 a')
      | otherwise
      =  liftedAcc
      $^ Alet (segmentsFromExp sh)
      $^ Alet (liftedBackpermutePreC avar0)
      $  construct avar1
      $  liftedBackpermuteC (atup (fstA avar0) (inject $ weakenA2 f_l `subApply` sndA avar0))
                            (weakenA2 . lifted $ a)

    permuteL :: (Shape sh, Shape sh', Elt e)
             => PreFun acc  aenv  (e -> e -> e)
             -> acc            aenv  (Array sh' e)
             -> PreFun acc  aenv  (sh -> sh')
             -> acc            aenv  (Array sh  e)
             -> LiftedAcc  acc aenv' (Array sh' e)
    permuteL (avoidFun -> Avoided (b1, comb))
             (cvtA -> AvoidedAcc defs)
             (avoidFun . sink b1 -> Avoided (b2, p))
             (cvtA -> AvoidedAcc a)
      | avoidLifting
      , ExtendContext ctx1 b1' <- liftExtend vectAcc strength b1 ctx size
      , ExtendContext ctx2 b2' <- liftExtend vectAcc strength b2 ctx1 (sink b1' $ size)
      , Just comb' <- rebuildToLift ctx1 comb
      , Just p'   <- rebuildToLift ctx2 p
      =  AvoidedAcc
      $^ bind b1'
      $  bind b2'
      $  Permute (sink b2' comb') (sink b2' . sink b1' $ defs) p' (sink b2' . sink b1' $ a)

    -- Lifted version
    permuteL (cvtF2 -> (_, Just (AvoidedFun b comb))) (cvtA -> defs) (cvtF1 -> (p_l,p_a)) (cvtA -> a)
      =  trace "permuteL" ("Lifting permute: " ++ show (isJust p_a))
      $  liftedAcc
      $^ bind b
      $  Alet (sink b $ lifted defs)
      $^ Alet (weakenA1 . sink b $ lifted a)
      $  let init     = avar0
             defaults = avar1
             shapes   = segmentsC init
             shapes'  = segmentsC defaults
             enums    = inject . Map (fun1 (Prj tupIx0)) . enumSegC $ shapes
             ixs      = weakenA2 (sink b p_l) `subApply` enums
             ixs'     = asOffsetsOfC (construct shapes $^ ixs) shapes'
             vals     = Permute (weakenA2 $ comb)
                                (valuesC defaults)
                                (fun1 (ixs' `Index`))
                                (valuesC init)
          in construct shapes' $^ vals

    permuteL _ _ _ _
      = error $ nestedError "first" "permute"

    stencilL :: (Elt e, Elt e', Stencil sh e stencil)
             => PreFun acc aenv (stencil -> e')
             -> Boundary                (EltRepr e)
             -> acc            aenv (Array sh e)
             -> LiftedAcc  acc aenv' (Array sh e')
    stencilL (cvtF1 -> (_, Just (AvoidedFun b1 f))) b (cvtA -> AvoidedAcc a)
      = AvoidedAcc
      $^ bind b1
      $  Stencil f b (sink b1 a)
    stencilL _                                      _ _
      = error $ "Disallowed nested parallelism: Stencil operations must reside at the top level of "
             ++ "the program nesting and the stencil function contain no nested parallelism."

    stencil2L :: (Elt e', Stencil sh e2 stencil2, Stencil sh e1 stencil1)
              => PreFun acc aenv (stencil1 ->
                                          stencil2 -> e')
              -> Boundary                (EltRepr e1)
              -> acc            aenv (Array sh e1)
              -> Boundary                (EltRepr e2)
              -> acc            aenv (Array sh e2)
              -> LiftedAcc  acc aenv' (Array sh e')
    stencil2L (cvtF2 -> (_, Just (AvoidedFun binds f)))
              b1
              (cvtA -> AvoidedAcc a1)
              b2
              (cvtA -> AvoidedAcc a2)
      = AvoidedAcc
      $^ bind binds
      $  Stencil2 f b1 (sink binds a1) b2 (sink binds a2)
    stencil2L _                                 _  _  _  _
      = error $ "Disallowed nested parallelism: Stencil operations must reside at the top level of "
             ++ "parallel nesting and the supplied stencil function contain no nested parallelism."

    scanl1Lift :: forall aenv e. Elt e
               => PreFun acc aenv (e -> e -> e)
               -> acc aenv (Nested (Array DIM1 e))
               -> acc aenv (Nested (Array DIM1 e))
    scanl1Lift f a
      = inject
      $  Alet a
      $  construct (segmentsC avar0)
      $  sndA
      $  unzip
      $^ Scanl1 (weakenA1 $ segmented f)
      $  let
           flags :: forall aenv e. Elt e => acc (aenv, Nested (Array DIM1 e)) (Vector Int)
           flags = fromHOAS mkHeadFlags (segmentsC avar0)
         in fromHOAS2 S.zip flags (valuesC avar0)

    scanlLift :: forall aenv e. Elt e
              => PreFun acc aenv (e -> e -> e)
              -> PreExp acc aenv e
              -> acc            aenv (Nested (Array DIM1 e))
              -> acc            aenv (Nested (Array DIM1 e))
    scanlLift f z a
      =  scanl1Lift f
      $^ Alet a
      $^ Alet (segmentsC avar0)
      $^ Alet (valuesC avar1)
      $^ Alet (weakenA3 $ inject $ Unit z)
      $  fromHOAS3
          (\seg vec z ->
             let
              shs'        = S.map (S.ilift1 (+1)) (shapes seg)
              offs'       = S.generate (S.shape shs') (\ix -> (offsets seg S.! ix) + S.shapeSize ix)
              seg'        = irregularSegs (totalSize seg + S.size shs') offs' shs'
              vec'        = S.permute const
                                      (S.fill (S.index1 $ S.size vec + S.size shs') (S.the z))
                                      (\ix -> S.index1 $ S.unindex1 ix + inc S.! ix)
                                      vec
              flags       = mkHeadFlags seg
              inc         = S.scanl1 (+) flags
             in liftedArray seg' vec')
          avar2
          avar1
          avar0

    scanr1Lift :: forall aenv e. Elt e
               => PreFun acc aenv (e -> e -> e)
               -> acc aenv (Nested (Array DIM1 e))
               -> acc aenv (Nested (Array DIM1 e))
    scanr1Lift f a
      = inject
      $  Alet a
      $  construct (segmentsC avar0)
      $  sndA
      $  unzip
      $^ Scanr1 (weakenA1 $ segmented f)
      $  let
           flags :: forall aenv e. Elt e => acc (aenv, Nested (Array DIM1 e)) (Vector Int)
           flags = fromHOAS mkTailFlags (segmentsC avar0)
         in fromHOAS2 S.zip flags (valuesC avar0)

    scanrLift :: forall aenv e. Elt e
              => PreFun acc aenv (e -> e -> e)
              -> PreExp acc aenv e
              -> acc            aenv (Nested (Array DIM1 e))
              -> acc            aenv (Nested (Array DIM1 e))
    scanrLift f z a
      =  scanr1Lift f
      $^ Alet a
      $^ Alet (segmentsC avar0)
      $^ Alet (valuesC avar1)
      $^ Alet (weakenA3 $ inject $ Unit z)
      $  fromHOAS3
          (\seg vec z ->
             let
              shs'        = S.map (S.ilift1 (+1)) (shapes seg)
              offs'       = S.generate (S.shape shs') (\ix -> (offsets seg S.! ix) + S.shapeSize ix)
              seg'        = irregularSegs (totalSize seg + S.size shs') offs' shs'
              vec'        = S.permute const
                                      (S.fill (S.index1 $ S.size vec + S.size shs') (S.the z))
                                      (\ix -> S.index1 $ S.unindex1 ix + inc S.! ix - 1)
                                      vec
              flags       = mkHeadFlags seg
              inc         = S.scanl1 (+) flags
             in liftedArray seg' vec')
          avar2
          avar1
          avar0

    segmentsFromExp :: forall sh. Shape sh
                    => LiftedExp acc () aenv' sh
                    -> acc aenv' (Segments sh)
    segmentsFromExp (AvoidedExp b sh) = inject
                                      $  Alet (unitSize size)
                                      $^ Alet (weakenA1 . inject . bind b $ Unit sh)
                                      $  regularSegsC avar1 avar0
    segmentsFromExp (LiftedExp sh)    = asSegmentsC sh

    avoidF :: PreFun acc aenv f
           -> AvoidFun acc () aenv' f
    avoidF (avoidFun -> Avoided (env, f))
      | ExtendContext ctx' env' <- liftExtend vectAcc Conservative env ctx size
      = case rebuildToLift ctx' f of
          Just f' -> Avoided (env', f')
          _       -> trace "liftPreOpenAcc" "Function contains no nested parallelism, but depends on lifted variables"
                   $ Unavoided
    avoidF _                              = Unavoided

    avoidE :: PreExp acc aenv f
           -> AvoidExp acc () aenv' f
    avoidE (avoidExp -> Avoided (env, e)) | ExtendContext ctx' env' <- liftExtend vectAcc Conservative env ctx size
                                          , Just e' <- rebuildToLift ctx' e
                                          = Avoided (env', e')
    avoidE _                              = Unavoided

type Vector' e = (Scalar Bool, Scalar Int, Scalar e, Vector e)

-- |Performs the lifting transform on a given scalar expression.
--
liftExp :: forall acc env env' aenv aenv' e. Kit acc
        => VectoriseAcc acc
        -> Strength
        -> Context env aenv env' aenv'
        -> Size acc aenv' Int
        -> PreOpenExp acc env       aenv  e
        -> PreOpenAcc acc aenv' (Vector' e)
liftExp vectAcc strength ctx size exp
  = case exp of
      Let bnd body              -> letL bnd body
      Var ix                    -> varL ctx ix id id
      Const c                   -> unlifted (Const c)
      Tuple tup                 -> liftTuple vectAcc strength ctx size tup
      Prj ix t                  -> mapVector' (Prj ix) (cvtE t)
      IndexNil                  -> unlifted IndexNil
      IndexAny                  -> unlifted IndexAny
      IndexCons sh sz           -> zipWithVector' IndexCons (cvtE sh) (cvtE sz)
      IndexHead sh              -> mapVector' IndexHead (cvtE sh)
      IndexTail sh              -> mapVector' IndexTail (cvtE sh)
      IndexTrans sh             -> mapVector' IndexTrans (cvtE sh)
      IndexSlice x ix sh        -> mapVector' (IndexSlice x ix) (cvtE sh)
      IndexFull x ix sl         -> zipWithVector' (IndexFull x) (cvtE ix) (cvtE sl)
      ToIndex sh ix             -> zipWithVector' ToIndex (cvtE sh) (cvtE ix)
      FromIndex sh ix           -> zipWithVector' FromIndex (cvtE sh) (cvtE ix)
      ToSlice x sh i            -> zipWithVector' (ToSlice x) (cvtE sh) (cvtE i)
      Cond p t e                -> condL p t e
      While p it i              -> whileL p it i
      PrimConst c               -> unlifted (PrimConst c)
      PrimApp f x               -> mapVector' (PrimApp f) (cvtE x)
      Index a sh                -> indexL a sh
      LinearIndex a i           -> linearIndexL a i
      Shape a                   -> shapeL a
      ShapeSize sh              -> mapVector' ShapeSize (cvtE sh)
      Intersect s t             -> zipWithVector' Intersect (cvtE s) (cvtE t)
      Union s t                 -> zipWithVector' Union (cvtE s) (cvtE t)
      Foreign ff f e            -> mapVector' (Foreign ff f) (cvtE e)
  where
    avoidLifting :: Bool
    avoidLifting | Conservative <- strength = True
                 | HoistOnly    <- strength = True
                 | otherwise                = False

    lifted :: forall t. Arrays t => LiftedAcc acc aenv' t -> acc aenv' (Nested t)
    lifted (AvoidedAcc a)   = replicateA a size
    lifted (LiftedAcc l)    = l

    unlifted :: forall a. Elt a => PreExp acc aenv' a -> PreOpenAcc acc aenv' (Vector' a)
    unlifted = extract . replicatedC (unitSize size) . unit

    mapVector' :: forall aenv' a b. (Elt a, Elt b)
               => (forall env. PreOpenExp acc env aenv' a -> PreOpenExp acc env aenv' b)
               -> acc aenv' (Vector' a)
               -> PreOpenAcc acc aenv' (Vector' b)
    mapVector' f v
      = Alet v
      $^ Alet (inject $ Aprj tupIx3 avar0)
      $^ Acond (the avar0) (
          asVector'C
        $^ Map (weakenA2 $ fun1 f) (inject $ Aprj tupIx0 avar1)
      ) (
          inject
        $ Alet (inject $ Aprj tupIx1 avar1)
        $ replicatedC (inject $ Aprj tupIx2 avar2)
        $^ Unit (weakenA3 (fun1 f) `subApplyE` the avar0)
      )

    zipWithVector' :: (Elt a, Elt b, Elt c)
                   => (forall env aenv. PreOpenExp acc env aenv a -> PreOpenExp acc env aenv b -> PreOpenExp acc env aenv c)
                   -> acc aenv' (Vector' a)
                   -> acc aenv' (Vector' b)
                   -> PreOpenAcc acc aenv' (Vector' c)
    zipWithVector' f v1 v2
      = mapVector' (\ab -> Let ab (f (Prj tupIx1 var0) (Prj tupIx0 var0))) (zip v1 v2)
      where
        -- uncurry :: (Elt a, Elt b, Elt c)
        --         => PreFun acc aenv (a -> b -> c)
        --         -> PreFun acc aenv ((a,b) -> c)
        -- uncurry f
        --   = Lam . Body
        --   $ Let (Prj tupIx1 var0)
        --   $ Let (Prj tupIx0 var1)
        --   $ subApplyE2 (weakenE3 f) var1 var0
        -- uncurry _ = error "Absurd"

        zip = fromHOAS2 $ \v1 v2 ->
                withVector' v1 (\c e -> withVector' v2
                                 (\c' e' -> replicated (c `min` c') (S.lift (e, e')))
                                 (asVector' . S.map (S.lift . (e,))))
                               (\v1' -> asVector' $ S.zip v1' (asVector v2))

    cvtE :: forall e. PreOpenExp acc env aenv e
         -> acc aenv' (Vector' e)
    cvtE exp' = inject $ liftExp vectAcc strength ctx size exp'

    cvtA :: forall sh' e'.
            (Elt e', Shape sh')
         => acc aenv (Array sh' e')
         -> LiftedAcc acc aenv' (Array sh' e')
    cvtA a | EmbedContext ctx' wk <- embedContext ctx
           = vectAcc strength ctx' size (weaken wk a)

    cvtF1 :: forall a b. PreOpenFun acc env aenv (a -> b)
          -> PreOpenAfun acc aenv' (Vector a -> Vector b)
    cvtF1 (Lam (Body f)) = rewrap . Alam . Abody
                         $ inject
                         $ liftExp vectAcc strength (PushLExpC ctx) (sizeOfVector' avar0) f
      where
        rewrap :: PreOpenAfun acc aenv' (Vector' a -> Vector' b) -> PreOpenAfun acc aenv' (Vector a -> Vector b)
        rewrap f = Alam . Abody . asVectorC $ weakenA1 f `apply` asVector'C avar0
    cvtF1 _              = $internalError "liftExp" "Inconsistent valuation"

    -- Lifted versions of operations
    -- ==============================

    varL :: forall env aenv env'' aenv''. Elt e
         => Context env aenv env'' aenv''
         -> Idx env e
         -> (forall e. Idx env''  e -> Idx env'  e)
         -> (forall a. Idx aenv'' a -> Idx aenv' a)
         -> PreOpenAcc acc aenv' (Vector' e)
    varL (PushLExpC _) ZeroIdx      _    cvtA = Avar (cvtA ZeroIdx)
    --varL (PushExpC _)  ZeroIdx      cvtE _    = replicateE size (Var $ cvtE ZeroIdx)
    varL (PushExpC d)  (SuccIdx ix) cvtE cvtA = varL d ix (cvtE . SuccIdx) cvtA
    varL (PushLExpC d) (SuccIdx ix) cvtE cvtA = varL d ix cvtE             (cvtA . SuccIdx)
    varL (PushAccC d)  ix           cvtE cvtA = varL d ix cvtE             (cvtA . SuccIdx)
    varL (PushLAccC d) ix           cvtE cvtA = varL d ix cvtE             (cvtA . SuccIdx)
    varL _             _            _    _    = $internalError "liftExp" "Inconsistent valuation"

    letL :: forall bnd_t. (Elt e, Elt bnd_t)
         => PreOpenExp acc env          aenv  bnd_t
         -> PreOpenExp acc (env, bnd_t) aenv  e
         -> PreOpenAcc acc aenv' (Vector' e)
    letL bnd body = Alet bnd' (inject body')
      where
        bnd'  = cvtE bnd

        body' :: PreOpenAcc acc (aenv', Vector' bnd_t) (Vector' e)
        body' = liftExp vectAcc strength (PushLExpC ctx) (weakenA1 size) body

    condL :: Elt e
          => PreOpenExp acc env     aenv  Bool
          -> PreOpenExp acc env     aenv  e
          -> PreOpenExp acc env     aenv  e
          -> PreOpenAcc acc aenv' (Vector' e)
    condL p t e = extract . asVector'C $^ ZipWith (fun2 decide) (asVectorC (cvtE p)) (inject $ ZipWith (fun2 tup) (asVectorC (cvtE t)) (asVectorC (cvtE e)))
      where
        decide p' ab = Cond p' (Prj (SuccTupIdx ZeroTupIdx) ab) (Prj ZeroTupIdx ab)

    -- The lifted while is non-trivial. Here is an overview. We use '^' to denote lifting.
    --
    -- @
    -- (while p it i)^
    --   = fst $ awhile (\(_,flags) -> any flags)
    --                  (\(values, flags) ->
    --                     let
    --                       values'  = zip (it^ values) flags
    --                       values'' = zipWith (\(v', f) v -> if f then v' else v) values' values
    --                       flags'   = p^ values''
    --                     in (values'', flags')
    --                  )
    --                  (i^, replicate sh False)
    -- @
    --
    whileL :: Elt e
           => PreOpenFun acc env     aenv  (e -> Bool)
           -> PreOpenFun acc env     aenv  (e -> e)
           -> PreOpenExp acc env     aenv  e
           -> PreOpenAcc acc aenv' (Vector' e)
    whileL p it i = extract . asVector'C $^ Aprj (SuccTupIdx ZeroTupIdx) (inject $ Awhile p' it' i')
      where
        p'  :: PreOpenAfun acc aenv' ((Vector e, Vector Bool) -> Scalar Bool)
        p'  = Alam $ Abody $ let
                flags     = sndA avar0
                any     f = inject $ Fold or (Const False) f
                or        = fun2 (PrimApp PrimLOr S.$$ tup)
              in any flags

        it' :: PreOpenAfun acc aenv' ((Vector e, Vector Bool) -> (Vector e, Vector Bool))
        it' = Alam $ Abody $ let
                values  = fstA avar0
                flags   = sndA avar0
                values' = inject $ ZipWith (fun2 tup)
                                           (inject $ weakenA1 (cvtF1 it) `subApply` values)
                                           flags
                values'' = inject $ ZipWith (Lam $ Lam $ Body $ Cond (sndE $ var1)
                                                                     (fstE $ var1)
                                                                     var0)
                                            values'
                                            values
                flags'   = inject $ (weakenA2) (cvtF1 p) `subApply` avar0
              in inject $ Alet values'' (atup avar0 flags')


        i'  :: acc aenv' (Vector e, Vector Bool)
        i'  = asVectorC (cvtE i) `atup` inject (replicateE size (Const True))

    indexL :: forall sh'. (Elt e, Shape sh')
           => acc            aenv  (Array sh' e)
           -> PreOpenExp acc env      aenv  sh'
           -> PreOpenAcc acc aenv' (Vector' e)
    indexL (cvtA -> a) (cvtE -> ix)
      | avoidLifting
      , AvoidedAcc a' <- a
      =  Alet a'
      $^ mapVector' (Index avar0) (weakenA1 ix)
      | otherwise
      =  extract
      $  liftedIndexC (lifted a) ix

    linearIndexL :: forall sh'. (Elt e, Shape sh')
                 => acc            aenv  (Array sh' e)
                 -> PreOpenExp acc env     aenv  Int
                 -> PreOpenAcc acc aenv' (Vector' e)
    linearIndexL (cvtA -> a) (cvtE -> ix)
      | avoidLifting
      , AvoidedAcc a' <- a
      =  Alet a'
      $^ mapVector' (LinearIndex avar0) (weakenA1 ix)
      | otherwise
      = extract $
        liftedLinearIndexC (lifted a) ix

    shapeL :: forall e'. (Shape e, Elt e')
           => acc            aenv  (Array e e')
           -> PreOpenAcc acc aenv' (Vector' e)
    shapeL (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a' <- a
      =  Alet a'
      $ replicatedC (unitSize $ weakenA1 size) (unit $ Shape avar0)
      | otherwise
      = extract
      $ shapes'C (segmentsC (lifted a))

    zipWith3 :: forall aenv a b c d. (Elt a, Elt b, Elt c, Elt d)
             => PreFun acc aenv (a -> b -> c -> d)
             -> acc aenv (Vector a)
             -> acc aenv (Vector b)
             -> acc aenv (Vector c)
             -> PreOpenAcc acc aenv (Vector d)
    zipWith3 f a b c = ZipWith (uncurry f) (inject $ ZipWith (fun2 tup) a b) c
      where
        uncurry :: PreFun acc aenv (a -> b -> c -> d)
                -> PreFun acc aenv ((a,b) -> c -> d)
        uncurry (Lam (Lam (Lam (Body f))))
          = Lam . Lam . Body
          $ Let (Prj tupIx1 var1)
          $ Let (Prj tupIx0 var2)
          $ weakenE ixt f
        uncurry _ = error "Absurd"

        ixt :: ((((),a),b),c) :> (((((),(a,b)),c),a),b)
        ixt ZeroIdx                          = SuccIdx . SuccIdx $ ZeroIdx
        ixt (SuccIdx ZeroIdx)                = ZeroIdx
        ixt (SuccIdx (SuccIdx ZeroIdx))      = SuccIdx ZeroIdx
        ixt (SuccIdx (SuccIdx (SuccIdx ix))) = SuccIdx . SuccIdx . SuccIdx . SuccIdx $ ix



type family VectorsOfTupleRepr t
type instance VectorsOfTupleRepr ()    = ()
type instance VectorsOfTupleRepr (t,e) = (VectorsOfTupleRepr t, Vector e)

type family ExpandEnv env env'
type instance ExpandEnv env ()        = env
type instance ExpandEnv env (env', t) = ExpandEnv (env, t) env'

type TupleEnv aenv t = ExpandEnv aenv (VectorsOfTupleRepr (TupleRepr t))

-- |Perform the lifting transform over a scalar tuple. We lift it as so:
--
-- @
-- (a1, a2,..., aN)^ =
--   let a1' = a1^
--       a2' = a2^
--       ...
--       aN' = aN^
--   in generate (\ix -> (a1' ! ix, a2' ! ix,..., aN' ! ix))
-- @
--
-- RCE: Ideally we would like to do this by lifting the tuple into a tuple of arrays.
-- Unfortunately this can't be done because the type system us unable to recognise that the
-- lifted tuple is an instance of IsTuple.
liftTuple :: forall acc env aenv env' aenv' e.
             (Elt e, Kit acc, IsTuple e)
          => VectoriseAcc acc
          -> Strength
          -> Context env aenv env' aenv'
          -> Size acc aenv' Int
          -> Tuple (PreOpenExp acc env aenv) (TupleRepr e)
          -> PreOpenAcc acc aenv' (Vector' e)
liftTuple vectAcc strength ctx size t = extract . asVector'C $^ cvtT t (liftExp vectAcc strength ctx size) gen size
  where
    cvtT :: forall t aenv'.
            Tuple (PreOpenExp acc env aenv) t
         -> (forall e. PreOpenExp acc env aenv e -> PreOpenAcc acc aenv' (Vector' e))
         -> (Size acc (ExpandEnv aenv' (VectorsOfTupleRepr t)) Int -> PreOpenAcc acc (ExpandEnv aenv' (VectorsOfTupleRepr t)) (Vector e))
         -> Size acc aenv' Int
         -> PreOpenAcc acc aenv'                                    (Vector e)
    cvtT NilTup        _    arr size = arr size
    cvtT(SnocTup t' e) lift arr size = Alet (asVectorC . inject $ lift e) (inject $ cvtT t' lift' arr (weakenA1 size))
      where
        lift' :: forall e e'. PreOpenExp acc env aenv e -> PreOpenAcc acc (aenv', Vector e') (Vector' e)
        lift' = weakenA1 . lift

    gen :: Size acc (TupleEnv aenv' e) Int -> PreOpenAcc acc (TupleEnv aenv' e) (Vector e)
    gen (Size b s) = Alet b $^ Generate (index1 s) (weakenA1 $ Lam (Body (Tuple t')))
      where
        t' :: Tuple (PreOpenExp acc ((),DIM1) (TupleEnv aenv' e)) (TupleRepr e)
        t' = weakenTup (ixt (undefined :: aenv') t) (mkTup t)
          where
            mkTup :: forall e c. Tuple c e
                  -> Tuple (PreOpenExp acc ((),DIM1) (VectorsOfTupleRepr e)) e
            mkTup NilTup          = NilTup
            mkTup (SnocTup t'' _) = SnocTup (weakenTup SuccIdx (mkTup t'')) e'
              where
                e' :: forall s e'. e ~ (s,e') => PreOpenExp acc ((),DIM1) (VectorsOfTupleRepr e) e'
                e' = Index avar0 var0

    weakenTup :: forall env aenv aenv' e. aenv :> aenv'
              -> Tuple (PreOpenExp acc env aenv) e
              -> Tuple (PreOpenExp acc env aenv') e
    weakenTup _ NilTup        = NilTup
    weakenTup v (SnocTup t e) = SnocTup (weakenTup v t) (weaken v e)

    tix :: forall t c env e. Tuple c t -> Idx env e -> Idx (ExpandEnv env (VectorsOfTupleRepr t)) e
    tix NilTup ix        = ix
    tix (SnocTup t (_:: c t')) ix = tix t ix'
      where
        ix' :: Idx (env, Vector t') e
        ix' = SuccIdx ix

    ixt :: forall t c env e.
           env {- dummy -}
        -> Tuple c t
        -> Idx (VectorsOfTupleRepr t) e
        -> Idx (ExpandEnv env (VectorsOfTupleRepr t)) e
    ixt _   (SnocTup NilTup _) ZeroIdx      = ZeroIdx
    ixt _   (SnocTup t      _) ZeroIdx      = tix t (ZeroIdx :: Idx (env, e) e)
    ixt _   (SnocTup t      _) (SuccIdx ix) = ixt env' t ix
      where
        env' :: forall s e'. t ~ (s,e') => (env, Vector e')
        env' = undefined -- dummy argumen
    ixt _   _                  _            = error "liftTuple: Inconsistent valuation"

data Avoid f acc env aenv e where
  Avoided :: (Extend acc aenv aenv', f acc env aenv' e) -> Avoid f acc env aenv e
  Unavoided :: Avoid f acc env aenv e

type AvoidExp = Avoid PreOpenExp
type AvoidFun = Avoid PreOpenFun

-- |Avoid vectorisation in the cases where it's not necessary, or impossible.
--
avoidExp :: forall acc aenv env e. Kit acc
         => PreOpenExp acc env aenv e
         -> AvoidExp acc env aenv e
avoidExp = cvtE
  where
    cvtE :: forall e env aenv. PreOpenExp acc env aenv e -> AvoidExp acc env aenv e
    cvtE exp =
      case exp of
        Let a b             -> letA a b
        Var ix              -> simple $ Var ix
        Const c             -> simple $ Const c
        Tuple tup           -> cvtT tup
        Prj tup e           -> Prj tup `cvtE1` e
        IndexNil            -> simple IndexNil
        IndexCons sh sz     -> cvtE2 IndexCons sh sz
        IndexHead sh        -> IndexHead `cvtE1` sh
        IndexTail sh        -> IndexTail `cvtE1` sh
        IndexTrans sh       -> IndexTrans `cvtE1` sh
        IndexAny            -> simple IndexAny
        IndexSlice x ix sh  -> IndexSlice x ix `cvtE1` sh
        IndexFull x ix sl   -> cvtE2 (IndexFull x) ix sl
        ToIndex sh ix       -> cvtE2 ToIndex sh ix
        FromIndex sh ix     -> cvtE2 FromIndex sh ix
        ToSlice x sh i      -> cvtE2 (ToSlice x) sh i
        Cond p t e          -> cvtE3 Cond p t e
        While p f x         -> whileA p f x
        PrimConst c         -> simple $ PrimConst c
        PrimApp f x         -> PrimApp f `cvtE1` x
        Index a sh          -> cvtA1E1 Index a sh
        LinearIndex a i     -> cvtA1E1 LinearIndex a i
        Shape a             -> Shape `cvtA1` a
        ShapeSize sh        -> ShapeSize `cvtE1` sh
        Intersect s t       -> cvtE2 Intersect s t
        Union s t           -> cvtE2 Union s t
        Foreign ff f e      -> Foreign ff f `cvtE1` e

    unavoided :: forall env aenv e f. String -> Avoid f acc env aenv e
    unavoided op = trace "avoidExp" ("Unable to avoid expression: " ++ op) $ Unavoided

    letA :: forall bnd_t e env aenv. (Elt e, Elt bnd_t)
         => PreOpenExp acc env          aenv bnd_t
         -> PreOpenExp acc (env, bnd_t) aenv e
         -> AvoidExp acc env          aenv e
    letA bnd body | Avoided (env , bnd' ) <- cvtE bnd
                  , Avoided (env', body') <- cvtE (sink env body)
                  = Avoided (append env env', Let (sink env' bnd') body')
                  | otherwise
                  = unavoided "let"

    whileA :: forall e env aenv. Elt e
           => PreOpenFun acc env aenv (e -> Bool)
           -> PreOpenFun acc env aenv (e -> e)
           -> PreOpenExp acc env aenv e
           -> AvoidExp acc env aenv e
    whileA (Lam (Body p)) (Lam (Body it)) i
      | Avoided (env0,  p') <- cvtE p
      , Avoided (env1, it') <- cvtE (sink env0 it)
      , Avoided (env2,  i') <- cvtE (sink env1 $ sink env0 i)
      = let
          p''  = (sink env2 . sink env1) p'
          it'' = sink env2 it'
        in Avoided (env0 `append` env1 `append` env2, While (Lam $ Body p'') (Lam $ Body it'') i')
    whileA _               _              _ = unavoided "while"


    simple :: forall e env aenv.
              PreOpenExp acc env aenv e
           -> AvoidExp      acc env aenv e
    simple e = Avoided (BaseEnv, e)

    cvtE1 :: forall e a env aenv. (forall env aenv. PreOpenExp acc env aenv a -> PreOpenExp acc env aenv e)
          -> PreOpenExp acc env aenv a
          -> AvoidExp acc env aenv e
    cvtE1 f (cvtE -> Avoided (env, a)) = Avoided (env, f a)
    cvtE1 _ e                          = unavoided (showPreExpOp e)

    cvtE2 :: forall e a b env aenv.
             (forall env aenv. PreOpenExp acc env aenv a -> PreOpenExp acc env aenv b -> PreOpenExp acc env aenv e)
          -> PreOpenExp acc env aenv a
          -> PreOpenExp acc env aenv b
          -> AvoidExp acc env aenv e
    cvtE2 f (cvtE -> Avoided (env, a)) (cvtE . sink env -> Avoided (env', b))
      = Avoided (env `append` env', f (sink env' a) b)
    cvtE2 f _                               _
      = unavoided (showPreExpOp (f undefined undefined))

    cvtE3 :: forall e a b c env aenv.
             (forall env aenv. PreOpenExp acc env aenv a -> PreOpenExp acc env aenv b -> PreOpenExp acc env aenv c -> PreOpenExp acc env aenv e)
          -> PreOpenExp acc env aenv a
          -> PreOpenExp acc env aenv b
          -> PreOpenExp acc env aenv c
          -> AvoidExp acc env aenv e
    cvtE3 f (cvtE                        -> Avoided (env, a))
            (cvtE . sink env             -> Avoided (env', b))
            (cvtE . sink env' . sink env -> Avoided (env'', c))
      = Avoided (env `append` env' `append` env'', f (sink env'' $ sink env' a) (sink env'' b) c)
    cvtE3 f _ _ _ = unavoided (showPreExpOp (f undefined undefined undefined))

    cvtT :: forall e env aenv. (IsTuple e, Elt e)
         => Tuple (PreOpenExp acc env aenv) (TupleRepr e)
         -> AvoidExp acc env aenv e
    cvtT t | Avoided (env, RebuildTup t) <- cvtT' t = Avoided (env, Tuple t)
      where
        cvtT' :: forall e.
                 Tuple (PreOpenExp acc env aenv) e
              -> Avoid RebuildTup acc env aenv e
        cvtT' NilTup        = Avoided (BaseEnv, (RebuildTup NilTup))
        cvtT' (SnocTup t e) | Avoided (env, RebuildTup t') <- cvtT' t
                            , Avoided (env', e') <- cvtE . sink env $ e
                            = Avoided (env `append` env', RebuildTup (SnocTup (unRTup $ sink env' $ RebuildTup t') e'))
        cvtT' _             = unavoided "tuple"
    cvtT _ = unavoided "tuple"

    cvtA1 :: forall a e env aenv. Arrays a
          => (forall env aenv. acc aenv a -> PreOpenExp acc env aenv e)
          -> acc aenv a
          -> AvoidExp acc env aenv e
    cvtA1 f a = Avoided (BaseEnv `PushEnv` a, f avar0)

    cvtA1E1 :: forall a b e env aenv. Arrays a
          => (forall env aenv. acc aenv a -> PreOpenExp acc env aenv b -> PreOpenExp acc env aenv e)
          -> acc aenv a
          -> PreOpenExp acc env aenv b
          -> AvoidExp acc env aenv e
    cvtA1E1 f a (cvtE -> Avoided (env, b))
      = Avoided (env `PushEnv` sink env a, f avar0 (weakenA1 b))
    cvtA1E1 f _ _
      = unavoided (showPreExpOp (f undefined undefined))

avoidFun :: Kit acc
         => PreOpenFun acc env aenv f
         -> AvoidFun acc env aenv f
avoidFun (Lam f)  | Avoided (env, f') <- avoidFun f
                  = Avoided (env, Lam f')
avoidFun (Body f) | Avoided (env, f') <- avoidExp f
                  = Avoided (env, Body f')
avoidFun _        = Unavoided

data ExtendContext acc aenv0' aenv1 where
  ExtendContext :: Context () aenv1 () aenv1'
                -> Extend acc aenv0' aenv1'
                -> ExtendContext acc aenv0' aenv1

liftExtend :: forall acc aenv0 aenv0' aenv1. Kit acc
           => VectoriseAcc acc
           -> Strength
           -> Extend acc aenv0 aenv1
           -> Context () aenv0 () aenv0'
           -> Size acc aenv0' Int
           -> ExtendContext acc aenv0' aenv1
liftExtend _ _ BaseEnv ctx _
  = ExtendContext ctx BaseEnv
liftExtend k strength (PushEnv env a) ctx size
  | ExtendContext ctx' env' <- liftExtend k strength env ctx size
  = case k strength ctx' (sink env' size) a of
      AvoidedAcc a' -> ExtendContext (PushAccC ctx')  (PushEnv env' a')
      LiftedAcc  a' -> ExtendContext (PushLAccC ctx') (PushEnv env' a')

data EmbedContext aenv aenv' = forall aenv''. EmbedContext (Context () aenv'' () aenv') (aenv :> aenv'')

embedContext :: Context env aenv env' aenv'
             -> EmbedContext aenv aenv'
embedContext EmptyC        = EmbedContext EmptyC id
embedContext (PushExpC d)  = embedContext d
embedContext (PushLExpC d) | EmbedContext d wk <- embedContext d
                           = EmbedContext (PushAccC d) (SuccIdx . wk)
embedContext (PushAccC d)  | EmbedContext d wk <- embedContext d
                           = EmbedContext (PushAccC d) (newTop wk)
embedContext (PushLAccC d) | EmbedContext d wk <- embedContext d
                           = EmbedContext (PushLAccC d) (newTop wk)

-- Nested operations.
-- ------------------

values :: forall sh e. (Shape sh, Elt e) => S.Acc (Nested (Array sh e)) -> S.Acc (Vector e)
values a = S.Acc $ S.Aprj ZeroTupIdx a

segments :: forall sh e. (Shape sh, Elt e) => S.Acc (Nested (Array sh e)) -> S.Acc (Segments sh)
segments a = S.Acc $ S.Aprj (SuccTupIdx ZeroTupIdx) a

liftedArray :: (Shape sh, Elt e) => S.Acc (Segments sh) -> S.Acc (Vector e) -> S.Acc (Nested (Array sh e))
liftedArray segs vals = S.Acc $ S.Atuple $ SnocAtup (SnocAtup NilAtup segs) vals

asAtuple :: forall a. (Arrays a, IsAtuple a) => S.Acc a -> Atuple S.Acc (TupleRepr a)
asAtuple a = tOA (prod (Proxy :: Proxy Arrays) (undefined :: a)) id
 where
   tOA :: forall t. ProdR Arrays t -> (forall e. TupleIdx t e -> TupleIdx (TupleRepr a) e) -> Atuple S.Acc t
   tOA ProdRunit     _   = NilAtup
   tOA (ProdRsnoc t) ixt = SnocAtup (tOA t (ixt . SuccTupIdx)) (S.Acc $ S.Aprj (ixt ZeroTupIdx) a)

replicate :: forall a. Arrays a => S.Exp Int -> S.Acc a -> S.Acc (Nested a)
replicate size a = case flavour (undefined :: a) of
                     ArraysFunit  -> S.Acc $ S.Atuple $ SnocAtup NilAtup $ S.unit size
                     ArraysFarray ->
                       let values = S.flatten $ S.replicate (S.lift (Z:.size:.All)) (S.flatten a)
                       in regular size (S.shape a) values
                     ArraysFtuple -> S.Acc $ S.Atuple $ replicateT (asAtuple a)
  where
    replicateT :: Atuple S.Acc t -> Atuple S.Acc (NestedTupleRepr t)
    replicateT NilAtup         = NilAtup
    replicateT (SnocAtup t a') = SnocAtup (replicateT t) (replicate size a')

-- A segmented replicate.
replicateSeg :: (Elt e, Shape sh) => S.Acc (Segments sh) -> S.Acc (Vector e) -> S.Acc (Vector e)
replicateSeg segs vals
  = generateSeg segs (\seg _ _ -> vals S.!! seg)

generateSeg :: forall e sh. (Elt e, Shape sh)
            => S.Acc (Segments sh)
            -> (S.Exp Int -> S.Exp sh -> S.Exp sh -> S.Exp e)
            -> S.Acc (Vector e)
generateSeg segs f = S.map (\(S.unlift -> (seg,sh,i)) -> f seg sh (S.fromIndex sh i)) domain
  where
    offs  = offsets segs

    -- For irregular segments
    negs  = S.fill (S.index1 $ totalSize segs) (S.tup3 (-1::S.Exp Int,S.ignore,-1::S.Exp Int) :: S.Exp (Int, sh, Int)) --Start with all -1s
    heads = S.permute combine negs (S.index1 . (offs S.!)) (S.zip3 offs (shapes segs) (S.fill (S.shape offs) 0))

    -- For regular
    squareDomain :: S.Exp Int -> S.Exp sh -> S.Acc (Array DIM2 (Int, sh, Int))
    squareDomain sz sh = S.generate (S.index2 sz (S.shapeSize sh)) (f . S.unindex2)
      where
        f (S.unlift -> (seg,i)) = S.tup3 (seg, sh, i)

    domain = withSegs segs (S.scanl1 (\a b -> dead b S.? (inc a, b)) heads)
                           (\sz sh -> S.flatten (squareDomain sz sh))

    combine :: S.Exp (Int,sh,Int) -> S.Exp (Int,sh,Int) -> S.Exp (Int,sh,Int)
    combine (S.untup3 -> (seg,sh,i)) (S.untup3 -> (seg',sh',i')) =
      S.shapeSize sh S.>* S.shapeSize sh' S.? ( S.lift (seg,sh,i) , S.lift (seg',sh',i') )

    dead :: S.Exp (Int,sh,Int) -> S.Exp Bool
    dead (S.untup3 -> (_,_,i)) = i S.==* -1

    inc :: S.Exp (Int,sh,Int) -> S.Exp (Int,sh,Int)
    inc (S.untup3 -> (seg,sh,i)) = S.lift (seg,sh,i+1)

enumSeg :: Shape sh => S.Acc (Segments sh) -> S.Acc (Vector (Int, sh, sh))
enumSeg segs = generateSeg segs (\seg sh ix -> S.lift (seg, sh, ix))

-- Get the offsets from the segment descriptors.
--
-- For regular segments, recompute the offsets each time as that is very cheap
-- and we want it to fuse.
--
offsets :: Shape sh => S.Acc (Segments sh) -> S.Acc (Vector Int)
offsets (S.unatup6 -> (f,_,s,sh,o,_)) = S.the f S.?| (o, S.generate (S.index1 (S.the s)) ((S.shapeSize (S.the sh) *) . S.unindex1))

shapes :: Shape sh => S.Acc (Segments sh) -> S.Acc (Vector sh)
shapes (S.unatup6 -> (f,_,s,sh,_,shs)) = S.the f S.?| (shs, S.fill (S.index1 (S.the s)) (S.the sh))

shapes' :: Shape sh => S.Acc (Segments sh) -> S.Acc (Vector' sh)
shapes' segs = withSegs segs (asVector' (shapes segs)) replicated

totalSize :: Shape sh => S.Acc (Segments sh) -> S.Exp Int
totalSize (S.unatup6 -> (_,ts,_,_,_,_)) = S.the ts

segmentsFromShapes :: Shape sh => S.Acc (Vector sh) -> S.Acc (Segments sh)
segmentsFromShapes ss = let (offs,sz) = S.scanl' (+) 0 (S.map S.shapeSize ss)
                         in irregularSegs (S.the sz) offs ss

regular :: (Shape sh, Elt e) => S.Exp Int -> S.Exp sh -> S.Acc (Vector e) -> S.Acc (Nested (Array sh e))
regular sz sh = liftedArray (regularSegs sz sh)

regularSegs :: Shape sh => S.Exp Int -> S.Exp sh -> S.Acc (Segments sh)
regularSegs sz sh = S.lift ( S.unit (S.constant False)
                           , S.unit (sz * S.shapeSize sh)
                           , S.unit sz
                           , S.unit sh
                           , S.emptyArray
                           , S.emptyArray )

irregularSegs :: Shape sh => S.Exp Int -> S.Acc (Vector Int) -> S.Acc (Vector sh) -> S.Acc (Segments sh)
irregularSegs ts offs shs = S.lift ( S.unit (S.constant True)
                                   , S.unit ts
                                   , S.unit (-1)
                                   , S.unit (S.constant empty)
                                   , offs
                                   , shs )

isIrregularSegs :: Shape sh => S.Acc (Segments sh) -> S.Acc (Scalar Bool)
isIrregularSegs (S.unatup6 -> (f,_,_,_,_,_)) = f

regularShape :: Shape sh => S.Acc (Segments sh) -> S.Acc (Scalar sh)
regularShape (S.unatup6 -> (_,_,_,sh,_,_)) = sh

regularSize :: Shape sh => S.Acc (Segments sh) -> S.Acc (Scalar Int)
regularSize (S.unatup6 -> (_,_,s,_,_,_)) = s

withSegs :: (Shape sh, Arrays a) => S.Acc (Segments sh) -> S.Acc a -> (S.Exp Int -> S.Exp sh -> S.Acc a) -> S.Acc a
withSegs (S.unatup6 -> (f,_,s,sh,_,_)) irr r = S.the f S.?| (irr, r (S.the s) (S.the sh))

indexInSeg :: (Shape sh, Elt e) => S.Acc (Nested (Array sh e)) -> S.Exp Int -> S.Exp sh -> S.Exp e
indexInSeg arr seg ix = let segs = segments arr
                        in values arr S.!! ((offsets segs S.!! seg) + (S.toIndex (shapes segs S.!! seg) ix))

indexSegLast :: forall a. Arrays a => S.Acc (Nested a) -> S.Acc a
indexSegLast arr =
  case flavour (undefined :: a) of
    ArraysFunit  -> S.Acc (S.Atuple NilAtup)
    ArraysFarray -> let segs  = segments arr
                        sh    = shapes segs  S.!! (S.size (shapes segs) - 1)
                        start = offsets segs S.!! (S.size (shapes segs) - 1)
                    in S.generate sh (\ix -> values arr S.!! (start + S.toIndex sh ix))
    ArraysFtuple -> S.Acc $ S.Atuple $ indexSegLastT (prod (Proxy :: Proxy Arrays) (undefined :: a)) (asAtuple arr)
  where
    indexSegLastT :: ProdR Arrays t -> Atuple S.Acc (NestedTupleRepr t) -> Atuple S.Acc t
    indexSegLastT ProdRunit      NilAtup         = NilAtup
    indexSegLastT (ProdRsnoc pr) (SnocAtup t a') = SnocAtup (indexSegLastT pr t) (indexSegLast a')

liftedScalar :: Elt e => S.Acc (Vector e) -> S.Acc (Nested (Scalar e))
liftedScalar vs = regular (S.size vs) (S.constant Z) vs

replicated :: Elt e => S.Exp Int -> S.Exp e -> S.Acc (Vector' e)
replicated c e = S.lift (S.unit (S.constant False), S.unit c, S.unit e, S.emptyArray)

asVector :: Elt e => S.Acc (Vector' e) -> S.Acc (Vector e)
asVector v = withVector' v (\c e -> S.fill (S.index1 c) e) id

asVector' :: Elt e => S.Acc (Vector e) -> S.Acc (Vector' e)
asVector' v = S.lift (S.unit (S.constant True), S.unit (S.size v), S.unit (v S.!! 0), v)

withVector' :: (Arrays a, Elt e) => S.Acc (Vector' e) -> (S.Exp Int -> S.Exp e -> S.Acc a) -> (S.Acc (Vector e) -> S.Acc a) -> S.Acc a
withVector' (S.unlift -> (r, c, e, es)) f g = S.the r S.?| (g es, f (S.the c) (S.the e))

asSegments :: Shape sh => S.Acc (Vector' sh) -> S.Acc (Segments sh)
asSegments v = withVector' v regularSegs segmentsFromShapes

makeFoldSegments :: forall sh. (Shape sh, Slice sh) => S.Acc (Segments (sh:.Int)) -> S.Acc (Vector Int, Segments sh)
makeFoldSegments segs = S.lift (generateSeg inSegs (\seg sh ix -> (offs S.!! seg) + S.toIndex sh ix), outSegs)
  where
    offs  = offsets segs
    shs   = S.map S.indexTail (shapes segs)
    outSegs = segmentsFromShapes (S.map nonEmpty shs)
    inSegs  = segmentsFromShapes shs

nonEmpty :: forall sh. Shape sh => S.Exp sh -> S.Exp sh
nonEmpty = S.union (S.constant $ listToShape $ P.replicate (dim (ignore::sh)) 1)

makeFoldSegSegments :: forall sh i. (Shape sh, Slice sh, IsIntegral i, Elt i)
                    => S.Acc (Segments (sh:.Int))
                    -> S.Acc (Nested (Vector i))
                    -> S.Acc (Segments (sh:.Int), Vector i)
makeFoldSegSegments segs isegs = S.lift (segmentsFromShapes shs', isegs')
  where
    shs  = shapes segs
    shs' = S.generate (S.shape shs) f
    f ix = let sh  = S.indexTail (shs S.! ix)
               shi = shapes (segments isegs) S.! ix
           in S.lift (sh :. S.shapeSize shi)

    isegs' = generateSeg (segments isegs) (\seg _ ix -> indexInSeg isegs seg ix + S.fromIntegral (offsets (segments isegs) S.!! seg))

-- RCE: I have a strong feeling this can be done better.
--
liftedCond :: forall a. Arrays a
           => S.Acc (Vector Bool) -- condition
           -> S.Acc (Nested a)    -- then
           -> S.Acc (Nested a)    -- else
           -> S.Acc (Nested a)
liftedCond pred th el
  = case (flavour (undefined :: a)) of
      ArraysFunit  -> th
      ArraysFarray -> liftedCond1 th el
      ArraysFtuple -> S.Acc $ S.Atuple $ cvtT (prod (Proxy :: Proxy Arrays) (undefined :: a)) (asAtuple th) (asAtuple el)
  where
    cvtT :: ProdR Arrays t -> Atuple S.Acc (NestedTupleRepr t) -> Atuple S.Acc (NestedTupleRepr t) -> Atuple S.Acc (NestedTupleRepr t)
    cvtT ProdRunit     NilAtup          NilAtup          = NilAtup
    cvtT (ProdRsnoc t) (SnocAtup t1 a1) (SnocAtup t2 a2) = SnocAtup (cvtT t t1 t2) (liftedCond pred a1 a2)
    cvtT _             _                _                = error "Unreachable code"

    liftedCond1 :: (Elt e, Shape sh) => S.Acc (Nested (Array sh e)) -> S.Acc (Nested (Array sh e)) -> S.Acc (Nested (Array sh e))
    liftedCond1 t e = liftedArray segs vals
      where
        shs_t = shapes (segments t)
        shs_e = shapes (segments e)
        shs   = S.zipWith (\f p -> let (t,e) = S.unlift p in f S.? (t, e))
                           pred
                           (S.zip shs_t shs_e)

        segs = segmentsFromShapes shs

        offs_t = offsets (segments t)
        offs_e = offsets (segments e)
        sz_v   = S.fold (+) 0 $ S.map S.shapeSize shs
        offs   = S.zipWith (\f p -> let (t,e) = S.unlift p in f S.? (t, e))
                           pred
                           (S.zip offs_t offs_e)
        flag_offs = replicateSeg segs $ S.zip pred offs

        vals_t = values t
        vals_e = values e
        ones   = S.fill (S.index1 $ S.the sz_v) (1 :: S.Exp Int)
        enums  = S.scanl1Seg (+) ones $ S.map S.shapeSize shs
        vals   = S.zipWith (\t ind -> let (f,s) = S.unlift t in f S.? (vals_t S.!! (s + ind), vals_e S.!! (s + ind)))
                           flag_offs
                           enums

--liftedAwhile :: forall t.
--                (Arrays t, Arrays (Nested t))
--             => (S.Acc (Nested t) -> S.Acc (Vector Bool))
--             -> (S.Acc (Nested t) -> S.Acc (Nested t))
--             -> S.Acc (Nested t)
--             -> S.Acc (Nested t)
--liftedAwhile pred iter init
--  = let
--      (a, _ :: S.Acc (Vector Bool), _ :: S.Acc (Scalar Bool)) = S.unlift $ S.awhile pred' iter' init'
--    in a
--  where
--    init' = let f = pred init
--            in S.lift (init, f, S.or f)

--    pred' :: S.Acc (Nested t, Vector Bool, Scalar Bool) -> S.Acc (Scalar Bool)
--    pred' f = let (_ :: S.Acc (Nested t), _ :: S.Acc (Vector Bool), c) = S.unlift f in c

--    iter' :: S.Acc (Nested t, Vector Bool, Scalar Bool) -> S.Acc (Nested t, Vector Bool, Scalar Bool)
--    iter' (S.unlift -> (a, f, _ :: S.Acc (Scalar Bool)))
--      = let a' = liftedCond f (iter a) a
--            f' = S.zipWith (S.&&*) f (pred a')
--            c' = S.or f'
--        in S.lift (a', f', c')

liftedReshape :: (Elt e, Shape sh, Shape sh') => S.Acc (Vector' sh) -> S.Acc (Nested (Array sh' e)) -> S.Acc (Nested (Array sh e))
liftedReshape extents a =
  let segs  = segments a
      segs' = withVector' extents (\c sh -> regularSegs c sh) (irregularSegs (totalSize segs) (offsets segs))
  in liftedArray segs' (values a)

--liftedGenerate :: (Elt e, Shape sh)
--               => S.Acc (Vector sh)
--               -> (S.Acc (Vector sh) -> S.Acc (Vector e))
--               -> S.Acc (Regular (Array sh e))
--liftedGenerate extents fun
--  = liftedArray extents (fun (enumSeg extents))

liftedZip :: (Elt a, Elt b, Shape sh)
          => S.Acc (Nested (Array sh a))
          -> S.Acc (Nested (Array sh b))
          -> S.Acc (Nested (Array sh (a,b)))
liftedZip as bs = liftedArray segs vals
  where
    segs = intersectSegments (segments as) (segments bs)

    vals = generateSeg segs (\seg _ ix -> S.lift (indexInSeg as seg ix, indexInSeg bs seg ix))

intersectSegments :: Shape sh => S.Acc (Segments sh) -> S.Acc (Segments sh) -> S.Acc (Segments sh)
intersectSegments as bs = withSegs as bothIrregular
                        $ \sz sh   -> withSegs bs bothIrregular
                        $ \sz' sh' -> regularSegs sz (sh `S.intersect` sh')
  where
    bothIrregular = segmentsFromShapes (S.zipWith S.intersect (shapes as) (shapes bs))

--liftedFold :: (Elt e, Shape sh, Slice sh)
--           => (S.Exp e -> S.Exp e -> S.Exp e)
--           -> S.Exp e
--           -> S.Acc (LiftedArray (sh:.Int) e)
--           -> S.Acc (LiftedArray sh        e)
--liftedFold f z a = liftedArray segs' vals
--  where
--    vals = S.foldSeg f z (values a) (replicateSeg segs' heads')

--    (segs, heads) = S.unzip $ S.map (\sh -> S.lift (S.indexTail sh, S.indexHead sh)) (segments a)

--    segs' = makeNonEmpty segs
--    heads' = S.zipWith (\sh h -> S.shapeSize sh S.==* 0 S.? (0,h)) segs heads

--liftedFoldSeg :: (Elt e, Shape sh, Slice sh)
--              => (S.Exp e -> S.Exp e -> S.Exp e)
--              -> S.Exp e
--              -> S.Acc (LiftedArray (sh:.Int) e)
--              -> S.Acc (LiftedArray DIM1      Int)
--              -> S.Acc (LiftedArray (sh:.Int) e)
--liftedFoldSeg f z a is = liftedArray segs vals
--  where
--    tails = S.map S.indexTail (segments a)
--    vals = S.foldSeg f z (values a) isegs
--    segs = S.zipWith (\x y -> S.lift (x:.y)) tails
--                                             (S.map S.unindex1 (segments is))
--    isegs = replicateVectors tails is


--liftedBackpermute :: (Elt e, Shape sh, Shape sh')
--                  => S.Acc (Vector sh')
--                  -> (S.Acc (Vector sh') -> S.Acc (Vector sh))
--                  -> S.Acc (LiftedArray sh  e)
--                  -> S.Acc (Regular (Array sh' e))
--liftedBackpermute shapes f a = liftedArray shapes vals'
--  where
--    segs   = segments a
--    vals   = values a
--    enums  = enumSeg shapes
--    ixs    = f enums
--    starts = replicateSeg shapes (fst $ offsets segs)
--    ixs'   = S.map S.index1 $ S.zipWith (+) starts (S.map S.shapeSize ixs)
--    vals'  = S.backpermute (S.shape ixs') (ixs' S.!) vals

liftedBackpermutePre :: Shape sh'
                     => S.Acc (Segments sh')
                     -> S.Acc (Vector Int, Vector sh')
liftedBackpermutePre segs = S.lift . S.unzip
                          $ generateSeg segs
                                        (\seg _ ix -> S.lift (seg, ix))

liftedBackpermute :: (Elt e, Shape sh)
                  => S.Acc (Vector Int, Vector sh)
                  -> S.Acc (Nested (Array sh e))
                  -> S.Acc (Vector e)
liftedBackpermute (S.unlift -> (segs, ixs)) a = vals'
  where
    vals  = values a
    offs  = offsets (segments a)
    shs   = shapes (segments a)
    vals' = S.generate (S.shape segs) f
    f ix  = let seg = segs S.! ix
            in vals S.!! ((offs S.!! seg) + (shs S.!! seg) `S.toIndex` (ixs S.! ix))

--liftedPermute :: (Elt e, Shape sh, Shape sh')
--              => (S.Exp e -> S.Exp e -> S.Exp e)
--              -> S.Acc (Regular (Array sh' e))
--              -> (S.Acc (Vector sh) -> S.Acc (Vector sh'))
--              -> S.Acc (Regular (Array sh e))
--              -> S.Acc (Regular (Array sh' e))
--liftedPermute combine defaults perm init = liftedArray shapes' vals
--  where
--    shapes  = segments ini
--    shapes' = segments defaults
--    enums   = enumSeg shapes
--    ixs     = perm enums
--    ixs'    = asOffsetsOf (liftedArray shapes ixs) shapes'
--    vals    = S.permute combine (values defaults) (ixs' S.!) (values init)

asOffsetsOf :: (Shape sh, Shape sh')
            => S.Acc (Nested (Array sh sh'))
            -> S.Acc (Segments sh')
            -> S.Acc (Vector DIM1)
asOffsetsOf ixs shapes' = S.map S.index1 $ S.zipWith (+) starts (S.map S.shapeSize (values ixs))
  where
    shapes = segments ixs
    starts = replicateSeg shapes (offsets shapes')

liftedIndex :: (Shape sh, Elt e)
            => S.Acc (Nested (Array sh e))
            -> S.Acc (Vector' sh)
            -> S.Acc (Vector' e)
liftedIndex arr ixs = asVector' $ S.backpermute (S.shape ixs') f (values arr)
  where
    ixs' = asVector ixs
    f ix = let off = offsets (segments arr) S.! ix
               sh  = shapes  (segments arr) S.! ix
           in S.index1 $ off + S.toIndex sh (ixs' S.! ix)


-- RCE: Using a generate here, as opposed to the backpermute used above, so that the linear indexing
-- is preserved. In reality, it may be better to do this as a backpermute or, equally as likely, i
-- makes no difference whatsoever.
liftedLinearIndex :: (Shape sh, Elt e)
                  => S.Acc (Nested (Array sh e))
                  -> S.Acc (Vector' Int)
                  -> S.Acc (Vector' e)
liftedLinearIndex arr ixs = asVector' $ S.backpermute (S.shape ixs') f (values arr)
  where
    ixs' = asVector ixs
    f ix = let off = offsets (segments arr) S.! ix
           in S.index1 $ off + ixs' S.! ix

-- |Compute head flags vector from segment descriptor for left-scans.
--
-- The vector will be full of zeros in the body of a segment, and non-zero
-- otherwise. The "flag" value, if greater than one, indicates that several
-- empty segments are represented by this single flag entry. This is additional
-- data is used by exclusive segmented scan.
--
mkHeadFlags :: S.Acc (Segments DIM1) -> S.Acc (Vector Int)
mkHeadFlags seg
  = S.init
  $ S.permute (+) zeros (\ix -> S.index1 (offset S.! ix)) ones
  where
    offset = offsets seg
    len    = totalSize seg
    zeros  = S.fill (S.index1  $ len + 1) 0
    ones   = S.fill (S.index1  $ S.size offset) 1

-- |Compute tail flags vector from segment vector for right-scans. That is, the
-- flag is placed at the last place in each segment.
--
mkTailFlags :: S.Acc (Segments DIM1) -> S.Acc (Vector Int)
mkTailFlags seg
  = S.init
  $ S.permute (+) zeros (\ix -> S.index1 (len - 1 - offset S.! ix)) ones
  where
    offset = offsets seg
    len    = totalSize seg
    zeros  = S.fill (S.index1 $ len + 1) 0
    ones   = S.fill (S.index1  $ S.size offset) 1

replicateC :: (Arrays a, Kit acc)
           => acc aenv (Scalar Int) -> acc aenv a -> acc aenv (Nested a)
replicateC = fromHOAS2 (replicate . S.the)

enumSegC :: (Shape sh, Kit acc) => acc aenv (Segments sh) -> acc aenv (Vector (Int, sh, sh))
enumSegC = fromHOAS enumSeg

liftedZipC :: (Kit acc, Shape sh, Elt a, Elt b)
           => acc aenv (Nested (Array sh a))
           -> acc aenv (Nested (Array sh b))
           -> acc aenv (Nested (Array sh (a,b)))
liftedZipC = fromHOAS2 liftedZip

liftedCondC :: (Arrays a, Arrays (Nested a), Kit acc)
            => acc aenv (Vector Bool)
            -> acc aenv (Nested a)
            -> acc aenv (Nested a)
            -> acc aenv (Nested a)
liftedCondC = fromHOAS3 liftedCond

liftedReshapeC :: (Elt e, Shape sh, Shape sh', Kit acc)
               => acc aenv (Vector' sh)
               -> acc aenv (Nested (Array sh' e))
               -> acc aenv (Nested (Array sh e))
liftedReshapeC = fromHOAS2 liftedReshape

liftedBackpermutePreC :: (Shape sh', Kit acc)
                      => acc aenv (Segments sh')
                      -> acc aenv (Vector Int, Vector sh')
liftedBackpermutePreC = fromHOAS liftedBackpermutePre

liftedBackpermuteC :: (Elt e, Shape sh, Kit acc)
                   => acc aenv (Vector Int, Vector sh)
                   -> acc aenv (Nested (Array sh e))
                   -> acc aenv (Vector e)
liftedBackpermuteC = fromHOAS2 liftedBackpermute

asOffsetsOfC :: (Shape sh, Shape sh', Kit acc)
             => acc aenv (Nested (Array sh sh'))
             -> acc aenv (Segments sh')
             -> acc aenv (Vector DIM1)
asOffsetsOfC = fromHOAS2 asOffsetsOf

liftedIndexC :: (Kit acc, Shape sh, Elt e)
             => acc aenv (Nested (Array sh e))
             -> acc aenv (Vector' sh)
             -> acc aenv (Vector' e)
liftedIndexC = fromHOAS2 liftedIndex

liftedLinearIndexC :: (Kit acc, Shape sh, Elt e)
                   => acc aenv (Nested (Array sh e))
                   -> acc aenv (Vector' Int)
                   -> acc aenv (Vector' e)
liftedLinearIndexC = fromHOAS2 liftedLinearIndex

indexLastC :: forall acc env aenv sh. Shape sh
           => PreOpenExp acc env aenv (sh:.Int)
           -> PreOpenExp acc env aenv Int
indexLastC | AsSlice <- asSlice (Proxy :: Proxy sh)
           = IndexHead . IndexTrans

flattenC :: forall acc aenv sh e. (Kit acc, Shape sh, Elt e)
         => acc aenv (Array sh e) -> acc aenv (Vector e)
flattenC = fromHOAS S.flatten

regularC :: (Kit acc, Shape sh, Elt e) => acc aenv (Scalar Int) -> acc aenv (Scalar sh) -> acc aenv (Vector e) -> acc aenv (Nested (Array sh e))
regularC = fromHOAS3 ((. S.the) . regular . S.the)

valuesC :: (Kit acc, Shape sh, Elt e) => acc aenv (Nested (Array sh e)) -> acc aenv (Vector e)
valuesC = fromHOAS values

segmentsC :: (Kit acc, Shape sh, Elt e) => acc aenv (Nested (Array sh e)) -> acc aenv (Segments sh)
segmentsC = fromHOAS segments

shapesC :: (Kit acc, Shape sh) => acc aenv (Segments sh) -> acc aenv (Vector sh)
shapesC = fromHOAS shapes

shapes'C :: (Kit acc, Shape sh) => acc aenv (Segments sh) -> acc aenv (Vector' sh)
shapes'C = fromHOAS shapes'

irregularC :: (Kit acc, Shape sh, Elt e) => acc aenv (Segments sh) -> acc aenv (Vector e) -> acc aenv (Nested (Array sh e))
irregularC segs vs = inject . Atuple $ NilAtup `SnocAtup` segs `SnocAtup` vs

segmentsFromShapesC :: (Kit acc, Shape sh) => acc aenv (Vector sh) -> acc aenv (Segments sh)
segmentsFromShapesC = fromHOAS segmentsFromShapes

regularSegsC :: (Kit acc, Shape sh) => acc aenv (Scalar Int) -> acc aenv (Scalar sh) -> acc aenv (Segments sh)
regularSegsC = fromHOAS2 ((. S.the) . regularSegs . S.the)

liftedScalarC :: (Kit acc, Elt e) => acc aenv (Vector e) -> acc aenv (Nested (Scalar e))
liftedScalarC = fromHOAS liftedScalar

isIrregularSegsC :: (Kit acc, Shape sh) => acc aenv (Segments sh) -> acc aenv (Scalar Bool)
isIrregularSegsC = fromHOAS isIrregularSegs

regularShapeC :: (Kit acc, Shape sh) => acc aenv (Segments sh) -> acc aenv (Scalar sh)
regularShapeC = fromHOAS regularShape

regularSizeC :: (Kit acc, Shape sh) => acc aenv (Segments sh) -> acc aenv (Scalar Int)
regularSizeC = fromHOAS regularSize

asVectorC :: (Kit acc, Elt e) => acc aenv (Vector' e) -> acc aenv (Vector e)
asVectorC = fromHOAS asVector

asVector'C :: (Kit acc, Elt e) => acc aenv (Vector e) -> acc aenv (Vector' e)
asVector'C = fromHOAS asVector'

asSegmentsC :: (Kit acc, Shape sh) => acc aenv (Vector' sh) -> acc aenv (Segments sh)
asSegmentsC = fromHOAS asSegments

replicatedC :: (Kit acc, Elt e) => acc aenv (Scalar Int) -> acc aenv (Scalar e) -> acc aenv (Vector' e)
replicatedC = fromHOAS2 (\i e -> replicated (S.the i) (S.the e))

makeFoldSegmentsC :: (Kit acc, Shape sh, Slice sh) => acc aenv (Segments (sh:.Int)) -> acc aenv (Vector Int, Segments sh)
makeFoldSegmentsC = fromHOAS makeFoldSegments

makeFoldSegSegmentsC :: (Kit acc, Shape sh, Slice sh, IsIntegral i, Elt i) => acc aenv (Segments (sh:.Int)) -> acc aenv (Nested (Vector i)) -> acc aenv (Segments (sh:.Int), Vector i)
makeFoldSegSegmentsC = fromHOAS2 makeFoldSegSegments


-- HOAS-conversion
-- ---------------

-- Conversion from HOAS to Debruijn form in such a way that it is easier to use during the transform
--

cvtHOAS :: (S.Afunction f, Kit acc) => f -> PreOpenAfun acc aenv (S.AfunctionR f)
cvtHOAS = weaken ixt . fromOpenAfun . S.convertAfun True True True True
  where
    ixt :: () :> aenv
    ixt ix = case ix of {}

cvtExpHOAS :: S.Function f => f -> OpenFun env aenv (S.FunctionR f)
cvtExpHOAS = weakenE ixt . weaken ixt . S.convertFun True
  where
    ixt :: () :> aenv
    ixt ix = case ix of {}

fromHOAS :: forall acc aenv a b. (Kit acc, Arrays a, Arrays b)
         => (S.Acc a -> S.Acc b)
         -> acc aenv a
         -> acc aenv b
fromHOAS f a =
  case (cvtHOAS f :: PreOpenAfun acc aenv (a -> b)) of
    Alam (Abody b) -> inject $ Alet a b
    _              -> error "Absurd"

fromHOAS2 :: forall acc aenv a b c. (Kit acc, Arrays a, Arrays b, Arrays c)
          => (S.Acc a -> S.Acc b -> S.Acc c)
          -> acc aenv a
          -> acc aenv b
          -> acc aenv c
fromHOAS2 f a b =
  case (cvtHOAS f :: PreOpenAfun acc aenv (a -> b -> c)) of
    Alam (Alam (Abody c)) -> inject $ Alet a $^ Alet (weaken SuccIdx b) c
    _                     -> error "Absurd"

fromHOAS3 :: forall acc aenv a b c d. (Kit acc, Arrays a, Arrays b, Arrays c, Arrays d)
          => (S.Acc a -> S.Acc b -> S.Acc c -> S.Acc d)
          -> acc aenv a
          -> acc aenv b
          -> acc aenv c
          -> acc aenv d
fromHOAS3 f a b c =
  case (cvtHOAS f :: PreOpenAfun acc aenv (a -> b -> c -> d)) of
    Alam (Alam (Alam (Abody d))) -> inject $ Alet a $^ Alet (weaken SuccIdx b) $^ Alet (weaken (SuccIdx . SuccIdx) c) d
    _                            -> error "Absurd"

fromExpHOAS3 :: forall env aenv a b c d. (Elt a, Elt b, Elt c, Elt d)
             => (S.Exp a -> S.Exp b -> S.Exp c -> S.Exp d)
             -> OpenExp env aenv a
             -> OpenExp env aenv b
             -> OpenExp env aenv c
             -> OpenExp env aenv d
fromExpHOAS3 f a b c =
  case (cvtExpHOAS f :: OpenFun env aenv (a -> b -> c -> d)) of
    Lam (Lam (Lam (Body d))) -> Let a $ Let (weakenE SuccIdx b) $ Let (weakenE (SuccIdx . SuccIdx) c) d
    _                        -> error "Absurd"

-- Utility functions
-- ------------------

fstA :: forall acc aenv a b. (Kit acc, Arrays a, Arrays b)
     => acc aenv (a,b)
     -> acc aenv a
fstA t = inject $ Aprj (SuccTupIdx ZeroTupIdx) t

sndA :: forall acc aenv a b. (Kit acc, Arrays a, Arrays b)
     => acc aenv (a,b)
     -> acc aenv b
sndA t = inject $ Aprj ZeroTupIdx t

fstE :: forall acc env aenv a b. (Elt a, Elt b)
     => PreOpenExp acc env aenv (a,b)
     -> PreOpenExp acc env aenv a
fstE = Prj (SuccTupIdx ZeroTupIdx)

sndE :: forall acc env aenv a b. (Elt a, Elt b)
     => PreOpenExp acc env aenv (a,b)
     -> PreOpenExp acc env aenv b
sndE = Prj ZeroTupIdx

tup :: forall acc env aenv a b. (Elt a,Elt b)
    => PreOpenExp acc env aenv a
    -> PreOpenExp acc env aenv b
    -> PreOpenExp acc env aenv (a,b)
tup a b = Tuple (SnocTup (SnocTup NilTup a) b)

atup :: forall acc aenv a b t. (Kit acc, Arrays a, Arrays b, Arrays t, IsAtuple t, ProdRepr t ~ (((),a),b))
     => acc aenv a
     -> acc aenv b
     -> acc aenv t
atup a b = inject $ Atuple $ NilAtup `SnocAtup` a `SnocAtup` b

atup3 :: forall acc aenv a b c t. (Kit acc, Arrays a, Arrays b, Arrays c, Arrays t, IsAtuple t, ProdRepr t ~ ((((),a),b),c))
      => acc aenv a
      -> acc aenv b
      -> acc aenv c
      -> acc aenv t
atup3 a b c = inject $ Atuple $ NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c

tupIx0 :: TupleIdx (t,a) a
tupIx0 = ZeroTupIdx

tupIx1 :: TupleIdx ((t,a),b) a
tupIx1 = SuccTupIdx ZeroTupIdx

tupIx2 :: TupleIdx (((t,a),b),c) a
tupIx2 = SuccTupIdx (SuccTupIdx ZeroTupIdx)

tupIx3 :: TupleIdx ((((t,a),b),c),d) a
tupIx3 = SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))

replicateA :: forall acc aenv a.
             (Kit acc, Arrays a)
          => acc aenv a
          -> Size acc  aenv Int
          -> acc aenv (Nested a)
replicateA a size
  = replicateC (unitSize size) a

replicateE :: forall acc aenv e.
              (Kit acc, Elt e)
           => Size acc aenv Int
           -> PreExp acc aenv e
           -> PreOpenAcc acc aenv (Vector e)
replicateE (Size b s) e = Alet b $^ Replicate (SliceFixed SliceNil) (IndexCons IndexNil s) (inject . weakenA1 $ Unit e)

var0 :: (Kit acc, Elt t)
     => PreOpenExp acc (env, t) aenv t
var0 = Var ZeroIdx

var1 :: (Kit acc, Elt t)
     => PreOpenExp acc ((env, t), s) aenv t
var1 = Var $ SuccIdx ZeroIdx

var2 :: (Kit acc, Elt t)
     => PreOpenExp acc (((env, t), s), r) aenv t
var2 = Var . SuccIdx . SuccIdx $ ZeroIdx

avar0 :: (Kit acc, Arrays t)
      => acc (aenv, t) t
avar0 = inject $ Avar ZeroIdx

avar1 :: (Kit acc, Arrays t)
      => acc ((aenv, t), s) t
avar1 = inject $ Avar $ SuccIdx ZeroIdx

avar2 :: (Kit acc, Arrays t)
      => acc (((aenv, t), s), r) t
avar2 = inject $ Avar $ SuccIdx . SuccIdx $ ZeroIdx

avar3 :: (Kit acc, Arrays t)
      => acc ((((aenv, t), s), r), q) t
avar3 = inject $ Avar $ SuccIdx . SuccIdx . SuccIdx $ ZeroIdx

avar4 :: (Kit acc, Arrays t)
      => acc (((((aenv, t), s), r), q), p) t
avar4 = inject $ Avar $ SuccIdx . SuccIdx . SuccIdx . SuccIdx $ ZeroIdx

the :: Elt e
    => acc aenv (Scalar e)
    -> PreOpenExp acc env aenv e
the a = Index a IndexNil

unit :: (Kit acc, Elt e)
     => PreExp acc aenv e
     -> acc aenv (Scalar e)
unit = inject . Unit

index1 :: PreOpenExp acc env aenv Int
       -> PreOpenExp acc env aenv DIM1
index1 = IndexCons IndexNil

index2 :: PreOpenExp acc env aenv Int
       -> PreOpenExp acc env aenv Int
       -> PreOpenExp acc env aenv DIM2
index2 h w = IndexNil `IndexCons` h `IndexCons` w

unindex1 :: PreOpenExp acc env aenv DIM1
         -> PreOpenExp acc env aenv Int
unindex1 = IndexHead

segmented :: (Elt e, Kit acc)
          => PreOpenFun acc env aenv (e -> e -> e)
          -> PreOpenFun acc env aenv ((Int, e) -> (Int, e) -> (Int, e))
segmented f = Lam . Lam . Body
  $ tup (PrimBOr integralType `PrimApp` tup (fstE var1) (fstE var0))
        (Cond (PrimNEq scalarType `PrimApp` tup (fstE var0) (Const 0))
              (sndE var0)
              (subApplyE2 (weakenE2 f) (sndE var0) (sndE var1)))

newTop :: env :> env'
       -> (env,t) :> (env', t)
newTop _  ZeroIdx = ZeroIdx
newTop wk (SuccIdx ix) = SuccIdx $ wk ix

weakenA1 :: Sink f
         => f aenv t
         -> f (aenv,s) t
weakenA1 = weaken SuccIdx

weakenA2 :: Sink f
         => f aenv t
         -> f ((aenv,r),s) t
weakenA2 = weaken (SuccIdx . SuccIdx)

weakenA3 :: Sink f
         => f aenv t
         -> f (((aenv,q),r),s) t
weakenA3 = weaken (SuccIdx . SuccIdx . SuccIdx)

weakenA4 :: Sink f
         => f aenv t
         -> f ((((aenv,p),q),r),s) t
weakenA4 = weaken (SuccIdx . SuccIdx . SuccIdx . SuccIdx)

weakenA5 :: Sink f
         => f aenv t
         -> f (((((aenv,o),p),q),r),s) t
weakenA5 = weaken (SuccIdx . SuccIdx . SuccIdx . SuccIdx . SuccIdx)

weakenE1 :: SinkExp f
         => f env     aenv t
         -> f (env,s) aenv t
weakenE1 = weakenE SuccIdx

weakenE2 :: SinkExp f
         => f env         aenv t
         -> f ((env,r),s) aenv t
weakenE2 = weakenE (SuccIdx . SuccIdx)

weakenE3 :: SinkExp f
         => f env         aenv t
         -> f (((env,q),r),s) aenv t
weakenE3 = weakenE (SuccIdx . SuccIdx . SuccIdx)

fun1 :: (Kit acc, Elt a, Elt b)
     => (forall env. PreOpenExp acc env aenv a -> PreOpenExp acc env aenv b)
     -> PreOpenFun acc env aenv (a -> b)
fun1 f = Lam (Body (f var0))

fun2 :: (Kit acc, Elt a, Elt b, Elt c)
     => (forall env. PreOpenExp acc env aenv a -> PreOpenExp acc env aenv b -> PreOpenExp acc env aenv c)
     -> PreOpenFun acc env aenv (a -> b -> c)
fun2 f = Lam (Lam (Body (f var1 var0)))

fun3 :: (Kit acc, Elt a, Elt b, Elt c, Elt d)
     => (forall env'. PreOpenExp acc env' aenv a -> PreOpenExp acc env' aenv b -> PreOpenExp acc env' aenv c -> PreOpenExp acc env' aenv d)
     -> PreOpenFun acc env aenv (a -> b -> c -> d)
fun3 f = Lam (Lam (Lam (Body (f var2 var1 var0))))

unliftA :: forall env aenv env' aenv'.
           Context env aenv env' aenv'
        -> (aenv :?> aenv')
unliftA (PushAccC _)    ZeroIdx      = Just ZeroIdx
unliftA (PushAccC d)    (SuccIdx ix) = SuccIdx <$> unliftA d ix
unliftA (PushLAccC _)   ZeroIdx      = Nothing
unliftA (PushLAccC d)   (SuccIdx ix) = SuccIdx <$> unliftA d ix
unliftA (PushExpC d)    ix           = unliftA d ix
unliftA (PushLExpC d)   ix           = SuccIdx <$> unliftA d ix
unliftA _               _            = error "unliftA: Inconsistent evalution"


rebuildToLift :: Rebuildable f
              => Context env aenv env' aenv'
              -> f aenv t
              -> Maybe (f aenv' t)
rebuildToLift d = rebuildPartial (liftA Avar . unliftA d)

alet :: (Kit acc, Arrays a, Arrays b)
     => acc aenv     a
     -> acc (aenv,a) b
     -> acc aenv     b
alet a = inject . Alet a

apply :: (Kit acc, Arrays a)
      => PreOpenAfun acc aenv (a -> b)
      -> acc             aenv a
      -> acc             aenv b
apply f = inject . subApply f

subApply2 :: (Kit acc, Arrays a)
         => PreOpenAfun acc aenv (a -> b -> c)
         -> acc             aenv a
         -> acc             aenv b
         -> PreOpenAcc  acc aenv c
subApply2 (Alam (Alam (Abody f))) a b
  = Alet a
  $ inject $ Alet (weakenA1 b)
  $ f
subApply2 _ _ _ = error "subApply2: inconsistent evaluation"

subApplyE :: Kit acc
          => PreOpenFun  acc env aenv (a -> b)
          -> PreOpenExp  acc env aenv a
          -> PreOpenExp  acc env aenv b
subApplyE (Lam (Body f)) a
  = Let a f
subApplyE _ _ = error "subApplyE: inconsistent evaluation"

subApplyE2 :: Kit acc
           => PreOpenFun  acc env aenv (a -> b -> c)
           -> PreOpenExp  acc env aenv a
           -> PreOpenExp  acc env aenv b
           -> PreOpenExp  acc env aenv c
subApplyE2 (Lam (Lam (Body f))) a b
  = Let a
  $ Let (weakenE1 b)
  $ f
subApplyE2 _ _ _ = error "subApplyE2: inconsistent evaluation"

partApply :: Kit acc
         => PreOpenAfun acc aenv (a -> r)
         -> acc             aenv a
         -> PreOpenAfun acc aenv r
partApply (Alam f) a
 = app id a f
 where
   app :: forall acc aenv aenv' a f. (Kit acc, Arrays a)
       => (aenv' :> (aenv, a))
       -> acc aenv a
       -> PreOpenAfun acc aenv' f
       -> PreOpenAfun acc aenv  f
   app ixt a (Abody b) = Abody (inject $ Alet a $ weaken ixt b)
   app ixt a (Alam  f) = Alam  (app ixt' (weaken SuccIdx a) f)
     where
       ixt' :: Idx (aenv', s) t
            -> Idx ((aenv, s), a) t
       ixt' ZeroIdx      = SuccIdx ZeroIdx
       ixt' (SuccIdx ix) = case ixt ix of
                             ZeroIdx      -> ZeroIdx
                             (SuccIdx ix) -> SuccIdx (SuccIdx ix)
partApply _ _
 = error "partApply: inconsistent evaluation"

infixr 0 $^

($^) :: Kit acc
     => (acc aenv a -> t)
     -> PreOpenAcc acc aenv a
     -> t
($^) f a = f $ inject a


simpleSize :: forall acc aenv e. Kit acc => PreExp acc aenv e ->  Size acc aenv e
simpleSize e = Size (inject $ Use () :: acc aenv ()) (weakenA1 e)

unitSize :: (Kit acc, Elt e) => Size acc aenv e -> acc aenv (Scalar e)
unitSize (Size b s) = inject $ Alet b $ inject $ Unit s

sizeOfVector' :: (Kit acc, Elt e) => acc aenv (Vector' e) -> Size acc aenv Int
sizeOfVector' a = Size (inject $ Aprj tupIx2 a) (the avar0)

-- Debugging
-- ----------
trace :: String -> String -> a -> a
trace header msg
  = Debug.trace Debug.dump_vectorisation
  $ header ++ ": " ++ msg

-- Sequence vectorisation
-- ------------------------

vectoriseSeq :: OpenNaturalSeq () a -> Maybe (OpenChunkedSeq () a)
vectoriseSeq = vectoriseOpenSeq Conservative EmptyC

vectoriseOpenSeq :: forall aenv aenv' a.
                    Strength
                 -> Context () aenv () aenv'
                 -> OpenNaturalSeq aenv a
                 -> Maybe (OpenChunkedSeq aenv' a)
vectoriseOpenSeq strength ctx seq =
  case seq of
    Producer p s -> Producer <$> cvtP p <*> vectoriseOpenSeq strength (PushLAccC ctx) s
    Consumer c   -> cvtC c
    Reify{}      -> Nothing
  where
    cvtP :: NaturalProducer OpenAcc aenv t -> Maybe (ChunkedProducer OpenAcc aenv' (Nested t))
    cvtP p =
      case p of
        Pull _             -> Nothing
        Subarrays sh a     -> subarrays <$> cvtE sh <*> pure a
        Produce l f        -> ProduceAccum <$> cvtL l <*> return (streamify f) <*> return nil
        MapBatch f f' a x  -> mapBatch <$> pure (cvtAF f) <*> cvtAF' f' <*> cvtA' a <*> pure (cvtA x)
        ProduceAccum{}     -> stageError

    cvtL :: Maybe (Exp aenv Int) -> Maybe (Maybe (Exp aenv' Int))
    cvtL Nothing = Just Nothing
    cvtL (Just l) | Just l' <- rebuildToLift ctx l
                  = Just (Just l')
                  | otherwise
                  = $internalError "vectoriseOpenSeq" "Limit depends on sequence elements"

    mapBatch :: forall a b c s. (Arrays a, Arrays b, Arrays c, Arrays s)
             => OpenAfun aenv' (Nested s -> Nested a -> Nested b)
             -> OpenAfun aenv' (s -> Nested b -> (s, Nested c))
             -> OpenAcc  aenv' s
             -> OpenAfun aenv' (Scalar Int -> Nested a)
             -> ChunkedProducer OpenAcc aenv' (Nested (s,c))
    mapBatch f f' a x = ProduceAccum Nothing f'' a
      where
        f'' = Alam . Alam . Abody
            $ repack
            $^ Alet (apply (weakenA2 x) $^ Unit (sndE (the avar1)))
            $ weakenA3 f' `partApply` avar1 `apply` (weakenA3 f `partApply` replicateC (unit (sndE (the avar2))) avar1 `apply` avar0)
        repack a = OpenAcc $ Alet a
                 $ atup (atup (replicateC (unit (sndE (the avar2))) (fstA avar0)) (sndA avar0)) (fstA avar0)

    subarrays :: (Shape sh, sh :<= DIM3, Elt e) => Exp aenv' sh -> Array sh e -> ChunkedProducer OpenAcc aenv' (Nested (Array sh e))
    subarrays sh arr = ProduceAccum subLimit f nil
      where
        f = Alam . Alam . Abody $ atup (liftedSubArrays (the avar1) (weakenA2 sh) arr) nil
        totalSize = Const (size (shape arr))
        subSize = ShapeSize sh
        subLimit = Just (totalSize `div` subSize)
        div a b = PrimApp (PrimIDiv integralType) (tup a b)


    streamify :: Arrays t => OpenAfun aenv (Scalar Int -> t) -> OpenAfun aenv' (Scalar (Int,Int) -> () -> (Nested t, ()))
    streamify f =
      let f' = liftOpenAfun1 Conservative ctx f
      in Alam . Alam . Abody . nest $ weakenA2 f' `apply` fromRange avar1

    fromRange :: forall aenv. OpenAcc aenv (Scalar (Int, Int)) -> OpenAcc aenv (Nested (Scalar Int))
    fromRange r =  liftedScalarC
                $^ Generate (index1 $ sndE r') (Lam . Body $ fstE r' `plus` unindex1 var0)
      where
        r' :: forall env. OpenExp env aenv (Int, Int)
        r' = the r
        plus a b = PrimApp (PrimAdd numType) (tup a b)

    nest :: forall aenv a. Arrays a => OpenAcc aenv a -> OpenAcc aenv (a,())
    nest = OpenAcc . Atuple . (\a -> NilAtup `SnocAtup` a `SnocAtup` nil)

    cvtC :: NaturalConsumer OpenAcc aenv t -> Maybe (OpenChunkedSeq aenv' t)
    cvtC c =
      case c of
        FoldBatch f f' a x -> foldBatch <$> pure (cvtAF f) <*> cvtAF' f' <*> cvtA' a <*> pure (cvtA x)
        Stuple t           -> Consumer . Stuple <$> cvtCT t

    foldBatch :: forall a b s. (Arrays a, Arrays b, Arrays s)
              => OpenAfun aenv' (Nested s -> Nested a -> Nested b)
              -> OpenAfun aenv' (s -> Nested b -> s)
              -> OpenAcc  aenv' s
              -> OpenAfun aenv' (Scalar Int -> Nested a)
              -> OpenChunkedSeq aenv' s
    foldBatch f f' a x = Producer (ProduceAccum Nothing f'' a) (Consumer (Last avar0 (weakenA1 a)))
      where
        f'' = Alam . Alam . Abody
            $ repack
            $^ Alet (apply (weakenA2 x) $^ Unit (sndE (the avar1)))
            $ weakenA3 f' `partApply` avar1 `apply` (weakenA3 f `partApply` replicateC (unit (sndE (the avar2))) avar1 `apply` avar0)
        repack a = OpenAcc $ Alet a
                 $ atup avar0 avar0

    cvtCT :: Atuple (OpenNaturalSeq aenv) t -> Maybe (Atuple (OpenChunkedSeq aenv') t)
    cvtCT NilAtup        = Just NilAtup
    cvtCT (SnocAtup t c) = SnocAtup <$> cvtCT t <*> vectoriseOpenSeq strength ctx c

    nil :: forall aenv. OpenAcc aenv ()
    nil = OpenAcc $ Atuple NilAtup

    cvtE :: Elt t => Exp aenv t -> Maybe (Exp aenv' t)
    cvtE e | Just e' <- rebuildToLift ctx e
           = Just e'
    cvtE _ = Nothing
    --
    -- cvtF :: Fun aenv t -> Fun aenv' t
    -- cvtF = vectoriseSeqOpenFun strength ctx
    --
    cvtA :: Arrays t => OpenAcc aenv t -> OpenAfun aenv' (Scalar Int -> Nested t)
    cvtA = liftOpenAcc strength ctx

    cvtA' :: OpenAcc aenv t -> Maybe (OpenAcc aenv' t)
    cvtA' t =
      case rebuildToLift ctx t of
        Nothing -> $internalError "vectoriseOpenSeq" "Sequence invariant array term depends on sequence elements"
        Just t' -> Just t'

    cvtAF :: forall a b c. OpenAfun aenv (a -> b -> c) -> OpenAfun aenv' (Nested a -> Nested b -> Nested c)
    cvtAF = liftOpenAfun2 Conservative ctx

    cvtAF' :: OpenAfun aenv t -> Maybe (OpenAfun aenv' t)
    cvtAF' t =
      case rebuildToLift ctx t of
        Nothing -> $internalError "vectoriseOpenSeq" "Sequence invariant array function depends on sequence elements"
        Just t' -> Just t'

    -- untup :: OpenAfun aenv t -> OpenAfun aenv t
    -- untup = untupleAfun BaseReducedMap

    stageError = $internalError "vectoriseOpenSeq" "AST is at wrong stage for vectorisation. It seems to have already been vectorised."

liftedSubArrays :: forall aenv sh e. (sh :<= DIM3, Elt e, Shape sh)
                => Exp aenv (Int, Int)
                -> Exp aenv sh
                -> Array sh e
                -> OpenAcc aenv (Nested (Array sh e))
liftedSubArrays index sh arr
  | AsSlice <- asSlice (Proxy :: Proxy sh)
  = regularC (unit (sndE index)) (unit sh) $
  case (maximumRank :: sh :<=: DIM3) of
    RankZ          -> flattenC $^ (Use arr)
    RankSnoc RankZ -> flattenC
                    $^ Subarray (index1 (fstE index `times` unindex1 sh)) (index1 (sndE index `times` unindex1 sh)) arr
    RankSnoc (RankSnoc RankZ)
      -> flattenC
      $^ Alet (inject $ Unit (twoDC index sh (Const fsh)))
      $  tail -- head `catC` body `catC` tail
      where
        -- head = inject $ Subarray (fstE . fst $ the avar0) (sndE . fst $ the avar0) arr
        -- body = inject
        --      $  Backpermute (weakenA1 $ index2 (sndE index) (IndexHead sh)) (Lam . Body $ reorderC (weakenA1 . weakenE1 $ sh) (Const fsh) var0)
        --      $^ Subarray (fstE . snd $ the avar0) (sndE . snd $ the avar0) arr
        tail = inject $ Subarray (fstE . fst $ the avar0) (sndE . fst $ the avar0) arr

        fst :: (Elt a, Elt b, Elt c) => Exp aenv' (a,b,c) -> Exp aenv' a
        fst = Prj (SuccTupIdx (SuccTupIdx ZeroTupIdx))
        snd :: (Elt a, Elt b, Elt c) => Exp aenv' (a,b,c) -> Exp aenv' b
        snd = Prj (SuccTupIdx ZeroTupIdx)
        trd :: (Elt a, Elt b, Elt c) => Exp aenv' (a,b,c) -> Exp aenv' c
        trd = Prj ZeroTupIdx

        fsh = fromElt (shape arr)
    _ -> error "Absurd"

  where
    times a b = PrimApp (PrimMul numType) (tup a b)
    catC :: OpenAcc aenv' (Array DIM2 e) -> OpenAcc aenv' (Array DIM2 e) -> OpenAcc aenv' (Array DIM2 e)
    catC = fromHOAS2 cat
    twoDC = fromExpHOAS3 twoD
    reorderC :: OpenExp env aenv' DIM2 -> OpenExp env aenv' DIM2 -> OpenExp env aenv' DIM2 -> OpenExp env aenv' DIM2
    reorderC = fromExpHOAS3 reorder

    twoD :: S.Exp (Int,Int) -> S.Exp DIM2 -> S.Exp DIM2 -> S.Exp ((DIM2,DIM2), (DIM2,DIM2), (DIM2,DIM2))
    twoD (S.unlift -> (i,n)) sh fsh =
      let
        (Z:.height:.width) = S.unlift sh

        toAbs :: S.Exp DIM2 -> S.Exp DIM2
        toAbs (S.unlift -> Z:.h:.w) = S.index2 (h*height) (w*width)

        fromAbs :: S.Exp DIM2 -> S.Exp DIM2
        fromAbs (S.unlift -> Z:.h:.w) = S.index2 (h `div` height) (w `div` width)

        (Z:.fheight:.(_::S.Exp Int)) = S.unlift (fromAbs fsh)
        i_y = i `mod` fheight
        i_x = i `div` fheight
        tail, body, head :: S.Exp (DIM2, DIM2)
        tail = S.lift (toAbs (S.index2 i_y i_x), toAbs (S.index2 ((fheight - i_y) `min` n) 1))
        tail_n = fheight - i_y
        body = S.lift (toAbs (S.index2 0 (i_x + 1)), toAbs (S.index2 fheight ((n - tail_n) `div` fheight)))
        head = S.lift (toAbs (S.index2 0 (i_x + 1 + S.indexHead (S.snd body))), toAbs (S.index2 ((n - tail_n) `mod` fheight) 1))
      in S.lift (tail,body,head)

    cat :: S.Acc (Array DIM2 e) -> S.Acc (Array DIM2 e) -> S.Acc (Array DIM2 e)
    cat a b = S.reshape (S.index2 h w) (S.flatten a S.++ S.flatten b)
      where
        (h_a,w) = S.unlift $ S.unindex2 (S.shape a)
        h = h_a + (S.fst . S.unindex2 $ S.shape b)

    reorder :: S.Exp DIM2 -> S.Exp DIM2 -> S.Exp DIM2 -> S.Exp DIM2
    reorder sh (S.unlift -> Z:.fh:.fw) (S.unlift -> Z:.y:.x) =
      let
        x_out = x + ((y `div` fw) * (S.indexHead sh))
        y_out = y `mod` fh
      in S.index2 y_out x_out

stripExpCtx :: Context env aenv env aenv -> Context () aenv () aenv
stripExpCtx c =
  case c of
    EmptyC -> EmptyC
    PushExpC c' -> stripExpCtx c'
    PushAccC c' -> PushAccC (stripExpCtx c')
    _ -> error "unreachable"

vectoriseSeqOpenExp :: forall env aenv a.
                       Bool
                    -> Strength
                    -> Context env aenv env aenv
                    -> OpenExp env aenv a
                    -> OpenExp env aenv a
vectoriseSeqOpenExp vectorise strength ctx = cvtE
  where
    cvtA :: OpenAcc aenv t -> OpenAcc aenv t
    cvtA a = vectoriseSeqOpenAcc vectorise strength (stripExpCtx ctx) a

    cvtT :: Tuple (OpenExp env aenv) t -> Tuple (OpenExp env aenv) t
    cvtT tup = case tup of
      NilTup      -> NilTup
      SnocTup t a -> cvtT t `SnocTup` cvtE a

    cvtF :: OpenFun env aenv t -> OpenFun env aenv t
    cvtF = vectoriseSeqOpenFun vectorise strength ctx

    cvtE :: OpenExp env aenv t -> OpenExp env aenv t
    cvtE exp =
      case exp of
        Let bnd body            -> Let (cvtE bnd) (vectoriseSeqOpenExp vectorise strength (PushExpC ctx) body)
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
        Foreign ff f e          -> Foreign ff (vectoriseSeqOpenFun vectorise strength EmptyC f) (cvtE e)

vectoriseSeqAcc :: Bool -> OpenAcc () a -> OpenAcc () a
vectoriseSeqAcc vectorise = vectoriseSeqOpenAcc vectorise Conservative EmptyC

vectoriseSeqOpenAcc :: forall aenv a.
                       Bool
                    -> Strength
                    -> Context () aenv () aenv
                    -> OpenAcc aenv a
                    -> OpenAcc aenv a
vectoriseSeqOpenAcc vectorise strength ctx = cvtA
  where
    cvtT :: Atuple (OpenAcc aenv) t -> Atuple (OpenAcc aenv) t
    cvtT atup = case atup of
      NilAtup      -> NilAtup
      SnocAtup t a -> cvtT t `SnocAtup` cvtA a

    cvtAfun :: OpenAfun aenv t -> OpenAfun aenv t
    cvtAfun = vectoriseSeqOpenAfun vectorise strength ctx

    cvtE :: Elt t => Exp aenv t -> Exp aenv t
    cvtE = vectoriseSeqOpenExp vectorise strength ctx

    cvtF :: Fun aenv t -> Fun aenv t
    cvtF = vectoriseSeqOpenFun vectorise strength ctx

    cvtA :: OpenAcc aenv t -> OpenAcc aenv t
    cvtA (OpenAcc pacc) = OpenAcc $ case pacc of
      Alet bnd body             -> Alet (cvtA bnd) (vectoriseSeqOpenAcc vectorise strength (PushAccC ctx) body)
      Avar ix                   -> Avar ix
      Atuple tup                -> Atuple (cvtT tup)
      Aprj tup a                -> Aprj tup (cvtA a)
      Apply f a                 -> Apply (cvtAfun f) (cvtA a)
      Aforeign ff afun acc      -> Aforeign ff (vectoriseSeqAfun vectorise afun) (cvtA acc)
      Acond p t e               -> Acond (cvtE p) (cvtA t) (cvtA e)
      Awhile p f a              -> Awhile (cvtAfun p) (cvtAfun f) (cvtA a)
      Use a                     -> Use a
      Unit e                    -> Unit (cvtE e)
      Reshape e a               -> Reshape (cvtE e) (cvtA a)
      Generate e f              -> Generate (cvtE e) (cvtF f)
      Transform sh ix f a       -> Transform (cvtE sh) (cvtF ix) (cvtF f) (cvtA a)
      Subarray ix sh arr        -> Subarray (cvtE ix) (cvtE sh) arr
      Replicate sl slix a       -> Replicate sl (cvtE slix) (cvtA a)
      Slice sl a slix           -> Slice sl (cvtA a) (cvtE slix)
      Map f a                   -> Map (cvtF f) (cvtA a)
      ZipWith f a1 a2           -> ZipWith (cvtF f) (cvtA a1) (cvtA a2)
      Fold f z a                -> Fold (cvtF f) (cvtE z) (cvtA a)
      Fold1 f a                 -> Fold1 (cvtF f) (cvtA a)
      Scanl f z a               -> Scanl (cvtF f) (cvtE z) (cvtA a)
      Scanl' f z a              -> Scanl' (cvtF f) (cvtE z) (cvtA a)
      Scanl1 f a                -> Scanl1 (cvtF f) (cvtA a)
      Scanr f z a               -> Scanr (cvtF f) (cvtE z) (cvtA a)
      Scanr' f z a              -> Scanr' (cvtF f) (cvtE z) (cvtA a)
      Scanr1 f a                -> Scanr1 (cvtF f) (cvtA a)
      Permute f1 a1 f2 a2       -> Permute (cvtF f1) (cvtA a1) (cvtF f2) (cvtA a2)
      Backpermute sh f a        -> Backpermute (cvtE sh) (cvtF f) (cvtA a)
      Stencil f b a             -> Stencil (cvtF f) b (cvtA a)
      Stencil2 f b1 a1 b2 a2    -> Stencil2 (cvtF f) b1 (cvtA a1) b2 (cvtA a2)
      Collect s _               -> Collect (fuseSeq (reduceOpenSeq s))
                                           (if vectorise
                                            then fuseSeq <$> vectoriseOpenSeq strength ctx s
                                            else Nothing)
      FoldSeg f z a s           -> FoldSeg (cvtF f) (cvtE z) (cvtA a) (cvtA s)
      Fold1Seg f a s            -> Fold1Seg (cvtF f) (cvtA a) (cvtA s)

vectoriseSeqAfun :: Bool -> OpenAfun () t -> OpenAfun () t
vectoriseSeqAfun vectorise = vectoriseSeqOpenAfun vectorise Conservative EmptyC

vectoriseSeqOpenFun :: Bool
                    -> Strength
                    -> Context env aenv env aenv
                    -> OpenFun env aenv t
                    -> OpenFun env aenv t
vectoriseSeqOpenFun vectorise strength ctx fun =
  case fun of
    Body b -> Body (vectoriseSeqOpenExp vectorise strength ctx b)
    Lam f  -> Lam (vectoriseSeqOpenFun vectorise strength (PushExpC ctx) f)

vectoriseSeqOpenAfun :: Bool
                     -> Strength
                     -> Context () aenv () aenv
                     -> OpenAfun aenv t
                     -> OpenAfun aenv t
vectoriseSeqOpenAfun vectorise strength ctx afun =
  case afun of
    Abody b -> Abody (vectoriseSeqOpenAcc vectorise strength ctx b)
    Alam f  -> Alam (vectoriseSeqOpenAfun vectorise strength (PushAccC ctx) f)


-- Sequence AST reduction
--
reduceStreamSeq :: Elt index
                => StreamSeq index OpenAcc a
                -> StreamSeq index OpenAcc a
reduceStreamSeq (StreamSeq binds seq) = StreamSeq binds (reduceOpenSeq seq)

reduceOpenSeq :: forall index aenv a. Elt index
              => PreOpenSeq index OpenAcc aenv a
              -> PreOpenSeq index OpenAcc aenv a
reduceOpenSeq seq =
  case seq of
    Producer p s -> Producer (cvtP p) (reduceOpenSeq s)
    Consumer c   -> cvtC c
    Reify a      -> Reify a
  where
    cvtP :: Producer index OpenAcc aenv t -> Producer index OpenAcc aenv t
    cvtP p =
      case p of
        Pull src           -> Pull src
        Subarrays sh a     -> subarrays sh a
        Produce l f        -> ProduceAccum l (streamify f) nil
        MapBatch f f' a x  -> mapBatch f f' a x
        ProduceAccum{}     -> stageError

    cvtC :: Consumer index OpenAcc aenv t -> PreOpenSeq index OpenAcc aenv t
    cvtC c =
      case c of
        FoldBatch f f' a x -> foldBatch f f' a x
        Stuple t           -> Consumer (Stuple (cvtCT t))

    mapBatch :: forall a b c s. (Arrays a, Arrays b, Arrays c, Arrays s)
             => OpenAfun aenv (s -> a -> b)
             -> OpenAfun aenv (s -> Nested b -> (s, Nested c))
             -> OpenAcc aenv s
             -> OpenAcc aenv a
             -> Producer index OpenAcc aenv (s,c)
    mapBatch f f' a x = ProduceAccum Nothing f'' a
      where
        f'' :: OpenAfun aenv (Scalar index -> s -> ((s,c),s))
        f'' = Alam . Alam . Abody . repack . alet (weakenA2 x)
            $ weakenA3 f' `partApply` avar1 `apply` (nest1 $ weakenA3 f `partApply` avar1 `apply` avar0)

        repack :: forall aenv. OpenAcc aenv (s, Nested c) -> OpenAcc aenv ((s, c),s)
        repack b = alet b $ atup (atup (fstA avar0) (fromHOAS deNest1 (sndA avar0))) (fstA avar0)

        nest1 :: forall aenv x. Arrays x => OpenAcc aenv x -> OpenAcc aenv (Nested x)
        nest1 = replicateC (unit (Const 1))

        deNest1 :: forall x. Arrays x => S.Acc (Nested x) -> S.Acc x
        deNest1 x =
          case flavour (undefined :: x) of
            ArraysFunit  -> S.use ()
            ArraysFarray -> S.reshape (S.the $ withSegs (segments x) (S.unit (shapes (segments x) S.!! 0)) (const S.unit))
                                      (values x)
            ArraysFtuple -> S.Acc $ S.Atuple $ deNestT (prod (Proxy :: Proxy Arrays) (undefined :: x)) (asAtuple x)
          where
            deNestT :: ProdR Arrays t -> Atuple S.Acc (NestedTupleRepr t) -> Atuple S.Acc t
            deNestT ProdRunit     NilAtup         = NilAtup
            deNestT (ProdRsnoc p) (SnocAtup t a') = SnocAtup (deNestT p t) (deNest1 a')

    foldBatch :: forall a b s. (Arrays a, Arrays b, Arrays s)
              => OpenAfun aenv (s -> a -> b)
              -> OpenAfun aenv (s -> Nested b -> s)
              -> OpenAcc aenv s
              -> OpenAcc aenv a
              -> PreOpenSeq index OpenAcc aenv s
    foldBatch f f' a x = Producer (ProduceAccum Nothing f'' a) (Consumer (Last avar0 (weakenA1 a)))
      where
        f'' :: OpenAfun aenv (Scalar index -> s -> (s,s))
        f'' = Alam . Alam . Abody . repack . alet (weakenA2 x)
            $ weakenA3 f' `partApply` avar1 `apply` (nest1 $ weakenA3 f `partApply` avar1 `apply` avar0)

        repack :: forall aenv. OpenAcc aenv s -> OpenAcc aenv (s,s)
        repack b = alet b $ atup avar0 avar0

        nest1 :: forall aenv x. Arrays x => OpenAcc aenv x -> OpenAcc aenv (Nested x)
        nest1 = replicateC (unit (Const 1))

    cvtCT :: Atuple (PreOpenSeq index OpenAcc aenv) t -> Atuple (PreOpenSeq index OpenAcc aenv) t
    cvtCT NilAtup        = NilAtup
    cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (reduceOpenSeq c)

    nil :: forall aenv. OpenAcc aenv ()
    nil = OpenAcc (Atuple NilAtup)

    streamify :: Arrays t
              => PreOpenAfun OpenAcc aenv (Scalar index -> t)
              -> PreOpenAfun OpenAcc aenv (Scalar index -> () -> (t, ()))
    streamify f = Alam . Alam . Abody $ atup (weakenA2 f `apply` avar1) nil

    subarrays :: forall sh aenv e. (Shape sh, sh :<= DIM3, Elt e)
              => Exp aenv sh
              -> Array sh e
              -> Producer index OpenAcc aenv (Array sh e)
    subarrays sh arr = ProduceAccum (Just (totalSize `div` subSize)) f (OpenAcc (Unit (Const (fromElt (empty :: sh)))))
      where
        f = Alam . Alam . Abody
          $ atup (OpenAcc (Subarray (the avar0) (weakenA2 sh) arr)) (OpenAcc (Unit (the avar0 `plusS` (weakenA2 sh))))

        totalSize = Const (size (shape arr))
        subSize = ShapeSize sh

        div a b = PrimApp (PrimIDiv integralType) (tup a b)
        mod a b = PrimApp (PrimMod integralType) (tup a b)
        plus a b = PrimApp (PrimAdd numType) (tup a b)
        times a b = PrimApp (PrimMul numType) (tup a b)

        plusS :: forall aenv. (Shape sh, sh :<= DIM3) => Exp aenv sh -> Exp aenv sh -> Exp aenv sh
        plusS a b =
          case (maximumRank :: sh :<=: DIM3) of
            RankZ          -> a
            RankSnoc RankZ -> index1 (unindex1 a `plus` unindex1 b)
            RankSnoc (RankSnoc RankZ) ->
              let
                y = height a `plus` height b
              in index2 (y `mod` height fsh)
                        (width a `plus` (y `div` height fsh `times` width b))
            _                         -> error "Vectorisation doesn't currently support subarrays on an array of dimension higher than 2"
          where
            height, width :: Exp aenv DIM2 -> Exp aenv Int
            height = IndexHead . IndexTail
            width  = IndexHead
            fsh :: Exp aenv sh
            fsh = Const (fromElt (shape arr))

    stageError = $internalError "vectoriseOpenSeq" "AST is at wrong stage for vectorisation. It seems to have already been vectorised."
