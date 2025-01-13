{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Vectorise
-- Copyright   : [2012..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Performs Blelloch's flattening transform on an embedded accelerate computation.
--

module Data.Array.Accelerate.Trafo.Vectorise (

  vectoriseSeq,
  vectoriseSeqAcc,
  vectoriseSeqAfun,

  liftOpenAfun1,
  liftOpenAfun2,
  Size,
  Strength(..),
  Context(..)

) where

import Prelude                                          hiding ( exp, replicate, concat )
import qualified Prelude                                as P
import Control.Applicative                              hiding ( Const )
import Data.Maybe

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Match            ( matchIdx )
import Data.Array.Accelerate.Array.Lifted
import Data.Array.Accelerate.Array.Representation      ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Pretty                    ()
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Classes.Eq       as S
import qualified Data.Array.Accelerate.Language         as S
import qualified Data.Array.Accelerate.Prelude          as S
import qualified Data.Array.Accelerate.Smart            as S
import qualified Data.Array.Accelerate.Trafo.Sharing    as S

import qualified Data.Array.Accelerate.Debug            as Debug
import Data.Array.Accelerate.Error


-- |Encodes the relationship between the old environments and the new environments during the
-- lifting transform
--
data Context env aenv env' aenv' where
  -- All environments are empty
  EmptyC    :: Context () () () ()

  -- An expression that has already been lifted
  PushLExpC :: Elt e
            => Context env aenv env' aenv'
            -> Context (env, e) aenv env' (aenv', Vector e)

  -- An unlifted expression
  PushExpC  :: Elt e
            => Context env aenv env' aenv'
            -> Context (env, e) aenv (env',e) aenv'

  -- A lifted array expression
  PushLAccC :: Arrays t
            => Context env aenv env' aenv'
            -> Context env (aenv, t) env' (aenv', Vector' t)

  -- An unlifted array expression
  PushAccC  :: Arrays t
            => Context env aenv env' aenv'
            -> Context env (aenv, t) env' (aenv', t)


data Strength = Aggressive | Conservative | HoistOnly | Nested deriving Show

type VectoriseAcc acc = forall aenv aenv' t.
                        Arrays t
                     => Strength
                     -> Context () aenv () aenv'
                     -> Size acc aenv'
                     -> acc aenv t
                     -> LiftedAcc acc aenv' t

data None sh = None sh
  deriving (Show, Eq)

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

-- Lifting terms
-- -------------

-- |The size parameter in the lifting transform.
--
type Size acc aenv = PreExp acc aenv Int

data LiftedAcc acc aenv t = AvoidedAcc (acc aenv t)
                          | LiftedAcc (acc aenv (Vector' t))

instance RebuildableAcc acc => Rebuildable (LiftedAcc acc) where
  type AccClo (LiftedAcc acc) = acc
  rebuildPartial v (AvoidedAcc a) = AvoidedAcc <$> rebuildPartial v a
  rebuildPartial v (LiftedAcc  a) = LiftedAcc  <$> rebuildPartial v a

instance Sink acc => Sink (LiftedAcc acc) where
  weaken k (AvoidedAcc a) = AvoidedAcc (weaken k a)
  weaken k (LiftedAcc a)  = LiftedAcc (weaken k a)

data LiftedExp acc env aenv t where
  AvoidedExp :: Extend acc aenv aenv'
             -> PreOpenExp acc env aenv' t
             -> LiftedExp acc env aenv t
  LiftedExp  :: acc aenv (Vector t)
             -> LiftedExp acc env aenv t

data AvoidedFun acc env aenv t where
  AvoidedFun :: Extend acc aenv aenv'
             -> PreOpenFun acc env aenv' t
             -> AvoidedFun acc env aenv t

type LiftedOpenAcc aenv t = LiftedAcc OpenAcc aenv t

over :: (acc aenv t           -> acc' aenv' t')
     -> (acc aenv (Vector' t) -> acc' aenv' (Vector' t'))
     -> LiftedAcc acc  aenv t
     -> LiftedAcc acc' aenv' t'
over f _  (AvoidedAcc a) = AvoidedAcc (f a)
over _ f' (LiftedAcc l)  = LiftedAcc (f' l)

injectL :: Kit acc => LiftedAcc (PreOpenAcc acc) aenv t -> LiftedAcc acc aenv t
injectL = over inject inject

vectoriseOpenAcc
    :: Arrays t
    => Strength
    -> Context () aenv () aenv'
    -> Size OpenAcc aenv'
    -> OpenAcc aenv t
    -> LiftedOpenAcc aenv' t
vectoriseOpenAcc strength ctx size (OpenAcc a) = liftPreOpenAcc vectoriseOpenAcc strength ctx size a

liftedSize
    :: forall acc aenv t. (Kit acc, Arrays t, Arrays (Vector' t))
    => acc aenv (Vector' t)
    -> Size acc aenv
liftedSize a =
  case flavour (undefined :: t) of
    ArraysFunit  -> Index (inject $ Aprj ZeroTupIdx a) IndexNil
    ArraysFarray -> ShapeSize (Shape $ inject $ Aprj (SuccTupIdx ZeroTupIdx) a)
    ArraysFtuple -> fromTup $ prod (Proxy :: Proxy Arrays) (undefined :: t)
  where
    fromTup :: (ArrRepr t ~ (l,e), IsAtuple t) => ProdR Arrays (TupleRepr t) -> Size acc aenv
    fromTup ProdRunit     = Const 0
    fromTup (ProdRsnoc _) = convince a
      where
        convince :: forall f l a e. (ArrRepr t ~ (l,e), TupleRepr t ~ (f,a), Arrays a)
                 => acc aenv (Vector' t)
                 -> Size acc aenv
        convince a | IsC <- isArraysFlat (undefined :: a)
                   = liftedSize $^ Aprj ZeroTupIdx a

-- |Lift a unary open array function
--
liftOpenAfun1
    :: forall aenv aenv' a b.
       Strength
    -> Context () aenv () aenv'
    -> OpenAfun aenv  (a -> b)
    -> OpenAfun aenv' (Vector' a -> Vector' b)
liftOpenAfun1 strength ctx (Alam (Abody f))
  | trace "liftOpenAfun1" ("Starting " ++ show strength ++ " vectorisation") True
  , IsC <- isArraysFlat (undefined :: a)
  , IsC <- isArraysFlat (undefined :: b)
  = case vectoriseOpenAcc Conservative (PushLAccC ctx) (liftedSize avar0) f of
      -- In the case that the body of the function does not depend on its argument,
      -- conservative vectorisation will return the unmodified body. In this,
      -- we just need to replicate the result.
      AvoidedAcc a' -> Alam . Abody $ replicateC (inject $ Unit (liftedSize avar0)) a'
      -- Otherwise, we have the lifted body.
      LiftedAcc  a' -> Alam . Abody $ a'
liftOpenAfun1 _ _ _
  = $internalError "liftOpenAfun1" "unreachable"

-- |Lift a binary open array function
--
liftOpenAfun2
    :: forall aenv aenv' a b c.
       Strength
    -> Context () aenv () aenv'
    -> Size OpenAcc aenv'
    -> OpenAfun aenv  (a -> b -> c)
    -> OpenAfun aenv' (Vector' a -> Vector' b -> Vector' c)
liftOpenAfun2 strength ctx sz (Alam (Alam (Abody f)))
  | trace "liftOpenAfun2" ("Starting " ++ show strength ++ " vectorisation") True
  , IsC <- isArraysFlat (undefined :: a)
  , IsC <- isArraysFlat (undefined :: b)
  , IsC <- isArraysFlat (undefined :: c)
  = case vectoriseOpenAcc Conservative (PushLAccC . PushLAccC $ ctx) (weakenA2 sz) f of
      -- In the case that the body of the function does not depend on its argument,
      -- conservative vectorisation will return the unmodified body. In this,
      -- we just need to replicate the result.
      AvoidedAcc a' -> Alam . Alam . Abody $ replicateC (inject $ Unit (weakenA2 sz)) a'
      -- Otherwise, we have the lifted body.
      LiftedAcc  a' -> Alam . Alam . Abody $ a'
liftOpenAfun2 _ _ _ _
  = $internalError "liftOpenAfun2" "unreachable"

-- |The core of the lifting transformation for array expression.
--
liftPreOpenAcc
    :: forall acc aenv aenv' t. (Kit acc, Arrays t)
    => VectoriseAcc acc
    -> Strength
    -> Context () aenv () aenv'
    -> Size acc aenv'
    -> PreOpenAcc acc aenv t
    -> LiftedAcc acc aenv' t
liftPreOpenAcc vectAcc strength ctx size acc
  | IsC <- isArraysFlat (undefined :: t)
  = case acc of
    Alet a b                -> aletL a b
    Avar ix                 -> avarL ix
    Atuple tup              -> atupleL tup
    Aprj tup a              -> aprjL tup a
    Apply f a               -> applyL f a
    Aforeign ff afun as     -> foreignL ff afun as
    Acond p t e             -> acondL p t e
    Awhile p it i           -> awhileL p it i
    Use a                   -> useL a
    Unit e                  -> unitL e
    Reshape e a             -> reshapeL e a
    Generate e f            -> generateL e f
    Replicate sl slix a     -> replicateL sl slix a
    Slice sl a slix         -> sliceL sl a slix
    Map f a                 -> mapL f a
    ZipWith f a1 a2         -> zipWithL f a1 a2
    Fold f z a              -> foldL f z a
    Fold1 f a               -> fold1L f a
    FoldSeg f z a s         -> foldSegL f z a s
    Fold1Seg f a s          -> fold1SegL f a s
    Scanl f z a             -> scanlL f z a
    Scanl' f z a            -> scanl'L f z a
    Scanl1 f a              -> scanl1L f a
    Scanr f z a             -> scanrL f z a
    Scanr' f z a            -> scanr'L f z a
    Scanr1 f a              -> scanr1L f a
    Permute f1 a1 f2 a2     -> permuteL f1 a1 f2 a2
    Backpermute sh f a      -> backpermuteL sh f a
    Stencil f b a           -> stencilL f b a
    Stencil2 f b1 a1 b2 a2  -> stencil2L f b1 a1 b2 a2

    -- Transform only appears as part of subsequent optimsations.
    Transform{}             -> $internalError "liftPreOpenAcc" "unexpected 'Transform'"
    Collect{}               -> error "Nested sequence"

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

    liftedAcc :: forall aenv t. acc aenv (Vector' t) -> LiftedAcc acc aenv t
    liftedAcc | HoistOnly <- strength
              = hoistingOnlyError
              | otherwise
              = trace "liftPreOpenAcc" ("Lifting Acc term " ++ showPreAccOp acc) . LiftedAcc

    liftE :: forall env env' aenv aenv' e.
             Context env aenv env' aenv'
          -> Size acc aenv'
          -> PreOpenExp acc env aenv e
          -> PreOpenAcc acc aenv' (Vector e)
    liftE | HoistOnly <- strength
          = hoistingOnlyError
          | otherwise
          = liftExp vectAcc strength

    cvtE :: forall e.
            PreExp acc aenv e
         -> LiftedExp acc () aenv' e
    cvtE e | avoidLifting
           , Avoided (b,e') <- avoidE e
           = AvoidedExp b e'
           | otherwise
           = trace "liftPreOpenAcc" ("Expression had to be lifted: " ++ showPreExpOp e)
           $ LiftedExp $ inject $ liftE ctx size e

    cvtE' :: forall e.
             PreExp acc aenv e
          -> LiftedExp acc () aenv' e
    cvtE' e | Avoided (b,e') <- avoidE e
            = AvoidedExp b e'
            | otherwise
            = trace "liftPreOpenAcc" ("Expression had to be lifted: " ++ showPreExpOp e)
            $ LiftedExp $ inject $ liftE ctx size e

    cvtT :: forall t.
            Atuple (acc aenv) t
         -> Atuple (LiftedAcc acc aenv') t
    cvtT NilAtup                                                           = NilAtup
    cvtT (SnocAtup t (a::acc aenv a)) | IsC <- isArraysFlat (undefined::a) = SnocAtup (cvtT t) (cvtA a)

    liftTupleIdx :: forall t a. TupleIdx t a -> TupleIdx (LiftedTupleRepr t) (Vector' a)
    liftTupleIdx ZeroTupIdx      = ZeroTupIdx
    liftTupleIdx (SuccTupIdx ix) = SuccTupIdx (liftTupleIdx ix)

    -- We are slightly introducing nested parallelism here in that we are  embedding `Aprj` in an
    -- `Exp`. Fusion should always be able to handle this case properly however.
    liftedSize :: forall aenv t. (Arrays t, Arrays (Vector' t))
               => acc aenv (Vector' t)
               -> Size acc aenv
    liftedSize a = case flavour (undefined :: t) of
                     ArraysFunit  -> Index (inject $ Aprj ZeroTupIdx a) IndexNil
                     ArraysFarray -> ShapeSize (Shape $ segments a)
                     ArraysFtuple -> fromTup $ prod (Proxy :: Proxy Arrays) (undefined :: t)
      where
        fromTup :: (ArrRepr t ~ (l,e), IsAtuple t) => ProdR Arrays (TupleRepr t) -> Size acc aenv
        fromTup ProdRunit     = Const 0
        fromTup (ProdRsnoc _) = convince a
          where
            convince :: forall f l a e. (ArrRepr t ~ (l,e), TupleRepr t ~ (f,a), Arrays a)
                     => acc aenv (Vector' t)
                     -> Size acc aenv
            convince a | IsC <- isArraysFlat (undefined :: a)
                       = liftedSize $^ Aprj ZeroTupIdx a

    liftAfun1 :: forall a b. (Arrays a, Arrays b)
              => PreOpenAfun acc aenv (a -> b)
              -> ( PreOpenAfun acc aenv' (Vector' a -> Vector' b)
                 , Maybe (PreOpenAfun acc aenv' (a -> b)))
    liftAfun1 (Alam (Abody b))
      | IsC <- isArraysFlat (undefined :: a)
      , IsC <- isArraysFlat (undefined :: b)
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
      = let l = Alam (Abody (inject $ liftE (PushLExpC ctx) (ShapeSize (Shape avar0)) e))
        in case (avoidF f) of
             Avoided (b, Lam (Body e')) | avoidLifting
                                        -> (l, Just $ AvoidedFun b (Lam (Body e')))
             _                          -> trace "liftPreOpenAcc" "Function had to be lifted"
                                        $  (l, Nothing)
    cvtF1 _              = $internalError "liftPreOpenAc.cvtF1" "inconsistent valuation"

    cvtF2 :: forall a b c. (Elt a, Elt b, Elt c)
          => PreFun  acc  aenv  (a -> b -> c)
          -> ( PreOpenAfun acc aenv' (Vector a -> Vector b -> Vector c)
             , Maybe (AvoidedFun acc () aenv' (a -> b -> c)))
    cvtF2 f@(Lam (Lam (Body e)))
      = let l = Alam (Alam (Abody (inject $ liftE (PushLExpC (PushLExpC ctx))
                                                  (ShapeSize (Shape avar0))
                                                  e)))

        in case (avoidF f) of
             Avoided (b, Lam (Lam (Body e'))) | avoidLifting
                                              -> (l, Just $ AvoidedFun b (Lam (Lam (Body e'))))
             _                                -> trace "liftPreOpenAcc" "Function had to be lifted"
                                              $  (l, Nothing)
    cvtF2 _              = $internalError "liftPreOpenAc.cvtF2" "inconsistent valuation"

    cvtF2' :: forall a b c. (Elt a, Elt b, Elt c)
           => PreFun  acc aenv  (a -> b -> c)
           -> ( PreOpenAfun acc aenv' (Vector a -> Vector b -> Vector c)
              , Maybe (AvoidedFun acc () aenv' (a -> b -> c)))
    cvtF2' f@(Lam (Lam (Body e)))
      = let l = Alam (Alam (Abody (inject $ liftE (PushLExpC (PushLExpC ctx))
                                                  (ShapeSize (Shape avar0))
                                                  e)))
        in case (avoidF f) of
             Avoided (b, Lam (Lam (Body e'))) -> (l, Just $ AvoidedFun b (Lam (Lam (Body e'))))
             _                                -> trace "liftPreOpenAcc" ("Function had to be lifted")
                                              $  (l, Nothing)
    cvtF2' _
      = $internalError "liftPreOpenAc.cvtF2'" "inconsistent valuation"

    unzip :: forall aenv a b sh. (Elt a, Elt b, Shape sh)
          => acc aenv (Array sh (a,b))
          -> acc aenv (Array sh a, Array sh b)
    unzip a = inject
            $ Alet a
            $ atup (inject $ Map (fun1 fstE) avar0)
                   (inject $ Map (fun1 sndE) avar0)

    construct :: forall aenv e sh. (Elt e, Shape sh)
              => acc aenv (Segments sh)
              -> acc aenv (Vector e)
              -> acc aenv (LiftedArray sh e)
    construct segs vals
      = inject
      $ Atuple (SnocAtup (SnocAtup NilAtup segs) vals)

    segments :: forall aenv e sh. (Elt e, Shape sh)
             => acc aenv (LiftedArray sh e)
             -> acc aenv (Segments sh)
    segments arrs
      = inject $ Aprj (SuccTupIdx ZeroTupIdx) arrs

    values :: forall aenv e sh. (Elt e, Shape sh)
           => acc aenv (LiftedArray sh e)
           -> acc aenv (Vector e)
    values arrs
      = inject $ Aprj ZeroTupIdx arrs

    lifted :: forall t. Arrays t => LiftedAcc acc aenv' t -> acc aenv' (Vector' t)
    lifted (AvoidedAcc a)   = replicateA a size
    lifted (LiftedAcc l)    = l

    liftedE :: forall t. Elt t => LiftedExp acc () aenv' t -> acc aenv' (Vector t)
    liftedE (AvoidedExp b e) = inject $ bind b $ replicateE e (sink b size)
    liftedE (LiftedExp  e) = e

    -- Unfortunately, IndexTail, IndexHead and IndexCons requires the Slice constraint, which we
    -- don't always have.
    indexSplit :: forall env aenv sh. Shape sh
               => PreOpenExp acc env aenv (sh:.Int)
               -> PreOpenExp acc env aenv (sh, Int)
    indexSplit sh = Let sh $ Let tail $ tup var0 (IndexHead head)
      where
        tail = IndexSlice ix slix var0

        head :: PreOpenExp acc ((env,(sh:.Int)),sh) aenv DIM1
        head = IndexSlice ix' slix' var1

        ix = sliceIndex (undefined :: Any sh :. Int)

        slix :: forall env. PreOpenExp acc env aenv (Any sh :. Int)
        slix = IndexCons IndexAny (Const (0 :: Int))

        slix' :: forall env. PreOpenExp acc (env,sh) aenv (None sh :. All)
        slix' = IndexCons (Tuple (SnocTup NilTup var0)) (Const ())

        ix' :: SliceIndex (EltRepr sh, ()) ((),Int) (EltRepr sh) (EltRepr sh, Int)
        ix' = SliceAll (sliceNoneIndex (undefined :: sh))

    indexCons :: forall env aenv sh. Shape sh
              => PreOpenExp acc env aenv sh
              -> PreOpenExp acc env aenv Int
              -> PreOpenExp acc env aenv (sh:.Int)
    indexCons t h = IndexFull (sliceIndex (undefined :: Any sh :. Int)) slix t
      where
        slix :: PreOpenExp acc env aenv (Any sh :. Int)
        slix = IndexCons IndexAny h

    -- Vector' versions of combinators
    -- ===============================

    aletL :: forall bnd. (Arrays bnd, Arrays t)
          => acc aenv bnd
          -> acc (aenv, bnd) t
          -> LiftedAcc acc aenv' t
    aletL bnd body | IsC <- isArraysFlat (undefined :: bnd)
                   , IsC <- isArraysFlat (undefined :: t)
                   = injectL
                   $ case (cvtA bnd) of
                       AvoidedAcc a -> over (Alet a) (Alet a)
                                    $ vectAcc strength (PushAccC ctx) (weakenA1 size) body
                       a            -> over (Alet (lifted a)) (Alet (lifted a))
                                    $ vectAcc strength (PushLAccC ctx) (weakenA1 size) body

    avarL :: Arrays t
          => Idx aenv t
          -> LiftedAcc acc aenv' t
    avarL | IsC <- isArraysFlat (undefined :: t)
          = cvtIx ctx
      where
        cvtIx :: forall env aenv env' aenv'. Arrays (Vector' t)
              => Context env aenv env' aenv'
              -> Idx aenv t
              -> LiftedAcc acc aenv' t
        cvtIx (PushLExpC d) ix             = weakenA1 (cvtIx d ix)
        --cvtIx (PushExpC  d)   ix           = weakenE1 (cvtIx d ix)
        cvtIx (PushLAccC _)   ZeroIdx      = liftedAcc $ avar0
        cvtIx (PushLAccC d)   (SuccIdx ix) = weakenA1 (cvtIx d ix)
        cvtIx (PushAccC  _)   ZeroIdx      = AvoidedAcc avar0
        cvtIx (PushAccC  d)   (SuccIdx ix) = weakenA1 (cvtIx d ix)
        cvtIx _               _            = $internalError "liftExp" "Inconsistent valuation"

    atupleL :: (Arrays t, IsAtuple t, Arrays (Vector' t))
            => Atuple (acc aenv) (TupleRepr t)
            -> LiftedAcc acc aenv' t
    atupleL t = case atl (cvtT t) of
                  Left (a,_)  -> AvoidedAcc (inject $ Atuple a)
                  Right a     -> case flavour (undefined :: t) of
                                   ArraysFunit  -> liftedAcc $^ Atuple (SnocAtup NilAtup (inject $ Unit size))
                                   ArraysFtuple -> liftedAcc $ inject $ Atuple a
                                   _            -> error "Absurd"
      where
        atl :: forall t.
               Atuple (LiftedAcc acc aenv') t
            -> Either (Atuple (acc aenv') t, Atuple (acc aenv') (LiftedTupleRepr t))
                      (Atuple (acc aenv') (LiftedTupleRepr t))
        atl NilAtup        = Left (NilAtup, NilAtup)
        atl (SnocAtup t a) = case atl t of
                               Left (av,li) | AvoidedAcc (a' :: acc aenv' a) <- a
                                            , IsC <- isArraysFlat (undefined :: a)
                                            -> Left (SnocAtup av a', SnocAtup li (replicateA a' size))
                                            | LiftedAcc (a' :: acc aenv' (Vector' a)) <- a
                                            , IsC <- isArraysFlat (undefined :: a)
                                            -> Right (SnocAtup li a')
                               Right li     | LiftedAcc (a' :: acc aenv' (Vector' a)) <- a
                                            , IsC <- isArraysFlat (undefined :: a)
                                            -> Right (SnocAtup li a')
                                            | AvoidedAcc (a' :: acc aenv' a) <- a
                                            , IsC <- isArraysFlat (undefined :: a)
                                            -> Right (SnocAtup li (replicateA a' size))


    aprjL :: forall a arrs. (Arrays a, Arrays arrs, IsAtuple arrs, Arrays (Vector' a))
          => TupleIdx (TupleRepr arrs) a
          -> acc aenv arrs
          -> LiftedAcc acc aenv' a
    aprjL tup a | IsC <- isArraysFlat (undefined :: arrs)
                , ArraysFtuple <- flavour (undefined :: arrs)
                = injectL $ over (Aprj tup) (Aprj (liftTupleIdx tup)) (cvtA a)
                | otherwise = error "Absurd"

    applyL :: forall a1 a2.
              (Arrays a1, Arrays a2, Arrays (Vector' a2))
           => PreOpenAfun acc aenv (a1 -> a2)
           -> acc aenv a1
           -> LiftedAcc acc aenv' a2
    applyL f a1 | IsC <- isArraysFlat (undefined :: a1)
                , let (lft, pln) = liftAfun1 f
                = case cvtA a1 of
                    AvoidedAcc a1' |  avoidLifting
                                   ,  Just f' <- pln
                                   -> AvoidedAcc $ inject $ Apply f' a1'
                                   |  otherwise
                                   -> liftedAcc $ inject $ Apply lft (replicateA a1' size)
                    a1'            -> liftedAcc $ inject $ Apply lft (lifted a1')

    foreignL :: (Arrays as, Arrays bs, Foreign asm)
             => asm                   (as -> bs)
             -> PreAfun     acc       (as -> bs)
             -> acc             aenv  as
             -> LiftedAcc   acc aenv' bs
    foreignL ff afun (cvtA -> AvoidedAcc as)
      = AvoidedAcc $ inject $ Aforeign ff afun as
    foreignL _  _    _
      = error $ nestedError "first" "foreign"

    acondL :: Arrays (Vector' t)
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
      = liftedAcc $ liftedCondC (liftedE p) (lifted t) (lifted e)

    -- TODO: Try to find a way to clean this up
    awhileL :: forall t. (Arrays t, Arrays (Vector' t))
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
           init  = avar0
           init' = inject $ Alet (values $ inject $ weakenA1 pred_l `subApply` init)
                          $ atup3 (weakenA1 init) avar0 (fromHOAS S.or avar0)

           pred' = Alam $ Abody $ inject $ Aprj ZeroTupIdx avar0

           iter' :: acc (aenv', s) (Vector' t)
                 -> acc (aenv', s) (Vector Bool)
                 -> acc (aenv', s) (Scalar Bool)
                 -> acc (aenv', s) (Vector' t, Vector Bool, Scalar Bool)
           iter' a f _ = let a' = liftedCondC f (inject $ weakenA1 iter_l `subApply` a) a
                             f' = fromHOAS (S.zipWith (S.&&*)) f (values $ inject $ weakenA1 pred_l `subApply` a')
                             c' = fromHOAS S.or f'
                         in atup3 a' f' c'

           iter'' :: PreOpenAfun acc aenv' ((Vector' t, Vector Bool, Scalar Bool)
                  -> (Vector' t, Vector Bool, Scalar Bool))
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
                               $ construct (inject $ replicateE (Const ()) size) (liftedE a)

    reshapeL :: forall sh sh' e.
                (Shape sh, Shape sh', Elt e)
             => PreExp acc aenv sh
             -> acc aenv (Array sh' e)
             -> LiftedAcc acc aenv' (Array sh e)
    reshapeL (cvtE -> sh) (cvtA -> a)
      | avoidLifting
      , AvoidedExp b sh' <- sh
      , AvoidedAcc a'    <- a
      = AvoidedAcc $ inject $ (bind b $ Reshape sh' (sink b a'))
      | otherwise
      = liftedAcc $ liftedReshapeC (liftedE sh) (lifted a)


    generateL :: forall sh e. (Elt e, Shape sh)
              => PreExp acc aenv sh
              -> PreFun acc aenv (sh -> e)
              -> LiftedAcc  acc aenv' (Array sh e)
    generateL (cvtE -> e) (cvtF1 -> (f_l, f_a))
      | avoidLifting
      , AvoidedExp b1 e' <- e
      , Just (AvoidedFun b2 f) <- f_a
      = AvoidedAcc
      $^ bind b2
      $  Alet (sink b2 $ inject $ bind b1 $ Unit e')
      $^ Generate (the avar0) (weakenA1 f)
      | avoidLifting
      , AvoidedExp b1 e' <- e
      =  AvoidedAcc
      $^ Alet (inject $ bind b1 $ Unit e')
      $^ Reshape (the avar0) (inject $ weakenA1 f_l `subApply` extentVector (the avar0))
      | otherwise
      = liftedAcc
      $^ Alet (liftedE e)
      $ construct avar0
                  (inject $ weakenA1 f_l `subApply` (enumSegC avar0))

    replicateL :: forall sh sl slix e co.
                  (Shape sh, Shape sl, Elt slix, Elt e)
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
      $^ Alet (inject $ Unit $ weakenA1 slix)
      $^ Backpermute (IndexFull sl (the avar0) (Shape avar1))
                     (Lam $ Body $ IndexSlice sl (weakenE1 (the avar0)) $ var0)
      $ avar1

    sliceL :: forall sh sl slix e co.
              (Shape sh, Shape sl, Elt slix, Elt e)
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
      $^ Alet (inject $ Unit $ weakenA1 slix)
      $^ Backpermute (IndexSlice sl (the avar0) (Shape avar1))
                     (fun1 $ IndexFull sl (weakenE1 (the avar0)))
      $ avar1

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
      | otherwise
      = liftedAcc
      $^ Alet (lifted a)
      $  construct (segments avar0) (inject $ weakenA1 f_l `Apply` values avar0)


    zipWithL :: forall sh a b c. (Elt a, Elt b, Elt c, Shape sh)
             => PreFun     acc aenv  (a -> b -> c)
             -> acc            aenv  (Array sh a)
             -> acc            aenv  (Array sh b)
             -> LiftedAcc  acc aenv' (Array sh c)
    zipWithL (cvtF2 -> (f_l, f_a)) (cvtA -> a) (cvtA -> b)
      | Just (AvoidedFun binds f) <- f_a
      , AvoidedAcc a'             <- a
      , AvoidedAcc b'             <- b
      = AvoidedAcc
      $^ bind binds
      $ ZipWith f (sink binds a') (sink binds b')
      | otherwise
      = liftedAcc
      $^ Alet (fromHOAS liftedZip (lifted a) (lifted b))
      $  construct (inject $ Aprj (SuccTupIdx . SuccTupIdx $ ZeroTupIdx) avar0)
                   (inject $ subApply2 (weakenA1 f_l) (inject $ Aprj (SuccTupIdx ZeroTupIdx) avar0)
                                                      (inject $ Aprj (ZeroTupIdx) avar0))

    foldL :: forall sh e. (Elt e, Shape sh)
          => PreFun acc     aenv  (e -> e -> e)
          -> PreExp acc     aenv  e
          -> acc            aenv  (Array (sh:.Int) e)
          -> LiftedAcc  acc aenv' (Array sh e)
    foldL (cvtF2' -> (_, Just (AvoidedFun b1 f))) (cvtE' -> AvoidedExp b2 z') (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a'    <- a
      = AvoidedAcc
      $^ bind b1
      $ Alet (sink b1 $^ bind b2 (Unit z'))
      $^ Fold (weakenA1 f) (the avar0) (weakenA1 $ sink b1 a')
      | otherwise
      = liftedAcc
      $^ bind b1
      $  Alet (sink b1 (lifted a))
      $^ Alet (unzip $^ Map (fun1 indexSplit) (segments avar0))
      $^ Alet (fromHOAS makeNonEmpty $ fstA avar0)
      $  construct avar0
      $^ Alet (weakenA3 $ sink b1 $^ bind b2 (Unit z'))
      $^ FoldSeg (weakenA4 f)
                 (the avar0)
                 (values avar3)
                 (replicateSegC avar1 (fromHOAS (S.zipWith (\sh h -> S.shapeSize sh S.==* 0 S.? (0,h))) (fstA avar2) (sndA avar2)))
    foldL _ _ _
      = error $ nestedError "first or second" "fold"

    fold1L :: forall sh e. (Elt e, Shape sh)
           => PreFun acc  aenv  (e -> e -> e)
           -> acc            aenv  (Array (sh:.Int) e)
           -> LiftedAcc  acc aenv' (Array sh e)
    fold1L (cvtF2' -> (_, Just (AvoidedFun b1 f))) (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a'    <- a
      = AvoidedAcc
      $^ bind b1
      $  Fold1 f (sink b1 a')
      | otherwise
      = liftedAcc
      $^ bind b1
      $  Alet (sink b1 (lifted a))
      $^ Alet (unzip $ inject $ Map (fun1 indexSplit) (segments avar0))
      $  construct (fstA avar0)
      $^ Fold1Seg (weakenA2 f)
                  (values avar1)
                  (replicateSegC (fstA avar0) (sndA avar0))
    fold1L _ _
      = error $ nestedError "first or second" "fold1"

    foldSegL :: forall sh e i. (Elt e, Shape sh, IsIntegral i, Elt i)
             => PreFun acc aenv (e -> e -> e)
             -> PreExp acc aenv e
             -> acc            aenv (Array (sh:.Int) e)
             -> acc            aenv (Segments i)
             -> LiftedAcc  acc aenv' (Array (sh:.Int) e)
    foldSegL (cvtF2' -> (_, Just (AvoidedFun b1 f))) (cvtE' -> AvoidedExp b2 z) (cvtA -> a) (cvtA -> segs)
      | avoidLifting
      , AvoidedAcc a'    <- a
      , AvoidedAcc segs' <- segs
      = AvoidedAcc
      $^ bind b1
      $  Alet (sink b1 . inject . bind b2 $ Unit z)
      $^ FoldSeg (weakenA1 f) (the avar0) (weakenA1 $ sink b1 a') (weakenA1 $ sink b1 segs')
      | otherwise
      = liftedAcc
      $^ bind b1
      $  Alet (sink b1 $ lifted a)
      $^ Alet (fromHOAS (S.map S.fst) $^ Map (fun1 indexSplit) (segments avar0))
      $^ Alet (weakenA2 $ sink b1 $ lifted segs)
      $  let

           segs' = inject $ ZipWith (fun2 indexCons)
                                    avar1
                                    (S.map S.unindex1 `fromHOAS` (segments avar0))
         in construct segs'
         $^ Alet (inject $ weakenA3 $ sink b1 $ bind b2 $ Unit z)
         $^ FoldSeg (weakenA4 f) (the avar0) (values avar3)
         $  fromHOAS replicateVectors avar2 avar1
    foldSegL _ _ _ _
      = error $ nestedError "first or second" "foldSeg"

    fold1SegL :: forall sh e i. (Elt e, Shape sh, IsIntegral i, Elt i)
              => PreFun acc aenv (e -> e -> e)
              -> acc            aenv (Array (sh:.Int) e)
              -> acc            aenv (Segments i)
              -> LiftedAcc  acc aenv' (Array (sh:.Int) e)
    fold1SegL (cvtF2' -> (_, Just (AvoidedFun b1 f))) (cvtA -> a) (cvtA -> segs)
      | avoidLifting
      , AvoidedAcc a'    <- a
      , AvoidedAcc segs' <- segs
      = AvoidedAcc
      $^ bind b1
      $  Fold1Seg f (sink b1 a') (sink b1 segs')
      | otherwise
      = liftedAcc
      $^ bind b1
      $  Alet (sink b1 $ lifted a)
      $^ Alet (fromHOAS (S.map S.fst) $^ Map (fun1 indexSplit) (segments avar0))
      $^ Alet (weakenA2 $ sink b1 $ lifted segs)
      $  let
           segs' = inject $ ZipWith (fun2 indexCons)
                                    avar1
                                    (S.map S.unindex1 `fromHOAS` (segments avar0))
         in construct segs'
         $^ Fold1Seg (weakenA3 f) (values avar2)
         $  fromHOAS replicateVectors avar1 avar0
    fold1SegL _ _ _
      = error $ nestedError "first" "foldSeg"

    scanl1L :: forall sh e. (Shape sh, Elt e)
            => PreFun acc  aenv  (e -> e -> e)
            -> acc            aenv  (Array (sh:.Int) e)
            -> LiftedAcc  acc aenv' (Array (sh:.Int) e)
    scanl1L _ _
      = error "TODO: vectorise scanl1L"
{--
    scanl1L (cvtF2' -> (_, Just (AvoidedFun b1 f))) (cvtA -> a)
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
--}

    scanlL :: forall sh e. (Shape sh, Elt e)
           => PreFun acc  aenv  (e -> e -> e)
           -> PreExp acc  aenv  e
           -> acc            aenv  (Array (sh:.Int) e)
           -> LiftedAcc  acc aenv' (Array (sh:.Int) e)
    scanlL _ _ _
      = error "TODO: vectorise scanlL"
{--
    scanlL (cvtF2' -> (_, Just (AvoidedFun b1 f))) (cvtE' -> AvoidedExp b2 z) (cvtA -> a)
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
--}

    scanl'L :: forall sh e. (Shape sh, Elt e)
            => PreFun acc  aenv  (e -> e -> e)
            -> PreExp acc  aenv  e
            -> acc            aenv  (Array (sh:.Int) e)
            -> LiftedAcc  acc aenv' (Array (sh:.Int) e, Array sh e)
    scanl'L _ _ _
      = error "TODO: vectorise scanl'"
{--
    scanl'L (cvtF2' -> (_, Just (AvoidedFun b1 f))) (cvtE' -> AvoidedExp b2 z) (cvtA -> a)
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
      $^ Alet (S.map S.unindex1 `fromHOAS` segments avar0)
      $^ Alet (values avar1)
      $^ Alet (weakenA3 $ sink b1 $ inject $ bind b2 $ Unit z)
      $^ Alet (values $ scanlLift (weakenA4 f) (the avar0) avar3)
      $  fromHOAS
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
               $ SnocAtup (SnocAtup NilAtup (liftedArray (S.map S.index1 seg') body))
                          (liftedArray (S.fill (S.shape sums) S.index0) sums))
            avar3
            avar2
            avar0
    scanl'L _ _ _
      = error $ nestedError "first or second" "scanl"
--}

    scanr1L :: forall sh e. (Shape sh, Elt e)
            => PreFun acc  aenv  (e -> e -> e)
            -> acc            aenv  (Array (sh:.Int) e)
            -> LiftedAcc  acc aenv' (Array (sh:.Int) e)
    scanr1L _ _
      = error "TODO: vectorise scanr1"
{--
    scanr1L (cvtF2' -> (_, Just (AvoidedFun b1 f))) (cvtA -> a)
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
--}

    scanrL :: forall sh e. (Shape sh, Elt e)
           => PreFun acc  aenv  (e -> e -> e)
           -> PreExp acc  aenv  e
           -> acc            aenv  (Array (sh:.Int) e)
           -> LiftedAcc  acc aenv' (Array (sh:.Int) e)
    scanrL _ _ _
      = error "TODO: vectorise scanr"
{--
    scanrL (cvtF2' -> (_, Just (AvoidedFun b1 f))) (cvtE' -> AvoidedExp b2 z) (cvtA -> a)
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
--}

    scanr'L :: forall sh e. (Shape sh, Elt e)
            => PreFun acc  aenv  (e -> e -> e)
            -> PreExp acc  aenv  e
            -> acc            aenv  (Array (sh:.Int) e)
            -> LiftedAcc  acc aenv' (Array (sh:.Int) e, Array sh e)
    scanr'L _ _ _
      = error "TODO: vectorise scanr'"
{--
    scanr'L (cvtF2' -> (_, Just (AvoidedFun b1 f))) (cvtE' -> AvoidedExp b2 z) (cvtA -> a)
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
      $^ Alet (segments avar0)
      $^ Alet (values avar1)
      $^ Alet (weakenA3 $ sink b1 $ inject $ bind b2 $ Unit z)
      $^ Alet (values $ scanrLift (weakenA4 f) (the avar0) avar3)
      $  fromHOAS
            (\seg vec vec' ->
              let
                -- reduction values
                seg'        = S.map (+1) $ S.map S.unindex1 seg
                heads       = P.fst $ S.scanl' (+) 0 seg'
                sums        = S.backpermute (S.shape seg) (\ix -> S.index1 $ heads S.! ix) vec'

                -- body segments
                inc         = S.scanl1 (+) $ mkHeadFlags seg
                body        = S.backpermute (S.shape vec)
                                            (\ix -> S.index1 $ S.unindex1 ix + inc S.! ix)
                                            vec'
              in S.Acc . S.Atuple
               $ SnocAtup (SnocAtup NilAtup (liftedArray (S.map S.index1 seg') body))
                          (liftedArray (S.fill (S.shape sums) S.index0) sums))
            avar3
            avar2
            avar0
    scanr'L _ _ _
      = error $ nestedError "first or second" "scanr'"
--}

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
      $^ Alet (liftedE sh)
      $  liftedBackpermuteC avar0
                            (inject $ weakenA1 f_l `subApply` enumSegC avar0)
                            (weakenA1 $ lifted a)

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
    permuteL (cvtF2' -> (_, Just (AvoidedFun b comb))) (cvtA -> defs) (cvtF1 -> (p_l,p_a)) (cvtA -> a)
      =  trace "permuteL" ("Lifting permute: " ++ show (isJust p_a))
      $  liftedAcc
      $^ bind b
      $  Alet (sink b $ lifted defs)
      $^ Alet (weakenA1 . sink b $ lifted a)
      $  let init     = avar0
             defaults = avar1
             shapes   = segments init
             shapes'  = segments defaults
             enums    = enumSegC shapes
             ixs      = weakenA2 (sink b p_l) `subApply` enums
             ixs'     = asOffsetsOfC (construct shapes $^ ixs) shapes'
             vals     = Permute (weakenA2 $ comb)
                                (values defaults)
                                (fun1 (ixs' `Index`))
                                (values init)
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

{--
    scanl1Lift :: forall aenv e. Elt e
               => PreFun acc aenv (e -> e -> e)
               -> acc aenv (LiftedArray DIM1 e)
               -> acc aenv (LiftedArray DIM1 e)
    scanl1Lift f a
      = inject
      $  Alet a
      $  construct (segments avar0)
      $  sndA
      $  unzip
      $^ Scanl1 (weakenA1 $ segmented f)
      $  let
           flags :: forall aenv e. Elt e => acc (aenv, LiftedArray DIM1 e) (Vector Int)
           flags = fromHOAS mkHeadFlags (segments avar0)
         in fromHOAS S.zip flags (values avar0)

    scanlLift :: forall aenv e. Elt e
              => PreFun acc aenv (e -> e -> e)
              -> PreExp acc aenv e
              -> acc            aenv (LiftedArray DIM1 e)
              -> acc            aenv (LiftedArray DIM1 e)
    scanlLift f z a
      =  scanl1Lift f
      $^ Alet a
      $^ Alet (segments avar0)
      $^ Alet (values avar1)
      $^ Alet (weakenA3 $ inject $ Unit z)
      $  fromHOAS
          (\seg vec z ->
             let
              seg'        = S.map (S.ilift1 (+1)) seg
              vec'        = S.permute const
                                      (S.fill (S.index1 $ S.size vec + S.size seg) (S.the z))
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
               -> acc aenv (LiftedArray DIM1 e)
               -> acc aenv (LiftedArray DIM1 e)
    scanr1Lift f a
      = inject
      $  Alet a
      $  construct (segments avar0)
      $  sndA
      $  unzip
      $^ Scanr1 (weakenA1 $ segmented f)
      $  let
           flags :: forall aenv e. Elt e => acc (aenv, LiftedArray DIM1 e) (Vector Int)
           flags = fromHOAS mkTailFlags (segments avar0)
         in fromHOAS S.zip flags (values avar0)

    scanrLift :: forall aenv e. Elt e
              => PreFun acc aenv (e -> e -> e)
              -> PreExp acc aenv e
              -> acc            aenv (LiftedArray DIM1 e)
              -> acc            aenv (LiftedArray DIM1 e)
    scanrLift f z a
      =  scanr1Lift f
      $^ Alet a
      $^ Alet (segments avar0)
      $^ Alet (values avar1)
      $^ Alet (weakenA3 $ inject $ Unit z)
      $  fromHOAS
          (\seg vec z ->
             let
              seg'        = S.map (S.ilift1 (+1)) seg
              vec'        = S.permute const
                                      (S.fill (S.index1 $ S.size vec + S.size seg) (S.the z))
                                      (\ix -> S.index1 $ S.unindex1 ix + inc S.! ix - 1)
                                      vec
              flags       = mkHeadFlags seg
              inc         = S.scanl1 (+) flags
             in liftedArray seg' vec')
          avar2
          avar1
          avar0
--}

    extentVector :: forall sh aenv. Shape sh
                 => PreExp acc aenv sh
                 -> acc            aenv (Vector sh)
    extentVector sh =  inject
                    $  Alet (inject $ Unit sh)
                    $^ Reshape (IndexCons (Const ()) (ShapeSize (the avar0)))
                    $^ Generate (the avar0)
                    $  fun1 id

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

-- |Performs the lifting transform on a given scalar expression.
--
-- Because lifting is performed in the presence of higher dimensional arrays, the output of the
-- transform has an extra element in the environment, the shape of the output array.
--
liftExp :: forall acc env env' aenv aenv' e. Kit acc
        => VectoriseAcc acc
        -> Strength
        -> Context env aenv env' aenv'
        -> Size acc aenv'
        -> PreOpenExp acc env       aenv  e
        -> PreOpenAcc acc aenv' (Vector e)
liftExp vectAcc strength ctx size exp
  = case exp of
      Let bnd body              -> letL bnd body
      Var ix                    -> varL ctx ix id id
      Const c                   -> replicateE size (Const c)
      Tuple tup                 -> liftTuple vectAcc strength ctx size tup
      Prj ix t                  -> Map (fun1 (Prj ix)) (cvtE t)
      IndexNil                  -> replicateE size IndexNil
      IndexAny                  -> replicateE size IndexAny
      IndexCons sh sz           -> ZipWith (fun2 IndexCons) (cvtE sh) (cvtE sz)
      IndexHead sh              -> Map (fun1 IndexHead) (cvtE sh)
      IndexTail sh              -> Map (fun1 IndexTail) (cvtE sh)
      IndexSlice x ix sh        -> ZipWith (fun2 (IndexSlice x)) (cvtE ix) (cvtE sh)
      IndexFull x ix sl         -> ZipWith (fun2 (IndexFull x)) (cvtE ix) (cvtE sl)
      ToIndex sh ix             -> ZipWith (fun2 ToIndex) (cvtE sh) (cvtE ix)
      FromIndex sh ix           -> ZipWith (fun2 FromIndex) (cvtE sh) (cvtE ix)
      Cond p t e                -> condL p t e
      While p it i              -> whileL p it i
      PrimConst c               -> replicateE size (PrimConst c)
      PrimApp f x               -> Map (fun1 (PrimApp f)) (cvtE x)
      Index a sh                -> indexL a sh
      LinearIndex a i           -> linearIndexL a i
      Shape a                   -> shapeL a
      ShapeSize sh              -> Map (fun1 ShapeSize) (cvtE sh)
      Intersect s t             -> ZipWith (fun2 Intersect) (cvtE s) (cvtE t)
      Union s t                 -> ZipWith (fun2 Union) (cvtE s) (cvtE t)
      Foreign ff f e            -> Map (fun1 (Foreign ff f)) (cvtE e)
  where
    avoidLifting :: Bool
    avoidLifting | Conservative <- strength = True
                 | HoistOnly    <- strength = True
                 | otherwise                = False

    lifted :: forall t. Arrays t => LiftedAcc acc aenv' t -> acc aenv' (Vector' t)
    lifted (AvoidedAcc a)   = replicateA a size
    lifted (LiftedAcc l)    = l

    cvtE :: forall e. PreOpenExp acc env aenv e
         -> acc aenv' (Vector e)
    cvtE exp' = inject $ liftExp vectAcc strength ctx size exp'

    cvtA :: forall sh' e'. (Elt e', Shape sh')
         => acc aenv (Array sh' e')
         -> LiftedAcc acc aenv' (Array sh' e')
    cvtA a | EmbedContext ctx' wk <- embedContext ctx
           = vectAcc strength ctx' size (weaken wk a)

    cvtF1 :: PreOpenFun acc env aenv (a -> b)
          -> PreOpenAfun acc aenv' (Vector a -> Vector b)
    cvtF1 (Lam (Body f)) = Alam . Abody
                         $ inject
                         $ liftExp vectAcc strength (PushLExpC ctx) (ShapeSize (Shape avar0)) f
    cvtF1 _              = $internalError "liftExp" "Inconsistent valuation"

    replicateE :: forall e aenv. Elt e
               => Size acc aenv
               -> PreExp acc aenv e
               -> PreOpenAcc acc aenv (Vector e)
    replicateE s c = Generate (index1 s) (Lam (Body $ weakenE1 c))

    -- Lifted versions of operations
    -- ==============================

    varL :: forall env aenv env'' aenv''. Elt e
         => Context env aenv env'' aenv''
         -> Idx env e
         -> (forall e. Idx env''  e -> Idx env'  e)
         -> (forall a. Idx aenv'' a -> Idx aenv' a)
         -> PreOpenAcc acc aenv' (Vector e)
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
         -> PreOpenAcc acc aenv' (Vector e)
    letL bnd body = Alet bnd' (inject body')
      where
        bnd'  = cvtE bnd

        body' :: PreOpenAcc acc (aenv', Vector bnd_t) (Vector e)
        body' = liftExp vectAcc strength (PushLExpC ctx) (weakenA1 size) body

    condL :: Elt e
          => PreOpenExp acc env     aenv  Bool
          -> PreOpenExp acc env     aenv  e
          -> PreOpenExp acc env     aenv  e
          -> PreOpenAcc acc aenv' (Vector e)
    condL p t e = ZipWith (fun2 decide) (cvtE p) (inject $ ZipWith (fun2 tup) (cvtE t) (cvtE e))
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
           => PreOpenFun acc env aenv  (e -> Bool)
           -> PreOpenFun acc env aenv  (e -> e)
           -> PreOpenExp acc env aenv  e
           -> PreOpenAcc acc     aenv' (Vector e)
    whileL p it i = Aprj (SuccTupIdx ZeroTupIdx) (inject $ Awhile p' it' i')
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
        i'  = cvtE i `atup` inject (replicateE size (Const True))

    indexL :: forall sh'. (Elt e, Shape sh')
           => acc                aenv  (Array sh' e)
           -> PreOpenExp acc env aenv  sh'
           -> PreOpenAcc acc     aenv' (Vector e)
    indexL (cvtA -> a) (cvtE -> ix)
      | avoidLifting
      , AvoidedAcc a' <- a
      = Backpermute (index1 size) (fun1 (Index ix)) a'
      | otherwise
      =  extract
      $  liftedIndexC (lifted a) ix

    linearIndexL :: forall sh'. (Elt e, Shape sh')
                 => acc                aenv  (Array sh' e)
                 -> PreOpenExp acc env aenv  Int
                 -> PreOpenAcc acc     aenv' (Vector e)
    linearIndexL (cvtA -> a) (cvtE -> ix)
      | avoidLifting
      , AvoidedAcc a' <- a
      =  Alet a'
      $^ Generate (index1 $ weakenA1 size)
                  (Lam $ Body $ LinearIndex avar0 $ Index (weakenA1 ix) $ var0)
      | otherwise
      = extract $
        fromHOAS liftedLinearIndex (lifted a) ix

    shapeL :: forall e'. (Shape e, Elt e')
           => acc            aenv  (Array e e')
           -> PreOpenAcc acc aenv' (Vector e)
    shapeL (cvtA -> a)
      | avoidLifting
      , AvoidedAcc a' <- a
      =  Alet a'
      $^ replicateE (weakenA1 size) (Shape avar0)
      | otherwise
      = extract
      $ segments `fromHOAS` (lifted a)


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
--
liftTuple
    :: forall acc env aenv env' aenv' e. (Elt e, Kit acc, IsTuple e)
    => VectoriseAcc acc
    -> Strength
    -> Context env aenv env' aenv'
    -> Size acc aenv'
    -> Tuple (PreOpenExp acc env aenv) (TupleRepr e)
    -> PreOpenAcc acc aenv' (Vector e)
liftTuple vectAcc strength ctx size t = cvtT t (liftExp vectAcc strength ctx size) gen size
  where
    cvtT :: forall t aenv'.
            Tuple (PreOpenExp acc env aenv) t
         -> (forall e. PreOpenExp acc env aenv e -> PreOpenAcc acc aenv' (Vector e))
         -> (Size acc (ExpandEnv aenv' (VectorsOfTupleRepr t)) -> PreOpenAcc acc (ExpandEnv aenv' (VectorsOfTupleRepr t)) (Vector e))
         -> Size acc aenv'
         -> PreOpenAcc acc aenv'                                    (Vector e)
    cvtT NilTup        _    arr size = arr size
    cvtT(SnocTup t' e) lift arr size = Alet (inject $ lift e) (inject $ cvtT t' lift' arr (weakenA1 size))
      where
        lift' :: forall e e'. PreOpenExp acc env aenv e -> PreOpenAcc acc (aenv', Vector e') (Vector e)
        lift' = weakenA1 . lift

    gen :: Size acc (TupleEnv aenv' e) -> PreOpenAcc acc (TupleEnv aenv' e) (Vector e)
    gen size = Generate (index1 size) (Lam (Body (Tuple t')))
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

-- instance Kit acc => Show (Avoid PreOpenFun acc env aenv e) where
--   show (Avoided (_,e)) = "lets ...\n" ++ show e
--   show Unavoided       = "Unavoided"

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
        IndexAny            -> simple IndexAny
        IndexSlice x ix sh  -> cvtE2 (IndexSlice x) ix sh
        IndexFull x ix sl   -> cvtE2 (IndexFull x) ix sl
        ToIndex sh ix       -> cvtE2 ToIndex sh ix
        FromIndex sh ix     -> cvtE2 FromIndex sh ix
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
           -> Size acc aenv0'
           -> ExtendContext acc aenv0' aenv1
liftExtend _ _ BaseEnv ctx _
  = ExtendContext ctx BaseEnv
liftExtend k strength (PushEnv env a) ctx size
  | ExtendContext ctx' env' <- liftExtend k strength env ctx size
  = case k strength ctx' (sink env' size) a of
      AvoidedAcc a' -> ExtendContext (PushAccC ctx')  (PushEnv env' a')
      LiftedAcc  a' | IsC <- isArraysFlat (undefined :: aenv1 ~ (aenv1', a) => a)
                    -> ExtendContext (PushLAccC ctx') (PushEnv env' a')

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

-- Vector' operations
-- ------------------

values :: forall sh e. (Shape sh, Elt e) => S.Acc (LiftedArray sh e) -> S.Acc (Vector e)
values a = S.Acc $ S.Aprj ZeroTupIdx a

segments :: forall sh e. (Shape sh, Elt e) => S.Acc (LiftedArray sh e) -> S.Acc (Segments sh)
segments a = S.Acc $ S.Aprj (SuccTupIdx ZeroTupIdx) a

liftedArray :: (Shape sh, Elt e) => S.Acc (Segments sh) -> S.Acc (Vector e) -> S.Acc (LiftedArray sh e)
liftedArray segs vals = S.Acc $ S.Atuple $ SnocAtup (SnocAtup NilAtup segs) vals

asAtuple :: forall a. (Arrays a, IsAtuple a) => S.Acc a -> Atuple S.Acc (TupleRepr a)
asAtuple a = tOA (prod (Proxy :: Proxy Arrays) (undefined :: a)) id
 where
   tOA :: forall t. ProdR Arrays t -> (forall e. TupleIdx t e -> TupleIdx (TupleRepr a) e) -> Atuple S.Acc t
   tOA ProdRunit     _   = NilAtup
   tOA (ProdRsnoc t) ixt = SnocAtup (tOA t (ixt . SuccTupIdx)) (S.Acc $ S.Aprj (ixt ZeroTupIdx) a)

replicate
    :: forall a. Arrays a
    => S.Exp Int
    -> S.Acc a
    -> S.Acc (Vector' a)
replicate size a =
  case flavour (undefined :: a) of
    ArraysFunit  -> S.Acc  $ S.Atuple $ SnocAtup NilAtup $ S.unit size
    ArraysFarray -> let values = S.flatten $ S.replicate (S.lift (Z:.All:.size)) (S.flatten a)
                        segs   = S.fill (S.index1 $ S.lift size) (S.shape a)
                    in liftedArray segs values
    ArraysFtuple
      | IsC <- isArraysFlat (undefined :: a)
      -> S.Acc $ S.Atuple $ replicateT (asAtuple a)
  where
    replicateT :: Atuple S.Acc t -> Atuple S.Acc (LiftedTupleRepr t)
    replicateT NilAtup        = NilAtup
    replicateT (SnocAtup t (a' :: S.Acc a')) | IsC <- isArraysFlat (undefined :: a')
                                             = SnocAtup (replicateT t) (replicate size a')

-- A segmented replicate.
replicateSeg
    :: (Elt e, Shape sh)
    => S.Acc (Segments sh)
    -> S.Acc (Vector e)
    -> S.Acc (Vector e)
replicateSeg segs vals =
  let
      (offs, length) = offsets segs
      vShape = S.index1 $ S.the length
      negs   = S.fill (S.index1 $ S.the length + 1) (-1 :: S.Exp Int)
      flags  = S.permute max negs (\ix -> S.index1 (offs S.! ix)) (S.enumFromN (S.shape segs) 0)
      flags' = S.scanl1 (\a b -> b S./=* -1 S.? (b, a)) flags
      vals'  = S.backpermute vShape ((S.!) (S.map S.index1 flags')) vals
  in
  vals'

replicateVectors
    :: (Shape sh, Elt e)
    => S.Acc (Segments sh)
    -> S.Acc (LiftedArray DIM1 e)
    -> S.Acc (Vector e)
replicateVectors segs vecs = S.map (values vecs S.!!) $ S.zipWith (+) offs' ixs
  where
    segs' = replicateSeg segs (segments vecs)
    ixs   = enumSegLinear segs'
    offs  = replicateSeg segs (fst $ offsets segs)
    offs' = replicateSeg segs' offs

enumSegLinear
    :: forall sh. Shape sh
    => S.Acc (Segments sh)
    -> S.Acc (Vector Int)
enumSegLinear segs = enum_s
  where
    sizes  = S.map S.shapeSize segs
    ones   = S.fill (S.shape segs) (1 :: S.Exp Int)
    ones_s = replicateSeg segs ones
    enum_s = S.afst $ S.scanl'Seg (+) 0 ones_s sizes

enumSeg
    :: forall sh. Shape sh
    => S.Acc (Segments sh)
    -> S.Acc (Vector sh)
enumSeg segs = S.zipWith S.fromIndex shapes (enumSegLinear segs)
  where
    shapes = replicateSeg segs segs

-- |Convert segment shapes into segment offsets along with total length
--
offsets :: Shape sh => S.Acc (Segments sh) -> (S.Acc (Segments Int), S.Acc (Scalar Int))
offsets segs = S.scanl' (+) 0 $ S.map (S.shapeSize) segs

makeNonEmpty :: forall sh. Shape sh => S.Acc (Segments sh) -> S.Acc (Segments sh)
makeNonEmpty = S.map nonEmpty
  where
    nonEmpty = S.union (S.constant $ listToShape $ P.replicate (rank (ignore::sh)) 1)

-- RCE: I have a strong feeling this can be done better.
--
liftedCond
    :: forall a. Arrays a
    => S.Acc (Vector Bool)    -- condition
    -> S.Acc (Vector' a)      -- true-branch
    -> S.Acc (Vector' a)      -- false-branch
    -> S.Acc (Vector' a)
liftedCond pred th el
  | IsC <- isArraysFlat (undefined :: a)
  = case (flavour (undefined :: a)) of
      ArraysFunit  -> th
      ArraysFarray -> liftedCond1 th el
      ArraysFtuple -> S.Acc $ S.Atuple $ cvtT (prod (Proxy :: Proxy Arrays) (undefined :: a)) (asAtuple th) (asAtuple el)
  where
    cvtT :: ProdR Arrays t -> Atuple S.Acc (LiftedTupleRepr t) -> Atuple S.Acc (LiftedTupleRepr t) -> Atuple S.Acc (LiftedTupleRepr t)
    cvtT ProdRunit     NilAtup          NilAtup          = NilAtup
    cvtT (ProdRsnoc t) (SnocAtup t1 a1) (SnocAtup t2 a2) = SnocAtup (cvtT t t1 t2) (liftedCond pred a1 a2)

    liftedCond1 :: (Elt e, Shape sh) => S.Acc (LiftedArray sh e) -> S.Acc (LiftedArray sh e) -> S.Acc (LiftedArray sh e)
    liftedCond1 t e = liftedArray segs vals
      where
        segs_t = segments t
        segs_e = segments e
        segs   = S.zipWith (\f p -> let (t,e) = S.unlift p in f S.? (t, e))
                           pred
                           (S.zip segs_t segs_e)

        (offs_t, _) = offsets segs_t
        (offs_e, _) = offsets segs_e
        sz_v   = S.fold (+) 0 $ S.map S.shapeSize segs
        offs   = S.zipWith (\f p -> let (t,e) = S.unlift p in f S.? (t, e))
                           pred
                           (S.zip offs_t offs_e)
        flag_offs = replicateSeg segs $ S.zip pred offs

        vals_t = values t
        vals_e = values e
        ones   = S.fill (S.index1 $ S.the sz_v) (1 :: S.Exp Int)
        enums  = S.scanl1Seg (+) ones $ S.map S.shapeSize segs
        vals   = S.zipWith (\t ind -> let (f,s) = S.unlift t in f S.? (vals_t S.!! (s + ind), vals_e S.!! (s + ind)))
                           flag_offs
                           enums

--liftedAwhile :: forall t.
--                (Arrays t, Arrays (Vector' t))
--             => (S.Acc (Vector' t) -> S.Acc (Vector Bool))
--             -> (S.Acc (Vector' t) -> S.Acc (Vector' t))
--             -> S.Acc (Vector' t)
--             -> S.Acc (Vector' t)
--liftedAwhile pred iter init
--  = let
--      (a, _ :: S.Acc (Vector Bool), _ :: S.Acc (Scalar Bool)) = S.unlift $ S.awhile pred' iter' init'
--    in a
--  where
--    init' = let f = pred init
--            in S.lift (init, f, S.or f)

--    pred' :: S.Acc (Vector' t, Vector Bool, Scalar Bool) -> S.Acc (Scalar Bool)
--    pred' f = let (_ :: S.Acc (Vector' t), _ :: S.Acc (Vector Bool), c) = S.unlift f in c

--    iter' :: S.Acc (Vector' t, Vector Bool, Scalar Bool) -> S.Acc (Vector' t, Vector Bool, Scalar Bool)
--    iter' (S.unlift -> (a, f, _ :: S.Acc (Scalar Bool)))
--      = let a' = liftedCond f (iter a) a
--            f' = S.zipWith (S.&&*) f (pred a')
--            c' = S.or f'
--        in S.lift (a', f', c')

liftedReshape
    :: (Elt e, Shape sh, Shape sh')
    => S.Acc (Vector sh)
    -> S.Acc (LiftedArray sh' e)
    -> S.Acc (LiftedArray sh e)
liftedReshape extents a = liftedArray extents (values a)

--liftedGenerate :: (Elt e, Shape sh)
--               => S.Acc (Vector sh)
--               -> (S.Acc (Vector sh) -> S.Acc (Vector e))
--               -> S.Acc (LiftedArray sh e)
--liftedGenerate extents fun
--  = liftedArray extents (fun (enumSeg extents))

liftedZip
    :: (Elt a, Elt b, Shape sh)
    => S.Acc (LiftedArray sh a)
    -> S.Acc (LiftedArray sh b)
    -> S.Acc (Segments sh, Vector a, Vector b)
liftedZip as bs = S.lift (segs, valsA, valsB)
  where
    segsA = segments as
    segsB = segments bs

    segs = S.zipWith S.intersect segsA segsB
    enums = enumSeg segs

    enumsA = S.zipWith S.toIndex (replicateSeg segs segsA) enums
    enumsB = S.zipWith S.toIndex (replicateSeg segs segsB) enums

    (offsA, _) = offsets segsA
    (offsB, _) = offsets segsB
    valsA = S.map (values as S.!!) (S.zipWith (+) enumsA (replicateSeg segs offsA))
    valsB = S.map (values bs S.!!) (S.zipWith (+) enumsB (replicateSeg segs offsB))


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
--                  -> S.Acc (LiftedArray sh' e)
--liftedBackpermute shapes f a = liftedArray shapes vals'
--  where
--    segs   = segments a
--    vals   = values a
--    enums  = enumSeg shapes
--    ixs    = f enums
--    starts = replicateSeg shapes (fst $ offsets segs)
--    ixs'   = S.map S.index1 $ S.zipWith (+) starts (S.map S.shapeSize ixs)
--    vals'  = S.backpermute (S.shape ixs') (ixs' S.!) vals

liftedBackpermute
    :: (Elt e, Shape sh, Shape sh')
    => S.Acc (Vector sh')
    -> S.Acc (Vector sh)
    -> S.Acc (LiftedArray sh  e)
    -> S.Acc (LiftedArray sh' e)
liftedBackpermute shapes ixs a = liftedArray shapes vals'
  where
    segs   = segments a
    vals   = values a
    starts = replicateSeg shapes (fst $ offsets segs)
    ixs'   = S.map S.index1 $ S.zipWith (+) starts (S.map S.shapeSize ixs)
    vals'  = S.backpermute (S.shape ixs') (ixs' S.!) vals

--liftedPermute :: (Elt e, Shape sh, Shape sh')
--              => (S.Exp e -> S.Exp e -> S.Exp e)
--              -> S.Acc (LiftedArray sh' e)
--              -> (S.Acc (Vector sh) -> S.Acc (Vector sh'))
--              -> S.Acc (LiftedArray sh e)
--              -> S.Acc (LiftedArray sh' e)
--liftedPermute combine defaults perm init = liftedArray shapes' vals
--  where
--    shapes  = segments ini
--    shapes' = segments defaults
--    enums   = enumSeg shapes
--    ixs     = perm enums
--    ixs'    = asOffsetsOf (liftedArray shapes ixs) shapes'
--    vals    = S.permute combine (values defaults) (ixs' S.!) (values init)

asOffsetsOf
    :: (Shape sh, Shape sh')
    => S.Acc (LiftedArray sh sh')
    -> S.Acc (Segments sh')
    -> S.Acc (Vector DIM1)
asOffsetsOf ixs shapes' = S.map S.index1 $ S.zipWith (+) starts (S.map S.shapeSize (values ixs))
  where
    shapes = segments ixs
    starts = replicateSeg shapes (fst $ offsets shapes')

liftedIndex
    :: (Shape sh, Elt e)
    => S.Acc (LiftedArray sh e)
    -> S.Acc (Vector sh)
    -> S.Acc (Vector e)
liftedIndex vals ixs = S.backpermute (S.shape ixs) ixt (values vals)
  where
    segs = segments vals
    starts = fst $ offsets segs
    ixt ix = let
               start = starts S.! ix
               off   = S.toIndex (segs S.! ix) (ixs S.! ix)
             in S.index1 (start + off)


-- RCE: Using a generate here, as opposed to the backpermute used above, so that the linear indexing
-- is preserved. In reality, it may be better to do this as a backpermute or, equally as likely, i
-- makes no difference whatsoever.
liftedLinearIndex
    :: (Shape sh, Elt e)
    => S.Acc (LiftedArray sh e)
    -> S.Acc (Vector Int)
    -> S.Acc (Vector e)
liftedLinearIndex vals ixs = S.generate (S.shape ixs) (\ix -> values vals S.!! ((starts S.! ix) + ixs S.!ix))
  where
    starts = fst $ offsets (segments vals)

{--
-- |Compute head flags vector from segment vector for left-scans.
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
    (offset, len)       = S.scanl' (+) 0 $ S.map S.unindex1 seg
    zeros               = S.fill (S.index1  $ S.the len + 1) 0
    ones                = S.fill (S.index1  $ S.size offset) 1

-- |Compute tail flags vector from segment vector for right-scans. That is, the
-- flag is placed at the last place in each segment.
--
mkTailFlags :: S.Acc (Segments DIM1) -> S.Acc (Segments Int)
mkTailFlags seg
  = S.init
  $ S.permute (+) zeros (\ix -> S.index1 (S.the len - 1 - offset S.! ix)) ones
  where
    (offset, len)       = S.scanr' (+) 0 $ S.map S.unindex1 seg
    zeros               = S.fill (S.index1 $ S.the len + 1) 0
    ones                = S.fill (S.index1  $ S.size offset) 1
--}

replicateC
    :: (Arrays a, Arrays (Vector' a), Kit acc)
    => acc aenv (Scalar Int)
    -> acc aenv a
    -> acc aenv (Vector' a)
replicateC = fromHOAS (replicate . S.the)

replicateSegC
    :: (Kit acc, Shape sh, Elt e)
    => acc aenv (Segments sh)
    -> acc aenv (Vector e)
    -> acc aenv (Vector e)
replicateSegC = fromHOAS replicateSeg

enumSegC :: (Shape sh, Kit acc) => acc aenv (Vector sh) -> acc aenv (Vector sh)
enumSegC = fromHOAS enumSeg

liftedCondC
    :: (Arrays a, Arrays (Vector' a), Kit acc)
    => acc aenv (Vector Bool)
    -> acc aenv (Vector' a)
    -> acc aenv (Vector' a)
    -> acc aenv (Vector' a)
liftedCondC = fromHOAS liftedCond

liftedReshapeC
    :: (Elt e, Shape sh, Shape sh', Kit acc)
    => acc aenv (Vector sh)
    -> acc aenv (LiftedArray sh' e)
    -> acc aenv (LiftedArray sh e)
liftedReshapeC = fromHOAS liftedReshape

liftedBackpermuteC
    :: (Elt e, Shape sh, Shape sh', Kit acc)
    => acc aenv (Vector sh')
    -> acc aenv (Vector sh)
    -> acc aenv (LiftedArray sh  e)
    -> acc aenv (LiftedArray sh' e)
liftedBackpermuteC = fromHOAS liftedBackpermute

asOffsetsOfC
    :: (Shape sh, Shape sh', Kit acc)
    => acc aenv (LiftedArray sh sh')
    -> acc aenv (Segments sh')
    -> acc aenv (Vector DIM1)
asOffsetsOfC = fromHOAS asOffsetsOf

liftedIndexC
    :: (Kit acc, Shape sh, Elt e)
    => acc aenv (LiftedArray sh e)
    -> acc aenv (Vector sh)
    -> acc aenv (Vector e)
liftedIndexC = fromHOAS liftedIndex


-- Duplicating and sinking simple scalar expressions down the AST so as to avoid
-- unnecessary vectorisation.
-- -----------------------------------------------------------------------------

--type SinkExps acc = forall env env' aenv t. Supplement acc env env' aenv -> acc aenv t -> acc aenv

--data WeakenedSupplement acc env0 env1 env0' env1' aenv where
--  WeakenedSupplement :: Supplement acc env1 env2 aenv
--                     -> (env1' :> env2)
--                     -> (env2  :> env1')
--                     -> WeakenedSupplement acc env0 env1 env0' env1' aenv

--sinkExpsIntoOpenAcc :: SinkExps OpenAcc
--sinkExpsIntoOpenAcc supp (OpenAcc a) = OpenAcc $ sinkExpsIntoAcc sinkExpsIntoOpenAcc supp a

--sinkExpsIntoAfun :: forall acc env env' aenv t. (Kit acc)
--                 => SinkExps acc
--                 -> Supplement acc env env' aenv
--                 -> PreOpenAfun acc aenv
--                 -> PreOpenAfun acc aenv
--sinkExpsIntoAfun sinker supp (Abody b) = Abody $ sinker supp b
--sinkExpsIntoAfun sinker supp (Alam f)  = Alam  $ sinkExpsIntoAfun sinker (weakenSupp1 supp) f

--sinkExpsIntoAcc :: forall acc env env' aenv t. (Kit acc)
--                => SinkExps acc
--                -> Supplement acc env env' aenv
--                -> PreOpenAcc acc aenv
--                -> PreOpenAcc acc aenv
--sinkExpsIntoAcc sinker supp pacc
--  = case pacc of
--      Alet bnd body             -> Alet (cvtA bnd) (sinker (weakenSupp1 supp) body)
--      Avar ix                   -> Avar ix
--      Atuple tup                -> Atuple (cvtT tup)
--      Aprj tup a                -> Aprj tup (cvtA a)
--      Apply f a                 -> Apply (cvtAfun f) (cvtA a)
--      Aforeign ff afun acc      -> Aforeign ff (sinkExpsIntoAfun sinker BaseSup afun) (cvtA acc)
--      Acond p t e               -> Acond (cvtE p) (cvtA t) (cvtA e)
--      Awhile p f a              -> Awhile (cvtAfun p) (cvtAfun f) (cvtA a)
--      Use a                     -> Use a
--      Unit e                    -> Unit (cvtE e)
--      Reshape e a               -> Reshape (cvtE e) (cvtA a)
--      Generate e f              -> Generate (cvtE e) (cvtF f)
--      Transform sh ix f a       -> Transform (cvtE sh) (cvtF ix) (cvtF f) (cvtA a)
--      Replicate sl slix a       -> Replicate sl (cvtE slix) (cvtA a)
--      Slice sl a slix           -> Slice sl (cvtA a) (cvtE slix)
--      Map f a                   -> Map (cvtF f) (cvtA a)
--      ZipWith f a1 a2           -> ZipWith (cvtF f) (cvtA a1) (cvtA a2)
--      Fold f z a                -> Fold (cvtF f) (cvtE z) (cvtA a)
--      Fold1 f a                 -> Fold1 (cvtF f) (cvtA a)
--      FoldSeg f z a s           -> FoldSeg (cvtF f) (cvtE z) (cvtA a) (cvtA s)
--      Fold1Seg f a s            -> Fold1Seg (cvtF f) (cvtA a) (cvtA s)
--      Scanl f z a               -> Scanl (cvtF f) (cvtE z) (cvtA a)
--      Scanl' f z a              -> Scanl' (cvtF f) (cvtE z) (cvtA a)
--      Scanl1 f a                -> Scanl1 (cvtF f) (cvtA a)
--      Scanr f z a               -> Scanr (cvtF f) (cvtE z) (cvtA a)
--      Scanr' f z a              -> Scanr' (cvtF f) (cvtE z) (cvtA a)
--      Scanr1 f a                -> Scanr1 (cvtF f) (cvtA a)
--      Permute f1 a1 f2 a2       -> Permute (cvtF f1) (cvtA a1) (cvtF f2) (cvtA a2)
--      Backpermute sh f a        -> Backpermute (cvtE sh) (cvtF f) (cvtA a)
--      Stencil f b a             -> Stencil (cvtF f) b (cvtA a)
--      Stencil2 f b1 a1 b2 a2    -> Stencil2 (cvtF f) b1 (cvtA a1) b2 (cvtA a2)
--  where
--    cvtA :: forall t. acc env' aenv t -> acc aenv
--    cvtA = sinker supp

--    cvtE :: forall t. Elt t => PreOpenExp acc env' aenv t -> PreOpenExp acc env aenv
--    cvtE = bindExps supp . sinkExpsIntoExp sinker supp

--    cvtF :: forall t. PreOpenFun acc env' aenv t -> PreOpenFun acc env aenv
--    cvtF = cvt supp
--      where
--        cvt :: forall t env env'. Supplement acc env env' aenv -> PreOpenFun acc env' aenv t -> PreOpenFun acc env aenv
--        cvt supp (Body b) = Body $ bindExps supp $ sinkExpsIntoExp sinker supp b
--        cvt supp (Lam  f) | WeakenedSupplement supp' ixt _ <- weakenSuppE1 supp
--                          = Lam  $ cvt supp' (weakenE ixt f)

--    cvtAfun :: forall t. PreOpenAfun acc aenv t -> PreOpenAfun acc aenv
--    cvtAfun = cvt supp
--      where
--        cvt :: forall t aenv. Supplement acc env env' aenv -> PreOpenAfun acc aenv t -> PreOpenAfun acc aenv
--        cvt supp (Abody b) = Abody $ sinker supp b
--        cvt supp (Alam  f) = Alam  $ cvt (weakenSupp1 supp) f

--    cvtT :: forall t. Atuple (acc env' aenv) t -> Atuple (acc env aenv)
--    cvtT NilAtup        = NilAtup
--    cvtT (SnocAtup t a) = SnocAtup (cvtT t) (cvtA a)

--sinkExpsIntoExp :: forall acc env env' aenv t. (Kit acc, Elt t)
--                => SinkExps acc
--                -> Supplement acc env env' aenv
--                -> PreOpenExp acc env'     aenv
--                -> PreOpenExp acc env'     aenv
--sinkExpsIntoExp sinker supp exp
--  = case exp of
--      Let bnd body              -> sinkLet (cvtE bnd) body
--      Var idx                   -> Var idx
--      Const c                   -> Const c
--      Tuple t                   -> Tuple (cvtT t)
--      Prj tup e                 -> Prj tup (cvtE e)
--      IndexNil                  -> IndexNil
--      IndexCons sl sz           -> IndexCons (cvtE sl) (cvtE sz)
--      IndexHead sh              -> IndexHead (cvtE sh)
--      IndexTail sh              -> IndexTail (cvtE sh)
--      IndexSlice x ix sh        -> IndexSlice x (cvtE ix) (cvtE sh)
--      IndexFull x ix sl         -> IndexFull x (cvtE ix) (cvtE sl)
--      IndexAny                  -> IndexAny
--      ToIndex sh ix             -> ToIndex (cvtE sh) (cvtE ix)
--      FromIndex sh i            -> FromIndex (cvtE sh) (cvtE i)
--      Cond p t e                -> Cond (cvtE p) (cvtE t) (cvtE e)
--      While p f x               -> While (cvtF p) (cvtF f) (cvtE x)
--      PrimConst c               -> PrimConst c
--      PrimApp f x               -> PrimApp f (cvtE x)
--      Index a sh                -> Index (cvtA a) (cvtE sh)
--      LinearIndex a i           -> LinearIndex (cvtA a) (cvtE i)
--      Shape a                   -> Shape (cvtA a)
--      ShapeSize sh              -> ShapeSize (cvtE sh)
--      Intersect sh sz           -> Intersect (cvtE sh) (cvtE sz)
--      Union sh sz               -> Union (cvtE sh) (cvtE sz)
--      Foreign ff f e            -> Foreign ff (cvtFun BaseSup f) (cvtE e)

--  where
--    cvtA :: forall t. acc env' aenv t -> acc aenv
--    cvtA = sinkExps supp . sinker supp

--    cvtE :: forall t. Elt t => PreOpenExp acc env' aenv t -> PreOpenExp acc env' aenv
--    cvtE = sinkExpsIntoExp sinker supp

--    cvtF :: forall t. PreOpenFun acc env' aenv t -> PreOpenFun acc env' aenv
--    cvtF = cvtFun supp

--    cvtFun :: forall t env env' aenv. Supplement acc env env' aenv -> PreOpenFun acc env' aenv t -> PreOpenFun acc env' aenv
--    cvtFun supp (Body b) = Body $ sinkExpsIntoExp sinker supp b
--    cvtFun supp (Lam  f) | WeakenedSupplement supp' ixt txi <- weakenSuppE1 supp
--                         = Lam $ weakenE txi $ cvtFun supp' (weakenE ixt f)

--    cvtT :: forall t. Tuple (PreOpenExp acc env' aenv) t -> Tuple (PreOpenExp acc env' aenv)
--    cvtT NilTup        = NilTup
--    cvtT (SnocTup t a) = SnocTup (cvtT t) (cvtE a)

--    sinkLet :: Elt bnd => PreOpenExp acc env' aenv bnd -> PreOpenExp acc (env',bnd) aenv t -> PreOpenExp acc env' aenv
--    sinkLet bnd body | shouldSinkExp bnd
--                     = Let bnd (sinkExpsIntoExp sinker (PushSup supp bnd) body)
--                     | WeakenedSupplement supp' ixt txi <- weakenSuppE1 supp
--                     = Let bnd (weakenE txi $ sinkExpsIntoExp sinker supp' (weakenE ixt body))

--shouldSinkExp :: PreOpenExp acc env aenv t -> Bool
--shouldSinkExp Const{}     = True
--shouldSinkExp IndexNil{}  = True
--shouldSinkExp IndexAny{}  = True
--shouldSinkExp PrimConst{} = True
--shouldSinkExp _           = False


--weakenSuppE1 :: Kit acc => Supplement acc env env' aenv -> WeakenedSupplement acc env (env,s) env' (env',s) aenv
--weakenSuppE1 BaseSup = WeakenedSupplement BaseSup id id
--weakenSuppE1 (PushSup supp b)
-- | WeakenedSupplement supp' ixt txi <- weakenSuppE1 supp
-- = WeakenedSupplement (PushSup supp' (weakenE (ixt . SuccIdx) b)) (swiz ixt) (ziws txi)
--  where
--    swiz :: ((env'1,s) :> env2) -> (((env'1,e),s) :> (env2,e))
--    swiz ixt ZeroIdx                = SuccIdx (ixt ZeroIdx)
--    swiz _   (SuccIdx ZeroIdx)      = ZeroIdx
--    swiz ixt (SuccIdx (SuccIdx ix)) = SuccIdx (ixt (SuccIdx ix))

--    ziws :: (env2 :> (env'1,s)) -> ((env2,e) :> ((env'1,e),s))
--    ziws _   ZeroIdx      = SuccIdx ZeroIdx
--    ziws ixt (SuccIdx ix) = case ixt ix of
--                             ZeroIdx      -> ZeroIdx
--                             (SuccIdx ix) -> SuccIdx (SuccIdx ix)


-- HOAS-conversion
-- ---------------

-- Conversion from HOAS to Debruijn form in such a way that it is easier to use during the transform
-- The many arguments and fundeps are necessary because of overlap.
--

type family AfunctionR (acc :: * -> * -> *) aenv f
type instance AfunctionR acc aenv (S.Acc a)      = acc aenv a
type instance AfunctionR acc aenv (S.Acc a -> r) = acc aenv a -> AfunctionR acc aenv r

class S.Afunction f => Convertible f where
    applyable :: Kit acc => {- dummy -} f -> PreOpenAfun acc aenv (S.AfunctionR f) -> AfunctionR acc aenv f

-- Convenient HOAS term conversion. Requires AllowAmbiguousTypes.
fromHOAS :: forall acc aenv f. (Convertible f, Kit acc) => f -> AfunctionR acc aenv f
fromHOAS f = applyable (undefined :: f) f'
  where
    f' :: PreOpenAfun acc aenv (S.AfunctionR f)
    f' = weaken undefined . fromOpenAfun . cvtS $ f

instance Arrays a => Convertible (S.Acc a) where
    applyable _ (Abody a) = a
    applyable _ _         = $internalError "applyable" "Inconsistent valuation"

instance (Arrays a, Convertible b) => Convertible (S.Acc a -> b) where
  applyable _ = as
    where
      as :: Kit acc => PreOpenAfun acc aenv (a -> S.AfunctionR b) -> acc aenv a -> AfunctionR acc aenv b
      as (Alam f) = \a -> applyable (undefined :: b) (rebindIndex f ZeroIdx `inlineA` extract a)
      as _        = $internalError "applyable" "Inconsistent valuation"

-- | Replace all occurrences of the first variable with the given array
-- expression. The environment shrinks.
--
inlineA :: Rebuildable f => f (aenv,s) t -> PreOpenAcc (AccClo f) aenv s -> f aenv t
inlineA f g = Stats.substitution "inlineA" $ rebuildA (subAtop g) f

rebindIndex
    :: forall acc aenv f a. (Kit acc, Arrays a)
    => PreOpenAfun acc aenv f
    -> Idx aenv a
    -> PreOpenAfun acc aenv f
rebindIndex (Alam  f) a
  = Alam $ rebindIndex f (SuccIdx a)
rebindIndex (Abody b) ix
  =  Abody
  $^ Alet (inject $ Avar ix)
  $  weaken (ixt ix) b
  where
    ixt :: forall aenv a s. Idx aenv a -> Idx aenv s -> Idx (aenv,a) s
    ixt ix ix'
      | Just Refl <- matchIdx ix ix' = ZeroIdx
      | otherwise                    = SuccIdx ix'

-- Utility functions
-- -----------------

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

tup :: forall acc env aenv a b. (Elt a, Elt b)
    => PreOpenExp acc env aenv a
    -> PreOpenExp acc env aenv b
    -> PreOpenExp acc env aenv (a,b)
tup a b = Tuple (SnocTup (SnocTup NilTup a) b)

atup :: forall acc aenv a b. (Kit acc, Arrays a, Arrays b)
     => acc aenv a
     -> acc aenv b
     -> acc aenv (a,b)
atup a b = inject $ Atuple $ NilAtup `SnocAtup` a `SnocAtup` b

atup3 :: forall acc aenv a b c. (Kit acc, Arrays a, Arrays b, Arrays c)
      => acc aenv a
      -> acc aenv b
      -> acc aenv c
      -> acc aenv (a,b,c)
atup3 a b c = inject $ Atuple $ NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c

replicateA
    :: forall acc aenv a. (Kit acc, Arrays a)
    => acc aenv a
    -> Size acc aenv
    -> acc aenv (Vector' a)
replicateA a size
  | IsC <- isArraysFlat (undefined :: a)
  = replicateC (inject $ Unit size) a

replicateE
    :: forall acc aenv e. (Kit acc, Elt e)
    => PreExp acc aenv e
    -> Size acc aenv
    -> PreOpenAcc acc aenv (Vector e)
replicateE e size = Replicate (SliceFixed SliceNil) (IndexCons IndexNil size) (inject $ Unit e)

var0 :: Elt t0 => PreOpenExp acc (env, t0) aenv t0
var0 = Var ZeroIdx

var1 :: Elt t1 => PreOpenExp acc ((env, t1), t0) aenv t1
var1 = Var $ SuccIdx ZeroIdx

avar0 :: (Kit acc, Arrays a0) => acc (aenv, a0) a0
avar0 = inject $ Avar ZeroIdx

avar1 :: (Kit acc, Arrays a1) => acc ((aenv, a1), a0) a1
avar1 = inject $ Avar $ SuccIdx ZeroIdx

avar2 :: (Kit acc, Arrays a2) => acc (((aenv, a2), a1), a0) a2
avar2 = inject $ Avar $ SuccIdx . SuccIdx $ ZeroIdx

avar3 :: (Kit acc, Arrays a3) => acc ((((aenv, a3), a2), a1), a0) a3
avar3 = inject $ Avar $ SuccIdx . SuccIdx . SuccIdx $ ZeroIdx

the :: Elt e => acc aenv (Scalar e) -> PreOpenExp acc env aenv e
the a = Index a (Const ())

index1 :: PreOpenExp acc env aenv Int -> PreOpenExp acc env aenv DIM1
index1 = IndexCons IndexNil

{--
segmented
    :: (Elt e, Kit acc)
    => PreOpenFun acc env aenv (e -> e -> e)
    -> PreOpenFun acc env aenv ((Int, e) -> (Int, e) -> (Int, e))
segmented f
    = Lam . Lam . Body
    $ tup (PrimBOr integralType `PrimApp` tup (fstE var1) (fstE var0))
          (Cond (PrimNEq scalarType `PrimApp` tup (fstE var0) (Const 0))
          (sndE var0)
          (subApplyE2 (weakenE2 f) (sndE var0) (sndE var1)))
--}

newTop :: env :> env' -> (env,t) :> (env', t)
newTop _  ZeroIdx      = ZeroIdx
newTop wk (SuccIdx ix) = SuccIdx $ wk ix

weakenA1 :: Sink f => f aenv t -> f (aenv,s0) t
weakenA1 = weaken SuccIdx

weakenA2 :: Sink f => f aenv t -> f ((aenv,s1),s0) t
weakenA2 = weaken (SuccIdx . SuccIdx)

weakenA3 :: Sink f => f aenv t -> f (((aenv,s2),s1),s0) t
weakenA3 = weaken (SuccIdx . SuccIdx . SuccIdx)

weakenA4 :: Sink f => f aenv t -> f ((((aenv,s3),s2),s1),s0) t
weakenA4 = weaken (SuccIdx . SuccIdx . SuccIdx . SuccIdx)

weakenE1 :: SinkExp f => f env aenv t -> f (env,s0) aenv t
weakenE1 = weakenE SuccIdx

-- weakenE2 :: SinkExp f => f env aenv t -> f ((env,s1),s0) aenv t
-- weakenE2 = weakenE (SuccIdx . SuccIdx)

fun1 :: (Elt a, Elt b)
     => (PreOpenExp acc (env,a) aenv a -> PreOpenExp acc (env,a) aenv b)
     -> PreOpenFun acc env aenv (a -> b)
fun1 f = Lam (Body (f var0))

fun2 :: (Elt a, Elt b, Elt c)
     => (PreOpenExp acc ((env,a), b) aenv a -> PreOpenExp acc ((env,a), b) aenv b -> PreOpenExp acc ((env,a), b) aenv c)
     -> PreOpenFun acc env aenv (a -> b -> c)
fun2 f = Lam (Lam (Body (f var1 var0)))

--weakenSupp1 :: Kit acc
--            => Supplement acc env env' aenv
--            -> Supplement acc env env' (aenv,s)
--weakenSupp1 BaseSup = BaseSup
--weakenSupp1 (PushSup s b) = PushSup (weakenSupp1 s) (weakenA1 b)

unliftA
    :: forall env aenv env' aenv'.
       Context env aenv env' aenv'
    -> (aenv :?> aenv')
unliftA (PushAccC _)    ZeroIdx      = Just ZeroIdx
unliftA (PushAccC d)    (SuccIdx ix) = SuccIdx <$> unliftA d ix
unliftA (PushLAccC _)   ZeroIdx      = Nothing
unliftA (PushLAccC d)   (SuccIdx ix) = SuccIdx <$> unliftA d ix
unliftA (PushExpC d)    ix           = unliftA d ix
unliftA (PushLExpC d)   ix           = SuccIdx <$> unliftA d ix
unliftA _               _            = error "unliftA: Inconsistent evalution"

--unliftE :: forall env aenv env' aenv'.
--           Context env aenv env' aenv'
--        -> (env :?> env')
--unliftE (PushAccC d)    ix           = unliftE d ix
--unliftE (PushLAccC d)   ix           = unliftE d ix
--unliftE (PushExpC _)    ZeroIdx      = Just ZeroIdx
--unliftE (PushExpC d)    (SuccIdx ix) = SuccIdx <$> unliftE d ix
--unliftE (PushLExpC _)   ZeroIdx      = Nothing
--unliftE (PushLExpC d)   (SuccIdx ix) = unliftE d ix
--unliftE _               _            = error "unliftE: Inconsistent evalution"

rebuildToLift
    :: Rebuildable f
    => Context env aenv env' aenv'
    -> f aenv t
    -> Maybe (f aenv' t)
rebuildToLift d = rebuildPartial (liftA Avar . unliftA d)

cvtS :: S.Afunction f => f -> OpenAfun aenv (S.AfunctionR f)
cvtS = weaken undefined . S.convertAfun True True True True

-- Application via let binding.
--
subApply
    :: (RebuildableAcc acc, Arrays a)
    => PreOpenAfun acc aenv (a -> b)
    -> acc             aenv a
    -> PreOpenAcc  acc aenv b
subApply (Alam (Abody f)) a = Alet a f
subApply _                _ = error "subApply: inconsistent evaluation"

subApply2
    :: (Kit acc, Arrays a)
    => PreOpenAfun acc aenv (a -> b -> c)
    -> acc             aenv a
    -> acc             aenv b
    -> PreOpenAcc  acc aenv c
subApply2 (Alam (Alam (Abody f))) a b
  = Alet a
  $ inject $ Alet (weakenA1 b)
  $ f
subApply2 _ _ _ = error "subApply2: inconsistent evaluation"

{--
subApplyE2
    :: Kit acc
    => PreOpenFun  acc env aenv (a -> b -> c)
    -> PreOpenExp  acc env aenv a
    -> PreOpenExp  acc env aenv b
    -> PreOpenExp  acc env aenv c
subApplyE2 (Lam (Lam (Body f))) a b
  = Let a
  $ Let (weakenE1 b)
  $ f
subApplyE2 _ _ _ = error "subApplyE2: inconsistent evaluation"
--}

--partApply :: Kit acc
--          => PreOpenAfun acc aenv (a -> r)
--          -> acc             aenv a
--          -> PreOpenAfun acc aenv r
--partApply (Alam f) a
--  = app id a f
--  where
--    app :: forall acc env aenv aenv' a f. (Kit acc, Arrays a)
--        => (aenv' :> (aenv, a))
--        -> acc aenv a
--        -> PreOpenAfun acc aenv' f
--        -> PreOpenAfun acc aenv  f
--    app ixt a (Abody b) = Abody (inject $ Alet a $ weaken ixt b)
--    app ixt a (Alam  f) = Alam  (app ixt' (weaken SuccIdx a) f)
--      where
--        ixt' :: Idx (aenv', s) t
--             -> Idx ((aenv, s), a) t
--        ixt' ZeroIdx      = SuccIdx ZeroIdx
--        ixt' (SuccIdx ix) = case ixt ix of
--                              ZeroIdx      -> ZeroIdx
--                              (SuccIdx ix) -> SuccIdx (SuccIdx ix)
--partApply _ _
--  = error "partApply: inconsistent evaluation"

infixr 0 $^

($^) :: Kit acc
     => (acc aenv a -> t)
     -> PreOpenAcc acc aenv a
     -> t
($^) f a = f $ inject a


-- Sequence vectorisation
-- ----------------------
sequenceFreeAfun :: OpenAfun aenv t -> Bool
sequenceFreeAfun afun =
  case afun of
    Alam f -> sequenceFreeAfun f
    Abody b -> sequenceFreeAcc b

sequenceFreeFun :: OpenFun env aenv t -> Bool
sequenceFreeFun afun =
  case afun of
    Lam f -> sequenceFreeFun f
    Body b -> sequenceFreeExp b

sequenceFreeExp :: OpenExp env aenv t -> Bool
sequenceFreeExp = travE
  where
    travF :: OpenFun env aenv t -> Bool
    travF = sequenceFreeFun

    travT :: Tuple (OpenExp env aenv) t -> Bool
    travT = sequenceFreeTup

    travA :: OpenAcc aenv t -> Bool
    travA = sequenceFreeAcc

    travE :: OpenExp env aenv t -> Bool
    travE exp =
      case exp of
        Let bnd body            -> travE bnd && travE body
        Var _                   -> True
        Const _                 -> True
        Tuple tup               -> travT tup
        Prj _ t                 -> travE t
        IndexNil                -> True
        IndexCons sh sz         -> travE sh && travE sz
        IndexHead sh            -> travE sh
        IndexTail sh            -> travE sh
        IndexAny                -> True
        IndexSlice _ ix sh      -> travE ix && travE sh
        IndexFull _ ix sl       -> travE ix && travE sl
        ToIndex sh ix           -> travE sh && travE ix
        FromIndex sh ix         -> travE sh && travE ix
        Cond p t e              -> travE p && travE t && travE e
        While p f x             -> travF p && travF f && travE x
        PrimConst _             -> True
        PrimApp _ x             -> travE x
        Index a sh              -> travA a && travE sh
        LinearIndex a i         -> travA a && travE i
        Shape a                 -> travA a
        ShapeSize sh            -> travE sh
        Intersect s t           -> travE s && travE t
        Union s t               -> travE s && travE t
        Foreign _ f e           -> travF f && travE e

sequenceFreeAtup :: Atuple (OpenAcc aenv) t -> Bool
sequenceFreeAtup t =
  case t of
    NilAtup -> True
    SnocAtup t e -> sequenceFreeAtup t && sequenceFreeAcc e

sequenceFreeTup :: Tuple (OpenExp env aenv) t -> Bool
sequenceFreeTup t =
  case t of
    NilTup -> True
    SnocTup t e -> sequenceFreeTup t && sequenceFreeExp e

sequenceFreeAcc :: OpenAcc aenv a -> Bool
sequenceFreeAcc = travA
  where
    travAfun :: OpenAfun aenv t -> Bool
    travAfun = sequenceFreeAfun

    travE :: Exp aenv t -> Bool
    travE = sequenceFreeExp

    travF :: Fun aenv t -> Bool
    travF = sequenceFreeFun

    travAT :: Atuple (OpenAcc aenv) t -> Bool
    travAT = sequenceFreeAtup

    travA :: OpenAcc aenv t -> Bool
    travA (OpenAcc acc) =
      case acc of
        Alet bnd body             -> travA bnd && travA body
        Avar _                    -> True
        Atuple tup                -> travAT tup
        Aprj _ a                  -> travA a
        Apply f a                 -> travAfun f && travA a
        Aforeign _ afun acc       -> travAfun afun && travA acc
        Acond p t e               -> travE p && travA t && travA e
        Awhile p f a              -> travAfun p && travAfun f && travA a
        Use _                     -> True
        Unit e                    -> travE e
        Reshape e a               -> travE e && travA a
        Generate e f              -> travE e && travF f
        Transform sh ix f a       -> travE sh && travF ix && travF f && travA a
        Replicate _ slix a        -> travE slix && travA a
        Slice _ a slix            -> travA a && travE slix
        Map f a                   -> travF f && travA a
        ZipWith f a1 a2           -> travF f && travA a1 && travA a2
        Fold f z a                -> travF f && travE z && travA a
        Fold1 f a                 -> travF f && travA a
        Scanl f z a               -> travF f && travE z && travA a
        Scanl' f z a              -> travF f && travE z && travA a
        Scanl1 f a                -> travF f && travA a
        Scanr f z a               -> travF f && travE z && travA a
        Scanr' f z a              -> travF f && travE z && travA a
        Scanr1 f a                -> travF f && travA a
        Permute f1 a1 f2 a2       -> travF f1 && travA a1 && travF f2 && travA a2
        Backpermute sh f a        -> travE sh && travF f && travA a
        Stencil f _ a             -> travF f && travA a
        Stencil2 f _ a1 _ a2      -> travF f && travA a1 && travA a2
        -- Interesting case:
        Collect _                 -> False
        FoldSeg f z a s           -> travF f && travE z && travA a && travA s
        Fold1Seg f a s            -> travF f && travA a && travA s

vectoriseSeq :: PreOpenSeq OpenAcc () () a -> PreOpenSeq OpenAcc () () a
vectoriseSeq = vectoriseOpenSeq Aggressive EmptyC

vectoriseOpenSeq
    :: forall aenv senv a.
       Strength
    -> Context () aenv () aenv
    -> PreOpenSeq OpenAcc aenv senv a
    -> PreOpenSeq OpenAcc aenv senv a
vectoriseOpenSeq strength ctx seq =
  case seq of
    Producer p s -> Producer (cvtP p) (vectoriseOpenSeq strength ctx s)
    Consumer c   -> Consumer (cvtC c)
    Reify ix     -> Reify ix
    where
      cvtP :: Producer OpenAcc aenv senv t -> Producer OpenAcc aenv senv t
      cvtP p =
        case p of
          StreamIn arrs           -> StreamIn arrs
          ToSeq sl slix a         -> ToSeq sl slix (cvtA a)
          ChunkedMapSeq f x       -> ChunkedMapSeq (cvtAfun f) x
          ZipWithSeq f x y        -> ZipWithSeq (cvtAfun f) x y
          ScanSeq f e x           -> ScanSeq (cvtF f) (cvtE e) x

          -- Interesting case:
          MapSeq f x
            | sequenceFreeAfun f  -> trace "vectoriseSeq" "MapSeq succesfully vectorised" -- ++ show (liftOpenAfun1 strength ctx (cvtAfun f))) $
                                   $ ChunkedMapSeq (liftOpenAfun1 strength ctx (cvtAfun f)) x
          -- The following case is needed because we don't know how to lift
          -- sequences yet.
            | otherwise           -> trace "vectoriseSeq" "MapSeq could not be vectorised" -- ++ show (cvtAfun f)) $
                                   $ MapSeq (cvtAfun f) x

      cvtC :: Consumer OpenAcc aenv senv t -> Consumer OpenAcc aenv senv t
      cvtC c =
        case c of
          FoldSeq f e x        -> FoldSeq (cvtF f) (cvtE e) x
          FoldSeqFlatten f a x -> FoldSeqFlatten (cvtAfun f) (cvtA a) x
          Stuple t             -> Stuple (cvtCT t)

      cvtCT :: Atuple (Consumer OpenAcc aenv senv) t -> Atuple (Consumer OpenAcc aenv senv) t
      cvtCT NilAtup        = NilAtup
      cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (cvtC c)

      cvtE :: Exp aenv t -> Exp aenv t
      cvtE = vectoriseSeqOpenExp strength ctx

      cvtF :: Fun aenv t -> Fun aenv t
      cvtF = vectoriseSeqOpenFun strength ctx

      cvtA :: OpenAcc aenv t -> OpenAcc aenv t
      cvtA = vectoriseSeqOpenAcc strength ctx

      cvtAfun :: OpenAfun aenv t -> OpenAfun aenv t
      cvtAfun = vectoriseSeqOpenAfun strength ctx

stripExpCtx :: Context env aenv env aenv -> Context () aenv () aenv
stripExpCtx c =
  case c of
    EmptyC      -> EmptyC
    PushExpC c' -> stripExpCtx c'
    PushAccC c' -> PushAccC (stripExpCtx c')
    _           -> $internalError "stripExpCtx" "unreachable"

vectoriseSeqOpenExp
    :: forall env aenv a.
       Strength
    -> Context env aenv env aenv
    -> OpenExp env aenv a
    -> OpenExp env aenv a
vectoriseSeqOpenExp strength ctx = cvtE
  where
    cvtA :: OpenAcc aenv t -> OpenAcc aenv t
    cvtA a = vectoriseSeqOpenAcc strength (stripExpCtx ctx) a

    cvtT :: Tuple (OpenExp env aenv) t -> Tuple (OpenExp env aenv) t
    cvtT tup =
      case tup of
        NilTup      -> NilTup
        SnocTup t a -> cvtT t `SnocTup` cvtE a

    cvtF :: OpenFun env aenv t -> OpenFun env aenv t
    cvtF = vectoriseSeqOpenFun strength ctx

    cvtE :: OpenExp env aenv t -> OpenExp env aenv t
    cvtE exp =
      case exp of
        Let bnd body            -> Let (cvtE bnd) (vectoriseSeqOpenExp strength (PushExpC ctx) body)
        Var ix                  -> Var ix
        Const c                 -> Const c
        Tuple tup               -> Tuple (cvtT tup)
        Prj tup t               -> Prj tup (cvtE t)
        IndexNil                -> IndexNil
        IndexCons sh sz         -> IndexCons (cvtE sh) (cvtE sz)
        IndexHead sh            -> IndexHead (cvtE sh)
        IndexTail sh            -> IndexTail (cvtE sh)
        IndexAny                -> IndexAny
        IndexSlice x ix sh      -> IndexSlice x (cvtE ix) (cvtE sh)
        IndexFull x ix sl       -> IndexFull x (cvtE ix) (cvtE sl)
        ToIndex sh ix           -> ToIndex (cvtE sh) (cvtE ix)
        FromIndex sh ix         -> FromIndex (cvtE sh) (cvtE ix)
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
        Foreign ff f e          -> Foreign ff (vectoriseSeqOpenFun strength EmptyC f) (cvtE e)

vectoriseSeqAcc :: OpenAcc () a -> OpenAcc () a
vectoriseSeqAcc = vectoriseSeqOpenAcc Aggressive EmptyC

vectoriseSeqOpenAcc
    :: forall aenv a.
       Strength
    -> Context () aenv () aenv
    -> OpenAcc aenv a
    -> OpenAcc aenv a
vectoriseSeqOpenAcc strength ctx = cvtA
  where
    cvtT :: Atuple (OpenAcc aenv) t -> Atuple (OpenAcc aenv) t
    cvtT atup = case atup of
                  NilAtup      -> NilAtup
                  SnocAtup t a -> cvtT t `SnocAtup` cvtA a

    cvtAfun :: OpenAfun aenv t -> OpenAfun aenv t
    cvtAfun = vectoriseSeqOpenAfun strength ctx

    cvtE :: Exp aenv t -> Exp aenv t
    cvtE = vectoriseSeqOpenExp strength ctx

    cvtF :: Fun aenv t -> Fun aenv t
    cvtF = vectoriseSeqOpenFun strength ctx

    cvtA :: OpenAcc aenv t -> OpenAcc aenv t
    cvtA (OpenAcc pacc)
      = OpenAcc
      $ case pacc of
          Alet bnd body             -> Alet (cvtA bnd) (vectoriseSeqOpenAcc strength (PushAccC ctx) body)
          Avar ix                   -> Avar ix
          Atuple tup                -> Atuple (cvtT tup)
          Aprj tup a                -> Aprj tup (cvtA a)
          Apply f a                 -> Apply (cvtAfun f) (cvtA a)
          Aforeign ff afun acc      -> Aforeign ff (vectoriseSeqAfun afun) (cvtA acc)
          Acond p t e               -> Acond (cvtE p) (cvtA t) (cvtA e)
          Awhile p f a              -> Awhile (cvtAfun p) (cvtAfun f) (cvtA a)
          Use a                     -> Use a
          Unit e                    -> Unit (cvtE e)
          Reshape e a               -> Reshape (cvtE e) (cvtA a)
          Generate e f              -> Generate (cvtE e) (cvtF f)
          Transform sh ix f a       -> Transform (cvtE sh) (cvtF ix) (cvtF f) (cvtA a)
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
          Collect s                 -> Collect (vectoriseOpenSeq strength ctx s)
          FoldSeg f z a s           -> FoldSeg (cvtF f) (cvtE z) (cvtA a) (cvtA s)
          Fold1Seg f a s            -> Fold1Seg (cvtF f) (cvtA a) (cvtA s)

vectoriseSeqAfun :: OpenAfun () t -> OpenAfun () t
vectoriseSeqAfun = vectoriseSeqOpenAfun Aggressive EmptyC

vectoriseSeqOpenFun
    :: forall env aenv t.
        Strength
    -> Context env aenv env aenv
    -> OpenFun env aenv t
    -> OpenFun env aenv t
vectoriseSeqOpenFun strength ctx fun =
  case fun of
    Body b -> Body (vectoriseSeqOpenExp strength ctx b)
    Lam f  -> Lam (vectoriseSeqOpenFun strength (PushExpC ctx) f)

vectoriseSeqOpenAfun
    :: Strength
    -> Context () aenv () aenv
    -> OpenAfun aenv t
    -> OpenAfun aenv t
vectoriseSeqOpenAfun strength ctx afun =
  case afun of
    Abody b -> Abody (vectoriseSeqOpenAcc strength ctx b)
    Alam f  -> Alam (vectoriseSeqOpenAfun strength (PushAccC ctx) f)


-- Debugging
-- ---------
trace :: String -> String -> a -> a
trace header msg
  = Debug.trace Debug.dump_vectorisation
  $ header ++ ": " ++ msg

