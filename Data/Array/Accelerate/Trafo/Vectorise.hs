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

  vectoriseAcc, vectoriseAfun, reduceStreamSeq,

) where

import Prelude                                          hiding ( exp, replicate, concat, maximum )
import qualified Prelude                                as P
import Data.Maybe                                       ( fromMaybe )
import Data.Typeable
#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative                              hiding ( Const, empty )
#endif

-- friends
import Data.Array.Accelerate.AST                       hiding ( Empty )
import Data.Array.Accelerate.Array.Lifted
import Data.Array.Accelerate.Array.Representation      ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar               hiding ( Segments )
import Data.Array.Accelerate.Trafo.Base                hiding ( PushExp )
import Data.Array.Accelerate.Trafo.Fusion
import Data.Array.Accelerate.Pretty                    ()
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar
import qualified Data.Array.Accelerate.Classes          as S
import qualified Data.Array.Accelerate.Language         as S
import qualified Data.Array.Accelerate.Prelude          as S
import qualified Data.Array.Accelerate.Smart            as S
import qualified Data.Array.Accelerate.Trafo.Sharing    as S

import Data.Array.Accelerate.Error

-- The lifted type relationship
-- ----------------------------
--

-- |Captures the relationship between a type and it's possible lifted
-- alternatives. For example, the type @Array sh e@ has the lifted equivalent
-- types @Array sh e@, @Regular (Array sh e)@, and @Irregular (Array sh e)@.
--
-- In the case of the product type @(a,b)@, the related types are the cartesian
-- product of the related types of @a@ and the related types of @b@.
--
data LiftedType t t' where
  UnitT       :: LiftedType ()           ()
  LiftedUnitT :: (IsAtuple a, ProdRepr a ~ ((),Scalar Int)) => LiftedType () a
  AvoidedT    :: LiftedType (Array sh e) (Array sh e)
  RegularT    :: LiftedType (Array sh e) (Regular (Array sh e))
  IrregularT  :: LiftedType (Array sh e) (Irregular (Array sh e))
  TupleT      :: (IsProduct Arrays t, IsProduct Arrays t', ArrRepr t ~ (a,b))
              => LiftedTupleType (TupleRepr t) (TupleRepr t')
              -> LiftedType t t'

data LiftedTupleType t t' where
  NilLtup  :: LiftedTupleType () ()
  SnocLtup :: (Arrays a, Arrays a')
           => LiftedTupleType t t'
           -> LiftedType a a'
           -> LiftedTupleType (t,a) (t',a')

-- |Encodes the relationship between the old environment and the lifted
-- environment
--
data Context aenv aenv' where
  BaseC     :: Context aenv aenv

  PushC     :: Arrays t'
            => Context aenv aenv'
            -> LiftedType t t'
            -> Context (aenv, t) (aenv', t')

  -- TODO: Environment splitting?

-- Lifting terms
-- ---------------
--

type VectoriseAcc acc = forall aenv aenv' t.
                        Arrays t
                     => Context aenv aenv'
                     -> Size acc aenv' Int
                     -> acc aenv t
                     -> LiftedAcc acc aenv' t

-- |The size parameter in the lifting transform.
--
data Size acc aenv e where
  Size :: Arrays a => acc aenv a -> PreExp acc (aenv,a) e -> Size acc aenv e

instance Kit acc => Sink (Size acc) where
  weaken k (Size b s) = Size (weaken k b) (weaken (newTop k) s)

-- The result of vectorisation. We get back a new term of type t' and a witness
-- that t' is a lifted type of t.
--
data LiftedAcc acc aenv t where
  LiftedAcc :: Arrays t' => LiftedType t t' -> acc aenv t' -> LiftedAcc acc aenv t

instance RebuildableAcc acc => Rebuildable (LiftedAcc acc) where
  type AccClo (LiftedAcc acc) = acc
  rebuildPartial v (LiftedAcc ty a) = LiftedAcc ty <$> rebuildPartial v a

instance Sink acc => Sink (LiftedAcc acc) where
  weaken k (LiftedAcc ty a) = LiftedAcc ty (weaken k a)

type LiftedOpenAcc aenv t = LiftedAcc OpenAcc aenv t

-- The result of vectorising a scalar expression. Due to the separate scalar
-- environment, we have to be careful about avoiding vectorisation. As a
-- consequenc of this, the transform will always yield a lifted version and
-- maybe a lifted version.
--
data LiftedExp acc env aenv aenv' t where
  LiftedExp  :: Maybe (PreOpenExp acc env aenv t)
             -> acc aenv' (Vector t)
             -> LiftedExp acc env aenv aenv' t

withL :: (acc aenv (Array sh e) -> a)
      -> (acc aenv (Regular (Array sh e))   -> a)
      -> (acc aenv (Irregular (Array sh e)) -> a)
      -> LiftedAcc acc aenv (Array sh e)
      -> a
withL f _ _ (LiftedAcc AvoidedT a)   = f a
withL _ f _ (LiftedAcc RegularT l)   = f l
withL _ _ f (LiftedAcc IrregularT l) = f l
withL _ _ _ _                        = error "Absurd"

withFL :: (Kit acc, Shape sh, Elt e)
       => Size acc aenv Int
       -> (acc aenv (Regular (Array sh e))   -> a)
       -> (acc aenv (Irregular (Array sh e)) -> a)
       -> LiftedAcc acc aenv (Array sh e)
       -> a
withFL size f _ (LiftedAcc AvoidedT a)   = f (replicateA size a)
withFL _    f _ (LiftedAcc RegularT l)   = f l
withFL _    _ f (LiftedAcc IrregularT l) = f l
withFL _    _ _ _                        = error "Absurd"

infixr 0 $*

($*) :: (forall t. Arrays t => acc aenv t -> acc' aenv' t)
     -> LiftedAcc acc aenv t'
     -> LiftedAcc acc' aenv' t'
f $* LiftedAcc t a = LiftedAcc t (f a)

newtype TupleIdx' t aenv a = TupleIdx' (TupleIdx t a)

mapAtuple :: (forall t. Arrays t => k t -> k' t)
          -> Atuple k  t'
          -> Atuple k' t'
mapAtuple _ NilAtup = NilAtup
mapAtuple f (SnocAtup t a) = SnocAtup (mapAtuple f t) (f a)

appL :: (Shape sh', Elt e')
     => (acc aenv (Array sh e)             -> acc' aenv' (Array sh' e'))
     -> (acc aenv (Regular (Array sh e))   -> acc' aenv' (Regular (Array sh' e')))
     -> (acc aenv (Irregular (Array sh e)) -> acc' aenv' (Irregular (Array sh' e')))
     -> LiftedAcc acc  aenv  (Array sh e)
     -> LiftedAcc acc' aenv' (Array sh' e')
appL f g h = withL (LiftedAcc AvoidedT . f) (LiftedAcc RegularT . g) (LiftedAcc IrregularT . h)

appFL :: (Kit acc, Shape sh', Elt e', Shape sh, Elt e)
      => Size acc aenv Int
      -> (acc aenv (Regular (Array sh e))   -> acc' aenv' (Regular (Array sh' e')))
      -> (acc aenv (Irregular (Array sh e)) -> acc' aenv' (Irregular (Array sh' e')))
      -> LiftedAcc acc  aenv  (Array sh e)
      -> LiftedAcc acc' aenv' (Array sh' e')
appFL size f g = withFL size (LiftedAcc RegularT . f) (LiftedAcc IrregularT . g)

-- For any type a, generate a witness that a is a lifted type for a.
--
avoidedType :: forall a. Arrays a
            => LiftedType a a
avoidedType =
  case flavour (undefined :: a) of
    ArraysFunit -> UnitT
    ArraysFarray -> AvoidedT
    ArraysFtuple -> TupleT (tup (prod Proxy (undefined :: a)))
  where
    tup :: ProdR Arrays t -> LiftedTupleType t t
    tup ProdRunit     = NilLtup
    tup (ProdRsnoc t) = SnocLtup (tup t) avoidedType

-- Same as for avoidedType, but this gives a witness that (Regular a) is a
-- lifted type for a.
--
regularType :: forall a. Arrays a
            => LiftedType a (Regular a)
regularType =
  case flavour (undefined :: a) of
    ArraysFunit  -> LiftedUnitT
    ArraysFarray -> RegularT
    ArraysFtuple -> TupleT (tup (prod Proxy (undefined :: a)))
  where
    tup :: ProdR Arrays t -> LiftedTupleType t (RegularTupleRepr t)
    tup ProdRunit     = NilLtup
    tup (ProdRsnoc t) = SnocLtup (tup t) regularType

-- Same as above, but for Irregular.
--
irregularType :: forall a. Arrays a
              => LiftedType a (Irregular a)
irregularType =
  case flavour (undefined :: a) of
    ArraysFunit  -> LiftedUnitT
    ArraysFarray -> IrregularT
    ArraysFtuple -> TupleT (tup (prod Proxy (undefined :: a)))
  where
    tup :: ProdR Arrays t -> LiftedTupleType t (IrregularTupleRepr t)
    tup ProdRunit     = NilLtup
    tup (ProdRsnoc t) = SnocLtup (tup t) irregularType

freeProdT :: forall a b t t'. (IsAtuple t, IsAtupleRepr t', ArrRepr t ~ (a,b))
          => LiftedTupleType (ProdRepr t) t'
          -> LiftedType t (FreeProd t')
freeProdT = TupleT

-- Given that a is always a lifted type for a, making the type relation
-- reflexive, we want to be able to check for the cases when this happens.
-- IsIso captures when two types are actually the same.
--
-- Note that this is not quite the same as true type equality. The reason being
-- that (TupleRepr a ~ TupleRepr b) does not imply (a ~ b) but it does allow for
-- casting between the two.
--
data IsIso a a' where
  IsoRefl  :: IsIso a a
  IsoTuple :: (IsAtuple a, IsAtuple a')
           => IsoTuple (TupleRepr a) (TupleRepr a')
           -> IsIso a a'

data IsoTuple t t' where
  NilIso  :: IsoTuple () ()
  SnocIso :: (Arrays a, Arrays a')
          => IsoTuple t t'
          -> IsIso a a'
          -> IsoTuple (t,a) (t',a')

isIso :: LiftedType a a' -> Maybe (IsIso a' a)
isIso UnitT      = Just IsoRefl
isIso AvoidedT   = Just IsoRefl
isIso (TupleT t) = IsoTuple <$> isIsoTup t
  where
    isIsoTup :: LiftedTupleType t t' -> Maybe (IsoTuple t' t)
    isIsoTup NilLtup = Just NilIso
    isIsoTup (SnocLtup t' ty) = SnocIso <$> isIsoTup t' <*> isIso ty
isIso _          = Nothing

-- If we have two lifted types for 'a' we need to establish what the most
-- general type their corresponding terms can be converted to. We can convert
-- from avoided to regular, avoided to irregular, and regular to irregular but
-- cannot do so in reverse. It is perhaps easiest to view the lifted typing
-- relationship as a lattice and the join as the least upper bound.
--
data Join k a where
  Join :: Arrays a'
       => LiftedType a a'
       -> k a'
       -> k a'
       -> Join k a

data JoinTuple k t where
  JoinTuple :: IsAtupleRepr t'
            => LiftedTupleType t t'
            -> Atuple k t'
            -> Atuple k t'
            -> JoinTuple k t

join :: forall acc aenv a a' a''. (Arrays a, Arrays a', Arrays a'', Kit acc)
     => Size acc aenv Int
     -> LiftedType a a'
     -> LiftedType a a''
     -> acc aenv a'
     -> acc aenv a''
     -> Join (acc aenv) a
join size l1 l2 a1 a2 =
  case (l1,l2) of
    (UnitT, l)                 -> Join l a2 a2
    (l, UnitT)                 -> Join l a1 a1
    (LiftedUnitT, LiftedUnitT) -> Join LiftedUnitT a1 a1
    (AvoidedT, AvoidedT)       -> Join AvoidedT a1 a2
    (AvoidedT, l)              -> join size RegularT l (replicateA size a1) a2
    (l, AvoidedT)              -> join size l RegularT a1 (replicateA size a2)
    (RegularT, RegularT)       -> Join RegularT a1 a2
    (RegularT, l)              -> join size IrregularT l (sparsifyC a1) a2
    (l, RegularT)              -> join size l IrregularT a1 (sparsifyC a2)
    (IrregularT, IrregularT)   -> Join IrregularT a1 a2
    (TupleT t1, TupleT t2)     | JoinTuple t a1' a2' <- joinT t1 t2 (asAtupleC a1) (asAtupleC a2)
                               -> Join (freeProdT t) (inject (Atuple a1')) (inject (Atuple a2'))
    _                          -> error "Impossible lifted type relation"
  where
    joinT :: LiftedTupleType t t'
          -> LiftedTupleType t t''
          -> Atuple (acc aenv) t'
          -> Atuple (acc aenv) t''
          -> JoinTuple (acc aenv) t
    joinT NilLtup           NilLtup           NilAtup          NilAtup
      = JoinTuple NilLtup NilAtup NilAtup
    joinT (SnocLtup lt1 l1) (SnocLtup lt2 l2) (SnocAtup t1 a1) (SnocAtup t2 a2)
      | Join l a1' a2' <- join size l1 l2 a1 a2
      , JoinTuple lt t1' t2' <- joinT lt1 lt2 t1 t2
      = JoinTuple (SnocLtup lt l) (SnocAtup t1' a1') (SnocAtup t2' a2')
    joinT _                 _                  _               _
      = error "Impossible lifted tuple type relation"

-- |Vectorise any sequence computations embedded in the 'Acc' term.
--
vectoriseAcc :: Arrays t => Acc t -> Acc t
vectoriseAcc (vectoriseOpenAcc BaseC (simpleSize (Const 1)) -> LiftedAcc ty a)
  | Just iso <- isIso ty
  = castAccC iso a
vectoriseAcc _
  = $internalError "vectoriseAcc" "Unexpected lifted result of vectorisation"

-- |Vectorise any sequence computations embedded in the array function.
--
vectoriseAfun ::Afun t -> Afun t
vectoriseAfun = vectoriseOpenAfun BaseC

vectoriseOpenAfun :: Context aenv aenv' -> OpenAfun aenv t -> OpenAfun aenv' t
vectoriseOpenAfun ctx (Abody a)
  | LiftedAcc ty a'  <- vectoriseOpenAcc ctx (simpleSize (Const 1)) a
  , Just iso         <- isIso ty
  = Abody (castAccC iso a')
  | otherwise
  = $internalError "vectoriseAfun" "Unexpected lifted result of vectorisation"
vectoriseOpenAfun ctx (Alam f)
  = Alam (vectoriseOpenAfun (PushC ctx avoidedType) f)

vectoriseOpenAcc :: Arrays t
                 => Context aenv aenv'
                 -> Size OpenAcc aenv' Int
                 -> OpenAcc aenv t
                 -> LiftedOpenAcc aenv' t
vectoriseOpenAcc ctx size (OpenAcc a) = liftPreOpenAcc vectoriseOpenAcc ctx size a



-- irregularSize :: forall acc aenv t.
--                  (Kit acc, Arrays t)
--               => acc aenv (Irregular t)
--               -> Size acc aenv Int
-- irregularSize a =
--   case flavour (undefined :: t) of
--     ArraysFunit  -> Size (inject $ Aprj ZeroTupIdx a) $ the avar0
--     ArraysFarray -> Size (segmentsSizeC (segmentsC a)) $ the avar0
--     ArraysFtuple -> fromTup $ prod (Proxy :: Proxy Arrays) (undefined :: t)
--   where
--     fromTup :: (ArrRepr t ~ (l,e), IsAtuple t) => ProdR Arrays (TupleRepr t) -> Size acc aenv Int
--     fromTup ProdRunit     = error "Unreachable"
--     fromTup (ProdRsnoc _) = irregularSize $^ Aprj tupIx0 a

regularSize :: forall acc aenv t.
               (Kit acc, Arrays t)
            => acc aenv (Regular t)
            -> Size acc aenv Int
regularSize a =
  case flavour (undefined :: t) of
    ArraysFunit  -> Size (inject $ Aprj ZeroTupIdx a) $ the avar0
    ArraysFarray -> Size (inject . Alet (unregularC a) $ unit (indexLastC (Shape avar0))) $ the avar0
    ArraysFtuple -> fromTup $ prod (Proxy :: Proxy Arrays) (undefined :: t)
  where
    fromTup :: (ArrRepr t ~ (l,e), IsAtuple t) => ProdR Arrays (TupleRepr t) -> Size acc aenv Int
    fromTup ProdRunit     = error "Unreachable"
    fromTup (ProdRsnoc _) = regularSize $^ Aprj tupIx0 a

-- |The core of the lifting transformation for array expressions.
--
liftPreOpenAcc :: forall acc aenv aenv' t. (Kit acc, Arrays t)
               => VectoriseAcc acc
               -> Context aenv aenv'
               -> Size acc aenv' Int
               -> PreOpenAcc acc aenv t
               -> LiftedAcc acc aenv' t
liftPreOpenAcc vectAcc ctx size acc
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
    Collect min max i s _
                        -> collectL min max i s

  where
    avoidedAcc   :: Arrays a => acc aenv' a             -> LiftedAcc acc aenv' a
    regularAcc   :: Arrays a => acc aenv' (Regular a)   -> LiftedAcc acc aenv' a
    irregularAcc :: Arrays a => acc aenv' (Irregular a) -> LiftedAcc acc aenv' a
    avoidedAcc   = LiftedAcc avoidedType
    regularAcc   = LiftedAcc regularType
    irregularAcc = LiftedAcc irregularType

    nestedError :: String -> String -> String
    nestedError place op = "Unexpect nested parallelism in " ++ place ++ " argument to " ++ op

    cvtA :: forall t. Arrays t => acc aenv t -> LiftedAcc acc aenv' t
    cvtA a = vectAcc ctx size a

    cvtE :: forall e. Elt e
         => PreExp acc aenv e
         -> LiftedExp acc () aenv' aenv' e
    cvtE e = liftExp vectAcc (ExpBase ctx) size e

    cvtE' :: forall e. Elt e
          => PreExp acc aenv e
          -> Maybe (PreExp acc aenv' e)
    cvtE' (cvtE -> LiftedExp ae _) = ae

    liftedTupleIdx :: forall aenv t t' a. ProdR Arrays t'
                   -> TupleIdx t a
                   -> LiftedTupleType t t'
                   -> LiftedAcc (TupleIdx' t') aenv a
    liftedTupleIdx (ProdRsnoc _)  ZeroTupIdx      (SnocLtup _ a) = LiftedAcc a (TupleIdx' ZeroTupIdx)
    liftedTupleIdx (ProdRsnoc pr) (SuccTupIdx ix) (SnocLtup t _) | LiftedAcc t' (TupleIdx' ix') <- liftedTupleIdx pr ix t
                                                                 = LiftedAcc t' (TupleIdx' (SuccTupIdx ix'))
    liftedTupleIdx _              _               _              = error "Absurd"


    cvtF1 :: forall a b. PreFun acc aenv (a -> b)
          -> ( Maybe (PreFun acc aenv' (a -> b))
             , PreOpenAfun acc aenv' (Vector a -> Vector b) )
    cvtF1 (Lam (Body (liftExp vectAcc (ExpPush (ExpBase ctx)) (sizeOfVector avar0) -> LiftedExp ab b)))
      = (Lam . Body <$> ab, Alam (Abody b))
    cvtF1 _ = $internalError "liftAcc" "Impossible"

    cvtF2 :: forall a b c. PreFun acc aenv (a -> b -> c)
          -> ( Maybe (PreFun acc aenv' (a -> b -> c))
             , PreOpenAfun acc aenv' (Vector a -> Vector b -> Vector c) )
    cvtF2 (Lam (Lam (Body (liftExp vectAcc (ExpPush (ExpPush (ExpBase ctx))) (sizeOfVector avar0) -> LiftedExp ab b))))
      = (Lam . Lam . Body <$> ab, Alam (Alam (Abody b)))
    cvtF2 _ = $internalError "liftAcc" "Impossible"

    irregularC :: forall aenv e sh.
                  (Elt e, Shape sh)
               => acc aenv (Segments sh)
               -> acc aenv (Vector e)
               -> acc aenv (Irregular (Array sh e))
    irregularC = fromHOAS2 irregular

    higher :: forall env aenv sh sh'. (Slice sh, Slice sh', Shape sh, Shape sh')
           => PreOpenFun acc env aenv (sh -> sh')
           -> PreOpenFun acc env aenv ((sh:.Int) -> (sh':.Int))
    higher (Lam (Body f)) = Lam . Body $ Let (indexInit var0) (indexSnoc (weakenE ixt f) (indexLastC var1))
      where
        ixt :: (env,sh) :> ((env,sh:.Int),sh)
        ixt ZeroIdx = ZeroIdx
        ixt (SuccIdx ix) = SuccIdx (SuccIdx ix)
    higher _ = error "Absurd"

    -- asRegular :: forall t. Arrays t => LiftedAcc acc aenv' t -> Maybe (acc aenv' (Regular t))
    -- asRegular (LiftedAcc ty a) = cvt ty a
    --   where
    --     cvt :: forall t t'. (Arrays t, Arrays t') => LiftedType t t' -> acc aenv' t' -> Maybe (acc aenv' (Regular t))
    --     cvt UnitT       = Just . replicateA size
    --     cvt LiftedUnitT = Just .^ Atuple . SnocAtup NilAtup .^ Aprj tupIx0
    --     cvt AvoidedT    = Just . replicateA size
    --     cvt RegularT    = Just
    --     cvt IrregularT  = const Nothing
    --     cvt (TupleT t)  = fmap (inject . Atuple) . cvtT t . asAtupleC
    --
    --     cvtT :: forall t t'. LiftedTupleType t t' -> Atuple (acc aenv') t' -> Maybe (Atuple (acc aenv') (RegularTupleRepr t))
    --     cvtT NilLtup NilAtup = Just NilAtup
    --     cvtT (SnocLtup t ty) (SnocAtup t' a) = SnocAtup <$> cvtT t t' <*> cvt ty a

    asIrregular :: forall t. Arrays t => LiftedAcc acc aenv' t -> acc aenv' (Irregular t)
    asIrregular (LiftedAcc ty a) = cvt ty a
      where
        cvt :: forall t t'. (Arrays t, Arrays t') => LiftedType t t' -> acc aenv' t' -> acc aenv' (Irregular t)
        cvt UnitT       = sparsifyC . replicateA size
        cvt LiftedUnitT = inject . Atuple . SnocAtup NilAtup .^ Aprj tupIx0
        cvt AvoidedT    = sparsifyC . replicateA size
        cvt RegularT    = sparsifyC
        cvt IrregularT  = id
        cvt (TupleT t)  = inject . Atuple . cvtT t . asAtupleC

        cvtT :: forall t t'. LiftedTupleType t t' -> Atuple (acc aenv') t' -> Atuple (acc aenv') (IrregularTupleRepr t)
        cvtT NilLtup         NilAtup         = NilAtup
        cvtT (SnocLtup t ty) (SnocAtup t' a) = SnocAtup (cvtT t t') (cvt ty a)
        cvtT _               _               = error "Absurd"

    liftedE :: forall t. Elt t => LiftedExp acc () aenv' aenv' t -> acc aenv' (Vector t)
    liftedE (LiftedExp (Just e) _) = replicateE size e
    liftedE (LiftedExp _        e) = e

    -- Regular versions of combinators.
    -- ===================================

    aletL :: forall bnd. Arrays bnd
          => acc aenv bnd
          -> acc (aenv, bnd) t
          -> LiftedAcc acc aenv' t
    aletL bnd body | LiftedAcc ty a <- cvtA bnd
                   = inject . Alet a $* vectAcc (PushC ctx ty) (weakenA1 size) body

    avarL :: Idx aenv t
          -> LiftedAcc acc aenv' t
    avarL = cvtIx ctx
      where
        cvtIx :: forall aenv aenv'.
                 Context aenv aenv'
              -> Idx aenv t
              -> LiftedAcc acc aenv' t
        cvtIx BaseC        ix           = LiftedAcc avoidedType $^ Avar ix
        cvtIx (PushC _ ty) ZeroIdx      = LiftedAcc ty avar0
        cvtIx (PushC c _)  (SuccIdx ix) = weakenA1 (cvtIx c ix)

    atupleL :: (Arrays t, IsAtuple t)
            => Atuple (acc aenv) (TupleRepr t)
            -> LiftedAcc acc aenv' t
    atupleL t | LiftedAtuple ty t' <- cvtT (mapAtuple cvtA t)
              , ArraysFtuple <- flavour (undefined :: t)
              = LiftedAcc (freeProdT ty) $^ Atuple t'
      where
        cvtT :: forall t. Atuple (LiftedAcc acc aenv') t -> LiftedAtuple acc aenv' t
        cvtT NilAtup = LiftedAtuple NilLtup NilAtup
        cvtT (SnocAtup t (LiftedAcc ty a))
          | LiftedAtuple ty' t' <- cvtT t
          = LiftedAtuple (SnocLtup ty' ty) (SnocAtup t' a)
    atupleL _ = error "Absurd"


    aprjL :: forall a arrs. (Arrays a, Arrays arrs, IsAtuple arrs)
          => TupleIdx (TupleRepr arrs) a
          -> acc aenv arrs
          -> LiftedAcc acc aenv' a
    aprjL tup a | LiftedAcc (TupleT t) a' <- cvtA a
                , LiftedAcc ty (TupleIdx' tup') <- liftedTupleIdx (prod Proxy (dummy a')) tup t
                = LiftedAcc ty $^ Aprj tup' a'
                | otherwise = error "Absurd"

    applyL :: forall a b.
              (Arrays a, Arrays b)
           => PreOpenAfun acc aenv (a -> b)
           -> acc aenv a
           -> LiftedAcc acc aenv' b
    applyL (Alam (Abody b)) (cvtA -> LiftedAcc ty a)
      = inject . flip Apply a . Alam . Abody $* vectAcc (PushC ctx ty) (weakenA1 size) b
    applyL _                _
      = error "Absurd"

    foreignL :: (Arrays arrs, Arrays t, Foreign f)
             => f (arrs -> t)
             -> PreAfun     acc       (arrs -> t)
             -> acc             aenv  arrs
             -> LiftedAcc   acc aenv' t
    foreignL ff afun (cvtA -> LiftedAcc ty as)
      | Just iso <- isIso ty
      = LiftedAcc avoidedType $ inject $ Aforeign ff afun (castAccC iso as)
    foreignL ff (Alam (Abody afun)) (cvtA -> LiftedAcc RegularT a)
      | Just (VectorisedRegularForeign ff') <- isVectorisedRegular ff
      , LiftedAcc RegularT afun' <- vectAcc (BaseC `PushC` regularType) (regularSize avar0) afun
      , ArraysFarray <- flavour (undefined :: t)
      = regularAcc (inject $ Aforeign ff' (Alam . Abody $ afun') a)
    foreignL _  _    _
      = error $ nestedError "first" "foreign"

    acondL :: PreExp acc aenv Bool
           -> acc aenv t
           -> acc aenv t
           -> LiftedAcc acc aenv' t
    acondL (cvtE -> p) (cvtA -> t) (cvtA -> e)
      | LiftedExp (Just p') _ <- p
      , LiftedAcc ty  t' <- t
      , LiftedAcc ty' e' <- e
      , Join ty'' t'' e'' <- join size ty ty' t' e'
      = LiftedAcc ty'' $^ Acond p' t'' e''
      | otherwise
      = irregularAcc $ liftedCondC (liftedE p) (asIrregular t) (asIrregular e)

    -- TODO: Reimplement this
    awhileL :: forall t. Arrays t
            => PreOpenAfun acc aenv (t -> Scalar Bool)
            -> PreOpenAfun acc aenv (t -> t)
            -> acc             aenv t
            -> LiftedAcc acc aenv' t
    awhileL = error "Vectorisation of loops not currently supported"
    -- awhileL (liftAfun1 -> (pred_l, pred_p)) (liftAfun1 -> (iter_l,iter_p)) (cvtA -> a)
    --   | AvoidedAcc a' <- a
    --   , Just pred_p'  <- pred_p
    --   , Just iter_p'  <- iter_p
    --   = AvoidedAcc $ inject $ Awhile pred_p' iter_p' a'
    --   | otherwise
    --   = IrregularAcc
    --   $^ Alet (asIrregular a)
    --   $^ let
    --        init' = inject $ Alet (irregularValuesC $ weakenA1 pred_l `apply` avar0)
    --                       $ atup3 avar1 avar0 (fromHOAS S.or avar0)
    --
    --        pred' = Alam $ Abody $ inject $ Aprj ZeroTupIdx avar0
    --
    --        iter' :: acc (aenv', s) (Irregular t)
    --              -> acc (aenv', s) (Vector Bool)
    --              -> acc (aenv', s) (Scalar Bool)
    --              -> acc (aenv', s) (Irregular t, Vector Bool, Scalar Bool)
    --        iter' a f _ = let a' = liftedCondC f (weakenA1 iter_l `apply` a) a
    --                          f' = fromHOAS2 (S.zipWith (S.&&*)) f (irregularValuesC $ weakenA1 pred_l `apply` a')
    --                          c' = fromHOAS S.or f'
    --                      in atup3 a' f' c'
    --
    --        iter'' :: PreOpenAfun acc aenv' ((Irregular t, Vector Bool, Scalar Bool)
    --               -> (Irregular t, Vector Bool, Scalar Bool))
    --        iter'' = Alam $ Abody $ iter' (inject $ Aprj (SuccTupIdx . SuccTupIdx $ ZeroTupIdx) avar0)
    --                                      (inject $ Aprj (SuccTupIdx ZeroTupIdx) avar0)
    --                                      (inject $ Aprj ZeroTupIdx avar0)
    --
    --      in Aprj (SuccTupIdx . SuccTupIdx $ ZeroTupIdx)
    --      $^ Awhile pred'
    --                (weakenA1 iter'')
    --                init'

    useL :: Arrays a
         => ArrRepr a
         -> LiftedAcc acc aenv' a
    useL a = avoidedAcc $^ Use a

    unitL :: Elt e
          => PreExp acc aenv e
          -> LiftedAcc acc aenv' (Scalar e)
    unitL (cvtE -> LiftedExp ae e)
      = case ae of
          Nothing -> regularAcc (regularC e)
          Just e' -> avoidedAcc $^ Unit e'

    reshapeL :: forall sh sh' e.
                (Shape sh, Shape sh', Elt e)
             => PreExp acc aenv sh
             -> acc aenv (Array sh' e)
             -> LiftedAcc acc aenv' (Array sh e)
    reshapeL (cvtE -> sh) (cvtA -> a)
      | LiftedAcc AvoidedT a'  <- a
      , LiftedExp (Just sh') _ <- sh
      = avoidedAcc $^ Reshape sh' a'
      | LiftedExp (Just sh') _ <- sh
      , LiftedAcc RegularT a'  <- a
      , AsSlice <- asSlice sh'
      = regularAcc
      $  regularC
      $^ Alet (unitSize size)
      $^ Reshape (indexSnoc (weakenA1 sh') (the avar0)) (unregularC (weakenA1 a'))
      | otherwise
      -- RCE: Should an invalid use of reshape always generate a runtime error?
      = irregularAcc $ irregularReshapeC (liftedE sh) (asIrregular a)


    generateL :: forall sh e. (Elt e, Shape sh)
              => PreExp acc aenv sh
              -> PreFun acc aenv (sh -> e)
              -> LiftedAcc  acc aenv' (Array sh e)
    generateL (cvtE -> sh) (cvtF1 -> (f_a, f_l))
      | Just f <- f_a
      , LiftedExp (Just sh') _ <- sh
      = avoidedAcc
      $^ Generate sh' f
      | LiftedExp (Just sh') _ <- sh
      , AsSlice <- asSlice sh'
      = regularAcc
      $^ Alet (unitSize size)
      $  regularC
      $^ Reshape (indexSnoc (weakenA1 sh') (the avar0))
      $^ weakenA1 f_l `subApply` flattenC (inject $ Generate (indexSnoc (weakenA1 sh') (the avar0)) (fun1 indexInit))
      | otherwise
      = irregularAcc
      $^ Alet (segmentsFromExp sh)
      $ irregularC avar0
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
      | LiftedAcc AvoidedT a'            <- a
      , LiftedExp (Just slix') _ <- slix
      = avoidedAcc
      $^ Replicate sl slix' a'
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
      | LiftedAcc AvoidedT a'            <- a
      , LiftedExp (Just slix') _ <- slix
      = avoidedAcc
      $^ Slice sl a' slix'
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
    mapL (cvtF1 -> (f_a, f_l)) (cvtA -> a)
      | Just f <- f_a
      = appL (inject . Map f)
             (regularC .^ Map f . unregularC)
             (\a' -> inject . Alet a'
                  $ irregularC (segmentsC avar0)
                  $^ Map (weakenA1 f) (irregularValuesC avar0))
             a
      | otherwise
      = appFL size
              (regularC . apply (generalizeRank f_l) . unregularC)
              (\a' -> inject . Alet a'
                   . irregularC (segmentsC avar0)
                   $ weakenA1 f_l `apply` irregularValuesC avar0)
              a

    zipWithL :: forall sh a b c. (Elt a, Elt b, Elt c, Shape sh)
             => PreFun     acc aenv  (a -> b -> c)
             -> acc            aenv  (Array sh a)
             -> acc            aenv  (Array sh b)
             -> LiftedAcc  acc aenv' (Array sh c)
    zipWithL (cvtF2 -> (f_a, f_l)) (cvtA -> a) (cvtA -> b)
      | Just f <- f_a
      = let
           zipA a' = inject . ZipWith f a'
           zipR a' = regularC .^ ZipWith f (unregularC a') . unregularC
           zipIr a' b' =  inject
                       .  Alet (liftedZipC a' b')
                       .^ Alet (unzipC (irregularValuesC avar0))
                       .  irregularC (segmentsC avar1)
                       $^ ZipWith (weakenA2 f) (fstA avar0) (sndA avar0)
        in withL (\a' -> withL (avoidedAcc . zipA  a')
                                (regularAcc . zipR  (replicateA size a'))
                                (irregularAcc . zipIr (sparsifyC (replicateA size a')))
                                b)
                  (\a' -> appFL size
                                (zipR a')
                                (zipIr (sparsifyC a'))
                                b)
                  (\a' -> irregularAcc $ zipIr a' (asIrregular b))
                  a
      | otherwise
      = let
          zipR  a' = regularC .^ subApply2 (generalizeRank2 f_l) (unregularC a') . unregularC
          zipIr a' b' =  inject
                      .  Alet (liftedZipC a' b')
                      .^ Alet (unzipC (irregularValuesC avar0))
                      .  irregularC (segmentsC avar1)
                      $^ subApply2 (weakenA2 f_l) (fstA avar0) (sndA avar0)
        in withFL size
                  (\a' -> appFL size
                                (zipR a')
                                (zipIr (sparsifyC a'))
                                b)
                  (\a' -> irregularAcc $ zipIr a' (asIrregular b))
                  a

    foldL :: forall sh e. (Elt e, Shape sh)
          => PreFun acc     aenv  (e -> e -> e)
          -> PreExp acc     aenv  e
          -> acc            aenv  (Array (sh:.Int) e)
          -> LiftedAcc  acc aenv' (Array sh e)
    foldL (cvtF2 -> (Just f,_)) (cvtE -> LiftedExp (Just z) _) (cvtA -> a)
      | AsSlice <- asSlice (Proxy :: Proxy sh)
      = let
          foldA = inject
                . Fold f z
          foldR = regularC .^ Fold f z . unregularC
          foldIr a = inject
                   .  Alet a
                   .^ Alet (makeFoldSegmentsC (segmentsC avar0))
                   .  irregularC (sndA avar0)
                   $^ FoldSeg (weakenA2 f) (weakenA2 z) (irregularValuesC avar1) (fstA avar0)
        in appL foldA foldR foldIr a
    foldL _ _ _
      = error $ nestedError "first or second" "fold"

    fold1L :: forall sh e. (Elt e, Shape sh)
           => PreFun acc  aenv  (e -> e -> e)
           -> acc            aenv  (Array (sh:.Int) e)
           -> LiftedAcc  acc aenv' (Array sh e)
    fold1L (cvtF2 -> (Just f,_)) (cvtA -> a)
      | AsSlice <- asSlice (Proxy :: Proxy sh)
      = let
          foldA = inject
                . Fold1 f
          foldR = regularC .^ Fold1 f . unregularC
          foldIr a = inject
                   .  Alet a
                   .^ Alet (makeFoldSegmentsC (segmentsC avar0))
                   .  irregularC (sndA avar0)
                   $^ Fold1Seg (weakenA2 f) (irregularValuesC avar1) (fstA avar0)
        in appL foldA foldR foldIr a
    fold1L _ _
      = error $ nestedError "first or second" "fold1"

    -- TODO: Check this. Don't think it is correct.
    foldSegL :: forall sh e i. (Elt e, Shape sh, IsIntegral i, Elt i)
             => PreFun acc aenv (e -> e -> e)
             -> PreExp acc aenv e
             -> acc            aenv (Array (sh:.Int) e)
             -> acc            aenv (Sugar.Segments i)
             -> LiftedAcc  acc aenv' (Array (sh:.Int) e)
    foldSegL (cvtF2 -> (Just f,_)) (cvtE -> LiftedExp (Just z) _) (cvtA -> a) (cvtA -> segs)
      | LiftedAcc AvoidedT a'    <- a
      , LiftedAcc AvoidedT segs' <- segs
      = avoidedAcc
      $^ FoldSeg f z a' segs'
      | AsSlice <- asSlice (Proxy :: Proxy sh)
      = irregularAcc
      $^ Alet (asIrregular a)
      $^ Alet (weakenA1 $ asIrregular segs)
      $^ Alet (makeFoldSegSegmentsC (segmentsC avar1) avar0)
      $  irregularC (fstA avar0)
      $^ FoldSeg (weakenA3 f) (weakenA3 z) (irregularValuesC avar2) (sndA avar0)
    foldSegL _ _ _ _
      = error $ nestedError "first or second" "foldSeg"

    fold1SegL :: forall sh e i. (Elt e, Shape sh, IsIntegral i, Elt i)
              => PreFun acc aenv (e -> e -> e)
              -> acc            aenv (Array (sh:.Int) e)
              -> acc            aenv (Sugar.Segments i)
              -> LiftedAcc  acc aenv' (Array (sh:.Int) e)
    fold1SegL (cvtF2 -> (Just f,_)) (cvtA -> a) (cvtA -> segs)
      | LiftedAcc AvoidedT a'    <- a
      , LiftedAcc AvoidedT segs' <- segs
      = avoidedAcc
      $^ Fold1Seg f a' segs'
      | AsSlice <- asSlice (Proxy :: Proxy sh)
      = irregularAcc
      $^ Alet (asIrregular a)
      $^ Alet (weakenA1 $ asIrregular segs)
      $^ Alet (makeFoldSegSegmentsC (segmentsC avar1) avar0)
      $  irregularC (fstA avar0)
      $^ Fold1Seg (weakenA3 f) (irregularValuesC avar2) (sndA avar0)
    fold1SegL _ _ _
      = error $ nestedError "first" "foldSeg"

    scanl1L :: forall e. Elt e
               => PreFun acc  aenv  (e -> e -> e)
               -> acc            aenv  (Vector e)
               -> LiftedAcc  acc aenv' (Vector e)
    scanl1L (cvtF2 -> (Just f,_)) (cvtA -> a)
      | LiftedAcc AvoidedT a' <- a
      = avoidedAcc
      $^ Scanl1 f a'
      | otherwise
      = irregularAcc
      $ scanl1Lift f (asIrregular a)
    scanl1L _ _
      = error $ nestedError "first" "scanl1"

    scanlL :: forall e. Elt e
               => PreFun acc  aenv  (e -> e -> e)
               -> PreExp acc  aenv  e
               -> acc            aenv  (Vector e)
               -> LiftedAcc  acc aenv' (Vector e)
    scanlL (cvtF2 -> (Just f,_)) (cvtE -> LiftedExp (Just z) _) (cvtA -> a)
      | LiftedAcc AvoidedT a' <- a
      = avoidedAcc
      $^ Scanl f z a'
      | otherwise
      = irregularAcc
      $  scanlLift f z (asIrregular a)
    scanlL _ _ _
      = error $ nestedError "first or second" "scanl"

    scanl'L :: forall e. Elt e
               => PreFun acc  aenv  (e -> e -> e)
               -> PreExp acc  aenv  e
               -> acc            aenv  (Vector e)
               -> LiftedAcc  acc aenv' (Vector e, Scalar e)
    scanl'L (cvtF2 -> (Just f,_)) (cvtE -> LiftedExp (Just z) _) (cvtA -> a)
      | LiftedAcc AvoidedT a' <- a
      = avoidedAcc
      $^ Scanl' f z a'
      | otherwise
      = irregularAcc
      $^ Alet (asIrregular a)
      $^ Alet (S.map S.unindex1 `fromHOAS` shapesC (segmentsC avar0))
      $^ Alet (irregularValuesC avar1)
      $^ Alet (irregularValuesC $ scanlLift (weakenA3 f) (weakenA3 z) avar2)
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
               $ SnocAtup (SnocAtup NilAtup (irregular (segmentsFromShapes (S.map S.index1 seg')) body))
                          (sparsify $ regular sums))
            avar2
            avar1
            avar0
    scanl'L _ _ _
      = error $ nestedError "first or second" "scanl"

    scanr1L :: forall e. Elt e
               => PreFun acc  aenv  (e -> e -> e)
               -> acc            aenv  (Vector e)
               -> LiftedAcc  acc aenv' (Vector e)
    scanr1L (cvtF2 -> (Just f,_)) (cvtA -> a)
      | LiftedAcc AvoidedT a' <- a
      = avoidedAcc
      $^ Scanr1 f a'
      | otherwise
      = irregularAcc
      $ scanr1Lift f (asIrregular a)
    scanr1L _ _
      = error $ nestedError "first" "scanr1"

    scanrL :: forall e. Elt e
               => PreFun acc  aenv  (e -> e -> e)
               -> PreExp acc  aenv  e
               -> acc            aenv  (Vector e)
               -> LiftedAcc  acc aenv' (Vector e)
    scanrL (cvtF2 -> (Just f,_)) (cvtE -> LiftedExp (Just z) _) (cvtA -> a)
      | LiftedAcc AvoidedT a' <- a
      = avoidedAcc
      $^ Scanr f z a'
      | otherwise
      = irregularAcc
      $ scanrLift f z (asIrregular a)
    scanrL _ _ _
      = error $ nestedError "first or second" "scanr"

    scanr'L :: forall e. Elt e
               => PreFun acc  aenv  (e -> e -> e)
               -> PreExp acc  aenv  e
               -> acc            aenv  (Vector e)
               -> LiftedAcc  acc aenv' (Vector e, Scalar e)
    scanr'L (cvtF2 -> (Just f,_)) (cvtE -> LiftedExp (Just z) _) (cvtA -> a)
      | LiftedAcc AvoidedT a' <- a
      = avoidedAcc
      $^ Scanr' f z a'
      | otherwise
      = irregularAcc
      $^ Alet (asIrregular a)
      $^ Alet (segmentsC avar0)
      $^ Alet (irregularValuesC avar1)
      $^ Alet (irregularValuesC $ scanrLift (weakenA3 f) (weakenA3 z) avar2)
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
               $ SnocAtup (SnocAtup NilAtup (irregular (segmentsFromShapes (S.map S.index1 seg')) body))
                          (sparsify $ regular sums))
            avar2
            avar1
            avar0
    scanr'L _ _ _
      = error $ nestedError "first or second" "scanr'"

    backpermuteL :: forall sh sh' e. (Shape sh, Shape sh', Elt e)
                 => PreExp acc  aenv  sh'
                 -> PreFun acc  aenv  (sh' -> sh)
                 -> acc            aenv  (Array sh e)
                 -> LiftedAcc  acc aenv' (Array sh' e)
    backpermuteL (cvtE -> sh) (cvtF1 -> (f_a, f_l)) (cvtA -> a)
      | LiftedAcc AvoidedT a'          <- a
      , LiftedExp (Just sh') _ <- sh
      , Just f                 <- f_a
      =  avoidedAcc
      $^ Backpermute sh' f a'
      | Just f <- f_a
      , LiftedExp (Just sh') _ <- sh
      , AsSlice         <- asSlice (Proxy :: Proxy sh)
      , AsSlice         <- asSlice (Proxy :: Proxy sh')
      = let
          reg a' = regularAcc . regularC
                 $^ Alet (unregularC a')
                 $^ Backpermute (indexSnoc (weakenA1 sh') (indexLastC (Shape avar0))) (higher (weakenA1 f)) avar0
          ireg a' = regularAcc . regularC
                   $^ Alet (unitSize size)
                   $^ Reshape (indexSnoc (weakenA1 sh') (the avar0))
                   $^ Alet (regularSegsC avar0 (weakenA1 $ unit sh'))
                   $^ Alet (liftedBackpermutePreC avar0)
                   $  liftedBackpermuteC (atup (fstA avar0) (inject $ Map (weakenA3 f) (sndA avar0)))
                                         (weakenA3 a')
        in withFL size reg ireg a
      | otherwise
      =  irregularAcc
      $^ Alet (segmentsFromExp sh)
      $^ Alet (liftedBackpermutePreC avar0)
      $  irregularC avar1
      $  liftedBackpermuteC (atup (fstA avar0) (inject $ weakenA2 f_l `subApply` sndA avar0))
                            (weakenA2 . asIrregular $ a)

    permuteL :: (Shape sh, Shape sh', Elt e)
             => PreFun acc  aenv  (e -> e -> e)
             -> acc            aenv  (Array sh' e)
             -> PreFun acc  aenv  (sh -> sh')
             -> acc            aenv  (Array sh  e)
             -> LiftedAcc  acc aenv' (Array sh' e)
    permuteL (cvtF2 -> (Just comb, _))
             (cvtA -> defs)
             (cvtF1 -> (p_a, p_l))
             (cvtA -> a)
      | Just p           <- p_a
      , LiftedAcc AvoidedT a'    <- a
      , LiftedAcc AvoidedT defs' <- defs
      =  avoidedAcc
      $^ Permute comb defs' p a'
      -- TODO: Regular permutation
      |  otherwise
      =  irregularAcc
      $^ Alet (asIrregular defs)
      $^ Alet (weakenA1 $ asIrregular a)
      $  let init     = avar0
             defaults = avar1
             shapes   = segmentsC init
             shapes'  = segmentsC defaults
             enums    = inject . Map (fun1 (Prj tupIx0)) . enumSegC $ shapes
             ixs      = weakenA2 p_l `subApply` enums
             ixs'     = asOffsetsOfC (irregularC shapes $^ ixs) shapes'
             vals     = Permute (weakenA2 $ comb)
                                (irregularValuesC defaults)
                                (fun1 (ixs' `Index`))
                                (irregularValuesC init)
          in irregularC shapes' $^ vals
    permuteL _ _ _ _
      = error $ nestedError "first" "permute"

    stencilL :: (Elt e, Elt e', Stencil sh e stencil)
             => PreFun acc aenv (stencil -> e')
             -> Boundary                (EltRepr e)
             -> acc            aenv (Array sh e)
             -> LiftedAcc  acc aenv' (Array sh e')
    stencilL (cvtF1 -> (Just f,_)) b (cvtA -> LiftedAcc AvoidedT a)
      = avoidedAcc
      $^ Stencil f b a
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
    stencil2L (cvtF2 -> (Just f,_))
              b1
              (cvtA -> LiftedAcc AvoidedT a1)
              b2
              (cvtA -> LiftedAcc AvoidedT a2)
      = avoidedAcc
      $^ Stencil2 f b1 a1 b2 a2
    stencil2L _                                 _  _  _  _
      = error $ "Disallowed nested parallelism: Stencil operations must reside at the top level of "
             ++ "parallel nesting and the supplied stencil function contain no nested parallelism."

    collectL :: Arrays arrs
             => PreExp acc aenv Int
             -> Maybe (PreExp acc aenv Int)
             -> Maybe (PreExp acc aenv Int)
             -> PreOpenNaturalSeq acc aenv arrs
             -> LiftedAcc acc aenv' arrs
    collectL min max i s
      | Just s' <- strengthenUnder ctx (reduceOpenSeq s)
      , Just cs <- vectoriseOpenSeq vectAcc ctx size s
      = LiftedAcc avoidedType
      $^ Alet (unitSize size)
      $^ Collect (maximum (the avar0) (fromMaybe (Const 1) (weakenA1 <$> cvtE' min)))
                 (weakenA1 <$> (cvtE' =<< max))
                 (weakenA1 <$> (cvtE' =<< i))
                 (weakenA1 $ fuseSeq s')
                 (Just (weakenA1 $ fuseSeq cs))
      | otherwise
      = error "Nested sequence computation is not closed in its accumulators"

    scanl1Lift :: forall aenv e. Elt e
               => PreFun acc aenv (e -> e -> e)
               -> acc aenv (Irregular (Array DIM1 e))
               -> acc aenv (Irregular (Array DIM1 e))
    scanl1Lift f a
      = inject
      $  Alet a
      $  irregularC (segmentsC avar0)
      $  sndA
      $  unzipC
      $^ Scanl1 (weakenA1 $ segmented f)
      $  let
           flags :: forall aenv e. Elt e => acc (aenv, Irregular (Array DIM1 e)) (Vector Int)
           flags = fromHOAS mkHeadFlags (segmentsC avar0)
         in fromHOAS2 S.zip flags (irregularValuesC avar0)

    scanlLift :: forall aenv e. Elt e
              => PreFun acc aenv (e -> e -> e)
              -> PreExp acc aenv e
              -> acc aenv (Irregular (Array DIM1 e))
              -> acc aenv (Irregular (Array DIM1 e))
    scanlLift f z a
      =  scanl1Lift f
      $^ Alet a
      $^ Alet (segmentsC avar0)
      $^ Alet (irregularValuesC avar1)
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
             in irregular seg' vec')
          avar2
          avar1
          avar0

    scanr1Lift :: forall aenv e. Elt e
               => PreFun acc aenv (e -> e -> e)
               -> acc aenv (Irregular (Array DIM1 e))
               -> acc aenv (Irregular (Array DIM1 e))
    scanr1Lift f a
      = inject
      $  Alet a
      $  irregularC (segmentsC avar0)
      $  sndA
      $  unzipC
      $^ Scanr1 (weakenA1 $ segmented f)
      $  let
           flags :: forall aenv e. Elt e => acc (aenv, Irregular (Array DIM1 e)) (Vector Int)
           flags = fromHOAS mkTailFlags (segmentsC avar0)
         in fromHOAS2 S.zip flags (irregularValuesC avar0)

    scanrLift :: forall aenv e. Elt e
              => PreFun acc aenv (e -> e -> e)
              -> PreExp acc aenv e
              -> acc            aenv (Irregular (Array DIM1 e))
              -> acc            aenv (Irregular (Array DIM1 e))
    scanrLift f z a
      =  scanr1Lift f
      $^ Alet a
      $^ Alet (segmentsC avar0)
      $^ Alet (irregularValuesC avar1)
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
             in irregular seg' vec')
          avar2
          avar1
          avar0

    segmentsFromExp :: forall sh. Shape sh
                    => LiftedExp acc () aenv' aenv' sh
                    -> acc aenv' (Segments sh)
    segmentsFromExp (LiftedExp (Just sh) _)
      = regularSegsC (unitSize size) (unit sh)
    segmentsFromExp (LiftedExp _ sh)
      = segmentsFromShapesC sh

data ExpContext env aenv aenv' aenv'' where
  ExpBase :: Context aenv aenv'
          -> ExpContext () aenv aenv' aenv'
  ExpPush :: ExpContext env aenv aenv' aenv''
          -> ExpContext (env,e) aenv aenv' (aenv'', Vector e)

-- |Performs the lifting transform on a given scalar expression.
--
liftExp :: forall acc env aenv aenv' aenv'' e. (Kit acc, Elt e)
        => VectoriseAcc acc
        -> ExpContext env aenv aenv' aenv''
        -> Size acc aenv'' Int
        -> PreOpenExp acc env aenv e
        -> LiftedExp acc env aenv' aenv'' e
liftExp vectAcc ctx size exp
  = relift $ case exp of
      Let bnd body       -> letL bnd body
      Var ix             -> varL ix
      Const c            -> unlifted (Const c)
      Tuple tup          -> cvtTuple tup
      Prj ix t           -> cvtE1 (Prj ix) (cvtE t)
      IndexNil           -> unlifted IndexNil
      IndexAny           -> unlifted IndexAny
      IndexCons sh sz    -> cvtE2 IndexCons (cvtE sh) (cvtE sz)
      IndexHead sh       -> cvtE1 IndexHead (cvtE sh)
      IndexTail sh       -> cvtE1 IndexTail (cvtE sh)
      IndexTrans sh      -> cvtE1 IndexTrans (cvtE sh)
      IndexSlice x ix sh -> cvtE1 (IndexSlice x ix) (cvtE sh)
      IndexFull x ix sl  -> cvtE2 (IndexFull x) (cvtE ix) (cvtE sl)
      ToIndex sh ix      -> cvtE2 ToIndex (cvtE sh) (cvtE ix)
      FromIndex sh ix    -> cvtE2 FromIndex (cvtE sh) (cvtE ix)
      ToSlice x sh i     -> cvtE2 (ToSlice x) (cvtE sh) (cvtE i)
      Cond p t e         -> condL (cvtE p) (cvtE t) (cvtE e)
      While p it i       -> whileL p it i
      PrimConst c        -> unlifted (PrimConst c)
      PrimApp f x        -> cvtE1 (PrimApp f) (cvtE x)
      Index a sh         -> indexL a sh
      LinearIndex a i    -> linearIndexL a i
      Shape a            -> shapeL a
      ShapeSize sh       -> cvtE1 ShapeSize (cvtE sh)
      Intersect s t      -> cvtE2 Intersect (cvtE s) (cvtE t)
      Union s t          -> cvtE2 Union (cvtE s) (cvtE t)
      Foreign ff f e     -> cvtE1 (Foreign ff f) (cvtE e)
  where
    relift :: Elt a
           => LiftedExp acc env aenv' aenv'' a
           -> LiftedExp acc env aenv' aenv'' a
    relift (LiftedExp (Just e) _)
      | Just e' <- strengthenE (const Nothing) e
      = LiftedExp (Just e) (replicateE size (weaken (under ctx) e'))
    relift le
      = le

    unlifted :: forall a. Elt a => (forall env aenv. PreOpenExp acc env aenv a) -> LiftedExp acc env aenv' aenv'' a
    unlifted a = LiftedExp (Just a) (replicateE size a)

    under :: forall env aenv aenv' aenv''.
             ExpContext env aenv aenv' aenv''
          -> aenv' :> aenv''
    under (ExpBase _)   = id
    under (ExpPush ctx) = SuccIdx . under ctx

    cvtE1 :: forall aenv' aenv'' a b. (Elt a, Elt b)
          => (forall env aenv. PreOpenExp acc env aenv a -> PreOpenExp acc env aenv b)
          -> LiftedExp acc env aenv' aenv'' a
          -> LiftedExp acc env aenv' aenv'' b
    cvtE1 f (LiftedExp ae e) = LiftedExp (f <$> ae) $^ Map (fun1 f) e

    cvtE2 :: (Elt a, Elt b, Elt c)
          => (forall env aenv. PreOpenExp acc env aenv a -> PreOpenExp acc env aenv b -> PreOpenExp acc env aenv c)
          -> LiftedExp acc env aenv' aenv'' a
          -> LiftedExp acc env aenv' aenv'' b
          -> LiftedExp acc env aenv' aenv'' c
    cvtE2 f (LiftedExp ae1 e1) (LiftedExp ae2 e2) = LiftedExp (f <$> ae1 <*> ae2) $^ ZipWith (fun2 f) e1 e2

    cvtE :: forall e. Elt e
         => PreOpenExp acc env aenv e
         -> LiftedExp acc env aenv' aenv'' e
    cvtE exp' = liftExp vectAcc ctx size exp'

    cvtA :: forall sh' e'. (Elt e', Shape sh')
         => acc aenv (Array sh' e')
         -> LiftedAcc acc aenv' (Array sh' e')
    cvtA (extract -> Avar ix) = cvtIx (arrayContext ctx) ix
      where
        arrayContext :: forall env aenv aenv' aenv''.
                        ExpContext env aenv aenv' aenv''
                     -> Context aenv aenv'
        arrayContext (ExpBase ctx)  = ctx
        arrayContext (ExpPush ctx') = arrayContext ctx'

        cvtIx :: forall t aenv aenv'. Arrays t
              => Context aenv aenv'
              -> Idx aenv t
              -> LiftedAcc acc aenv' t
        cvtIx BaseC         ix           = LiftedAcc avoidedType $^ Avar ix
        cvtIx (PushC _ ty)  ZeroIdx      = LiftedAcc ty avar0
        cvtIx (PushC ctx _) (SuccIdx ix) = weakenA1 (cvtIx ctx ix)
    cvtA _ = $internalError "liftExp" "Embedded array term"


    cvtF1 :: forall a b. PreOpenFun acc env aenv (a -> b)
          -> ( Maybe (PreOpenFun acc env aenv' (a -> b))
             , PreOpenAfun acc aenv'' (Vector a -> Vector b) )
    cvtF1 (Lam (Body (liftExp vectAcc (ExpPush ctx) (weakenA1 size) -> LiftedExp ab b)))
      = (Lam . Body <$> ab, Alam (Abody b))
    cvtF1 _              = $internalError "liftExp" "Impossible"

    -- Lifted versions of operations
    -- ==============================

    varL :: Elt e
         => Idx env e
         -> LiftedExp acc env aenv' aenv'' e
    varL ix = LiftedExp (Just (Var ix)) (inject (Avar (varL' ctx ix)))
      where
        varL' :: forall env aenv aenv' aenv''.
                 ExpContext env aenv aenv' aenv''
              -> Idx env e
              -> Idx aenv'' (Vector e)
        varL' (ExpBase _) _ = error "Unreachable"
        varL' (ExpPush _) ZeroIdx = ZeroIdx
        varL' (ExpPush ctx) (SuccIdx ix) = SuccIdx (varL' ctx ix)

    letL :: forall bnd_t. Elt bnd_t
         => PreOpenExp acc env          aenv bnd_t
         -> PreOpenExp acc (env, bnd_t) aenv e
         -> LiftedExp acc env aenv' aenv'' e
    letL (cvtE -> LiftedExp abnd bnd) (liftExp vectAcc (ExpPush ctx) (weakenA1 size) -> LiftedExp abody body)
      = LiftedExp (Let <$> abnd <*> abody)
                  (inject (Alet bnd body))

    -- Note:
    -- Preserving the true semantics of an operation like `Cond` is tricky with
    -- vectorisation. This particular implementation is safe provided there are
    -- no array terms embedded in scalar expressions other than array variables.
    -- For now this is true.
    --
    condL :: LiftedExp acc env aenv' aenv'' Bool
          -> LiftedExp acc env aenv' aenv'' e
          -> LiftedExp acc env aenv' aenv'' e
          -> LiftedExp acc env aenv' aenv'' e
    condL (LiftedExp ap p) (LiftedExp at t) (LiftedExp ae e)
      = LiftedExp (Cond <$> ap <*> at <*> ae)
      $^ case (ap >>= strengthenE (const Nothing)) of
           Just p' -> Acond (weaken (under ctx) p') t e
           Nothing -> zipWith3 (fun3 Cond) p t e

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
    whileL :: PreOpenFun acc env aenv (e -> Bool)
           -> PreOpenFun acc env aenv (e -> e)
           -> PreOpenExp acc env aenv e
           -> LiftedExp  acc env aenv' aenv'' e
    whileL (cvtF1 -> (ap, p)) (cvtF1 -> (ait, it)) (cvtE -> LiftedExp ai i)
      =  LiftedExp (While <$> ap <*> ait <*> ai)
      $^ Aprj (SuccTupIdx ZeroTupIdx) $^ Awhile p' it' i'
      where
        p'  :: PreOpenAfun acc aenv'' ((Vector e, Vector Bool) -> Scalar Bool)
        p'  = Alam $ Abody $ let
                flags     = sndA avar0
                any     f = inject $ Fold or (Const False) f
                or        = fun2 (PrimApp PrimLOr S.$$ tup)
              in any flags

        it' :: PreOpenAfun acc aenv'' ((Vector e, Vector Bool) -> (Vector e, Vector Bool))
        it' = Alam $ Abody $ let
                values  = fstA avar0
                flags   = sndA avar0
                values' = inject $ ZipWith (fun2 tup)
                                           (inject $ weakenA1 it `subApply` values)
                                           flags
                values'' = inject $ ZipWith (Lam $ Lam $ Body $ Cond (sndE $ var1)
                                                                     (fstE $ var1)
                                                                     var0)
                                            values'
                                            values
                flags'   = inject $ weakenA2 p `subApply` avar0
              in inject $ Alet values'' (atup avar0 flags')


        i'  :: acc aenv'' (Vector e, Vector Bool)
        i'  = i `atup` replicateE size (Const True)

    indexL :: (Elt e, Shape sh)
           => acc            aenv  (Array sh e)
           -> PreOpenExp acc env       aenv   sh
           -> LiftedExp  acc env aenv' aenv'' e
    indexL (cvtA -> LiftedAcc ty a) (cvtE -> LiftedExp aix ix)
      | AvoidedT <- ty
      = LiftedExp (Index <$> pure a <*> aix)
                  (inject . Alet (weaken (under ctx) a) $^ Map (fun1 (Index avar0)) (weakenA1 ix))
      | RegularT <- ty
      = LiftedExp Nothing (liftedRegularIndexC (weaken (under ctx) a) ix)
      | IrregularT <- ty
      = LiftedExp Nothing (liftedIrregularIndexC (weaken (under ctx) a) ix)
    indexL _ _
      = error "Absurd"

    linearIndexL :: forall sh'. (Elt e, Shape sh')
                 => acc            aenv  (Array sh' e)
                 -> PreOpenExp acc env     aenv  Int
                 -> LiftedExp acc env aenv' aenv'' e
    linearIndexL (cvtA -> LiftedAcc ty a) (cvtE -> LiftedExp aix ix)
      | AvoidedT <- ty
      = LiftedExp (LinearIndex <$> pure a <*> aix)
                  (inject . Alet (weaken (under ctx) a) $^ Map (fun1 (LinearIndex avar0)) (weakenA1 ix))
      | RegularT <- ty
      = LiftedExp Nothing (liftedRegularLinearIndexC (weaken (under ctx) a) ix)
      | IrregularT <- ty
      = LiftedExp Nothing (liftedIrregularLinearIndexC (weaken (under ctx) a) ix)
    linearIndexL _ _
      = error "Absurd"

    shapeL :: (Shape sh, Elt e')
           => acc aenv (Array sh e')
           -> LiftedExp acc env aenv' aenv'' sh
    shapeL (cvtA -> LiftedAcc ty a)
      | AvoidedT <- ty
      = LiftedExp (Just (Shape a)) (inject . Alet (weaken (under ctx) a) $ replicateE (weakenA1 size) (Shape avar0))
      -- We can still avoid vectorisation if we depend on an array term that was
      -- regularly vectorised.
      | RegularT <- ty
      , AsSlice <- asSlice (the (regularShapeC a))
      = LiftedExp (Just (indexInit (Shape (unregularC a))))
                  (inject . Alet (weaken (under ctx) a) $ replicateE (weakenA1 size) (indexInit (Shape (unregularC avar0))))
      | IrregularT <- ty
      = LiftedExp Nothing (shapesC (segmentsC (weaken (under ctx) a)))
    shapeL _
      = error "Absurd"

    cvtTuple :: IsTuple e
             => Tuple (PreOpenExp acc env aenv) (TupleRepr e)
             -> LiftedExp acc env aenv' aenv'' e
    cvtTuple (cvtT -> (at, LiftedTuple aenv t))
      =  LiftedExp (Tuple <$> at)
      .  bind aenv
      .^ Alet (unitSize (sink aenv $ size))
      $^ Generate (index1 (the avar0)) (Lam . Body $ Tuple (weakenTup SuccIdx t))

    cvtT :: forall t.
            Tuple (PreOpenExp acc env aenv) t
         -> ( Maybe (Tuple (PreOpenExp acc env aenv') t)
            , LiftedTuple acc aenv'' t)
    cvtT NilTup
      = (Just NilTup, LiftedTuple BaseEnv NilTup)
    cvtT (SnocTup t (cvtE -> LiftedExp ae e))
      | (at, LiftedTuple aenv t') <- cvtT t
      = (SnocTup <$> at <*> ae, LiftedTuple (PushEnv aenv (sink aenv e)) (SnocTup (weakenTup SuccIdx t') (Index avar0 var0)))

    weakenTup :: forall env aenv aenv' t. aenv :> aenv'
              -> Tuple (PreOpenExp acc env aenv) t
              -> Tuple (PreOpenExp acc env aenv') t
    weakenTup v = unRTup . weaken v . RebuildTup

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

data LiftedAtuple acc aenv t where
  LiftedAtuple :: (IsAtupleRepr t')
               => LiftedTupleType t t'
               -> Atuple (acc aenv) t'
               -> LiftedAtuple acc aenv t

data LiftedTuple acc aenv t where
  LiftedTuple :: Extend acc aenv aenv'
              -> Tuple (PreOpenExp acc ((),DIM1) aenv') t
              -> LiftedTuple acc aenv t


-- Lifted operations.
-- ------------------

segments :: forall sh e. (Shape sh, Elt e) => S.Acc (Irregular (Array sh e)) -> S.Acc (Segments sh)
segments a = S.Acc $ S.Aprj (SuccTupIdx ZeroTupIdx) a

irregular :: (Shape sh, Elt e) => S.Acc (Segments sh) -> S.Acc (Vector e) -> S.Acc (Irregular (Array sh e))
irregular segs vals = S.Acc $ S.Atuple $ SnocAtup (SnocAtup NilAtup segs) vals

replicate :: forall a. Arrays a => S.Exp Int -> S.Acc a -> S.Acc (Regular a)
replicate size a = case flavour (undefined :: a) of
                     ArraysFunit  -> S.Acc $ S.Atuple $ SnocAtup NilAtup $ S.unit size
                     ArraysFarray ->
                       let values = S.replicate (S.lift (Z:.size:.All)) (S.flatten a)
                       in regular $ S.reshape (S.indexSnoc (S.shape a) size) values
                     ArraysFtuple -> S.Acc $ S.Atuple $ replicateT (asAtuple a)
  where
    replicateT :: Atuple S.Acc t -> Atuple S.Acc (RegularTupleRepr t)
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

    domain = S.scanl1 (\a b -> dead b S.? (inc a, b)) heads

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
offsets (S.unatup3 -> (_,o,_)) = o

shapes :: Shape sh => S.Acc (Segments sh) -> S.Acc (Vector sh)
shapes (S.unatup3 -> (_,_,shs)) = shs

totalSize :: Shape sh => S.Acc (Segments sh) -> S.Exp Int
totalSize (S.unatup3 -> (ts,_,_)) = S.the ts

segmentsFromShapes :: Shape sh => S.Acc (Vector sh) -> S.Acc (Segments sh)
segmentsFromShapes ss = let (offs,sz) = S.scanl' (+) 0 (S.map S.shapeSize ss)
                         in irregularSegs (S.the sz) offs ss

regularSegs :: Shape sh => S.Exp Int -> S.Exp sh -> S.Acc (Segments sh)
regularSegs sz sh = S.lift (S.unit (sz * S.shapeSize sh), S.enumFromN (S.index1 sz) 0, S.fill (S.index1 sz) sh)

irregularSegs :: Shape sh => S.Exp Int -> S.Acc (Vector Int) -> S.Acc (Vector sh) -> S.Acc (Segments sh)
irregularSegs ts offs shs = S.lift ( S.unit ts
                                   , offs
                                   , shs )

regularShape :: (Shape sh, Elt e) => S.Acc (Regular (Array  sh e)) -> S.Exp sh
regularShape = S.indexInit . S.shape . unregular

indexInSeg :: (Shape sh, Elt e) => S.Acc (Irregular (Array sh e)) -> S.Exp Int -> S.Exp sh -> S.Exp e
indexInSeg arr seg ix = let segs = segments arr
                        in irregularValues arr S.!! ((offsets segs S.!! seg) + (S.toIndex (shapes segs S.!! seg) ix))

unregular :: (Shape sh, Elt e) => S.Acc (Regular (Array sh e)) -> S.Acc (Array (sh:.Int) e)
unregular = S.Acc . S.Aprj ZeroTupIdx

regular :: (Shape sh, Elt e) => S.Acc (Array (sh:.Int) e) -> S.Acc (Regular (Array sh e))
regular es = S.Acc (S.Atuple $ NilAtup `SnocAtup` es)

irregularValues :: (Shape sh, Elt e) => S.Acc (Irregular (Array sh e)) -> S.Acc (Vector e)
irregularValues = S.Acc . S.Aprj ZeroTupIdx

asAtuple :: forall a. (Arrays a, IsAtuple a) => S.Acc a -> Atuple S.Acc (TupleRepr a)
asAtuple a = tOA (prod (Proxy :: Proxy Arrays) (undefined :: a)) id
 where
   tOA :: forall t. ProdR Arrays t -> (forall e. TupleIdx t e -> TupleIdx (TupleRepr a) e) -> Atuple S.Acc t
   tOA ProdRunit     _   = NilAtup
   tOA (ProdRsnoc t) ixt = SnocAtup (tOA t (ixt . SuccTupIdx)) (S.Acc $ S.Aprj (ixt ZeroTupIdx) a)

sparsify :: forall a. (Arrays a) => S.Acc (Regular a) -> S.Acc (Irregular a)
sparsify ra =
  case flavour (undefined :: a) of
    ArraysFunit  -> S.Acc . S.Atuple $ NilAtup `SnocAtup` S.Acc (S.Aprj ZeroTupIdx ra)
    ArraysFarray -> sparsifyArray ra
    ArraysFtuple -> S.Acc . S.Atuple $ sparsifyTup (prod (Proxy :: Proxy Arrays) (undefined :: a)) (asAtuple ra)
  where
    sparsifyArray :: (Elt e, Shape sh) => S.Acc (Regular (Array sh e)) -> S.Acc (Irregular (Array sh e))
    sparsifyArray ra = S.Acc (S.Atuple $ NilAtup `SnocAtup` S.lift (S.unit sz, offs, shs) `SnocAtup` vs)
      where
        sz = S.size (unregular ra)
        vs = S.flatten (unregular ra)
        offs = S.enumFromStepN (S.index1 sz) 0 (S.shapeSize (regularShape ra))
        shs  = S.fill (S.index1 sz) (regularShape ra)

    sparsifyTup :: ProdR Arrays t -> Atuple S.Acc (RegularTupleRepr t) -> Atuple S.Acc (IrregularTupleRepr t)
    sparsifyTup ProdRunit      NilAtup         = NilAtup
    sparsifyTup (ProdRsnoc pr) (SnocAtup at a) = SnocAtup (sparsifyTup pr at) (sparsify a)
    sparsifyTup _              _               = error "Impossible tuple"


makeFoldSegments :: forall sh. (Shape sh, Slice sh) => S.Acc (Segments (sh:.Int)) -> S.Acc (Vector Int, Segments sh)
makeFoldSegments segs = S.lift (generateSeg inSegs (\seg sh ix -> (offs S.!! seg) + S.toIndex sh ix), outSegs)
  where
    offs  = offsets segs
    shs   = S.map S.indexTail (shapes segs)
    outSegs = segmentsFromShapes (S.map nonEmpty shs)
    inSegs  = segmentsFromShapes shs

nonEmpty :: forall sh. Shape sh => S.Exp sh -> S.Exp sh
nonEmpty = S.union (S.constant $ listToShape $ P.replicate (rank (ignore::sh)) 1)

makeFoldSegSegments :: forall sh i. (Shape sh, Slice sh, IsIntegral i, Elt i)
                    => S.Acc (Segments (sh:.Int))
                    -> S.Acc (Irregular (Vector i))
                    -> S.Acc (Segments (sh:.Int), Vector i)
makeFoldSegSegments segs isegs = S.lift (segmentsFromShapes shs', isegs')
  where
    shs  = shapes segs
    shs' = S.generate (S.shape shs) f
    f ix = let sh  = S.indexTail (shs S.! ix)
               shi = shapes (segments isegs) S.! ix
           in S.lift (sh :. S.shapeSize shi)

    isegs' = generateSeg (segments isegs) (\seg _ ix -> indexInSeg isegs seg ix `plus` fromIntegral (offsets (segments isegs) S.!! seg))

    -- RCE: Because we only have an IsIntegral constraint on i, we can't use the (+) and
    -- fromIntegral from the prelude. Even though we know that IsIntegral i => Num (Exp i), we
    -- can't in general prove that to the type checker.
    --
    plus :: S.Exp i -> S.Exp i -> S.Exp i
    plus a b = S.Exp (S.PrimApp (PrimAdd numType) (S.lift (a,b)))

    fromIntegral :: S.Exp Int -> S.Exp i
    fromIntegral i = S.Exp (S.PrimApp (PrimFromIntegral integralType numType) i)

-- RCE: I have a strong feeling this can be done better.
--
liftedCond :: forall a. Arrays a
           => S.Acc (Vector Bool) -- condition
           -> S.Acc (Irregular a)    -- then
           -> S.Acc (Irregular a)    -- else
           -> S.Acc (Irregular a)
liftedCond pred th el
  = case (flavour (undefined :: a)) of
      ArraysFunit  -> th
      ArraysFarray -> liftedCond1 th el
      ArraysFtuple -> S.Acc $ S.Atuple $ cvtT (prod (Proxy :: Proxy Arrays) (undefined :: a)) (asAtuple th) (asAtuple el)
  where
    cvtT :: ProdR Arrays t -> Atuple S.Acc (IrregularTupleRepr t) -> Atuple S.Acc (IrregularTupleRepr t) -> Atuple S.Acc (IrregularTupleRepr t)
    cvtT ProdRunit     NilAtup          NilAtup          = NilAtup
    cvtT (ProdRsnoc t) (SnocAtup t1 a1) (SnocAtup t2 a2) = SnocAtup (cvtT t t1 t2) (liftedCond pred a1 a2)
#if __GLASGOW_HASKELL__ < 800
    cvtT _             _                _                = error "unreachable"
#endif

    liftedCond1 :: (Elt e, Shape sh) => S.Acc (Irregular (Array sh e)) -> S.Acc (Irregular (Array sh e)) -> S.Acc (Irregular (Array sh e))
    liftedCond1 t e = irregular segs vals
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

        vals_t = irregularValues t
        vals_e = irregularValues e
        ones   = S.fill (S.index1 $ S.the sz_v) (1 :: S.Exp Int)
        enums  = S.scanl1Seg (+) ones $ S.map S.shapeSize shs
        vals   = S.zipWith (\t ind -> let (f,s) = S.unlift t in f S.? (vals_t S.!! (s + ind), vals_e S.!! (s + ind)))
                           flag_offs
                           enums

--liftedAwhile :: forall t.
--                (Arrays t, Arrays (Regular t))
--             => (S.Acc (Regular t) -> S.Acc (Vector Bool))
--             -> (S.Acc (Regular t) -> S.Acc (Regular t))
--             -> S.Acc (Regular t)
--             -> S.Acc (Regular t)
--liftedAwhile pred iter init
--  = let
--      (a, _ :: S.Acc (Vector Bool), _ :: S.Acc (Scalar Bool)) = S.unlift $ S.awhile pred' iter' init'
--    in a
--  where
--    init' = let f = pred init
--            in S.lift (init, f, S.or f)

--    pred' :: S.Acc (Regular t, Vector Bool, Scalar Bool) -> S.Acc (Scalar Bool)
--    pred' f = let (_ :: S.Acc (Regular t), _ :: S.Acc (Vector Bool), c) = S.unlift f in c

--    iter' :: S.Acc (Regular t, Vector Bool, Scalar Bool) -> S.Acc (Regular t, Vector Bool, Scalar Bool)
--    iter' (S.unlift -> (a, f, _ :: S.Acc (Scalar Bool)))
--      = let a' = liftedCond f (iter a) a
--            f' = S.zipWith (S.&&*) f (pred a')
--            c' = S.or f'
--        in S.lift (a', f', c')

irregularReshape :: (Elt e, Shape sh, Shape sh') => S.Acc (Vector sh) -> S.Acc (Irregular (Array sh' e)) -> S.Acc (Irregular (Array sh e))
irregularReshape extents a =
  let segs  = segments a
      segs' = irregularSegs (totalSize segs) (offsets segs) extents
  in irregular segs' (irregularValues a)

--liftedGenerate :: (Elt e, Shape sh)
--               => S.Acc (Vector sh)
--               -> (S.Acc (Vector sh) -> S.Acc (Vector e))
--               -> S.Acc (Regular (Array sh e))
--liftedGenerate extents fun
--  = irregular extents (fun (enumSeg extents))

liftedZip :: (Elt a, Elt b, Shape sh)
          => S.Acc (Irregular (Array sh a))
          -> S.Acc (Irregular (Array sh b))
          -> S.Acc (Irregular (Array sh (a,b)))
liftedZip as bs = irregular segs vals
  where
    segs = intersectSegments (segments as) (segments bs)

    vals = generateSeg segs (\seg _ ix -> S.lift (indexInSeg as seg ix, indexInSeg bs seg ix))

intersectSegments :: Shape sh => S.Acc (Segments sh) -> S.Acc (Segments sh) -> S.Acc (Segments sh)
intersectSegments as bs = segmentsFromShapes (S.zipWith S.intersect (shapes as) (shapes bs))

--liftedFold :: (Elt e, Shape sh, Slice sh)
--           => (S.Exp e -> S.Exp e -> S.Exp e)
--           -> S.Exp e
--           -> S.Acc (LiftedArray (sh:.Int) e)
--           -> S.Acc (LiftedArray sh        e)
--liftedFold f z a = irregular segs' vals
--  where
--    vals = S.foldSeg f z (regularValues a) (replicateSeg segs' heads')

--    (segs, heads) = S.unzip $ S.map (\sh -> S.lift (S.indexTail sh, S.indexHead sh)) (segments a)

--    segs' = makeNonEmpty segs
--    heads' = S.zipWith (\sh h -> S.shapeSize sh S.==* 0 S.? (0,h)) segs heads

--liftedFoldSeg :: (Elt e, Shape sh, Slice sh)
--              => (S.Exp e -> S.Exp e -> S.Exp e)
--              -> S.Exp e
--              -> S.Acc (LiftedArray (sh:.Int) e)
--              -> S.Acc (LiftedArray DIM1      Int)
--              -> S.Acc (LiftedArray (sh:.Int) e)
--liftedFoldSeg f z a is = irregular segs vals
--  where
--    tails = S.map S.indexTail (segments a)
--    vals = S.foldSeg f z (regularValues a) isegs
--    segs = S.zipWith (\x y -> S.lift (x:.y)) tails
--                                             (S.map S.unindex1 (segments is))
--    isegs = replicateVectors tails is


--liftedBackpermute :: (Elt e, Shape sh, Shape sh')
--                  => S.Acc (Vector sh')
--                  -> (S.Acc (Vector sh') -> S.Acc (Vector sh))
--                  -> S.Acc (LiftedArray sh  e)
--                  -> S.Acc (Regular (Array sh' e))
--liftedBackpermute shapes f a = irregular shapes vals'
--  where
--    segs   = segments a
--    vals   = regularValues a
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
                  -> S.Acc (Irregular (Array sh e))
                  -> S.Acc (Vector e)
liftedBackpermute (S.unlift -> (segs, ixs)) a = vals'
  where
    vals  = irregularValues a
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
--liftedPermute combine defaults perm init = irregular shapes' vals
--  where
--    shapes  = segments ini
--    shapes' = segments defaults
--    enums   = enumSeg shapes
--    ixs     = perm enums
--    ixs'    = asOffsetsOf (irregular shapes ixs) shapes'
--    vals    = S.permute combine (regularValues defaults) (ixs' S.!) (regularValues init)

asOffsetsOf :: (Shape sh, Shape sh')
            => S.Acc (Irregular (Array sh sh'))
            -> S.Acc (Segments sh')
            -> S.Acc (Vector DIM1)
asOffsetsOf ixs shapes' = S.map S.index1 $ S.zipWith (+) starts (S.map S.shapeSize (irregularValues ixs))
  where
    shapes = segments ixs
    starts = replicateSeg shapes (offsets shapes')

liftedRegularIndex :: (Shape sh, Elt e)
                   => S.Acc (Regular (Array sh e))
                   -> S.Acc (Vector sh)
                   -> S.Acc (Vector e)
liftedRegularIndex arr ixs = S.backpermute (S.shape ixs) f (unregular arr)
  where
    f ix = S.indexSnoc (ixs S.! ix) (S.unindex1 ix)

liftedIrregularIndex :: (Shape sh, Elt e)
                     => S.Acc (Irregular (Array sh e))
                     -> S.Acc (Vector sh)
                     -> S.Acc (Vector e)
liftedIrregularIndex arr ixs = S.backpermute (S.shape ixs) f (irregularValues arr)
  where
    f ix = let off = offsets (segments arr) S.! ix
               sh  = shapes  (segments arr) S.! ix
           in S.index1 $ off + S.toIndex sh (ixs S.! ix)


liftedRegularLinearIndex :: (Shape sh, Elt e)
                         => S.Acc (Regular (Array sh e))
                         -> S.Acc (Vector Int)
                         -> S.Acc (Vector e)
liftedRegularLinearIndex arr ixs = S.generate (S.shape ixs) f
  where
    f ix = unregular arr S.!! (ixs S.! ix + (S.shapeSize ix) * S.shapeSize (regularShape arr))

liftedIrregularLinearIndex :: (Shape sh, Elt e)
                           => S.Acc (Irregular (Array sh e))
                           -> S.Acc (Vector Int)
                           -> S.Acc (Vector e)
liftedIrregularLinearIndex arr ixs = S.backpermute (S.shape ixs) f (irregularValues arr)
  where
    f ix = let off = offsets (segments arr) S.! ix
           in S.index1 $ off + ixs S.! ix

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

castAcc :: (Arrays a, Arrays a') => IsIso a a' -> S.Acc a -> S.Acc a'
castAcc IsoRefl      a = a
castAcc (IsoTuple t) a = S.Acc . S.Atuple $ castTup t (asAtuple a)
  where
    castTup :: IsoTuple t t' -> Atuple S.Acc t -> Atuple S.Acc t'
    castTup NilIso NilAtup = NilAtup
    castTup (SnocIso t' is) (SnocAtup t'' a) = castTup t' t'' `SnocAtup` castAcc is a
    castTup _ _ = error "Absurd"

replicateC :: (Arrays a, Kit acc)
           => acc aenv (Scalar Int) -> acc aenv a -> acc aenv (Regular a)
replicateC = fromHOAS2 (replicate . S.the)

enumSegC :: (Shape sh, Kit acc) => acc aenv (Segments sh) -> acc aenv (Vector (Int, sh, sh))
enumSegC = fromHOAS enumSeg

zipC :: (Kit acc, Elt a, Elt b, Shape sh)
     => acc aenv (Array sh a)
     -> acc aenv (Array sh b)
     -> acc aenv (Array sh (a,b))
zipC = fromHOAS2 S.zip

unzipC :: (Kit acc, Elt a, Elt b, Shape sh)
       => acc aenv (Array sh (a,b))
       -> acc aenv (Array sh a, Array sh b)
unzipC = fromHOAS (S.lift . S.unzip)

liftedZipC :: (Kit acc, Shape sh, Elt a, Elt b)
           => acc aenv (Irregular (Array sh a))
           -> acc aenv (Irregular (Array sh b))
           -> acc aenv (Irregular (Array sh (a,b)))
liftedZipC = fromHOAS2 liftedZip

liftedCondC :: (Arrays a, Kit acc)
            => acc aenv (Vector Bool)
            -> acc aenv (Irregular a)
            -> acc aenv (Irregular a)
            -> acc aenv (Irregular a)
liftedCondC = fromHOAS3 liftedCond

irregularReshapeC :: (Elt e, Shape sh, Shape sh', Kit acc)
                  => acc aenv (Vector sh)
                  -> acc aenv (Irregular (Array sh' e))
                  -> acc aenv (Irregular (Array sh e))
irregularReshapeC = fromHOAS2 irregularReshape

liftedBackpermutePreC :: (Shape sh', Kit acc)
                      => acc aenv (Segments sh')
                      -> acc aenv (Vector Int, Vector sh')
liftedBackpermutePreC = fromHOAS liftedBackpermutePre

liftedBackpermuteC :: (Elt e, Shape sh, Kit acc)
                   => acc aenv (Vector Int, Vector sh)
                   -> acc aenv (Irregular (Array sh e))
                   -> acc aenv (Vector e)
liftedBackpermuteC = fromHOAS2 liftedBackpermute

asOffsetsOfC :: (Shape sh, Shape sh', Kit acc)
             => acc aenv (Irregular (Array sh sh'))
             -> acc aenv (Segments sh')
             -> acc aenv (Vector DIM1)
asOffsetsOfC = fromHOAS2 asOffsetsOf

liftedRegularIndexC :: (Kit acc, Shape sh, Elt e)
                    => acc aenv (Regular (Array sh e))
                    -> acc aenv (Vector sh)
                    -> acc aenv (Vector e)
liftedRegularIndexC = fromHOAS2 liftedRegularIndex

liftedIrregularIndexC :: (Kit acc, Shape sh, Elt e)
                      => acc aenv (Irregular (Array sh e))
                      -> acc aenv (Vector sh)
                      -> acc aenv (Vector e)
liftedIrregularIndexC = fromHOAS2 liftedIrregularIndex

liftedRegularLinearIndexC :: (Kit acc, Shape sh, Elt e)
                          => acc aenv (Regular (Array sh e))
                          -> acc aenv (Vector Int)
                          -> acc aenv (Vector e)
liftedRegularLinearIndexC = fromHOAS2 liftedRegularLinearIndex

liftedIrregularLinearIndexC :: (Kit acc, Shape sh, Elt e)
                            => acc aenv (Irregular (Array sh e))
                            -> acc aenv (Vector Int)
                            -> acc aenv (Vector e)
liftedIrregularLinearIndexC = fromHOAS2 liftedIrregularLinearIndex

indexLastC :: forall acc env aenv sh. Shape sh
           => PreOpenExp acc env aenv (sh:.Int)
           -> PreOpenExp acc env aenv Int
indexLastC | AsSlice <- asSlice (Proxy :: Proxy sh)
           = IndexHead . IndexTrans

flattenC :: forall acc aenv sh e. (Kit acc, Shape sh, Elt e)
         => acc aenv (Array sh e) -> acc aenv (Vector e)
flattenC = fromHOAS S.flatten

regularC :: (Kit acc, Shape sh, Elt e) => acc aenv (Array (sh:.Int) e) -> acc aenv (Regular (Array sh e))
regularC = fromHOAS regular

unregularC :: (Kit acc, Shape sh, Elt e) => acc aenv (Regular (Array sh e)) -> acc aenv (Array (sh:.Int) e)
unregularC = fromHOAS unregular

segmentsC :: (Kit acc, Shape sh, Elt e) => acc aenv (Irregular (Array sh e)) -> acc aenv (Segments sh)
segmentsC = fromHOAS segments

shapesC :: (Kit acc, Shape sh) => acc aenv (Segments sh) -> acc aenv (Vector sh)
shapesC = fromHOAS shapes

irregularValuesC :: (Kit acc, Shape sh, Elt e) => acc aenv (Irregular (Array sh e)) -> acc aenv (Vector e)
irregularValuesC = inject . Aprj ZeroTupIdx

segmentsFromShapesC :: (Kit acc, Shape sh) => acc aenv (Vector sh) -> acc aenv (Segments sh)
segmentsFromShapesC = fromHOAS segmentsFromShapes

regularSegsC :: (Kit acc, Shape sh) => acc aenv (Scalar Int) -> acc aenv (Scalar sh) -> acc aenv (Segments sh)
regularSegsC = fromHOAS2 ((. S.the) . regularSegs . S.the)

sparsifyC :: (Kit acc, Arrays a) => acc aenv (Regular a) -> acc aenv (Irregular a)
sparsifyC = fromHOAS sparsify

regularShapeC :: (Kit acc, Shape sh, Elt e) => acc aenv (Regular (Array sh e)) -> acc aenv (Scalar sh)
regularShapeC = fromHOAS (S.unit . regularShape)

makeFoldSegmentsC :: (Kit acc, Shape sh, Slice sh) => acc aenv (Segments (sh:.Int)) -> acc aenv (Vector Int, Segments sh)
makeFoldSegmentsC = fromHOAS makeFoldSegments

makeFoldSegSegmentsC :: (Kit acc, Shape sh, Slice sh, IsIntegral i, Elt i) => acc aenv (Segments (sh:.Int)) -> acc aenv (Irregular (Vector i)) -> acc aenv (Segments (sh:.Int), Vector i)
makeFoldSegSegmentsC = fromHOAS2 makeFoldSegSegments

castAccC :: (Kit acc, Arrays a, Arrays a') => IsIso a a' -> acc aenv a -> acc aenv a'
castAccC iso = fromHOAS (castAcc iso)

asAtupleC :: forall acc aenv a. (Arrays a, IsAtuple a, Kit acc) => acc aenv a -> Atuple (acc aenv) (TupleRepr a)
asAtupleC a = tOA (prod (Proxy :: Proxy Arrays) (undefined :: a)) id
  where
    tOA :: forall t. ProdR Arrays t -> (forall e. TupleIdx t e -> TupleIdx (TupleRepr a) e) -> Atuple (acc aenv) t
    tOA ProdRunit     _   = NilAtup
    tOA (ProdRsnoc t) ixt = SnocAtup (tOA t (ixt . SuccTupIdx)) (inject $ Aprj (ixt ZeroTupIdx) a)

-- HOAS-conversion
-- ---------------

-- Conversion from HOAS to Debruijn form in such a way that it is easier to use during the transform
--

cvtHOAS :: (S.Afunction f, Kit acc) => f -> PreOpenAfun acc aenv (S.AfunctionR f)
cvtHOAS = weaken ixt . fromOpenAfun . S.convertAfun True True True True
  where
    ixt :: () :> aenv
    ixt ix = case ix of {}

cvtExpHOAS :: (Kit acc, S.Function f) => f -> PreOpenFun acc env aenv (S.FunctionR f)
cvtExpHOAS = weakenE ixt . weaken ixt . fromOpenFun . S.convertFun True
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

fromExpHOAS3 :: forall acc env aenv a b c d. (Elt a, Elt b, Elt c, Elt d, Kit acc)
             => (S.Exp a -> S.Exp b -> S.Exp c -> S.Exp d)
             -> PreOpenExp acc env aenv a
             -> PreOpenExp acc env aenv b
             -> PreOpenExp acc env aenv c
             -> PreOpenExp acc env aenv d
fromExpHOAS3 f a b c =
  case (cvtExpHOAS f :: PreOpenFun acc env aenv (a -> b -> c -> d)) of
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

tup :: forall acc env aenv a b. (Elt a, Elt b)
    => PreOpenExp acc env aenv a
    -> PreOpenExp acc env aenv b
    -> PreOpenExp acc env aenv (a,b)
tup a b = Tuple (SnocTup (SnocTup NilTup a) b)

atup :: forall acc aenv a b t. (Kit acc, Arrays a, Arrays b, Arrays t, IsAtuple t, ProdRepr t ~ (((),a),b))
     => acc aenv a
     -> acc aenv b
     -> acc aenv t
atup a b = inject $ Atuple $ NilAtup `SnocAtup` a `SnocAtup` b

-- atup3 :: forall acc aenv a b c t. (Kit acc, Arrays a, Arrays b, Arrays c, Arrays t, IsAtuple t, ProdRepr t ~ ((((),a),b),c))
--       => acc aenv a
--       -> acc aenv b
--       -> acc aenv c
--       -> acc aenv t
-- atup3 a b c = inject $ Atuple $ NilAtup `SnocAtup` a `SnocAtup` b `SnocAtup` c

tupIx0 :: TupleIdx (t,a) a
tupIx0 = ZeroTupIdx

tupIx1 :: TupleIdx ((t,a),b) a
tupIx1 = SuccTupIdx ZeroTupIdx

-- tupIx2 :: TupleIdx (((t,a),b),c) a
-- tupIx2 = SuccTupIdx (SuccTupIdx ZeroTupIdx)
--
-- tupIx3 :: TupleIdx ((((t,a),b),c),d) a
-- tupIx3 = SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))

replicateA :: forall acc aenv a.
             (Kit acc, Arrays a)
           => Size acc  aenv Int
           -> acc aenv a
           -> acc aenv (Regular a)
replicateA size a
  = replicateC (unitSize size) a

replicateE :: forall acc aenv e.
              (Kit acc, Elt e)
           => Size acc aenv Int
           -> PreExp acc aenv e
           -> acc aenv (Vector e)
replicateE (Size b s) e = inject . Alet b $^ Replicate (SliceFixed SliceNil) (IndexCons IndexNil s) (inject . weakenA1 $ Unit e)

var0 :: Elt t0 => PreOpenExp acc (env, t0) aenv t0
var0 = Var ZeroIdx

var1 :: Elt t1 => PreOpenExp acc ((env, t1), t0) aenv t1
var1 = Var $ SuccIdx ZeroIdx

var2 :: Elt t
     => PreOpenExp acc (((env, t), s), r) aenv t
var2 = Var . SuccIdx . SuccIdx $ ZeroIdx

avar0 :: (Kit acc, Arrays t)
      => acc (aenv, t) t
avar0 = inject $ Avar ZeroIdx

avar1 :: (Kit acc, Arrays a1) => acc ((aenv, a1), a0) a1
avar1 = inject $ Avar $ SuccIdx ZeroIdx

avar2 :: (Kit acc, Arrays a2) => acc (((aenv, a2), a1), a0) a2
avar2 = inject $ Avar $ SuccIdx . SuccIdx $ ZeroIdx

-- avar3 :: (Kit acc, Arrays t)
--       => acc ((((aenv, t), s), r), q) t
-- avar3 = inject $ Avar $ SuccIdx . SuccIdx . SuccIdx $ ZeroIdx
--
-- avar4 :: (Kit acc, Arrays t)
--       => acc (((((aenv, t), s), r), q), p) t
-- avar4 = inject $ Avar $ SuccIdx . SuccIdx . SuccIdx . SuccIdx $ ZeroIdx
--
-- avar5 :: (Kit acc, Arrays t)
--       => acc ((((((aenv, t), s), r), q), p), o) t
-- avar5 = inject $ Avar $ SuccIdx . SuccIdx . SuccIdx . SuccIdx . SuccIdx $ ZeroIdx

the :: Elt e
    => acc aenv (Scalar e)
    -> PreOpenExp acc env aenv e
the a = Index a IndexNil

unit :: (Kit acc, Elt e)
     => PreExp acc aenv e
     -> acc aenv (Scalar e)
unit = inject . Unit

index1 :: PreOpenExp acc env aenv Int -> PreOpenExp acc env aenv DIM1
index1 = IndexCons IndexNil

index2 :: PreOpenExp acc env aenv Int
       -> PreOpenExp acc env aenv Int
       -> PreOpenExp acc env aenv DIM2
index2 h w = IndexNil `IndexCons` h `IndexCons` w

unindex1 :: PreOpenExp acc env aenv DIM1
         -> PreOpenExp acc env aenv Int
unindex1 = IndexHead

indexSnoc :: (Shape sh, Slice sh)
          => PreOpenExp acc env aenv sh
          -> PreOpenExp acc env aenv Int
          -> PreOpenExp acc env aenv (sh:.Int)
indexSnoc sh i = IndexTrans (IndexCons (IndexTrans sh) i)

indexInit :: (Shape sh, Slice sh)
          => PreOpenExp acc env aenv (sh:.Int)
          -> PreOpenExp acc env aenv sh
indexInit = IndexTrans . IndexTail . IndexTrans

segmented :: (Elt e, Kit acc)
          => PreOpenFun acc env aenv (e -> e -> e)
          -> PreOpenFun acc env aenv ((Int, e) -> (Int, e) -> (Int, e))
segmented f = Lam . Lam . Body
  $ tup (PrimBOr integralType `PrimApp` tup (fstE var1) (fstE var0))
        (Cond (PrimNEq scalarType `PrimApp` tup (fstE var0) (Const 0))
              (sndE var0)
              (subApplyE2 (weakenE2 f) (sndE var0) (sndE var1)))

maximum :: PreOpenExp acc env aenv Int
        -> PreOpenExp acc env aenv Int
        -> PreOpenExp acc env aenv Int
maximum a b = PrimApp (PrimMax scalarType) (tup a b)

newTop :: env :> env'
       -> (env,t) :> (env', t)
newTop _  ZeroIdx = ZeroIdx
newTop wk (SuccIdx ix) = SuccIdx $ wk ix

swapTop :: ((env,s),t) :> ((env,t),s)
swapTop ZeroIdx                 = SuccIdx ZeroIdx
swapTop (SuccIdx ZeroIdx)       = ZeroIdx
swapTop (SuccIdx (SuccIdx idx)) = SuccIdx (SuccIdx idx)

weakenA1 :: Sink f
         => f aenv t
         -> f (aenv,s) t
weakenA1 = weaken SuccIdx

weakenA2 :: Sink f => f aenv t -> f ((aenv,s1),s0) t
weakenA2 = weaken (SuccIdx . SuccIdx)

weakenA3 :: Sink f => f aenv t -> f (((aenv,s2),s1),s0) t
weakenA3 = weaken (SuccIdx . SuccIdx . SuccIdx)

weakenA4 :: Sink f => f aenv t -> f ((((aenv,s3),s2),s1),s0) t
weakenA4 = weaken (SuccIdx . SuccIdx . SuccIdx . SuccIdx)

-- weakenA5 :: Sink f
--          => f aenv t
--          -> f (((((aenv,o),p),q),r),s) t
-- weakenA5 = weaken (SuccIdx . SuccIdx . SuccIdx . SuccIdx . SuccIdx)
--
-- weakenA6 :: Sink f
--          => f aenv t
--          -> f ((((((aenv,n),o),p),q),r),s) t
-- weakenA6 = weaken (SuccIdx . SuccIdx . SuccIdx . SuccIdx . SuccIdx . SuccIdx)

weakenE1 :: SinkExp f
         => f env     aenv t
         -> f (env,s) aenv t
weakenE1 = weakenE SuccIdx

weakenE2 :: SinkExp f => f env aenv t -> f ((env,s1),s0) aenv t
weakenE2 = weakenE (SuccIdx . SuccIdx)

fun1 :: (Elt a, Elt b)
     => (forall env. PreOpenExp acc env aenv a -> PreOpenExp acc env aenv b)
     -> PreOpenFun acc env aenv (a -> b)
fun1 f = Lam (Body (f var0))

fun2 :: (Elt a, Elt b, Elt c)
     => (forall env. PreOpenExp acc env aenv a -> PreOpenExp acc env aenv b -> PreOpenExp acc env aenv c)
     -> PreOpenFun acc env aenv (a -> b -> c)
fun2 f = Lam (Lam (Body (f var1 var0)))

fun3 :: (Elt a, Elt b, Elt c, Elt d)
     => (forall env'. PreOpenExp acc env' aenv a -> PreOpenExp acc env' aenv b -> PreOpenExp acc env' aenv c -> PreOpenExp acc env' aenv d)
     -> PreOpenFun acc env aenv (a -> b -> c -> d)
fun3 f = Lam (Lam (Lam (Body (f var2 var1 var0))))

fromContext :: forall acc aenv aenv' t. (Kit acc, Arrays t)
            => Context aenv aenv'
            -> Idx aenv t
            -> Maybe (PreOpenAcc acc aenv' t)
fromContext BaseC        ix           = Just $ Avar ix
fromContext (PushC _ ty) ZeroIdx      = castAccC' <$> isIso ty <*> pure (Avar ZeroIdx)
  where
    castAccC' iso = extract . castAccC iso . inject
fromContext (PushC d _)  (SuccIdx ix) = weakenA1 <$> fromContext d ix


strengthenUnder :: (Rebuildable f, AccClo f ~ acc, Kit acc)
                => Context aenv aenv'
                -> f aenv t
                -> Maybe (f aenv' t)
strengthenUnder d = rebuildPartial (fromContext d)

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

-- apply2 :: (Kit acc, Arrays a)
--        => PreOpenAfun acc aenv (a -> b -> c)
--        -> acc             aenv a
--        -> acc             aenv b
--        -> acc             aenv c
-- apply2 f a = inject . subApply2 f a
--
-- apply3 :: (Kit acc, Arrays a)
--        => PreOpenAfun acc aenv (a -> b -> c -> d)
--        -> acc             aenv a
--        -> acc             aenv b
--        -> acc             aenv c
--        -> acc             aenv d
-- apply3 (Alam (Alam (Alam (Abody f)))) a b c
--   = inject . Alet a
--   $^ Alet (weakenA1 b)
--   $^ Alet (weakenA2 c)
--   $ f

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

infixr 9 .^
(.^) :: Kit acc
     => (acc aenv b -> c)
     -> (a -> PreOpenAcc acc aenv b)
     -> (a -> c)
f .^ g = f . inject . g


simpleSize :: forall acc aenv e. Kit acc => PreExp acc aenv e ->  Size acc aenv e
simpleSize e = Size (inject $ Use () :: acc aenv ()) (weakenA1 e)

unitSize :: (Kit acc, Elt e) => Size acc aenv e -> acc aenv (Scalar e)
unitSize (Size b s) = inject $ Alet b $ inject $ Unit s

sizeOfVector :: (Kit acc, Elt e) => acc aenv (Vector e) -> Size acc aenv Int
sizeOfVector a = Size a (ShapeSize (Shape avar0))

generalizeRank :: (Kit acc, Shape sh, Elt e, Elt e')
               => PreOpenAfun acc aenv (Vector e -> Vector e')
               -> PreOpenAfun acc aenv (Array sh e -> Array sh e')
generalizeRank (Alam (Abody f)) =  Alam . Abody
                                $^ Reshape (Shape avar0)
                                $^ Alet (flattenC avar0)
                                $  weaken (swapTop . SuccIdx) f
generalizeRank _ = error "Absurd"

generalizeRank2 :: (Kit acc, Shape sh, Elt e, Elt e', Elt e'')
                => PreOpenAfun acc aenv (Vector e -> Vector e' -> Vector e'')
                -> PreOpenAfun acc aenv (Array sh e -> Array sh e' -> Array sh e'')
generalizeRank2 f
  =  Alam . Alam . Abody
  $^ Alet (zipC avar1 avar0)
  $^ Reshape (Shape avar0)
  $^ Alet (unzipC avar0)
  $^ subApply2 (weakenA4 f) (flattenC (fstA avar0)) (flattenC (sndA avar0))

-- Debugging
-- ----------
-- trace :: String -> String -> a -> a
-- trace header msg
--   = Debug.trace Debug.dump_vectorisation
--   $ header ++ ": " ++ msg

-- Sequence vectorisation
-- ------------------------

vectoriseOpenSeq :: forall acc aenv aenv' a. Kit acc
                 => VectoriseAcc acc
                 -> Context aenv aenv'
                 -> Size acc aenv' Int
                 -> PreOpenNaturalSeq acc aenv a
                 -> Maybe (PreOpenChunkedSeq  acc aenv' a)
vectoriseOpenSeq vectAcc ctx size seq =
  case seq of
    Producer p s -> do
      LiftedAcc ty p' <- cvtP p
      Producer p' <$> vectoriseOpenSeq vectAcc (PushC ctx ty) (weakenA1 size) s
    Consumer c   -> cvtC c
    Reify _      -> error "Unexpected reify"
  where
    cvtP :: NaturalProducer acc aenv t -> Maybe (LiftedAcc (ChunkedProducer acc) aenv' t)
    cvtP p =
      case p of
        Pull _              -> Nothing
        Subarrays sh a      -> LiftedAcc RegularT <$> (subarrays <$> cvtE sh <*> pure a)
        Produce l (Alam (Abody f))
          | LiftedAcc ty f' <- vectAcc (ctx `PushC` RegularT) (regularSize avar0) f
          -> LiftedAcc ty <$> (ProduceAccum <$> cvtL l <*> return (streamify f') <*> return nil)
        -- MapBatch f c c' a x -> Just $ mapBatch f c c' a x
        ProduceAccum{}      -> stageError
        Produce _ _         -> error "Absurd"

    cvtL :: Maybe (PreExp acc aenv Int) -> Maybe (Maybe (PreExp acc aenv' Int))
    cvtL Nothing = Just Nothing
    cvtL (Just l) | Just l' <- strengthenUnder ctx l
                  = Just (Just l')
                  | otherwise
                  = $internalError "vectoriseOpenSeq" "Limit depends on sequence elements"

    -- mapBatch :: forall a b c s. (Arrays a, Arrays b, Arrays c, Arrays s)
    --          => PreOpenAfun acc aenv (s -> a -> b)
    --          -> PreOpenAfun acc aenv (s -> Regular b -> (s, Regular c))
    --          -> PreOpenAfun acc aenv (s -> Irregular b -> (s, Irregular c))
    --          -> acc  aenv s
    --          -> acc  aenv a
    --          -> LiftedAcc (ChunkedProducer acc) aenv' (s,c)
    -- mapBatch (Alam (Alam (Abody f))) (cvtAF' -> Just c) (cvtAF' -> Just c') (cvtA' -> Just s) (cvtA -> LiftedAcc ty a)
    --   | LiftedAcc ty' f' <- cvtAUnder (ctx `PushC` avoidedType `PushC` ty) f
    --   , Just iso <- isIso ty'
    --   = let
    --       theType = TupleT (NilLtup `SnocLtup` avoidedType `SnocLtup` avoidedType)
    --
    --       f'' :: PreOpenAfun acc aenv' (Scalar (Int, Int) -> s -> ((s,c),s))
    --       f'' = Alam . Alam . Abody $
    --         let
    --           sz = sndE (the avar1)
    --           a' = inject $ Alet (unit sz) (weaken (newTop (SuccIdx . SuccIdx)) a)
    --           b  = castAccC iso $ apply3 (weakenA2 . Alam . Alam . Alam . Abody $ f') avar0 a' (unit sz)
    --         in fromHOAS repack $ apply2 (weakenA2 c) avar0 (replicateA (simpleSize sz) b)
    --
    --       repack :: Arrays c' => S.Acc (s, c') -> S.Acc ((s,c'),s)
    --       repack (S.unatup2 -> (s,c)) = S.lift ((s, c), s)
    --
    --     in LiftedAcc theType $ ProduceAccum Nothing f'' s

    subarrays :: (Shape sh, sh :<= DIM3, Elt e) => PreExp acc aenv' sh -> Array sh e -> ChunkedProducer acc aenv' (Regular (Array sh e))
    subarrays sh arr = ProduceAccum subLimit f nil
      where
        f = Alam . Alam . Abody $ atup (liftedSubArrays (the avar1) (weakenA2 sh) arr) nil
        totalSize = Const (Sugar.size (shape arr))
        subSize = ShapeSize sh
        subLimit = Just (totalSize `div` subSize)
        div a b = PrimApp (PrimIDiv integralType) (tup a b)


    streamify :: Arrays t' => acc (aenv', Regular (Scalar Int)) t' -> PreOpenAfun acc aenv' (Scalar (Int,Int) -> () -> (t', ()))
    streamify f =
      let f' = Alam . Abody $ f
      in Alam . Alam . Abody . nest $ weakenA2 f' `apply` fromRange avar1

    fromRange :: forall aenv. acc aenv (Scalar (Int, Int)) -> acc aenv (Regular (Scalar Int))
    fromRange r =  regularC
                $^ Generate (index1 $ sndE r') (Lam . Body $ fstE r' `plus` unindex1 var0)
      where
        r' :: forall env. PreOpenExp acc env aenv (Int, Int)
        r' = the r
        plus a b = PrimApp (PrimAdd numType) (tup a b)

    nest :: forall aenv a. Arrays a => acc aenv a -> acc aenv (a,())
    nest = inject . Atuple . (\a -> NilAtup `SnocAtup` a `SnocAtup` nil)

    cvtC :: NaturalConsumer acc aenv t -> Maybe (PreOpenChunkedSeq acc aenv' t)
    cvtC c =
      case c of
        FoldBatch f a x -> foldBatch f a x
        Stuple t        -> Consumer . Stuple <$> cvtCT t
        Last _ _        -> stageError
        Elements x      -> Just (elements x)
        Tabulate x      -> Just (tabulate x)

    foldBatch :: forall a x. (Arrays a, Arrays x)
              => PreOpenAfun acc aenv (a -> x -> a)
              -> acc  aenv a
              -> acc  aenv x
              -> Maybe (PreOpenChunkedSeq acc aenv' a)
    foldBatch (Alam (Alam (Abody f))) a x
      | LiftedAcc tya f' <- vectAcc (ctx `PushC` avoidedType `PushC` avoidedType) (simpleSize (sndE (the avar0))) (weakenA1 (alet (weakenA1 x) f))
      = do
          a'  <- cvtA' a
          iso <- isIso tya
          let f'' = alet (weaken swapTop (castAccC iso f')) $ atup avar0 avar0
          return
            $ Producer (ProduceAccum Nothing (Alam (Alam (Abody f''))) a') (Consumer (Last avar0 (weakenA1 a')))
    foldBatch _ _ _
      = error "Impossible function"

    elements :: (Shape sh, Elt e)
             => acc aenv (Array sh e)
             -> PreOpenChunkedSeq acc aenv' (Vector e)
    elements x
      | LiftedAcc ty x' <- cvtA x
      = Producer (ProduceAccum Nothing (Alam . Alam . Abody $ (fromHOAS2 (f ty) (weakenA2 x') avar0)) a)
                          (Consumer (Last avar0 (weakenA1 a)))
      where
        f ty x a = let x' = a S.++ flatten ty x in S.lift (x',x')

        a :: Elt e => acc aenv' (Vector e)
        a = inject . Use $ newArray empty undefined

        flatten :: (Shape sh, Elt e) => LiftedType (Array sh e) x -> S.Acc x -> S.Acc (Vector e)
        flatten AvoidedT   = S.flatten
        flatten RegularT   = S.flatten . unregular
        flatten IrregularT = irregularValues
        flatten _          = error "Impossible lifted type"

    tabulate :: forall sh e. (Shape sh, Elt e)
             => acc aenv (Array sh e)
             -> PreOpenChunkedSeq acc aenv' (Array (sh:.Int) e)
    tabulate x
      | LiftedAcc ty x' <- cvtA x
      = Producer (ProduceAccum Nothing (Alam . Alam . Abody $ (fromHOAS3 (f ty) avar1 (weakenA2 x') avar0)) a)
                 (Consumer (Last avar0 (weakenA1 (inject (Use (newArray empty undefined))))))
      where
        f :: LiftedType (Array sh e) x
          -> S.Acc (Scalar (Int,Int))
          -> S.Acc x
          -> S.Acc (Array (sh:.Int) e)
          -> S.Acc (Array (sh:.Int) e, Array (sh:.Int) e)
        f ty ix x a
          = let x'  = reduce ty x
                x'' = S.fst (S.the ix) S.==* S.constant 0
                    S.?| ( x'
                         , concat a x')

            in S.lift (x'',x'')
          where
            concat x y =
              let
                sh_x = S.indexInit (S.shape x)
                sh_y = S.indexInit (S.shape y)
                sz_x = S.indexLast (S.shape x)
                sz_y = S.indexLast (S.shape y)
              in S.generate (S.indexSnoc (S.intersect sh_x sh_y) (sz_x + sz_y))
                            (\ix -> S.indexLast ix  S.<* sz_x S.? (x S.! ix, y S.! ix))

            reduce :: LiftedType (Array sh e) x -> S.Acc x -> S.Acc (Array (sh:.Int) e)
            reduce AvoidedT x = S.reshape (S.indexSnoc (S.shape x) 1) x
            reduce RegularT x = unregular x
            reduce IrregularT x =
              let
                shs = shapes (segments x)
                sh = S.fold1 S.intersect shs
              in S.generate (S.indexSnoc (S.the sh) (S.length shs)) (\ix -> indexInSeg x (S.indexLast ix) (S.indexInit ix))
            reduce _ _ = error "Impossible lifted type"

        a :: acc aenv' (Array (sh:.Int) e)
        a = inject $ Use (newArray empty undefined)

    cvtCT :: Atuple (PreOpenNaturalSeq acc aenv) t -> Maybe (Atuple (PreOpenChunkedSeq acc aenv') t)
    cvtCT NilAtup        = Just NilAtup
    cvtCT (SnocAtup t c) = SnocAtup <$> cvtCT t <*> vectoriseOpenSeq vectAcc ctx size c

    nil :: forall aenv. acc aenv ()
    nil = inject $ Atuple NilAtup

    cvtE :: PreExp acc aenv t -> Maybe (PreExp acc aenv' t)
    cvtE e | Just e' <- strengthenUnder ctx e
           = Just e'
    cvtE _ = Nothing
    --
    -- cvtF :: Fun aenv t -> Fun aenv' t
    -- cvtF = vectoriseSeqOpenFun ctx
    --

    -- cvtAUnder :: forall aenv aenv' t. Arrays t
    --           => Context aenv aenv'
    --           -> acc aenv t
    --           -> LiftedAcc acc (aenv', Scalar Int) t
    -- cvtAUnder ctx = vectoriseOpenAcc (PushC ctx avoidedType) (simpleSize (the avar0)) . weakenA1

    cvtA :: Arrays t => acc aenv t -> LiftedAcc acc aenv' t
    cvtA = vectAcc ctx size

    cvtA' :: Arrays t => acc aenv t -> Maybe (acc aenv' t)
    cvtA' (cvtA -> LiftedAcc ty a) | Just iso <- isIso ty
                                   = Just (castAccC iso a)
                                   | otherwise
                                   = Nothing
    --
    -- cvtAF' :: PreOpenAfun acc aenv t -> Maybe (PreOpenAfun acc aenv' t)
    -- cvtAF' t =
    --   case strengthenUnder ctx t of
    --     Nothing -> $internalError "vectoriseOpenSeq" "Sequence invariant array function depends on sequence elements"
    --     Just t' -> Just t'

    -- untup :: PreOpenAfun acc aenv t -> PreOpenAfun acc aenv t
    -- untup = untupleAfun BaseReducedMap

    stageError = $internalError "vectoriseOpenSeq" "AST is at wrong stage for vectorisation. It seems to have already been vectorised."

liftedSubArrays :: forall acc aenv sh e. (sh :<= DIM3, Elt e, Shape sh, Kit acc)
                => PreExp acc aenv (Int, Int)
                -> PreExp acc aenv sh
                -> Array sh e
                -> acc aenv (Regular (Array sh e))
liftedSubArrays index sh arr
  | AsSlice <- asSlice (Proxy :: Proxy sh)
  = regularC $
  case (maximumRank :: sh :<=: DIM3) of
    RankZ          -> flattenC $^ Use arr
    RankSnoc RankZ -> inject $ Reshape (indexSnoc sh (sndE index))
      $^ Subarray (index1 (fstE index `times` unindex1 sh)) (index1 (sndE index `times` unindex1 sh)) arr
    RankSnoc (RankSnoc RankZ)
      -> inject . Reshape (indexSnoc sh (sndE index))
      $^ Alet (inject $ Unit (twoDC index sh (Const fsh)))
      $  head `catC` body `catC` tail
      where
        head = inject $ Subarray (fstE . fst $ the avar0) (sndE . fst $ the avar0) arr
        body = inject
             $  Backpermute (weakenA1 $ index2 (sndE index) (IndexHead sh)) (Lam . Body $ reorderC (weakenA1 . weakenE1 $ sh) (Const fsh) var0)
             $^ Subarray (fstE . snd $ the avar0) (sndE . snd $ the avar0) arr
        tail = inject $ Subarray (fstE . fst $ the avar0) (sndE . fst $ the avar0) arr

        fst :: (Elt a, Elt b, Elt c) => PreExp acc aenv' (a,b,c) -> PreExp acc aenv' a
        fst = Prj (SuccTupIdx (SuccTupIdx ZeroTupIdx))
        snd :: (Elt a, Elt b, Elt c) => PreExp acc aenv' (a,b,c) -> PreExp acc aenv' b
        snd = Prj (SuccTupIdx ZeroTupIdx)
        -- trd :: (Elt a, Elt b, Elt c) => PreExp acc aenv' (a,b,c) -> PreExp acc aenv' c
        -- trd = Prj ZeroTupIdx

        fsh = fromElt (shape arr)
    _ -> error "Absurd"

  where
    times a b = PrimApp (PrimMul numType) (tup a b)
    catC :: acc aenv' (Array DIM2 e) -> acc aenv' (Array DIM2 e) -> acc aenv' (Array DIM2 e)
    catC = fromHOAS2 cat

    twoDC :: PreOpenExp acc env aenv' (Int,Int) -> PreOpenExp acc env aenv' DIM2 -> PreOpenExp acc env aenv' DIM2 -> PreOpenExp acc env aenv' ((DIM2,DIM2), (DIM2,DIM2), (DIM2,DIM2))
    twoDC = fromExpHOAS3 twoD

    reorderC :: PreOpenExp acc env aenv' DIM2 -> PreOpenExp acc env aenv' DIM2 -> PreOpenExp acc env aenv' DIM2 -> PreOpenExp acc env aenv' DIM2
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


-- Sequence AST reduction
--
reduceStreamSeq :: Kit acc
                => StreamSeq Int acc a
                -> StreamSeq Int acc a
reduceStreamSeq (StreamSeq binds seq) = StreamSeq binds (reduceOpenSeq seq)

reduceOpenSeq :: forall acc aenv a. Kit acc
              => PreOpenNaturalSeq acc aenv a
              -> PreOpenNaturalSeq acc aenv a
reduceOpenSeq seq =
  case seq of
    Producer p s -> Producer (cvtP p) (reduceOpenSeq s)
    Consumer c   -> cvtC c
    Reify a      -> Reify a
  where
    cvtP :: NaturalProducer acc aenv t -> NaturalProducer acc aenv t
    cvtP p =
      case p of
        Pull src           -> Pull src
        Subarrays sh a     -> subarrays sh a
        Produce l f        -> ProduceAccum l (streamify f) nil
        -- MapBatch f c _ a x -> mapBatch f c a x
        ProduceAccum{}     -> stageError

    cvtC :: NaturalConsumer acc aenv t -> PreOpenNaturalSeq acc aenv t
    cvtC c =
      case c of
        FoldBatch f a x -> foldBatch f a x
        Stuple t        -> Consumer (Stuple (cvtCT t))
        Last _ _        -> stageError
        Elements x      -> elements x
        Tabulate x      -> tabulate x

    -- mapBatch :: forall a b c s. (Arrays a, Arrays b, Arrays c, Arrays s)
    --          => PreOpenAfun acc aenv (s -> a -> b)
    --          -> PreOpenAfun acc aenv (s -> Regular b -> (s, Regular c))
    --          -> acc aenv s
    --          -> acc aenv a
    --          -> NaturalProducer acc aenv (s,c)
    -- mapBatch f c a x = ProduceAccum Nothing f'' a
    --   where
    --     f'' :: PreOpenAfun acc aenv (Scalar index -> s -> ((s,c),s))
    --     f'' = Alam . Alam . Abody . repack . alet (weakenA2 x)
    --         $ weakenA3 c `partApply` avar1 `apply` (nest1 $ weakenA3 f `partApply` avar1 `apply` avar0)
    --
    --     repack :: forall aenv. acc aenv (s, Regular c) -> acc aenv ((s, c),s)
    --     repack b = alet b $ atup (atup (fstA avar0) (fromHOAS deNest1 (sndA avar0))) (fstA avar0)
    --
    --     nest1 :: forall aenv x. Arrays x => acc aenv x -> acc aenv (Regular x)
    --     nest1 = replicateC (unit (Const 1))
    --
    --     deNest1 :: forall x. Arrays x => S.Acc (Regular x) -> S.Acc x
    --     deNest1 x =
    --       case flavour (undefined :: x) of
    --         ArraysFunit  -> S.use ()
    --         ArraysFarray -> S.reshape (S.indexInit (S.shape (unregular x)))
    --                                   (unregular x)
    --         ArraysFtuple -> S.Acc $ S.Atuple $ deNestT (prod (Proxy :: Proxy Arrays) (undefined :: x)) (asAtuple x)
    --       where
    --         deNestT :: ProdR Arrays t -> Atuple S.Acc (RegularTupleRepr t) -> Atuple S.Acc t
    --         deNestT ProdRunit     NilAtup         = NilAtup
    --         deNestT (ProdRsnoc p) (SnocAtup t a') = SnocAtup (deNestT p t) (deNest1 a')
    --         deNestT _             _               = error "Absurd"

    foldBatch :: forall a s. (Arrays a, Arrays s)
              => PreOpenAfun acc aenv (s -> a -> s)
              -> acc aenv s
              -> acc aenv a
              -> PreOpenNaturalSeq acc aenv s
    foldBatch f a x = Producer (ProduceAccum Nothing f' a) (Consumer (Last avar0 (weakenA1 a)))
      where
        f' :: PreOpenAfun acc aenv (Scalar Int -> s -> (s,s))
        f' = Alam . Alam . Abody . repack . alet (weakenA2 x)
           $ weakenA3 f `partApply` avar1 `apply` avar0

        repack :: forall aenv. acc aenv s -> acc aenv (s,s)
        repack b = alet b $ atup avar0 avar0

    cvtCT :: Atuple (PreOpenNaturalSeq acc aenv) t -> Atuple (PreOpenNaturalSeq acc aenv) t
    cvtCT NilAtup        = NilAtup
    cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (reduceOpenSeq c)

    nil :: forall aenv. acc aenv ()
    nil = inject (Atuple NilAtup)

    elements :: (Shape sh, Elt e)
             => acc aenv (Array sh e)
             -> PreOpenNaturalSeq acc aenv (Vector e)
    elements x = Producer (ProduceAccum Nothing (Alam . Alam . Abody $ (fromHOAS2 f (weakenA2 x) avar0)) a)
                          (Consumer (Last avar0 (weakenA1 a)))
      where
        f x a = let x' = a S.++ S.flatten x in S.lift (x',x')
        a :: Elt e => acc aenv (Vector e)
        a = inject . Use $ newArray empty undefined

    tabulate :: forall sh e. (Shape sh, Elt e)
             => acc aenv (Array sh e)
             -> PreOpenNaturalSeq acc aenv (Array (sh:.Int) e)
    tabulate x = Producer (ProduceAccum Nothing (Alam . Alam . Abody $ (fromHOAS3 f avar1 (weakenA2 x) avar0)) a)
                          (Consumer (Last avar0 (weakenA1 (inject (Use (newArray empty undefined))))))
      where
        f :: S.Acc (Scalar Int) -> S.Acc (Array sh e) -> S.Acc (Array (sh:.Int) e) -> S.Acc (Array (sh:.Int) e, Array (sh:.Int) e)
        f ix x a = let x' =    S.the ix S.==* S.constant 0
                          S.?| (S.reshape (S.indexSnoc (S.shape x) (S.constant 1)) x
                               , concat x a)

                   in S.lift (x',x')
        concat x y =
          let
            sh_x = S.shape x
            sh_y = S.indexInit (S.shape y)
            sz_y = S.indexLast (S.shape y)
          in S.generate (S.indexSnoc (S.intersect sh_x sh_y) (sz_y + 1))
                        (\ix -> S.indexLast ix  S.<* sz_y S.? (y S.! ix, x S.! S.indexInit ix))

        a :: acc aenv (Array (sh:.Int) e)
        a = inject $ Use (newArray empty undefined)

    streamify :: Arrays t
              => PreOpenAfun acc aenv (Scalar Int -> t)
              -> PreOpenAfun acc aenv (Scalar Int -> () -> (t, ()))
    streamify f = Alam . Alam . Abody $ atup (weakenA2 f `apply` avar1) nil

    subarrays :: forall sh aenv e. (Shape sh, sh :<= DIM3, Elt e)
              => PreExp acc aenv sh
              -> Array sh e
              -> NaturalProducer acc aenv (Array sh e)
    subarrays sh arr = ProduceAccum (Just (totalSize `div` subSize)) f (inject (Unit (Const (fromElt (empty :: sh)))))
      where
        f = Alam . Alam . Abody
          $ atup (inject (Subarray (the avar0) (weakenA2 sh) arr)) (inject (Unit (the avar0 `plusS` (weakenA2 sh))))

        totalSize = Const (size (shape arr))
        subSize = ShapeSize sh

        div a b = PrimApp (PrimIDiv integralType) (tup a b)
        mod a b = PrimApp (PrimMod integralType) (tup a b)
        plus a b = PrimApp (PrimAdd numType) (tup a b)
        times a b = PrimApp (PrimMul numType) (tup a b)

        plusS :: forall aenv. (Shape sh, sh :<= DIM3) => PreExp acc aenv sh -> PreExp acc aenv sh -> PreExp acc aenv sh
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
            height, width :: PreExp acc aenv DIM2 -> PreExp acc aenv Int
            height = IndexHead . IndexTail
            width  = IndexHead
            fsh :: PreExp acc aenv sh
            fsh = Const (fromElt (shape arr))

    stageError = $internalError "vectoriseOpenSeq" "AST is at wrong stage for vectorisation. It seems to have already been vectorised."


-- Utility functions
--

dummy :: k a -> a
dummy _ = undefined
