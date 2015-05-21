{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Analysis.Shape
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2014] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Analysis.Shape (

  -- * query AST dimensionality
  AccDim, accDim, delayedDim, preAccDim,
  expDim,

  -- * Shape analysis
  ShapeTree(..),
  ArraysPartial(..),
  valToValPartial, ValPartial(..), shapeTreeMaxSize,
  seqShapes, seqShapesOpenAcc,

) where

-- standard libraries
import Control.Applicative          ( (<$>), (<*>) )
import Control.Monad.Writer         ( runWriterT, WriterT, tell )
import Control.Monad.Trans          ( lift )
import Data.Monoid                  ( Monoid(..), (<>) )
import Data.Typeable

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Error
-- import Data.Array.Accelerate.Interpreter (evalPrimConst, evalPrim)
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Array.Lifted
import Data.Array.Accelerate.Array.Representation               ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar


type AccDim acc  = forall aenv sh e. acc aenv (Array sh e) -> Int

-- |Reify the dimensionality of the result type of an array computation
--
accDim :: AccDim OpenAcc
accDim (OpenAcc acc) = preAccDim accDim acc

delayedDim :: AccDim DelayedOpenAcc
delayedDim (Manifest acc)   = preAccDim delayedDim acc
delayedDim (Delayed sh _ _) = expDim sh


-- |Reify dimensionality of a computation parameterised over a recursive closure
--
preAccDim :: forall acc aenv sh e. AccDim acc -> PreOpenAcc acc aenv (Array sh e) -> Int
preAccDim k pacc =
  case pacc of
    Alet  _ acc          -> k acc
    Avar _               -> case arrays (undefined :: Array sh e) of
                              ArraysRarray -> ndim (eltType (undefined::sh))
                              _            -> error "halt, fiend!"

    Apply _ _            -> case arrays (undefined :: Array sh e) of
                              ArraysRarray -> ndim (eltType (undefined::sh))
                              _            -> error "umm, hello"

    Aforeign _ _ _      -> case arrays (undefined :: Array sh e) of
                              ArraysRarray -> ndim (eltType (undefined::sh))
                              _            -> error "I don't even like snails!"

    Atuple _             -> case arrays (undefined :: Array sh e) of
                              ArraysRarray -> ndim (eltType (undefined::sh))
                              _            -> error "can we keep him?"

    Aprj _ _             -> case arrays (undefined :: Array sh e) of
                              ArraysRarray -> ndim (eltType (undefined::sh))
                              _            -> error "inconceivable!"

    Collect _            -> case arrays (undefined :: Array sh e) of
                              ArraysRarray -> ndim (eltType (undefined::sh))
                              _            -> error "ppbbbbbt~"

    Acond _ acc _        -> k acc
    Awhile _ _ acc       -> k acc
    Use Array{}          -> ndim (eltType (undefined::sh))
    Unit _               -> 0
    Generate _ _         -> ndim (eltType (undefined::sh))
    Transform _ _ _ _    -> ndim (eltType (undefined::sh))
    Reshape _ _          -> ndim (eltType (undefined::sh))
    Replicate _ _ _      -> ndim (eltType (undefined::sh))
    Slice _ _ _          -> ndim (eltType (undefined::sh))
    Map _ acc            -> k acc
    ZipWith _ _ acc      -> k acc
    Fold _ _ acc         -> k acc - 1
    Fold1 _ acc          -> k acc - 1
    FoldSeg _ _ acc _    -> k acc
    Fold1Seg _ acc _     -> k acc
    Scanl _ _ acc        -> k acc
    Scanl1 _ acc         -> k acc
    Scanr _ _ acc        -> k acc
    Scanr1 _ acc         -> k acc
    Permute _ acc _ _    -> k acc
    Backpermute _ _ _    -> ndim (eltType (undefined::sh))
    Stencil _ _ acc      -> k acc
    Stencil2 _ _ acc _ _ -> k acc


-- |Reify dimensionality of a scalar expression yielding a shape
--
expDim :: forall acc env aenv sh. Elt sh => PreOpenExp acc env aenv sh -> Int
expDim _ = ndim (eltType (undefined :: sh))


-- Count the number of components to a tuple type
--
ndim :: TupleType a -> Int
ndim UnitTuple       = 0
ndim (SingleTuple _) = 1
ndim (PairTuple a b) = ndim a + ndim b




-- Trees of existentially qualified shapes
data ShapeTree where
  EmptyShapeTree :: ShapeTree
  Leaf           :: Shape sh => sh -> ShapeTree
  Append         :: ShapeTree -> ShapeTree -> ShapeTree

instance Monoid ShapeTree where
  mempty  = EmptyShapeTree
  mappend = Append

shapeTreeHom :: Monoid a => (forall sh. Shape sh => sh -> a) -> ShapeTree -> a
shapeTreeHom h = h'
  where
    h' t = case t of
      EmptyShapeTree -> mempty
      Leaf x         -> h x
      Append t1 t2   -> h' t1 <> h' t2

-- Maximum monoid on naturals.
newtype MaxNat = MaxNat {runMaxNat :: Int}

instance Monoid MaxNat where
  mempty      = MaxNat 0
  mappend x y = MaxNat $ runMaxNat x `max` runMaxNat y

shapeTreeMaxSize :: ShapeTree -> Int
shapeTreeMaxSize = runMaxNat . shapeTreeHom (MaxNat . size)

-- Arrays with potentially undefined parts.
data ArraysPartial a where
  PartialArray :: (Shape sh, Elt e) => Maybe sh -> Maybe (sh -> e) -> ArraysPartial (Array sh e)
  PartialAtup  :: IsAtuple t => Atuple ArraysPartial (TupleRepr t) -> ArraysPartial t

{-
sameShape :: ArraysPartial a -> ArraysPartial a -> Bool
sameShape (PartialArray (Just sh1) _) (PartialArray (Just sh2) _) = size sh1 == size sh2
sameShape (PartialAtup t1) (PartialAtup t2) = go t1 t2
  where
    go :: Atuple ArraysPartial t -> Atuple ArraysPartial t -> Bool
    go NilAtup NilAtup = True
    go (SnocAtup t1 a1) (SnocAtup t2 a2) = go t1 t2 && sameShape a1 a2
    go _ _ = error "unreachable"
sameShape _ _ = False
-}

(.:) :: Shape sh => Int -> sh -> (sh :. Int)
(.:) sz sh = listToShape (shapeToList sh ++ [sz])

indexInit :: Shape sh => (sh :. Int) -> sh
indexInit sh = listToShape (init $ shapeToList sh)

liftReg1 :: forall arrs. Arrays arrs => ArraysPartial arrs -> ArraysPartial (Regular arrs)
liftReg1 a =
  case flavour (undefined :: arrs) of
    ArraysFunit  -> PartialAtup $ NilAtup `SnocAtup` PartialArray (Just Z) (Just (const 1))
    ArraysFarray | PartialArray sh e <-a-> PartialAtup (NilAtup `SnocAtup` PartialArray ((.:) 1 <$> sh) ((\ f -> f . indexInit) <$> e))
    ArraysFtuple | PartialAtup tup   <-a-> PartialAtup $ go (prod (Proxy :: Proxy Arrays) (undefined :: arrs)) tup
      where
        go :: ProdR Arrays t -> Atuple ArraysPartial t -> Atuple ArraysPartial (RegularTupleRepr t)
        go ProdRunit NilAtup = NilAtup
        go (ProdRsnoc pr) (tup `SnocAtup` (arr :: ArraysPartial a))
          | IsC <- isArraysFlat (undefined :: a)
          = go pr tup `SnocAtup` liftReg1 arr
        go _ _ = error "liftReg1"
    _ -> error "liftReg1"

toPartial :: forall t. Arrays t => t -> ArraysPartial t
toPartial v =
  case flavour (undefined :: t) of
    ArraysFunit  -> PartialAtup NilAtup
    ArraysFarray -> PartialArray (Just (shape v)) (Just (v!))
    ArraysFtuple -> PartialAtup $ go (prod (Proxy :: Proxy Arrays) (undefined :: t)) (fromAtuple v)
      where
        go :: ProdR Arrays t1 -> t1 -> Atuple ArraysPartial t1
        go ProdRunit () = NilAtup
        go (ProdRsnoc pr) (tup, arr) = go pr tup `SnocAtup` toPartial arr


toPartialShapesOnly :: forall t. Arrays t => t -> ArraysPartial t
toPartialShapesOnly v =
  case flavour (undefined :: t) of
    ArraysFunit  -> PartialAtup NilAtup
    ArraysFarray -> PartialArray (Just (shape v)) Nothing
    ArraysFtuple -> PartialAtup $ go (prod (Proxy :: Proxy Arrays) (undefined :: t)) (fromAtuple v)
      where
        go :: ProdR Arrays t1 -> t1 -> Atuple ArraysPartial t1
        go ProdRunit () = NilAtup
        go (ProdRsnoc pr) (tup, arr) = go pr tup `SnocAtup` toPartialShapesOnly arr


partialBottom :: forall t. Arrays t => ArraysPartial t
partialBottom =
  case flavour (undefined :: t) of
    ArraysFunit  -> PartialAtup NilAtup
    ArraysFarray -> PartialArray Nothing Nothing
    ArraysFtuple -> PartialAtup $ go (prod (Proxy :: Proxy Arrays) (undefined :: t))
      where
        go :: ProdR Arrays t1 -> Atuple ArraysPartial t1
        go ProdRunit = NilAtup
        go (ProdRsnoc pr) = go pr `SnocAtup` partialBottom

shapePartial :: ArraysPartial (Array sh e) -> Maybe sh
shapePartial (PartialArray sh _) = sh
shapePartial (PartialAtup _) = $internalError "shapePartial" "Unexpected tuple"

toShapeTree :: Arrays a => ArraysPartial a -> Maybe ShapeTree
toShapeTree (PartialArray sh _) = Leaf <$> sh
toShapeTree (PartialAtup t)     = go t
  where
    go :: Atuple ArraysPartial t -> Maybe ShapeTree
    go NilAtup        = return mempty
    go (SnocAtup t a) = mappend <$> go t <*> toShapeTree a

data ValPartial env where
  ValBottom    :: ValPartial env
  PushPartial  :: ValPartial env -> ArraysPartial t -> ValPartial (env, t)
  PushTotal    :: ValPartial env ->               t -> ValPartial (env, t)
  PushTotalShapesOnly :: ValPartial env ->        t -> ValPartial (env, t)

prjArraysPartial :: Arrays t => Idx env t -> ValPartial env -> ArraysPartial t
prjArraysPartial ZeroIdx       (PushPartial _   v) = v
prjArraysPartial ZeroIdx       (PushTotal   _   v) = toPartial v
prjArraysPartial ZeroIdx       (PushTotalShapesOnly _ v) = toPartialShapesOnly v
prjArraysPartial (SuccIdx idx) (PushPartial val _) = prjArraysPartial idx val
prjArraysPartial (SuccIdx idx) (PushTotal   val _) = prjArraysPartial idx val
prjArraysPartial (SuccIdx idx) (PushTotalShapesOnly val _) = prjArraysPartial idx val
prjArraysPartial _             ValBottom        = partialBottom

valToValPartial :: Val a -> ValPartial a
valToValPartial Empty = ValBottom
valToValPartial (aenv `Push` a) = valToValPartial aenv `PushTotal` a

-- Monad for handling partiality and tracking intermediate shapes
type Shapes = WriterT ShapeTree Maybe

type EvalAcc acc = forall aenv a. Arrays a => acc aenv a -> ValPartial aenv -> Shapes (ArraysPartial a)

seqShapes :: PreOpenSeq DelayedOpenAcc aenv () arrs -> ValPartial aenv -> Maybe ShapeTree
seqShapes s aenv = snd <$> runWriterT (evalShapeSeq evalDelayedOpenAcc s aenv ValBottom)

seqShapesOpenAcc :: PreOpenSeq OpenAcc aenv () arrs -> ValPartial aenv -> Maybe ShapeTree
seqShapesOpenAcc s aenv = snd <$> runWriterT (evalShapeSeq evalOpenAcc s aenv ValBottom)

evalShapeSeq :: forall acc aenv senv arrs.
                EvalAcc acc
             -> PreOpenSeq acc aenv senv arrs
             -> ValPartial aenv -> ValPartial senv -> Shapes ()
evalShapeSeq eval s aenv senv =
  case s of
    Producer p s0 -> do
      a <- evalP p
      tell =<< lift (toShapeTree a)
      evalShapeSeq eval s0 aenv (senv `PushPartial` a)
    Consumer c -> evalC c
    Reify _ _ -> return ()
  where
    evalP :: Producer acc aenv senv a -> Shapes (ArraysPartial a)
    evalP p =
      case p of
        StreamIn _ -> return partialBottom
        ToSeq _ sl _ acc -> do
           -- This acc argument will not be executed in each step of
           -- the sequence. Throw away the intermediate sizes:
          let arr = fst <$> runWriterT (eval acc aenv)
          return $ PartialArray (sliceShape sl <$> (shapePartial =<< arr)) Nothing
        GeneralMapSeq p a _ -> eval   a (bringIntoScope p aenv senv)
        MapSeq     f _ x    -> evalPreOpenAfun1 eval f aenv (prjArraysPartial x senv)
        ZipWithSeq f _ x y  -> evalPreOpenAfun2 eval f aenv (prjArraysPartial x senv) (prjArraysPartial y senv)
        ScanSeq _ _ _      ->  return $ PartialArray (Just Z) Nothing

    evalC :: Consumer acc aenv senv a -> Shapes ()
    evalC c =
      case c of
        FoldSeqFlatten _ f acc x
          | PartialArray sh _ <- prjArraysPartial x senv
          -> do
          let a1 = case fst <$> runWriterT (eval acc aenv) of
                      Just x -> x
                      _ -> partialBottom
          _a2 <- evalPreOpenAfun3 eval f aenv
                   a1
                   (PartialArray (Just (Z :. 1)) (const <$> sh))
                   (PartialArray ((Z :.) . size <$> sh) Nothing)
          return ()
        FoldSeqFlatten{} -> error "unreachable"
        FoldSeqRegular p f acc -> do
          let a1 = case fst <$> runWriterT (eval acc aenv) of
                      Just x -> x
                      _ -> partialBottom
          _a2 <- evalPreOpenAfun1 eval f (bringIntoScopeReg1 p aenv senv) a1
          return ()
        Stuple tup ->
          let f :: Atuple (Consumer acc aenv senv) t -> Shapes ()
              f NilAtup = return ()
              f (SnocAtup t c) = f t >> evalC c
          in f tup

bringIntoScope :: SeqPrelude aenv senv aenv' envReg
                 -> ValPartial aenv -> ValPartial senv -> ValPartial aenv'
bringIntoScope (SeqPrelude aconsts extc exts) aenv senv =
  ext exts (ext extc aenv (eval aconsts)) senv
  where
    ext :: ExtReg a a' b c c' -> ValPartial a -> ValPartial b -> ValPartial c
    ext ExtEmpty a _ = a
    ext (ExtPush e x) a b = ext e a b `PushPartial` prjArraysPartial x b

    eval :: Atuple Aconst arrs -> ValPartial arrs
    eval NilAtup = ValBottom
    eval (SnocAtup arrs a) = eval arrs `PushPartial` eval1 a

    eval1 :: Aconst a -> ArraysPartial a
    eval1 (SliceArr slix _ a) = PartialArray (Just (sliceShape slix (shape a))) Nothing
    eval1 (ArrList _) = partialBottom
    eval1 (RegArrList sh _) = PartialArray (Just sh) Nothing

bringIntoScopeReg1 :: SeqPrelude aenv senv aenv' envReg
                 -> ValPartial aenv -> ValPartial senv -> ValPartial envReg
bringIntoScopeReg1 (SeqPrelude aconsts extc exts) aenv senv =
  ext exts (ext extc aenv (eval aconsts)) senv
  where
    ext :: ExtReg a a' b c c' -> ValPartial a' -> ValPartial b -> ValPartial c'
    ext ExtEmpty a _ = a
    ext (ExtPush e x) a b = ext e a b `PushPartial` liftReg1 (prjArraysPartial x b)

    eval :: Atuple Aconst arrs -> ValPartial arrs
    eval NilAtup = ValBottom
    eval (SnocAtup arrs a) = eval arrs `PushPartial` eval1 a

    eval1 :: Aconst a -> ArraysPartial a
    eval1 (SliceArr slix _ a) = PartialArray (Just (sliceShape slix (shape a))) Nothing
    eval1 (ArrList _) = partialBottom
    eval1 (RegArrList sh _) = PartialArray (Just sh) Nothing


evalPreOpenAfun1 :: (Arrays a, Arrays b)
                 => EvalAcc acc
                 -> PreOpenAfun acc aenv (a -> b)
                 -> ValPartial aenv -> ArraysPartial a -> Shapes (ArraysPartial b)
evalPreOpenAfun1 ev (Alam (Abody a)) aenv sh = ev a (aenv `PushPartial` sh)
evalPreOpenAfun1 _ _ _ _ = $internalError "evalPreOpenAfun1" ".."

evalPreOpenAfun2 :: (Arrays a, Arrays b, Arrays c)
                 => EvalAcc acc
                 -> PreOpenAfun acc aenv (a -> b -> c)
                 -> ValPartial aenv -> ArraysPartial a -> ArraysPartial b -> Shapes (ArraysPartial c)
evalPreOpenAfun2 ev (Alam (Alam (Abody a))) aenv sh1 sh2 = ev a (aenv `PushPartial` sh1 `PushPartial` sh2)
evalPreOpenAfun2 _ _ _ _ _ = $internalError "evalPreOpenAfun2" ".."

evalPreOpenAfun3 :: (Arrays a, Arrays b, Arrays c, Arrays d)
                 => EvalAcc acc
                 -> PreOpenAfun acc aenv (a -> b -> c -> d)
                 -> ValPartial aenv -> ArraysPartial a -> ArraysPartial b -> ArraysPartial c -> Shapes (ArraysPartial d)
evalPreOpenAfun3 ev (Alam (Alam (Alam (Abody a)))) aenv sh1 sh2 sh3 = ev a (aenv `PushPartial` sh1 `PushPartial` sh2 `PushPartial` sh3)
evalPreOpenAfun3 _ _ _ _ _ _ = $internalError "evalPreOpenAfun2" ".."

evalDelayedOpenAcc :: EvalAcc DelayedOpenAcc
evalDelayedOpenAcc (Manifest a) aenv =
  case a of
    -- If a is a variable, assume the shape is already accounted
    -- for.
    Avar _ -> evalPreOpenAcc evalDelayedOpenAcc a aenv

    -- Otherwise, remember the size of this intermediate
    -- result.
    _                 -> do
      a' <- evalPreOpenAcc evalDelayedOpenAcc a aenv
      tell =<< lift (toShapeTree a')
      return a'
evalDelayedOpenAcc Delayed{..} aenv = 
  do let a = PartialArray (evalPreOpenExp evalDelayedOpenAcc extentD EmptyElt aenv) Nothing
     tell =<< lift (toShapeTree a)
     return a

evalOpenAcc :: EvalAcc OpenAcc
evalOpenAcc (OpenAcc a) aenv =
  case a of
    -- If a is a variable, assume the shape is already accounted
    -- for.
    Avar _ -> evalPreOpenAcc evalOpenAcc a aenv

    -- Otherwise, remember the size of this intermediate
    -- result.
    _ -> do
      a' <- evalPreOpenAcc evalOpenAcc a aenv
      tell =<< lift (toShapeTree a')
      return a'

evalPreOpenAcc :: forall acc aenv arrs.
                  EvalAcc acc
               -> PreOpenAcc acc aenv arrs
               -> ValPartial aenv
               -> Shapes (ArraysPartial arrs)
evalPreOpenAcc eval acc aenv =
  let
      evalE :: PreExp acc aenv t -> Shapes t
      evalE exp = lift $ evalPreOpenExp eval exp EmptyElt aenv

      evalA :: Arrays t => acc aenv t -> Shapes (ArraysPartial t)
      evalA a = eval a aenv
  in
  case acc of
    Alet a1 a2 ->
      do a1' <- evalA a1
         eval a2 (aenv `PushPartial` a1')
    Avar x -> return (prjArraysPartial x aenv)
    Atuple atup -> PartialAtup <$> evalAtuple eval atup aenv
    Aprj ix atup ->
      do atup' <- evalA atup
         case atup' of
           PartialAtup t ->
             return (evalAprj ix t)
           _ -> $internalError "evalPreOpenAcc" "expected tuple"
    Apply f a -> evalPreOpenAfun1 eval f aenv =<< evalA a
    Aforeign{} -> lift Nothing
    Acond{} -> lift Nothing
    Awhile{} -> lift Nothing
    Use arr -> return (toPartial (toArr arr))
    Unit _ -> return (PartialArray (Just Z) Nothing)
    Collect{} -> lift Nothing

    Map _ acc                   -> sameShapeOp <$> evalA acc
    Generate sh _               -> (\ x -> PartialArray (Just x) Nothing) <$> evalE sh
    Transform sh _ _ acc        -> fixedShapeOp <$> evalE sh <*> evalA acc
    Backpermute sh _ acc        -> fixedShapeOp <$> evalE sh <*> evalA acc
    Reshape sh acc              -> fixedShapeOp <$> evalE sh <*> evalA acc
    ZipWith _ acc1 acc2         -> intersectShapeOp <$> evalA acc1 <*> evalA acc2
    Replicate slice slix acc    -> replicateOp slice <$> evalE slix <*> evalA acc
    Slice slice acc slix        -> sliceOp slice <$> evalA acc <*> evalE slix

    -- Consumers
    -- ---------
    Fold _ _ acc                -> foldOp <$> evalA acc
    Fold1 _ acc                 -> fold1Op <$> evalA acc
    FoldSeg _ _ acc seg         -> foldSegOp  <$> evalA acc <*> evalA seg
    Fold1Seg _ acc seg          -> fold1SegOp <$> evalA acc <*> evalA seg
    Scanl _ _ acc               -> scanOp <$> evalA acc
    Scanl' _ _ acc              -> scan'Op <$> evalA acc
    Scanl1 _ acc                -> sameShapeOp <$> evalA acc
    Scanr _ _ acc               -> scanOp  <$> evalA acc
    Scanr' _ _ acc              -> scan'Op <$> evalA acc
    Scanr1 _ acc                -> sameShapeOp <$> evalA acc
    Permute _ def _ acc         -> permuteOp <$> evalA def <*> evalA acc
    Stencil _ _ acc             -> sameShapeOp <$> evalA acc
    Stencil2 _ _ acc1 _ acc2    -> intersectShapeOp <$> evalA acc1 <*> evalA acc2

fixedShapeOp :: (Shape sh, Elt e) => sh -> ArraysPartial (Array sh' e') -> ArraysPartial (Array sh e)
fixedShapeOp sh _ = PartialArray (Just sh) Nothing

sameShapeOp :: (Shape sh, Elt e, Elt e') => ArraysPartial (Array sh e) -> ArraysPartial (Array sh e')
sameShapeOp arr = PartialArray (shapePartial arr) Nothing

intersectShapeOp :: (Shape sh, Elt e'') => ArraysPartial (Array sh e) -> ArraysPartial (Array sh e') -> ArraysPartial (Array sh e'')
intersectShapeOp acc1 acc2 = PartialArray (intersect <$> shapePartial acc1 <*> shapePartial acc2) Nothing

replicateOp :: (Shape sh, Shape sl, Elt slix, Elt e)
            => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
            -> slix -> ArraysPartial (Array sl e) -> ArraysPartial (Array sh e)
replicateOp slice slix arr = PartialArray (toElt <$> sh) Nothing
  where
    sh = extend slice (fromElt slix) <$> (fromElt <$> shapePartial arr)

    extend :: SliceIndex slix sl co dim
           -> slix
           -> sl
           -> dim
    extend SliceNil              ()        ()       = ()
    extend (SliceAll sliceIdx)   (slx, ()) (sl, sz)
      = let dim' = extend sliceIdx slx sl
        in  (dim', sz)
    extend (SliceFixed sliceIdx) (slx, sz) sl
      = let dim' = extend sliceIdx slx sl
        in  (dim', sz)

sliceOp :: (Shape sh, Shape sl, Elt slix, Elt e)
        => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
        -> ArraysPartial (Array sh e) -> slix -> ArraysPartial (Array sl e)
sliceOp slice arr slix = PartialArray (toElt <$> sh') Nothing
  where
    sh' = restrict slice (fromElt slix) <$> (fromElt <$> shapePartial arr)

    restrict :: SliceIndex slix sl co sh
             -> slix
             -> sh
             -> sl
    restrict SliceNil              ()        ()       = ()
    restrict (SliceAll sliceIdx)   (slx, ()) (sl, sz)
      = let sl' = restrict sliceIdx slx sl
        in  (sl', sz)
    restrict (SliceFixed sliceIdx) (slx, i)  (sl, sz)
      = let sl' = restrict sliceIdx slx sl
        in  $indexCheck "slice" i sz $ sl'

foldOp :: (Shape sh, Elt e) => ArraysPartial (Array (sh :. Int) e) -> ArraysPartial (Array sh e)
foldOp acc =
  let sh =
        do sh0 :. _ <- shapePartial acc
           case size sh0 of
             0 -> return (listToShape . map (max 1) . shapeToList $ sh0)
             _ -> return sh0
  in PartialArray sh Nothing

fold1Op :: (Shape sh, Elt e) => ArraysPartial (Array (sh :. Int) e) -> ArraysPartial (Array sh e)
fold1Op acc = PartialArray ((\ (sh :. _) -> sh) <$> shapePartial acc) Nothing

foldSegOp :: (Shape sh, Elt e) => ArraysPartial (Array (sh :. Int) e) -> ArraysPartial (Vector i) -> ArraysPartial (Array (sh :. Int) e)
foldSegOp arr seg =
  let sh =
        do sh0 :. _ <- shapePartial arr
           Z  :. n <- shapePartial seg
           return (sh0 :. n)
  in PartialArray sh Nothing

fold1SegOp :: (Shape sh, Elt e) => ArraysPartial (Array (sh :. Int) e) -> ArraysPartial (Vector i) -> ArraysPartial (Array (sh :. Int) e)
fold1SegOp arr seg =
  let sh =
        do sh :. _ <- shapePartial arr
           Z  :. n <- shapePartial seg
           return (sh :. n)
  in PartialArray sh Nothing

scan'Op :: Elt e => ArraysPartial (Vector e) -> ArraysPartial (Vector e, Scalar e)
scan'Op acc = PartialAtup $ NilAtup `SnocAtup` PartialArray (shapePartial acc) Nothing `SnocAtup` PartialArray (Just Z) Nothing

scanOp :: Elt e => ArraysPartial (Vector e) -> ArraysPartial (Vector e)
scanOp acc =
  PartialArray
    ((\ (Z :. n) -> Z :. n + 1) <$> shapePartial acc)
    Nothing

permuteOp :: (Shape sh, Elt e) => ArraysPartial (Array sh e) -> ArraysPartial (Array sh' e) -> ArraysPartial (Array sh e)
permuteOp def _ = PartialArray (shapePartial def) Nothing

evalPreOpenFun :: (Elt a, Elt b)
               => EvalAcc acc
               -> PreOpenFun acc env aenv (a -> b)
               -> ValElt env -> ValPartial aenv -> a -> Maybe b
evalPreOpenFun ev (Lam (Body a)) env aenv x = evalPreOpenExp ev a (env `PushElt` fromElt x) aenv
evalPreOpenFun _ _ _ _ _ = $internalError "evalPreOpenFun" ".."

evalPreOpenExp :: forall acc env aenv e.
                  EvalAcc acc
               -> PreOpenExp acc env aenv e
               -> ValElt env -> ValPartial aenv -> Maybe e
evalPreOpenExp eval exp env aenv =
  let
      evalE :: PreOpenExp acc env aenv t' -> Maybe t'
      evalE e = evalPreOpenExp eval e env aenv

      evalF :: (Elt a, Elt b) => PreOpenFun acc env aenv (a -> b) -> a -> Maybe b
      evalF f = evalPreOpenFun eval f env aenv

      evalA :: Arrays a => acc aenv a -> Maybe (ArraysPartial a)
      evalA a = fst <$> runWriterT (eval a aenv) -- Assumes Array expressions are hoisted out of expressions.
  in
  case exp of
    Let exp1 exp2 ->
      do v1 <- evalE exp1
         evalPreOpenExp eval exp2 (env `PushElt` fromElt v1) aenv
    Var ix -> return (prjElt ix env)
    Const c -> return (toElt c)

    -- FMMA TODO: Importing evalPrimConst and evalPrim causes a cyclic
    -- dependency.
    -- Possible fix: Move those operations to a new module
    -- Data.Array.Accelerate.Interpreter.Prim.
    PrimConst _c -> Nothing -- return (evalPrimConst c)
    PrimApp _f _x -> Nothing -- evalPrim f <$> evalE x
    Tuple tup -> toTuple <$> evalTuple eval tup env aenv
    Prj ix tup -> evalPrj ix . fromTuple <$> evalE tup
    IndexNil -> return Z
    IndexAny -> return Any
    IndexCons sh sz -> (:.) <$> evalE sh <*> evalE sz
    IndexHead sh    -> (\ x -> let _  :. ix = x in ix) <$> evalE sh
    IndexTail sh    -> (\ x -> let ix :. _  = x in ix) <$> evalE sh
    IndexTrans sh   -> transpose <$> evalE sh
    IndexSlice slice slix sh    ->
      do slix' <- evalE slix
         sh' <- evalE sh
         return $ toElt $ restrict slice (fromElt slix')
                                         (fromElt sh')
      where
        restrict :: SliceIndex slix sl co sh -> slix -> sh -> sl
        restrict SliceNil              ()        ()         = ()
        restrict (SliceAll sliceIdx)   (slx, ()) (sl, sz)   =
          let sl' = restrict sliceIdx slx sl
          in  (sl', sz)
        restrict (SliceFixed sliceIdx) (slx, _i)  (sl, _sz) =
          restrict sliceIdx slx sl

    IndexFull slice slix sh ->
      do slix' <- evalE slix
         sh' <- evalE sh
         return $ toElt $ extend slice (fromElt slix')
                                       (fromElt sh')
      where
        extend :: SliceIndex slix sl co sh -> slix -> sl -> sh
        extend SliceNil              ()        ()       = ()
        extend (SliceAll sliceIdx)   (slx, ()) (sl, sz) =
          let sh' = extend sliceIdx slx sl
          in  (sh', sz)
        extend (SliceFixed sliceIdx) (slx, sz) sl       =
          let sh' = extend sliceIdx slx sl
          in  (sh', sz)

    ToIndex sh ix               -> toIndex <$> evalE sh <*> evalE ix
    FromIndex sh ix             -> fromIndex <$> evalE sh <*> evalE ix
    Cond c t e ->
      do b <- evalE c
         case b of
           True -> evalE t
           False -> evalE e
    While cond body seed -> go =<< evalE seed
      where
        f       = evalF body
        p       = evalF cond
        go x =
          do b <- p x
             case b of
               True ->  go =<< f x
               False -> return x

    -- The interesting cases: Index and LinearIndex may fail, if the
    -- indexed arrays have an undefined element vector.
    Index acc ix ->
      do acc' <- evalA acc
         case acc' of
           PartialArray _ (Just f) -> f <$> evalE ix
           _ -> Nothing
    LinearIndex acc ix ->
      do acc' <- evalA acc
         case acc' of
           PartialArray (Just sh) (Just f) -> f . fromIndex sh <$> evalE ix
           _ -> Nothing
    Shape acc                   -> shapePartial =<< evalA acc
    ShapeSize sh                -> size <$> evalE sh
    Intersect sh1 sh2           -> intersect <$> evalE sh1 <*> evalE sh2
    Union sh1 sh2               -> union <$> evalE sh1 <*> evalE sh2
    Foreign _ f e               -> evalPreOpenFun eval f EmptyElt ValBottom =<< evalE e

evalAtuple :: EvalAcc acc -> Atuple (acc aenv) t -> ValPartial aenv -> Shapes (Atuple ArraysPartial t)
evalAtuple _ NilAtup _ = return NilAtup
evalAtuple ev (SnocAtup t a) aenv =
  SnocAtup <$> evalAtuple ev t aenv <*> ev a aenv

evalAprj :: TupleIdx t a -> Atuple ArraysPartial t -> ArraysPartial a
evalAprj ZeroTupIdx       (SnocAtup _ a) = a
evalAprj (SuccTupIdx idx) (SnocAtup t _) = evalAprj idx t
evalAprj _ _ = $internalError "evalAprj" "invalid projection"

evalTuple :: EvalAcc acc -> Tuple (PreOpenExp acc env aenv) t -> ValElt env -> ValPartial aenv -> Maybe t
evalTuple _ NilTup            _env _aenv = return ()
evalTuple ev (tup `SnocTup` e) env  aenv  = (,) <$> evalTuple ev tup env aenv <*> evalPreOpenExp ev e env aenv

evalPrj :: TupleIdx t e -> t -> e
evalPrj ZeroTupIdx       (_, v)   = v
evalPrj (SuccTupIdx idx) (tup, _) = evalPrj idx tup
