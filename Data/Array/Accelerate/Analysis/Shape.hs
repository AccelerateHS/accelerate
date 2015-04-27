{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
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
  evalShape1, evalShape2, valToValPartial, ValPartial(..), prjArraysPartial, partialBottom, toShapeTree, shapeTreeMaxSize,

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
  PartialArray :: Shape sh   => Maybe sh -> Maybe (sh -> e) -> ArraysPartial (Array sh e)
  PartialAtup  :: IsAtuple t => Atuple ArraysPartial (TupleRepr t) -> ArraysPartial t

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

-- You know nothing, Jon Snow.
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
  EmptyPartial :: ValPartial ()
  PushPartial  :: ValPartial env -> ArraysPartial t -> ValPartial (env, t)
  PushTotal    :: ValPartial env ->              t -> ValPartial (env, t)

prjArraysPartial :: Arrays t => Idx env t -> ValPartial env -> ArraysPartial t
prjArraysPartial ZeroIdx       (PushPartial _   v) = v
prjArraysPartial ZeroIdx       (PushTotal   _   v) = toPartial v
prjArraysPartial (SuccIdx idx) (PushPartial val _) = prjArraysPartial idx val
prjArraysPartial (SuccIdx idx) (PushTotal   val _) = prjArraysPartial idx val
prjArraysPartial _             _                   = $internalError "prj" "inconsistent valuation"

valToValPartial :: Val a -> ValPartial a
valToValPartial Empty = EmptyPartial
valToValPartial (aenv `Push` a) = valToValPartial aenv `PushTotal` a

-- Monad for handling partiality and tracking intermediate shapes
type Shapes = WriterT ShapeTree Maybe

evalShape1 :: (Arrays a, Arrays b)
           => DelayedOpenAfun aenv (a -> b)
           -> ValPartial aenv -> ArraysPartial a -> Maybe (ArraysPartial b, ShapeTree)
evalShape1 f aenv a = runWriterT $ evalDelayedOpenAfun1 f aenv a

evalShape2 :: (Arrays a, Arrays b, Arrays c)
           => DelayedOpenAfun aenv (a -> b -> c)
           -> ValPartial aenv -> ArraysPartial a -> ArraysPartial b -> Maybe (ArraysPartial c, ShapeTree)
evalShape2 f aenv a b = runWriterT $ evalDelayedOpenAfun2 f aenv a b

evalDelayedOpenAfun1 :: (Arrays a, Arrays b)
                     => DelayedOpenAfun aenv (a -> b)
                     -> ValPartial aenv -> ArraysPartial a -> Shapes (ArraysPartial b)
evalDelayedOpenAfun1 (Alam (Abody a)) aenv sh = evalDelayedOpenAcc a (aenv `PushPartial` sh)
evalDelayedOpenAfun1 _ _ _ = $internalError "evalDelayedOpenAfun1" ".."

evalDelayedOpenAfun2 :: (Arrays a, Arrays b, Arrays c)
                     => DelayedOpenAfun aenv (a -> b -> c)
                     -> ValPartial aenv -> ArraysPartial a -> ArraysPartial b -> Shapes (ArraysPartial c)
evalDelayedOpenAfun2 (Alam (Alam (Abody a))) aenv sh1 sh2 = evalDelayedOpenAcc a (aenv `PushPartial` sh1 `PushPartial` sh2)
evalDelayedOpenAfun2 _ _ _ _ = $internalError "evalDelayedOpenAfun2" ".."

evalDelayedOpenAcc :: forall aenv a. Arrays a
                   => DelayedOpenAcc aenv a
                   -> ValPartial aenv -> Shapes (ArraysPartial a)
evalDelayedOpenAcc Delayed{..}    aenv = return $ PartialArray (evalPreOpenExp extentD EmptyElt aenv) Nothing
evalDelayedOpenAcc (Manifest acc) aenv =
  let
      manifest :: Arrays a' => DelayedOpenAcc aenv a' -> Shapes (ArraysPartial a')
      manifest a =
        do a' <- evalDelayedOpenAcc a aenv
           tell =<< lift (toShapeTree a') -- Remember the size of this intermediate result.
           return a'

      delayed :: Arrays a' => DelayedOpenAcc aenv a' -> Shapes (ArraysPartial a')
      delayed a = evalDelayedOpenAcc a aenv

      evalE :: DelayedExp aenv t -> Shapes t
      evalE exp = lift $ evalPreOpenExp exp EmptyElt aenv
  in
  case acc of
    Alet a1 a2 ->
      do a1' <- manifest a1
         evalDelayedOpenAcc a2 (aenv `PushPartial` a1')
    Avar x -> return (prjArraysPartial x aenv)
    Atuple atup -> PartialAtup <$> evalAtuple atup aenv
    Aprj ix atup ->
      do atup' <- manifest atup
         case atup' of
           PartialAtup t ->
             return (evalAprj ix t)
           _ -> $internalError "evalDelayedOpenAcc" "expected tuple"
    Apply f a -> evalDelayedOpenAfun1 f aenv =<< manifest a
    Aforeign{} -> lift Nothing
    Acond{} -> lift Nothing
    Awhile{} -> lift Nothing
    Use arr -> return (toPartial (toArr arr))
    Unit e -> 
      case runWriterT (evalE e) of
        Just (e', t) ->
          tell t >> return (PartialArray (Just Z) (Just (const e')))
        Nothing -> return (PartialArray (Just Z) Nothing)
    Collect{} -> lift Nothing

    Map _ acc                   -> sameShapeOp <$> delayed acc
    Generate sh _               -> (\ x -> PartialArray (Just x) Nothing) <$> evalE sh
    Transform sh _ _ acc        -> fixedShapeOp <$> evalE sh <*> delayed acc
    Backpermute sh _ acc        -> fixedShapeOp <$> evalE sh <*> delayed acc
    Reshape sh acc              -> fixedShapeOp <$> evalE sh <*> manifest acc
    ZipWith _ acc1 acc2         -> intersectShapeOp <$> delayed acc1 <*> delayed acc2
    Replicate slice slix acc    -> replicateOp slice <$> evalE slix <*> manifest acc
    Slice slice acc slix        -> sliceOp slice <$> manifest acc <*> evalE slix

    -- Consumers
    -- ---------
    Fold _ _ acc                -> foldOp <$> delayed acc
    Fold1 _ acc                 -> fold1Op <$> delayed acc
    FoldSeg _ _ acc seg         -> foldSegOp  <$> delayed acc <*> delayed seg
    Fold1Seg _ acc seg          -> fold1SegOp <$> delayed acc <*> delayed seg
    Scanl _ _ acc               -> scanOp <$> delayed acc
    Scanl' _ _ acc              -> scan'Op <$> delayed acc
    Scanl1 _ acc                -> sameShapeOp <$> delayed acc
    Scanr _ _ acc               -> scanOp  <$> delayed acc
    Scanr' _ _ acc              -> scan'Op <$> delayed acc
    Scanr1 _ acc                -> sameShapeOp <$> delayed acc
    Permute _ def _ acc         -> permuteOp <$> manifest def <*> delayed acc
    Stencil _ _ acc             -> sameShapeOp <$> manifest acc
    Stencil2 _ _ acc1 _ acc2    -> intersectShapeOp <$> manifest acc1 <*> manifest acc2

fixedShapeOp :: Shape sh => sh -> ArraysPartial (Array sh' e') -> ArraysPartial (Array sh e)
fixedShapeOp sh _ = PartialArray (Just sh) Nothing

sameShapeOp :: (Shape sh, Elt e) => ArraysPartial (Array sh e) -> ArraysPartial (Array sh e')
sameShapeOp arr = PartialArray (shapePartial arr) Nothing

intersectShapeOp :: Shape sh => ArraysPartial (Array sh e) -> ArraysPartial (Array sh e') -> ArraysPartial (Array sh e'')
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

foldOp :: Shape sh => ArraysPartial (Array (sh :. Int) e) -> ArraysPartial (Array sh e)
foldOp acc =
  let sh =
        do sh0 :. _ <- shapePartial acc
           case size sh0 of
             0 -> return (listToShape . map (max 1) . shapeToList $ sh0)
             _ -> return sh0
  in PartialArray sh Nothing

fold1Op :: Shape sh => ArraysPartial (Array (sh :. Int) e) -> ArraysPartial (Array sh e)
fold1Op acc = PartialArray ((\ (sh :. _) -> sh) <$> shapePartial acc) Nothing

foldSegOp :: Shape sh => ArraysPartial (Array (sh :. Int) e) -> ArraysPartial (Vector i) -> ArraysPartial (Array (sh :. Int) e)
foldSegOp arr seg =
  let sh = 
        do sh0 :. _ <- shapePartial arr
           Z  :. n <- shapePartial seg
           return (sh0 :. n)
  in PartialArray sh Nothing

fold1SegOp :: Shape sh => ArraysPartial (Array (sh :. Int) e) -> ArraysPartial (Vector i) -> ArraysPartial (Array (sh :. Int) e)
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

permuteOp :: Shape sh => ArraysPartial (Array sh e) -> ArraysPartial (Array sh' e) -> ArraysPartial (Array sh e)
permuteOp def _ = PartialArray (shapePartial def) Nothing

evalPreOpenFun :: (Elt a, Elt b)
               => PreOpenFun DelayedOpenAcc env aenv (a -> b)
               -> ValElt env -> ValPartial aenv -> a -> Maybe b
evalPreOpenFun (Lam (Body a)) env aenv x = evalPreOpenExp a (env `PushElt` fromElt x) aenv
evalPreOpenFun _ _ _ _ = $internalError "evalPreOpenFun" ".."

evalPreOpenExp :: forall env aenv e. PreOpenExp DelayedOpenAcc env aenv e
               -> ValElt env -> ValPartial aenv -> Maybe e
evalPreOpenExp exp env aenv =
  let
      evalE :: PreOpenExp DelayedOpenAcc env aenv t' -> Maybe t'
      evalE e = evalPreOpenExp e env aenv

      evalF :: (Elt a, Elt b) => PreOpenFun DelayedOpenAcc env aenv (a -> b) -> a -> Maybe b
      evalF f = evalPreOpenFun f env aenv

      evalA :: Arrays a => DelayedOpenAcc aenv a -> Maybe (ArraysPartial a)
      evalA a = fst <$> runWriterT (evalDelayedOpenAcc a aenv) -- Assumes Array expressions are hoisted out of expressions.
  in
  case exp of
    Let exp1 exp2 ->
      do v1 <- evalE exp1
         evalPreOpenExp exp2 (env `PushElt` fromElt v1) aenv
    Var ix -> return (prjElt ix env)
    Const c -> return (toElt c)

    -- FMMA TODO: Importing evalPrimConst and evalPrim causes a cyclic
    -- dependency.
    -- Possible fix: Move those operations to a new module
    -- Data.Array.Accelerate.Interpreter.Prim.
    PrimConst _c -> Nothing -- return (evalPrimConst c)
    PrimApp _f _x -> Nothing -- evalPrim f <$> evalE x
    Tuple tup -> toTuple <$> evalTuple tup env aenv
    Prj ix tup -> evalPrj ix . fromTuple <$> evalE tup
    IndexNil -> return Z
    IndexAny -> return Any
    IndexCons sh sz -> (:.) <$> evalE sh <*> evalE sz
    IndexHead sh    -> (\ x -> let _  :. ix = x in ix) <$> evalE sh
    IndexTail sh    -> (\ x -> let ix :. _  = x in ix) <$> evalE sh
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
    Foreign _ f e               -> evalPreOpenFun f EmptyElt EmptyPartial =<< evalE e

evalAtuple :: Atuple (DelayedOpenAcc aenv) t -> ValPartial aenv -> Shapes (Atuple ArraysPartial t)
evalAtuple NilAtup _ = return NilAtup
evalAtuple (SnocAtup t a) aenv =
  SnocAtup <$> evalAtuple t aenv <*> evalDelayedOpenAcc a aenv

evalAprj :: TupleIdx t a -> Atuple ArraysPartial t -> ArraysPartial a
evalAprj ZeroTupIdx       (SnocAtup _ a) = a
evalAprj (SuccTupIdx idx) (SnocAtup t _) = evalAprj idx t
evalAprj _ _ = $internalError "evalAprj" "invalid projection"

evalTuple :: Tuple (PreOpenExp DelayedOpenAcc env aenv) t -> ValElt env -> ValPartial aenv -> Maybe t
evalTuple NilTup            _env _aenv = return ()
evalTuple (tup `SnocTup` e) env  aenv  = (,) <$> evalTuple tup env aenv <*> evalPreOpenExp e env aenv

evalPrj :: TupleIdx t e -> t -> e
evalPrj ZeroTupIdx       (_, v)   = v
evalPrj (SuccTupIdx idx) (tup, _) = evalPrj idx tup
