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
  evalShape, valToValMA, ValMA(..), MA(..),

) where

-- standard libraries
import Control.Applicative          ( (<$>), (<*>) )
import Control.Arrow                ( second )
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

-- Arrays with potentially undefined element vector (shapes only)
data MA a where
  MAarr :: (Shape sh, Elt e) => sh -> (sh -> e)         -> MA (Array sh e)
  MAsh  :: Shape sh          => sh                      -> MA (Array sh e)
  MAtup :: IsAtuple t        => Atuple MA (TupleRepr t) -> MA t

toMA :: forall t. Arrays t => t -> MA t
toMA v =
  case flavour (undefined :: t) of
    ArraysFunit -> MAtup NilAtup
    ArraysFarray -> MAarr (shape v) (v!)
    ArraysFtuple -> MAtup $ go (prod (Proxy :: Proxy Arrays) (undefined :: t)) (fromAtuple v)
      where
        go :: ProdR Arrays t1 -> t1 -> Atuple MA t1
        go ProdRunit () = NilAtup
        go (ProdRsnoc pr) (tup, arr) = go pr tup `SnocAtup` toMA arr

shapeMA :: MA (Array sh e) -> sh
shapeMA (MAarr sh _) = sh
shapeMA (MAsh sh)    = sh
shapeMA (MAtup _)    = $internalError "shapeMA" "Unexpected tuple"

toShapeTree :: Arrays a => MA a -> ShapeTree
toShapeTree (MAarr sh _) = Leaf sh
toShapeTree (MAsh sh)    = Leaf sh
toShapeTree (MAtup t)    = go t
  where
    go :: Atuple MA t -> ShapeTree
    go NilAtup        = mempty
    go (SnocAtup t a) = go t <> toShapeTree a

data ValMA env where
  EmptyMA :: ValMA ()
  PushMA  :: ValMA env -> MA t -> ValMA (env, t)
  PushA   :: ValMA env ->    t -> ValMA (env, t)

prjMA :: Arrays t => Idx env t -> ValMA env -> MA t
prjMA ZeroIdx       (PushMA _   v) = v
prjMA ZeroIdx       (PushA  _   v) = toMA v
prjMA (SuccIdx idx) (PushMA val _) = prjMA idx val
prjMA (SuccIdx idx) (PushA  val _) = prjMA idx val
prjMA _             _              = $internalError "prj" "inconsistent valuation"

valToValMA :: Val a -> ValMA a
valToValMA Empty = EmptyMA
valToValMA (aenv `Push` a) = valToValMA aenv `PushA` a

-- Monad for handling partiality and tracking intermediate shapes
type Shapes = WriterT ShapeTree Maybe

evalShape :: (Shape sh, Elt e, Shape sh', Elt e')
          => DelayedOpenAfun aenv (Array sh e -> Array sh' e')
          -> ValMA aenv -> sh -> Maybe (sh', Int)
evalShape f aenv sh =
  second shapeTreeMaxSize <$>
    (runWriterT $ shapeMA <$> evalDelayedOpenAfun f aenv (MAsh sh))

evalDelayedOpenAfun :: (Arrays a, Arrays b)
                    => DelayedOpenAfun aenv (a -> b)
                    -> ValMA aenv -> MA a -> Shapes (MA b)
evalDelayedOpenAfun (Alam (Abody a)) aenv sh = evalDelayedOpenAcc a (aenv `PushMA` sh)
evalDelayedOpenAfun _ _ _ = $internalError "evalDelayedOpenAfun" ".."

evalPreOpenFun :: (Elt a, Elt b)
               => PreOpenFun DelayedOpenAcc env aenv (a -> b)
               -> ValElt env -> ValMA aenv -> a -> Shapes b
evalPreOpenFun (Lam (Body a)) env aenv x = evalPreOpenExp a (env `PushElt` fromElt x) aenv
evalPreOpenFun _ _ _ _ = $internalError "evalPreOpenFun" ".."


evalDelayedOpenAcc :: forall aenv a. Arrays a
                   => DelayedOpenAcc aenv a
                   -> ValMA aenv -> Shapes (MA a)
evalDelayedOpenAcc Delayed{..}    aenv = MAsh <$> evalPreOpenExp extentD EmptyElt aenv
evalDelayedOpenAcc (Manifest acc) aenv =
  let
      manifest :: Arrays a' => DelayedOpenAcc aenv a' -> Shapes (MA a')
      manifest a =
        do a' <- evalDelayedOpenAcc a aenv
           tell (toShapeTree a') -- Remember the size of this intermediate result.
           return a'

      delayed :: Arrays a' => DelayedOpenAcc aenv a' -> Shapes (MA a')
      delayed a = evalDelayedOpenAcc a aenv

      evalE :: DelayedExp aenv t -> Shapes t
      evalE exp = evalPreOpenExp exp EmptyElt aenv
  in
  case acc of
    Alet a1 a2 ->
      do a1' <- manifest a1
         evalDelayedOpenAcc a2 (aenv `PushMA` a1')
    Avar x -> return (prjMA x aenv)
    Atuple atup -> MAtup <$> evalAtuple atup aenv
    Aprj ix atup ->
      do atup' <- manifest atup
         case atup' of
           MAtup t ->
             return (evalAprj ix t)
           _ -> $internalError "evalDelayedOpenAcc" "expected tuple"
    Apply f a -> evalDelayedOpenAfun f aenv =<< manifest a
    Aforeign{} -> lift Nothing
    Acond{} -> lift Nothing
    Awhile{} -> lift Nothing
    Use arr -> return (toMA (toArr arr))
    Unit _ -> return (MAsh Z)
    Collect{} -> lift Nothing

    Map _ acc                   -> sameShapeOp <$> delayed acc
    Generate sh _               -> MAsh <$> evalE sh
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

fixedShapeOp :: Shape sh => sh -> MA (Array sh' e') -> MA (Array sh e)
fixedShapeOp = const . MAsh

sameShapeOp :: (Shape sh, Elt e) => MA (Array sh e) -> MA (Array sh e')
sameShapeOp = MAsh . shapeMA

intersectShapeOp :: Shape sh => MA (Array sh e) -> MA (Array sh e') -> MA (Array sh e'')
intersectShapeOp acc1 acc2 = MAsh (shapeMA acc1 `intersect` shapeMA acc2)

replicateOp :: (Shape sh, Shape sl, Elt slix, Elt e)
            => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
            -> slix -> MA (Array sl e) -> MA (Array sh e)
replicateOp slice slix arr = MAsh (toElt sh)
  where
    sh = extend slice (fromElt slix) (fromElt (shapeMA arr))

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
        -> MA (Array sh e) -> slix -> MA (Array sl e)
sliceOp slice arr slix = MAsh (toElt sh')
  where
    sh' = restrict slice (fromElt slix) (fromElt (shapeMA arr))

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

foldOp :: Shape sh => MA (Array (sh :. Int) e) -> MA (Array sh e)
foldOp acc =
  let sh :. _ = shapeMA acc
  in
  MAsh $ case size sh of
           0 -> listToShape . map (max 1) . shapeToList $ sh
           _ -> sh

fold1Op :: Shape sh => MA (Array (sh :. Int) e) -> MA (Array sh e)
fold1Op acc =
  let sh :. _ = shapeMA acc
  in MAsh sh

foldSegOp :: Shape sh => MA (Array (sh :. Int) e) -> MA (Vector i) -> MA (Array (sh :. Int) e)
foldSegOp arr seg =
  let sh :. _ = shapeMA arr
      Z  :. n = shapeMA seg
  in MAsh (sh :. n)

fold1SegOp :: Shape sh => MA (Array (sh :. Int) e) -> MA (Vector i) -> MA (Array (sh :. Int) e)
fold1SegOp arr seg =
  let sh :. _ = shapeMA arr
      Z  :. n = shapeMA seg
  in MAsh (sh :. n)

scan'Op :: Elt e => MA (Vector e) -> MA (Vector e, Scalar e)
scan'Op acc = MAtup $ NilAtup `SnocAtup` MAsh (shapeMA acc) `SnocAtup` MAsh Z

scanOp :: Elt e => MA (Vector e) -> MA (Vector e)
scanOp acc = let Z :. n = shapeMA acc in MAsh (Z :. n + 1)

permuteOp :: Shape sh => MA (Array sh e) -> MA (Array sh' e) -> MA (Array sh e)
permuteOp def _ = MAsh (shapeMA def)

evalPreOpenExp :: forall env aenv e. PreOpenExp DelayedOpenAcc env aenv e
               -> ValElt env -> ValMA aenv -> Shapes e
evalPreOpenExp exp env aenv =
  let
      evalE :: PreOpenExp DelayedOpenAcc env aenv t' -> Shapes t'
      evalE e = evalPreOpenExp e env aenv

      evalF :: (Elt a, Elt b) => PreOpenFun DelayedOpenAcc env aenv (a -> b) -> a -> Shapes b
      evalF f = evalPreOpenFun f env aenv

      evalA :: Arrays a => DelayedOpenAcc aenv a -> Shapes (MA a)
      evalA a = evalDelayedOpenAcc a aenv
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
    PrimConst _c -> lift Nothing -- return (evalPrimConst c)
    PrimApp _f _x -> lift Nothing -- evalPrim f <$> evalE x
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
           MAarr _ f -> f <$> evalE ix
           _ -> lift Nothing
    LinearIndex acc ix ->
      do acc' <- evalA acc
         case acc' of
           MAarr sh f -> f . fromIndex sh <$> evalE ix
           _ -> lift Nothing
    Shape acc                   -> shapeMA <$> evalA acc
    ShapeSize sh                -> size <$> evalE sh
    Intersect sh1 sh2           -> intersect <$> evalE sh1 <*> evalE sh2
    Union sh1 sh2               -> union <$> evalE sh1 <*> evalE sh2
    Foreign _ f e               -> evalPreOpenFun f EmptyElt EmptyMA =<< evalE e

evalAtuple :: Atuple (DelayedOpenAcc aenv) t -> ValMA aenv -> Shapes (Atuple MA t)
evalAtuple NilAtup _ = return NilAtup
evalAtuple (SnocAtup t a) aenv =
  SnocAtup <$> evalAtuple t aenv <*> evalDelayedOpenAcc a aenv

evalAprj :: TupleIdx t a -> Atuple MA t -> MA a
evalAprj ZeroTupIdx       (SnocAtup _ a) = a
evalAprj (SuccTupIdx idx) (SnocAtup t _) = evalAprj idx t
evalAprj _ _ = $internalError "evalAprj" "invalid projection"

evalTuple :: Tuple (PreOpenExp DelayedOpenAcc env aenv) t -> ValElt env -> ValMA aenv -> Shapes t
evalTuple NilTup            _env _aenv = return ()
evalTuple (tup `SnocTup` e) env  aenv  = (,) <$> evalTuple tup env aenv <*> evalPreOpenExp e env aenv

evalPrj :: TupleIdx t e -> t -> e
evalPrj ZeroTupIdx       (_, v)   = v
evalPrj (SuccTupIdx idx) (tup, _) = evalPrj idx tup
