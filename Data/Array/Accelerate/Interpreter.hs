{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK prune #-}
-- |
-- Module      : Data.Array.Accelerate.Interpreter
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2008..2009] Sean Lee
--               [2009..2014] Trevor L. McDonell
--               [2014..2014] Frederik M. Madsen
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This interpreter is meant to be a reference implementation of the semantics
-- of the embedded array language.  The emphasis is on defining the semantics
-- clearly, not on performance.
--
-- /Surface types versus representation types/
--
-- As a general rule, we perform all computations on representation types and we store all data
-- as values of representation types.  To guarantee the type safety of the interpreter, this
-- currently implies a lot of conversions between surface and representation types.  Optimising
-- the code by eliminating back and forth conversions is fine, but only where it doesn't
-- negatively affects clarity — after all, the main purpose of the interpreter is to serve as an
-- executable specification.
--

module Data.Array.Accelerate.Interpreter (

  -- * Interpret an array expression
  Arrays, run, run1, streamOut,

  -- Internal (hidden)
  evalPrim, evalPrimConst, evalPrj

) where

-- standard libraries
import Control.Monad
import Data.Bits
import Data.Char                                        ( chr, ord )
import Prelude                                          hiding ( sum )

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation               ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Trafo                              hiding ( Delayed )
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Smart                    as Sugar
import qualified Data.Array.Accelerate.Trafo                    as AST
import qualified Data.Array.Accelerate.Array.Representation     as R


-- Program execution
-- -----------------

-- | Run a complete embedded array program using the reference interpreter.
--
run :: Arrays a => Sugar.Acc a -> a
run acc
  = let a = convertAccWith config acc
    in  evalOpenAcc a Empty


-- | Prepare and run an embedded array program of one argument
--
run1 :: (Arrays a, Arrays b) => (Sugar.Acc a -> Sugar.Acc b) -> a -> b
run1 afun
  = let f = convertAfunWith config afun
    in  evalOpenAfun f Empty


-- | Stream a lazily read list of input arrays through the given program,
-- collecting results as we go
--
streamOut :: Arrays a => Sugar.Seq [a] -> [a]
streamOut seq = let seq' = convertSeqWith config seq
                in evalDelayedSeq defaultSeqConfig seq'


config :: Phase
config =  Phase
  { recoverAccSharing      = True
  , recoverExpSharing      = True
  , recoverSeqSharing      = True
  , floatOutAccFromExp     = True
  , enableAccFusion        = True
  , convertOffsetOfSegment = False
  }


-- Delayed Arrays
-- --------------

-- Note that in contrast to the representation used in the optimised AST, the
-- delayed array representation used here is _only_ for delayed arrays --- we do
-- not require an optional Manifest|Delayed data type to evaluate the program.
--
data Delayed a where
  Delayed :: (Shape sh, Elt e)
          => sh
          -> (sh -> e)
          -> (Int -> e)
          -> Delayed (Array sh e)


-- Array expression evaluation
-- ---------------------------

type EvalAcc acc = forall aenv a. acc aenv a -> Val aenv -> a

-- Evaluate an open array function
--
evalOpenAfun :: DelayedOpenAfun aenv f -> Val aenv -> f
evalOpenAfun (Alam  f) aenv = \a -> evalOpenAfun f (aenv `Push` a)
evalOpenAfun (Abody b) aenv = evalOpenAcc b aenv


-- The core interpreter for optimised array programs
--
evalOpenAcc
    :: forall aenv a.
       DelayedOpenAcc aenv a
    -> Val aenv
    -> a
evalOpenAcc AST.Delayed{}       _    = $internalError "evalOpenAcc" "expected manifest array"
evalOpenAcc (AST.Manifest pacc) aenv =
  let
      manifest :: DelayedOpenAcc aenv a' -> a'
      manifest acc = evalOpenAcc acc aenv

      delayed :: DelayedOpenAcc aenv (Array sh e) -> Delayed (Array sh e)
      delayed AST.Manifest{}  = $internalError "evalOpenAcc" "expected delayed array"
      delayed AST.Delayed{..} = Delayed (evalE extentD) (evalF indexD) (evalF linearIndexD)

      evalE :: DelayedExp aenv t -> t
      evalE exp = evalPreExp evalOpenAcc exp aenv

      evalF :: DelayedFun aenv f -> f
      evalF fun = evalPreFun evalOpenAcc fun aenv
  in
  case pacc of
    Avar ix                     -> prj ix aenv
    Alet acc1 acc2              -> evalOpenAcc acc2 (aenv `Push` manifest acc1)
    Atuple atup                 -> toTuple ArraysProxy $ evalAtuple atup aenv
    Aprj ix atup                -> evalPrj ix . fromTuple ArraysProxy $ manifest atup
    Apply afun acc              -> evalOpenAfun afun aenv  $ manifest acc
    Aforeign _ afun acc         -> evalOpenAfun afun Empty $ manifest acc
    Acond p acc1 acc2
      | evalE p                 -> manifest acc1
      | otherwise               -> manifest acc2

    Awhile cond body acc        -> go (manifest acc)
      where
        p       = evalOpenAfun cond aenv
        f       = evalOpenAfun body aenv
        go !x
          | p x ! Z     = go (f x)
          | otherwise   = x

    Use arr                     -> toArr arr
    Unit e                      -> unitOp (evalE e)
    Collect s                   -> evalSeq defaultSeqConfig s aenv

    -- Producers
    -- ---------
    Map f acc                   -> mapOp (evalF f) (delayed acc)
    Generate sh f               -> generateOp (evalE sh) (evalF f)
    Transform sh p f acc        -> transformOp (evalE sh) (evalF p) (evalF f) (delayed acc)
    Backpermute sh p acc        -> backpermuteOp (evalE sh) (evalF p) (delayed acc)
    Reshape sh acc              -> reshapeOp (evalE sh) (manifest acc)

    ZipWith f acc1 acc2         -> zipWithOp (evalF f) (delayed acc1) (delayed acc2)
    Replicate slice slix acc    -> replicateOp slice (evalE slix) (manifest acc)
    Slice slice acc slix        -> sliceOp slice (manifest acc) (evalE slix)

    -- Consumers
    -- ---------
    Fold f z acc                -> foldOp (evalF f) (evalE z) (delayed acc)
    Fold1 f acc                 -> fold1Op (evalF f) (delayed acc)
    FoldSeg f z acc seg         -> foldSegOp (evalF f) (evalE z) (delayed acc) (delayed seg)
    Fold1Seg f acc seg          -> fold1SegOp (evalF f) (delayed acc) (delayed seg)
    Scanl f z acc               -> scanlOp (evalF f) (evalE z) (delayed acc)
    Scanl' f z acc              -> scanl'Op (evalF f) (evalE z) (delayed acc)
    Scanl1 f acc                -> scanl1Op (evalF f) (delayed acc)
    Scanr f z acc               -> scanrOp (evalF f) (evalE z) (delayed acc)
    Scanr' f z acc              -> scanr'Op (evalF f) (evalE z) (delayed acc)
    Scanr1 f acc                -> scanr1Op (evalF f) (delayed acc)
    Permute f def p acc         -> permuteOp (evalF f) (manifest def) (evalF p) (delayed acc)
    Stencil sten b acc          -> stencilOp (evalF sten) b (manifest acc)
    Stencil2 sten b1 acc1 b2 acc2-> stencil2Op (evalF sten) b1 (manifest acc1) b2 (manifest acc2)

-- Array tuple construction and projection
--
evalAtuple :: Atuple (DelayedOpenAcc aenv) t -> Val aenv -> t
evalAtuple NilAtup        _    = ()
evalAtuple (SnocAtup t a) aenv = (evalAtuple t aenv, evalOpenAcc a aenv)


-- Array primitives
-- ----------------

unitOp :: Elt e => e -> Scalar e
unitOp e = newArray Z (const e)


generateOp
    :: (Shape sh, Elt e)
    => sh
    -> (sh -> e)
    -> Array sh e
generateOp = newArray


transformOp
    :: (Shape sh, Shape sh', Elt b)
    => sh'
    -> (sh' -> sh)
    -> (a -> b)
    -> Delayed (Array sh a)
    -> Array sh' b
transformOp sh' p f (Delayed _ xs _)
  = newArray sh' (\ix -> f (xs $ p ix))


reshapeOp
    :: (Shape sh, Shape sh', Elt e)
    => sh
    -> Array sh' e
    -> Array sh  e
reshapeOp newShape arr@(Array _ adata)
  = $boundsCheck "reshape" "shape mismatch" (size newShape == size (shape arr))
  $ Array (fromElt newShape) adata


replicateOp
    :: (Shape sh, Shape sl, Elt slix, Elt e)
    => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
    -> slix
    -> Array sl e
    -> Array sh e
replicateOp slice slix arr
  = newArray (toElt sh) (\ix -> arr ! liftToElt pf ix)
  where
    (sh, pf) = extend slice (fromElt slix) (fromElt (shape arr))

    extend :: SliceIndex slix sl co dim
           -> slix
           -> sl
           -> (dim, dim -> sl)
    extend SliceNil              ()        ()       = ((), const ())
    extend (SliceAll sliceIdx)   (slx, ()) (sl, sz)
      = let (dim', f') = extend sliceIdx slx sl
        in  ((dim', sz), \(ix, i) -> (f' ix, i))
    extend (SliceFixed sliceIdx) (slx, sz) sl
      = let (dim', f') = extend sliceIdx slx sl
        in  ((dim', sz), \(ix, _) -> f' ix)


sliceOp
    :: (Shape sh, Shape sl, Elt slix, Elt e)
    => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
    -> Array sh e
    -> slix
    -> Array sl e
sliceOp slice arr slix
  = newArray (toElt sh') (\ix -> arr ! liftToElt pf ix)
  where
    (sh', pf) = restrict slice (fromElt slix) (fromElt (shape arr))

    restrict :: SliceIndex slix sl co sh
             -> slix
             -> sh
             -> (sl, sl -> sh)
    restrict SliceNil              ()        ()       = ((), const ())
    restrict (SliceAll sliceIdx)   (slx, ()) (sl, sz)
      = let (sl', f') = restrict sliceIdx slx sl
        in  ((sl', sz), \(ix, i) -> (f' ix, i))
    restrict (SliceFixed sliceIdx) (slx, i)  (sl, sz)
      = let (sl', f') = restrict sliceIdx slx sl
        in  $indexCheck "slice" i sz $ (sl', \ix -> (f' ix, i))


mapOp :: (Shape sh, Elt a, Elt b)
      => (a -> b)
      -> Delayed (Array sh a)
      -> Array sh b
mapOp f (Delayed sh xs _)
  = newArray sh (\ix -> f (xs ix))


zipWithOp
    :: (Shape sh, Elt a, Elt b, Elt c)
    => (a -> b -> c)
    -> Delayed (Array sh a)
    -> Delayed (Array sh b)
    -> Array sh c
zipWithOp f (Delayed shx xs _) (Delayed shy ys _)
  = newArray (shx `intersect` shy) (\ix -> f (xs ix) (ys ix))


foldOp
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> e
    -> Delayed (Array (sh :. Int) e)
    -> Array sh e
foldOp f z (Delayed (sh :. n) arr _)
  | size sh == 0
  = newArray (listToShape . map (max 1) . shapeToList $ sh) (const z)

  | otherwise
  = newArray sh (\ix -> iter (Z:.n) (\(Z:.i) -> arr (ix :. i)) f z)


fold1Op
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> Delayed (Array (sh :. Int) e)
    -> Array sh e
fold1Op f (Delayed (sh :. n) arr _)
  = newArray sh (\ix -> iter1 (Z:.n) (\(Z:.i) -> arr (ix :. i)) f)


foldSegOp
    :: forall sh e i. (Shape sh, Elt e, Elt i, IsIntegral i)
    => (e -> e -> e)
    -> e
    -> Delayed (Array (sh :. Int) e)
    -> Delayed (Segments i)
    -> Array (sh :. Int) e
foldSegOp f z (Delayed (sh :. _) arr _) seg@(Delayed (Z :. n) _ _)
  | IntegralDict <- integralDict (integralType :: IntegralType i)
  = newArray (sh :. n)
  $ \(sz :. ix) -> let start = fromIntegral $ offset ! (Z :. ix)
                       end   = fromIntegral $ offset ! (Z :. ix+1)
                   in
                   iter (Z :. end-start) (\(Z:.i) -> arr (sz :. start+i)) f z
  where
    offset      = scanlOp (+) 0 seg


fold1SegOp
    :: forall sh e i. (Shape sh, Elt e, Elt i, IsIntegral i)
    => (e -> e -> e)
    -> Delayed (Array (sh :. Int) e)
    -> Delayed (Segments i)
    -> Array (sh :. Int) e
fold1SegOp f (Delayed (sh :. _) arr _) seg@(Delayed (Z :. n) _ _)
  | IntegralDict <- integralDict (integralType :: IntegralType i)
  = newArray (sh :. n)
  $ \(sz :. ix) -> let start = fromIntegral $ offset ! (Z :. ix)
                       end   = fromIntegral $ offset ! (Z :. ix+1)
                   in
                   iter1 (Z :. end-start) (\(Z:.i) -> arr (sz :. start+i)) f
  where
    offset      = scanlOp (+) 0 seg


scanl1Op
    :: Elt e
    => (e -> e -> e)
    -> Delayed (Vector e)
    -> Vector e
scanl1Op f (Delayed sh@(Z :. n) _ ain)
  = adata `seq` Array (fromElt sh) adata
  where
    f'          = sinkFromElt2 f
    --
    (adata, _)  = runArrayData $ do
      aout <- newArrayData n

      let write (Z:.0) = unsafeWriteArrayData aout 0 (fromElt $ ain 0)
          write (Z:.i) = do
            x <- unsafeReadArrayData aout (i-1)
            y <- return . fromElt $  ain  i
            unsafeWriteArrayData aout i (f' x y)

      iter1 sh write (>>)
      return (aout, undefined)


scanlOp
    :: Elt e
    => (e -> e -> e)
    -> e
    -> Delayed (Vector e)
    -> Vector e
scanlOp f z (Delayed (Z :. n) _ ain)
  = adata `seq` Array (fromElt sh') adata
  where
    sh'         = Z :. n+1
    f'          = sinkFromElt2 f
    --
    (adata, _)  = runArrayData $ do
      aout <- newArrayData (n+1)

      let write (Z:.0) = unsafeWriteArrayData aout 0 (fromElt z)
          write (Z:.i) = do
            x <- unsafeReadArrayData aout (i-1)
            y <- return . fromElt $  ain  (i-1)
            unsafeWriteArrayData aout i (f' x y)

      iter sh' write (>>) (return ())
      return (aout, undefined)


scanl'Op
    :: Elt e
    => (e -> e -> e)
    -> e
    -> Delayed (Vector e)
    -> (Vector e, Scalar e)
scanl'Op f z (scanlOp f z -> arr)
  = let
        arr'    = case arr of Array _ adata -> Array ((), n-1) adata
        sum     = unitOp (arr ! (Z:.n-1))
        n       = size (shape arr)
    in
    (arr', sum)


scanrOp
    :: Elt e
    => (e -> e -> e)
    -> e
    -> Delayed (Vector e)
    -> Vector e
scanrOp f z (Delayed (Z :. n) _ ain)
  = adata `seq` Array (fromElt sh') adata
  where
    sh'         = Z :. n+1
    f'          = sinkFromElt2 f
    --
    (adata, _)  = runArrayData $ do
      aout <- newArrayData (n+1)

      let write (Z:.0) = unsafeWriteArrayData aout n (fromElt z)
          write (Z:.i) = do
            x <- unsafeReadArrayData aout (n-i+1)
            y <- return . fromElt $  ain  (n-i)
            unsafeWriteArrayData aout (n-i) (f' x y)

      iter sh' write (>>) (return ())
      return (aout, undefined)


scanr1Op
    :: Elt e
    => (e -> e -> e)
    -> Delayed (Vector e)
    -> Vector e
scanr1Op f (Delayed sh@(Z :. n) _ ain)
  = adata `seq` Array (fromElt sh) adata
  where
    f'          = sinkFromElt2 f
    --
    (adata, _)  = runArrayData $ do
      aout <- newArrayData n

      let write (Z:.0) = unsafeWriteArrayData aout (n-1) (fromElt $ ain (n-1))
          write (Z:.i) = do
            x <- unsafeReadArrayData aout (n-i)
            y <- return . fromElt $  ain  (n-i-1)
            unsafeWriteArrayData aout (n-i-1) (f' x y)

      iter1 sh write (>>)
      return (aout, undefined)


scanr'Op
    :: forall e. Elt e
    => (e -> e -> e)
    -> e
    -> Delayed (Vector e)
    -> (Vector e, Scalar e)
scanr'Op f z (Delayed (Z :. n) _ ain)
  = (Array ((),n) adata, unitOp (toElt asum))
  where
    f' x y      = sinkFromElt2 f (fromElt x) y
    --
    (adata, asum) = runArrayData $ do
      aout <- newArrayData n

      let trav i !y | i < 0     = return y
          trav i y              = do
            unsafeWriteArrayData aout i y
            trav (i-1) (f' (ain i) y)

      final <- trav (n-1) (fromElt z)
      return (aout, final)


permuteOp
    :: (Shape sh, Shape sh', Elt e)
    => (e -> e -> e)
    -> Array sh' e
    -> (sh -> sh')
    -> Delayed (Array sh  e)
    -> Array sh' e
permuteOp f def@(Array _ adef) p (Delayed sh _ ain)
  = adata `seq` Array (fromElt sh') adata
  where
    sh'         = shape def
    n'          = size sh'
    f'          = sinkFromElt2 f
    --
    (adata, _)  = runArrayData $ do
      aout <- newArrayData n'

      let -- initialise array with default values
          init i
            | i >= n'   = return ()
            | otherwise = do
                x <- unsafeReadArrayData adef i
                unsafeWriteArrayData aout i x
                init (i+1)

          -- project each element onto the destination array and update
          update src
            = let dst   = p src
                  i     = toIndex sh  src
                  j     = toIndex sh' dst
              in
              unless (fromElt dst == R.ignore) $ do
                x <- return . fromElt $  ain  i
                y <- unsafeReadArrayData aout j
                unsafeWriteArrayData aout j (f' x y)

      init 0
      iter sh update (>>) (return ())
      return (aout, undefined)


backpermuteOp
    :: (Shape sh, Shape sh', Elt e)
    => sh'
    -> (sh' -> sh)
    -> Delayed (Array sh e)
    -> Array sh' e
backpermuteOp sh' p (Delayed _ arr _)
  = newArray sh' (\ix -> arr $ p ix)


stencilOp
    :: (Elt a, Elt b, Stencil sh a stencil)
    => (stencil -> b)
    -> Boundary (EltRepr a)
    -> Array sh a
    -> Array sh b
stencilOp stencil boundary arr
  = newArray sh f
  where
    f           = stencil . stencilAccess bounded
    sh          = shape arr
    --
    bounded ix  =
      case bound sh ix boundary of
        Left v    -> toElt v
        Right ix' -> arr ! ix'


stencil2Op
    :: (Elt a, Elt b, Elt c, Stencil sh a stencil1, Stencil sh b stencil2)
    => (stencil1 -> stencil2 -> c)
    -> Boundary (EltRepr a)
    -> Array sh a
    -> Boundary (EltRepr b)
    -> Array sh b
    -> Array sh c
stencil2Op stencil boundary1 arr1 boundary2 arr2
  = newArray (sh1 `intersect` sh2) f
  where
    sh1         = shape arr1
    sh2         = shape arr2
    f ix        = stencil (stencilAccess bounded1 ix)
                          (stencilAccess bounded2 ix)

    bounded1 ix =
      case bound sh1 ix boundary1 of
        Left v    -> toElt v
        Right ix' -> arr1 ! ix'

    bounded2 ix =
      case bound sh2 ix boundary2 of
        Left v    -> toElt v
        Right ix' -> arr2 ! ix'

toSeqOp :: (Elt slix, Shape sl, Shape dim, Elt e)
           => SliceIndex (EltRepr slix)
                         (EltRepr sl)
                         co
                         (EltRepr dim)
           -> slix
           -> Array dim e
           -> [Array sl e]
toSeqOp sliceIndex slix arr = map (sliceOp sliceIndex arr) (enumSlices sliceIndex slix)

-- Scalar expression evaluation
-- ----------------------------

-- Evaluate a closed scalar expression
--
evalPreExp :: EvalAcc acc -> PreExp acc aenv t -> Val aenv -> t
evalPreExp evalAcc e aenv = evalPreOpenExp evalAcc e EmptyElt aenv

-- Evaluate a closed scalar function
--
evalPreFun :: EvalAcc acc -> PreFun acc aenv t -> Val aenv -> t
evalPreFun evalAcc f aenv = evalPreOpenFun evalAcc f EmptyElt aenv

-- Evaluate an open scalar function
--
evalPreOpenFun :: EvalAcc acc -> PreOpenFun acc env aenv t -> ValElt env -> Val aenv -> t
evalPreOpenFun evalAcc (Body e) env aenv = evalPreOpenExp evalAcc e env aenv
evalPreOpenFun evalAcc (Lam f)  env aenv =
  \x -> evalPreOpenFun evalAcc f (env `PushElt` fromElt x) aenv


-- Evaluate an open scalar expression
--
-- NB: The implementation of 'Index' and 'Shape' demonstrate clearly why
--     array expressions must be hoisted out of scalar expressions before code
--     execution. If these operations are in the body of a function that gets
--     mapped over an array, the array argument would be evaluated many times
--     leading to a large amount of wasteful recomputation.
--
evalPreOpenExp
    :: forall acc env aenv t.
       EvalAcc acc
    -> PreOpenExp acc env aenv t
    -> ValElt env
    -> Val aenv
    -> t
evalPreOpenExp evalAcc pexp env aenv =
  let
      evalE :: PreOpenExp acc env aenv t' -> t'
      evalE e = evalPreOpenExp evalAcc e env aenv

      evalF :: PreOpenFun acc env aenv f' -> f'
      evalF f = evalPreOpenFun evalAcc f env aenv

      evalA :: acc aenv a -> a
      evalA a = evalAcc a aenv
  in
  case pexp of
    Let exp1 exp2               -> let !v1  = evalE exp1
                                       env' = env `PushElt` fromElt v1
                                   in  evalPreOpenExp evalAcc exp2 env' aenv
    Var ix                      -> prjElt ix env
    Const c                     -> toElt c
    PrimConst c                 -> evalPrimConst c
    PrimApp f x                 -> evalPrim f (evalE x)
    Tuple tup                   -> toTuple EltProxy $ evalTuple evalAcc tup env aenv
    Prj ix tup                  -> evalPrj ix . fromTuple EltProxy $ evalE tup
    IndexNil                    -> Z
    IndexAny                    -> Any
    IndexCons sh sz             -> evalE sh :. evalE sz
    IndexHead sh                -> let _  :. ix = evalE sh in ix
    IndexTail sh                -> let ix :. _  = evalE sh in ix
    IndexSlice slice slix sh    -> toElt $ restrict slice (fromElt (evalE slix))
                                                          (fromElt (evalE sh))
      where
        restrict :: SliceIndex slix sl co sh -> slix -> sh -> sl
        restrict SliceNil              ()        ()         = ()
        restrict (SliceAll sliceIdx)   (slx, ()) (sl, sz)   =
          let sl' = restrict sliceIdx slx sl
          in  (sl', sz)
        restrict (SliceFixed sliceIdx) (slx, _i)  (sl, _sz) =
          restrict sliceIdx slx sl

    IndexFull slice slix sh     -> toElt $ extend slice (fromElt (evalE slix))
                                                        (fromElt (evalE sh))
      where
        extend :: SliceIndex slix sl co sh -> slix -> sl -> sh
        extend SliceNil              ()        ()       = ()
        extend (SliceAll sliceIdx)   (slx, ()) (sl, sz) =
          let sh' = extend sliceIdx slx sl
          in  (sh', sz)
        extend (SliceFixed sliceIdx) (slx, sz) sl       =
          let sh' = extend sliceIdx slx sl
          in  (sh', sz)

    ToIndex sh ix               -> toIndex (evalE sh) (evalE ix)
    FromIndex sh ix             -> fromIndex (evalE sh) (evalE ix)
    Cond c t e
      | evalE c                 -> evalE t
      | otherwise               -> evalE e

    While cond body seed        -> go (evalE seed)
      where
        f       = evalF body
        p       = evalF cond
        go !x
          | p x         = go (f x)
          | otherwise   = x

    Index acc ix                -> evalA acc ! evalE ix
    LinearIndex acc i           -> let a  = evalA acc
                                       ix = fromIndex (shape a) (evalE i)
                                   in a ! ix
    Shape acc                   -> shape (evalA acc)
    ShapeSize sh                -> size (evalE sh)
    Intersect sh1 sh2           -> intersect (evalE sh1) (evalE sh2)
    Union sh1 sh2               -> union (evalE sh1) (evalE sh2)
    Foreign _ f e               -> evalPreOpenFun evalAcc f EmptyElt Empty $ evalE e


-- Scalar primitives
-- -----------------

evalPrimConst :: PrimConst a -> a
evalPrimConst (PrimMinBound ty) = evalMinBound ty
evalPrimConst (PrimMaxBound ty) = evalMaxBound ty
evalPrimConst (PrimPi       ty) = evalPi ty

evalPrim :: PrimFun p -> p
evalPrim (PrimAdd             ty) = evalAdd ty
evalPrim (PrimSub             ty) = evalSub ty
evalPrim (PrimMul             ty) = evalMul ty
evalPrim (PrimNeg             ty) = evalNeg ty
evalPrim (PrimAbs             ty) = evalAbs ty
evalPrim (PrimSig             ty) = evalSig ty
evalPrim (PrimQuot            ty) = evalQuot ty
evalPrim (PrimRem             ty) = evalRem ty
evalPrim (PrimIDiv            ty) = evalIDiv ty
evalPrim (PrimMod             ty) = evalMod ty
evalPrim (PrimBAnd            ty) = evalBAnd ty
evalPrim (PrimBOr             ty) = evalBOr ty
evalPrim (PrimBXor            ty) = evalBXor ty
evalPrim (PrimBNot            ty) = evalBNot ty
evalPrim (PrimBShiftL         ty) = evalBShiftL ty
evalPrim (PrimBShiftR         ty) = evalBShiftR ty
evalPrim (PrimBRotateL        ty) = evalBRotateL ty
evalPrim (PrimBRotateR        ty) = evalBRotateR ty
evalPrim (PrimFDiv            ty) = evalFDiv ty
evalPrim (PrimRecip           ty) = evalRecip ty
evalPrim (PrimSin             ty) = evalSin ty
evalPrim (PrimCos             ty) = evalCos ty
evalPrim (PrimTan             ty) = evalTan ty
evalPrim (PrimAsin            ty) = evalAsin ty
evalPrim (PrimAcos            ty) = evalAcos ty
evalPrim (PrimAtan            ty) = evalAtan ty
evalPrim (PrimAsinh           ty) = evalAsinh ty
evalPrim (PrimAcosh           ty) = evalAcosh ty
evalPrim (PrimAtanh           ty) = evalAtanh ty
evalPrim (PrimExpFloating     ty) = evalExpFloating ty
evalPrim (PrimSqrt            ty) = evalSqrt ty
evalPrim (PrimLog             ty) = evalLog ty
evalPrim (PrimFPow            ty) = evalFPow ty
evalPrim (PrimLogBase         ty) = evalLogBase ty
evalPrim (PrimTruncate     ta tb) = evalTruncate ta tb
evalPrim (PrimRound        ta tb) = evalRound ta tb
evalPrim (PrimFloor        ta tb) = evalFloor ta tb
evalPrim (PrimCeiling      ta tb) = evalCeiling ta tb
evalPrim (PrimAtan2           ty) = evalAtan2 ty
evalPrim (PrimLt              ty) = evalLt ty
evalPrim (PrimGt              ty) = evalGt ty
evalPrim (PrimLtEq            ty) = evalLtEq ty
evalPrim (PrimGtEq            ty) = evalGtEq ty
evalPrim (PrimEq              ty) = evalEq ty
evalPrim (PrimNEq             ty) = evalNEq ty
evalPrim (PrimMax             ty) = evalMax ty
evalPrim (PrimMin             ty) = evalMin ty
evalPrim PrimLAnd                 = evalLAnd
evalPrim PrimLOr                  = evalLOr
evalPrim PrimLNot                 = evalLNot
evalPrim PrimOrd                  = evalOrd
evalPrim PrimChr                  = evalChr
evalPrim PrimBoolToInt            = evalBoolToInt
evalPrim (PrimFromIntegral ta tb) = evalFromIntegral ta tb


-- Tuple construction and projection
-- ---------------------------------

evalTuple :: EvalAcc acc -> Tuple (PreOpenExp acc env aenv) t -> ValElt env -> Val aenv -> t
evalTuple _       NilTup            _env _aenv = ()
evalTuple evalAcc (tup `SnocTup` e) env  aenv  =
  (evalTuple evalAcc tup env aenv, evalPreOpenExp evalAcc e env aenv)

evalPrj :: TupleIdx t e -> t -> e
evalPrj ZeroTupIdx       (!_, v)   = v
evalPrj (SuccTupIdx idx) (tup, !_) = evalPrj idx tup
  -- FIXME: Strictly speaking, we ought to force all components of a tuples;
  --        not only those that we happen to encounter during the recursive
  --        walk.


-- Implementation of scalar primitives
-- -----------------------------------

evalLAnd :: (Bool, Bool) -> Bool
evalLAnd (x, y) = x && y

evalLOr  :: (Bool, Bool) -> Bool
evalLOr (x, y) = x || y

evalLNot :: Bool -> Bool
evalLNot = not

evalOrd :: Char -> Int
evalOrd = ord

evalChr :: Int -> Char
evalChr = chr

evalBoolToInt :: Bool -> Int
evalBoolToInt = fromEnum

evalFromIntegral :: IntegralType a -> NumType b -> a -> b
evalFromIntegral ta (IntegralNumType tb)
  | IntegralDict <- integralDict ta
  , IntegralDict <- integralDict tb
  = fromIntegral

evalFromIntegral ta (FloatingNumType tb)
  | IntegralDict <- integralDict ta
  , FloatingDict <- floatingDict tb
  = fromIntegral


-- Extract methods from reified dictionaries
--

-- Constant methods of Bounded
--

evalMinBound :: BoundedType a -> a
evalMinBound (IntegralBoundedType ty)
  | IntegralDict <- integralDict ty
  = minBound

evalMinBound (NonNumBoundedType   ty)
  | NonNumDict   <- nonNumDict ty
  = minBound

evalMaxBound :: BoundedType a -> a
evalMaxBound (IntegralBoundedType ty)
  | IntegralDict <- integralDict ty
  = maxBound

evalMaxBound (NonNumBoundedType   ty)
  | NonNumDict   <- nonNumDict ty
  = maxBound

-- Constant method of floating
--

evalPi :: FloatingType a -> a
evalPi ty | FloatingDict <- floatingDict ty = pi

evalSin :: FloatingType a -> (a -> a)
evalSin ty | FloatingDict <- floatingDict ty = sin

evalCos :: FloatingType a -> (a -> a)
evalCos ty | FloatingDict <- floatingDict ty = cos

evalTan :: FloatingType a -> (a -> a)
evalTan ty | FloatingDict <- floatingDict ty = tan

evalAsin :: FloatingType a -> (a -> a)
evalAsin ty | FloatingDict <- floatingDict ty = asin

evalAcos :: FloatingType a -> (a -> a)
evalAcos ty | FloatingDict <- floatingDict ty = acos

evalAtan :: FloatingType a -> (a -> a)
evalAtan ty | FloatingDict <- floatingDict ty = atan

evalAsinh :: FloatingType a -> (a -> a)
evalAsinh ty | FloatingDict <- floatingDict ty = asinh

evalAcosh :: FloatingType a -> (a -> a)
evalAcosh ty | FloatingDict <- floatingDict ty = acosh

evalAtanh :: FloatingType a -> (a -> a)
evalAtanh ty | FloatingDict <- floatingDict ty = atanh

evalExpFloating :: FloatingType a -> (a -> a)
evalExpFloating ty | FloatingDict <- floatingDict ty = exp

evalSqrt :: FloatingType a -> (a -> a)
evalSqrt ty | FloatingDict <- floatingDict ty = sqrt

evalLog :: FloatingType a -> (a -> a)
evalLog ty | FloatingDict <- floatingDict ty = log

evalFPow :: FloatingType a -> ((a, a) -> a)
evalFPow ty | FloatingDict <- floatingDict ty = uncurry (**)

evalLogBase :: FloatingType a -> ((a, a) -> a)
evalLogBase ty | FloatingDict <- floatingDict ty = uncurry logBase

evalTruncate :: FloatingType a -> IntegralType b -> (a -> b)
evalTruncate ta tb
  | FloatingDict <- floatingDict ta
  , IntegralDict <- integralDict tb
  = truncate

evalRound :: FloatingType a -> IntegralType b -> (a -> b)
evalRound ta tb
  | FloatingDict <- floatingDict ta
  , IntegralDict <- integralDict tb
  = round

evalFloor :: FloatingType a -> IntegralType b -> (a -> b)
evalFloor ta tb
  | FloatingDict <- floatingDict ta
  , IntegralDict <- integralDict tb
  = floor

evalCeiling :: FloatingType a -> IntegralType b -> (a -> b)
evalCeiling ta tb
  | FloatingDict <- floatingDict ta
  , IntegralDict <- integralDict tb
  = ceiling

evalAtan2 :: FloatingType a -> ((a, a) -> a)
evalAtan2 ty | FloatingDict <- floatingDict ty = uncurry atan2


-- Methods of Num
--

evalAdd :: NumType a -> ((a, a) -> a)
evalAdd (IntegralNumType ty) | IntegralDict <- integralDict ty = uncurry (+)
evalAdd (FloatingNumType ty) | FloatingDict <- floatingDict ty = uncurry (+)

evalSub :: NumType a -> ((a, a) -> a)
evalSub (IntegralNumType ty) | IntegralDict <- integralDict ty = uncurry (-)
evalSub (FloatingNumType ty) | FloatingDict <- floatingDict ty = uncurry (-)

evalMul :: NumType a -> ((a, a) -> a)
evalMul (IntegralNumType ty) | IntegralDict <- integralDict ty = uncurry (*)
evalMul (FloatingNumType ty) | FloatingDict <- floatingDict ty = uncurry (*)

evalNeg :: NumType a -> (a -> a)
evalNeg (IntegralNumType ty) | IntegralDict <- integralDict ty = negate
evalNeg (FloatingNumType ty) | FloatingDict <- floatingDict ty = negate

evalAbs :: NumType a -> (a -> a)
evalAbs (IntegralNumType ty) | IntegralDict <- integralDict ty = abs
evalAbs (FloatingNumType ty) | FloatingDict <- floatingDict ty = abs

evalSig :: NumType a -> (a -> a)
evalSig (IntegralNumType ty) | IntegralDict <- integralDict ty = signum
evalSig (FloatingNumType ty) | FloatingDict <- floatingDict ty = signum

evalQuot :: IntegralType a -> ((a, a) -> a)
evalQuot ty | IntegralDict <- integralDict ty = uncurry quot

evalRem :: IntegralType a -> ((a, a) -> a)
evalRem ty | IntegralDict <- integralDict ty = uncurry rem

evalIDiv :: IntegralType a -> ((a, a) -> a)
evalIDiv ty | IntegralDict <- integralDict ty = uncurry div

evalMod :: IntegralType a -> ((a, a) -> a)
evalMod ty | IntegralDict <- integralDict ty = uncurry mod

evalBAnd :: IntegralType a -> ((a, a) -> a)
evalBAnd ty | IntegralDict <- integralDict ty = uncurry (.&.)

evalBOr :: IntegralType a -> ((a, a) -> a)
evalBOr ty | IntegralDict <- integralDict ty = uncurry (.|.)

evalBXor :: IntegralType a -> ((a, a) -> a)
evalBXor ty | IntegralDict <- integralDict ty = uncurry xor

evalBNot :: IntegralType a -> (a -> a)
evalBNot ty | IntegralDict <- integralDict ty = complement

evalBShiftL :: IntegralType a -> ((a, Int) -> a)
evalBShiftL ty | IntegralDict <- integralDict ty = uncurry shiftL

evalBShiftR :: IntegralType a -> ((a, Int) -> a)
evalBShiftR ty | IntegralDict <- integralDict ty = uncurry shiftR

evalBRotateL :: IntegralType a -> ((a, Int) -> a)
evalBRotateL ty | IntegralDict <- integralDict ty = uncurry rotateL

evalBRotateR :: IntegralType a -> ((a, Int) -> a)
evalBRotateR ty | IntegralDict <- integralDict ty = uncurry rotateR

evalFDiv :: FloatingType a -> ((a, a) -> a)
evalFDiv ty | FloatingDict <- floatingDict ty = uncurry (/)

evalRecip :: FloatingType a -> (a -> a)
evalRecip ty | FloatingDict <- floatingDict ty = recip



evalLt :: ScalarType a -> ((a, a) -> Bool)
evalLt (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = uncurry (<)
evalLt (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = uncurry (<)
evalLt (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = uncurry (<)

evalGt :: ScalarType a -> ((a, a) -> Bool)
evalGt (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = uncurry (>)
evalGt (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = uncurry (>)
evalGt (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = uncurry (>)

evalLtEq :: ScalarType a -> ((a, a) -> Bool)
evalLtEq (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = uncurry (<=)
evalLtEq (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = uncurry (<=)
evalLtEq (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = uncurry (<=)

evalGtEq :: ScalarType a -> ((a, a) -> Bool)
evalGtEq (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = uncurry (>=)
evalGtEq (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = uncurry (>=)
evalGtEq (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = uncurry (>=)

evalEq :: ScalarType a -> ((a, a) -> Bool)
evalEq (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = uncurry (==)
evalEq (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = uncurry (==)
evalEq (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = uncurry (==)

evalNEq :: ScalarType a -> ((a, a) -> Bool)
evalNEq (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = uncurry (/=)
evalNEq (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = uncurry (/=)
evalNEq (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = uncurry (/=)

evalMax :: ScalarType a -> ((a, a) -> a)
evalMax (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = uncurry max
evalMax (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = uncurry max
evalMax (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = uncurry max

evalMin :: ScalarType a -> ((a, a) -> a)
evalMin (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = uncurry min
evalMin (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = uncurry min
evalMin (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = uncurry min



-- Sequence evaluation
-- ---------------

-- Position in sequence.
--
type SeqPos = Int

-- Configuration for sequence evaluation.
--
data SeqConfig = SeqConfig
  { chunkSize :: Int -- Allocation limit for a sequence in
                     -- words. Actual runtime allocation should be the
                     -- maximum of this size and the size of the
                     -- largest element in the sequence.
  }

-- Default sequence evaluation configuration for testing purposes.
--
defaultSeqConfig :: SeqConfig
defaultSeqConfig = SeqConfig { chunkSize = 2 }

-- A chunk of allocated elements. TODO: Change to use LiftedAcc.
--
data Chunk a = Chunk
  { elems :: [a]  -- Elements.
  }

-- The empty chunk. O(1).
emptyChunk :: forall a. Chunk a
emptyChunk = Chunk { elems = [] }

-- The singleton chunk. O(1).
singletonChunk :: forall a. a -> Chunk a
singletonChunk a = Chunk { elems = [a] }

-- Number of arrays in chunk. O(1).
--
clen :: forall a. Chunk a -> Int
clen = length . elems

elemsPerChunk :: SeqConfig -> Int -> Int
elemsPerChunk conf n
  | n < 1 = chunkSize conf
  | otherwise =
    let (a,b) = chunkSize conf `quotRem` n
    in a + signum b

-- Chunk append. O(n+m).
--
appendChunk :: forall a. Chunk a -> Chunk a -> Chunk a -- (Chunk a, Chunk a) leftovers c2
appendChunk c1 c2 = Chunk { elems = elems c1 ++ elems c2 }

-- Drop a number of arrays from a chunk. O(1). Note: Require keeping a
-- scan of element sizes.
--
cdrop :: forall a. Int -> Chunk a -> Chunk a
cdrop n c = c { elems = drop n (elems c) }

-- Take a number of arrays from a chunk. O(1). Note: Require keeping a
-- scan of element sizes.
--
ctake :: forall a. Int -> Chunk a -> Chunk a
ctake n c = c { elems = take n (elems c) }

-- Get all the shapes of a chunk of arrays. O(1).
--
chunkShapes :: forall sh a. Shape sh => Chunk (Array sh a) -> Vector sh
chunkShapes c = fromList (Z :. clen c) (map shape (elems c))

-- Get all the elements of a chunk of arrays. O(1).
--
chunkElems :: forall sh a. Elt a => Chunk (Array sh a) -> Vector a
chunkElems c =
  let xs = concatMap toList (elems c)
  in fromList (Z :. length xs) xs

-- fmap for Chunk. O(n).
--
mapChunk :: forall a b. (a -> b) -> Chunk a -> Chunk b -- (Chunk a, Chunk b) leftovers c
mapChunk f c = c { elems = map f (elems c) }

-- zipWith for Chunk. O(n).
--
zipWithChunk :: forall a b c. (a -> b -> c) -> Chunk a -> Chunk b -> Chunk c -- (Chunk a, Chunk b, Chunk c) leftovers c1 c2
zipWithChunk f c1 c2 = Chunk
  { elems = zipWith f (elems c1) (elems c2)
  }

-- A version of zipWith that keeps the trailing elements of the
-- longest argument. O(n).
zipWithChunk' :: forall a. (a -> a -> a) -> Chunk a -> Chunk a -> Chunk a
zipWithChunk' f c1 c2 =
  zipWithChunk f c1 c2 `appendChunk` cdrop (clen c2) c1 `appendChunk` cdrop (clen c1) c2

-- Pre-scan and reduce a Chunk. O(nlogn). TODO: Can work in-place for
-- fixed element-size sequences.
--
scanReduceChunk :: forall a. (a -> a -> a) -> a -> Chunk a -> (Chunk a, a) -- (Chunk a, Chunk a, a) leftovers c
scanReduceChunk f a0 c = go c
  where
    go :: Chunk a -> (Chunk a, a)
    go c =
      case elems c of
        []  -> (c, a0)
        [a] -> (singletonChunk a0, a)
        _ ->
          let k = clen c `div` 2
              (c', a') = go (ctake k c)
              (c'', a'') = go (cdrop k c)
          in (c' `appendChunk` mapChunk (f a') c'', f a' a'')

-- Reduce a Chunk. O(nlogn). TODO: Can work in-place for fixed
-- element-size sequences.
--
reduceChunk :: forall a. (a -> a -> a) -> a -> Chunk a -> a
reduceChunk f a0 c = go c
  where
    go :: Chunk a -> a
    go c =
      case elems c of
        [] -> a0
        [a] -> a
        _ ->
          let k = clen c `div` 2
              c' = zipWithChunk' f (cdrop k c) (ctake k c)
          in go c'

-- Replicate for Chunk. O(n). TODO Leftovers?
--
replicateChunk :: forall a. Int -> a -> Chunk a
replicateChunk k a = Chunk
  { elems = replicate k a
  }

-- A window on a sequence.
--
data Window a = Window
  { chunk :: Chunk a   -- Current allocated chunk.
  , wpos  :: SeqPos    -- Position of the window on the sequence, given
                       -- in number of elements.
  }

-- The initial empty window.
--
window0 :: forall a. Window a
window0 = Window { chunk = emptyChunk, wpos = 0 }

-- Index the given window by the given index on the sequence.
--
(!#) :: forall a. Window a -> SeqPos -> Chunk a
w !# i
  | j <- i - wpos w
  , j >= 0
  = cdrop j (chunk w)
  | otherwise = error $ "Window indexed before position. wpos = " ++ show (wpos w) ++ " i = " ++ show i

-- Move the give window by supplying the next chunk.
--
moveWin :: forall a. Window a -> Chunk a -> Window a
moveWin w c = w { chunk = c
                , wpos = wpos w + clen (chunk w)
                }

-- A cursor on a sequence.
--
data Cursor senv a = Cursor
  { ref  :: Idx senv a -- Reference to the sequence.
  , cpos :: SeqPos     -- Position of the cursor on the sequence,
                       -- given in number of elements.
  }

-- Initial cursor.
--
cursor0 :: forall senv a. Idx senv a -> Cursor senv a
cursor0 x = Cursor { ref = x, cpos = 0 }

-- Advance cursor by a relative amount.
--
moveCursor :: forall senv a. Int -> Cursor senv a -> Cursor senv a
moveCursor k c = c { cpos = cpos c + k }

-- Valuation for an environment of sequence windows.
--
data Val' senv where
  Empty' :: Val' ()
  Push'  :: Val' senv -> Window t -> Val' (senv, t)

-- Projection of a window from a window valuation using a de Bruijn
-- index.
--
prj' :: forall senv t. Idx senv t -> Val' senv -> Window t
prj' ZeroIdx       (Push' _   v) = v
prj' (SuccIdx idx) (Push' val _) = prj' idx val
prj' _             _             = $internalError "prj" "inconsistent valuation"

-- Projection of a chunk from a window valuation using a sequence
-- cursor.
--
prjChunk :: forall senv a. Cursor senv a -> Val' senv -> Chunk a
prjChunk c senv = prj' (ref c) senv !# cpos c

-- An executable sequence.
--
data ExecSeq senv arrs where
  ExecP :: Arrays a => Window a -> ExecP senv a -> ExecSeq (senv, a) arrs -> ExecSeq senv  arrs
  ExecC :: Arrays a =>             ExecC senv a ->                           ExecSeq senv  a
  ExecR ::                         Cursor senv a ->                          ExecSeq senv  [a]

-- An executable producer.
--
data ExecP senv a where
  ExecToSeq :: Int -- Elements per chunk
               -> [Array sh e]
               -> ExecP senv (Array sh e)

  ExecUseLazy :: (Elt slix, Shape sl, Elt e)
              => SliceIndex (EltRepr slix)
                            (EltRepr sl)
                            co
                            dim
              -> Int -- Elements per chunk
              -> [slix]
              -> (slix -> Array sl e)
              -> ExecP senv (Array sl e)

  ExecMap :: (Chunk a -> Chunk b)
          -> Cursor senv a
          -> ExecP senv b

  ExecZipWith :: (Chunk a -> Chunk b -> Chunk c)
              -> Cursor senv a
              -> Cursor senv b
              -> ExecP senv c

  ExecScanSeq :: (a -> Chunk b -> (Chunk a, a))
              -> Cursor senv b
              -> a
              -> ExecP senv a

-- An executable consumer.
--
data ExecC senv a where
  ExecFoldSeq' :: (acc -> Chunk b -> acc)
                  -> (acc -> res)
                  -> Cursor senv b
                  -> acc
                  -> ExecC senv res
  ExecStuple   :: IsAtuple a
                  => Atuple (ExecC senv) (TupleRepr a)
                  -> ExecC senv a

minCursor :: ExecSeq senv a -> SeqPos
minCursor s = travS s 0
  where
    travS :: ExecSeq senv a -> Int -> SeqPos
    travS s i =
      case s of
        ExecP _ p s' -> travP p i `min` travS s' (i+1)
        ExecC   c    -> travC c i
        ExecR   _    -> maxBound

    k :: Cursor senv a -> Int -> SeqPos
    k c i
      | i == idxToInt (ref c) = cpos c
      | otherwise             = maxBound

    travP :: ExecP senv a -> Int -> SeqPos
    travP p i =
      case p of
        ExecMap _ c -> k c i
        ExecZipWith _ c1 c2 -> k c1 i `min` k c2 i
        ExecScanSeq _ c _ -> k c i
        _ -> maxBound

    travT :: Atuple (ExecC senv) t -> Int -> SeqPos
    travT NilAtup        _ = maxBound
    travT (SnocAtup t c) i = travT t i `min` travC c i

    travC :: ExecC senv a -> Int -> SeqPos
    travC c i =
      case c of
        ExecFoldSeq' _ _ c _ -> k c i
        ExecStuple t         -> travT t i

evalDelayedSeq :: SeqConfig
               -> DelayedSeq arrs
               -> arrs
evalDelayedSeq cfg (DelayedSeq aenv s) | aenv' <- evalExtend aenv Empty
                                       = evalSeq cfg s aenv'

evalSeq :: forall aenv arrs.
            SeqConfig
         -> PreOpenSeq DelayedOpenAcc aenv () arrs
         -> Val aenv -> arrs
evalSeq conf s aenv = evalSeq' s
  where
    evalSeq' :: PreOpenSeq DelayedOpenAcc aenv senv arrs -> arrs
    evalSeq' (Producer _ s) = evalSeq' s
    evalSeq' (Consumer _)   = loop (initSeq aenv s)
    evalSeq' (Reify _)      = reify (initSeq aenv s)

    -- Initialize the producers and the accumulators of the consumers
    -- with the given array enviroment.
    initSeq :: forall senv arrs'.
                Val aenv
             -> PreOpenSeq DelayedOpenAcc aenv senv arrs'
             -> ExecSeq senv arrs'
    initSeq aenv s =
      case s of
        Producer   p s' -> ExecP window0 (initProducer   aenv p) (initSeq aenv s')
        Consumer   c    -> ExecC         (initConsumer   aenv c)
        Reify      ix   -> ExecR (cursor0 ix)

    -- Generate a list from the sequence.
    reify :: forall arrs. ExecSeq () [arrs]
          -> [arrs]
    reify s = case step s Empty' of
                (Just s', a) -> a ++ reify s'
                (Nothing, a) -> a

    -- Iterate the given sequence until it terminates.
    -- A sequence only terminates when one of the producers are exhausted.
    loop :: Arrays arrs
         => ExecSeq () arrs
         -> arrs
    loop s =
      case step' s of
        (Nothing, arrs) -> arrs
        (Just s', _)    -> loop s'

      where
        step' :: ExecSeq () arrs -> (Maybe (ExecSeq () arrs), arrs)
        step' s = step s Empty'

    -- One iteration of a sequence.
    step :: forall senv arrs'.
            ExecSeq senv arrs'
         -> Val' senv
         -> (Maybe (ExecSeq senv arrs'), arrs')
    step s senv =
      case s of
        ExecP w p s' ->
          let (c, mp')  = produce p senv
              finished  = null (elems (w !# minCursor s'))
              w'        = if finished then moveWin w c else w
              (ms'', a) = step s' (senv `Push'` w')
          in case ms'' of
            Nothing  -> (Nothing, a)
            Just s'' | finished
                     , Just p' <- mp'
                     -> (Just (ExecP w' p' s''), a)
                     | not finished
                     -> (Just (ExecP w' p  s''), a)
                     | otherwise
                     -> (Nothing, a)
        ExecC   c    -> let (c', acc) = consume c senv
                        in (Just (ExecC c'), acc)
        ExecR ix     -> let c = prjChunk ix senv in (Just (ExecR (moveCursor (clen c) ix)), elems c)

    initProducer :: forall a senv.
                    Val aenv
                 -> Producer DelayedOpenAcc aenv senv a
                 -> ExecP senv a
    initProducer aenv p =
      case p of
        ToSeq sliceIndex slix (delayed -> Delayed sh ix _) ->
          let slix' = restrictSlice sliceIndex sh (evalPreExp evalOpenAcc slix aenv)
              n   = R.size (R.sliceShape sliceIndex (fromElt sh))
              k   = elemsPerChunk conf n
          in ExecToSeq k (toSeqOp sliceIndex slix' (newArray sh ix))
        UseLazy sliceIndex sl arr ->
          let sl' = restrictSlice sliceIndex (shape arr) (evalPreExp evalOpenAcc sl aenv)
              sls = enumSlices sliceIndex sl'
              n   = R.size (R.sliceShape sliceIndex (fromElt (shape arr)))
              k   = elemsPerChunk conf n
              f slix = sliceOp sliceIndex arr slix
          in ExecUseLazy sliceIndex k sls f
        MapSeq     f x       -> ExecMap     (mapChunk (evalOpenAfun f aenv)) (cursor0 x)
        ZipWithSeq f x y     -> ExecZipWith (zipWithChunk (evalOpenAfun f aenv)) (cursor0 x) (cursor0 y)
        ScanSeq    f acc x   -> initProducer aenv (ScanSeqAct f f acc acc x)
        ScanSeqAct f g acc0 acc1 x -> ExecScanSeq h (cursor0 x) a0
          where
            f' = evalOpenAfun f aenv
            g' = evalOpenAfun g aenv
            a0 = evalOpenAcc acc0 aenv
            b0 = evalOpenAcc acc1 aenv
            h a bs =
              let (bs', b) = scanReduceChunk g' b0 bs -- Scan and reduce chunk.
                  as = mapChunk (f' a) bs'            -- Add accumulator.
                  a' = f' a b                         -- Find new accumulator.
              in (as, a')

    initConsumer :: forall a senv.
                    Val aenv
                 -> Consumer DelayedOpenAcc aenv senv a
                 -> ExecC senv a
    initConsumer aenv c =
      case c of
        FromSeq x ->
          ExecFoldSeq'
            appendChunk
            (\ c -> (chunkShapes c, chunkElems c))
            (cursor0 x)
            emptyChunk
        FoldSeq f acc x ->
          let f' = evalOpenAfun f aenv
              a0 = evalOpenAcc acc aenv
              k = 1 -- TODO elemsPerChunk conf (size (shape a0))
          in ExecFoldSeq'
               (zipWithChunk' f')
               (reduceChunk f' a0)
               (cursor0 x)
               (replicateChunk k a0)
        FoldSeqAct f g acc1 acc2 x ->
          let f' = evalOpenAfun f aenv
              g' = evalOpenAfun g aenv
              a0 = evalOpenAcc acc1 aenv
              b0 = evalOpenAcc acc2 aenv
          in ExecFoldSeq'
               (\ a bs -> f' a (reduceChunk g' b0 bs))
               id
               (cursor0 x)
               a0
        FoldSeqFlatten f acc x ->
          let f' = evalOpenAfun f aenv
              a0 = evalOpenAcc acc aenv
          in ExecFoldSeq'
               (\ a bs -> f' a (chunkShapes bs) (chunkElems bs))
               id
               (cursor0 x)
               a0
        Stuple t ->
          let initTup :: Atuple (Consumer DelayedOpenAcc aenv senv) t -> Atuple (ExecC senv) t
              initTup NilAtup        = NilAtup
              initTup (SnocAtup t c) = SnocAtup (initTup t) (initConsumer aenv c)
          in ExecStuple (initTup t)

    delayed :: DelayedOpenAcc aenv (Array sh e) -> Delayed (Array sh e)
    delayed AST.Manifest{}  = $internalError "evalOpenAcc" "expected delayed array"
    delayed AST.Delayed{..} = Delayed (evalPreExp evalOpenAcc extentD aenv)
                                      (evalPreFun evalOpenAcc indexD aenv)
                                      (evalPreFun evalOpenAcc linearIndexD aenv)

produce :: ExecP senv a -> Val' senv -> (Chunk a, Maybe (ExecP senv a))
produce p senv =
  case p of
    ExecToSeq k xs ->
      let (xs', xs'') = (take k xs, drop k xs)
          c           = Chunk xs'
          mp          = if null xs''
                        then Nothing
                        else Just (ExecToSeq k xs'')
      in (c, mp)
    ExecUseLazy sliceIndex k slixs f ->
      let (slixs', slixs'') = (take k slixs, drop k slixs)
          c                 = Chunk { elems = map f slixs' }
      in ( c
         , if null slixs'' then Nothing else Just $ ExecUseLazy sliceIndex k slixs'' f)
    ExecMap f x ->
      let c = prjChunk x senv
      in (f c, Just $ ExecMap f (moveCursor (clen c) x))
    ExecZipWith f x y ->
      let c1 = prjChunk x senv
          c2 = prjChunk y senv
          k = clen c1 `min` clen c2
      in (f c1 c2, Just $ ExecZipWith f (moveCursor k x) (moveCursor k y))
    ExecScanSeq f x a   ->
      let c = prjChunk x senv
          (as, a') = f a c
      in (as, Just $ ExecScanSeq f (moveCursor (clen c) x) a')

consume :: forall senv a. ExecC senv a -> Val' senv -> (ExecC senv a, a)
consume c senv =
  case c of
    ExecFoldSeq' f g x acc  ->
      let c    = prjChunk x senv
          acc' = f acc c
      -- Even though we call g here, lazy evaluation should guarantee it is
      -- only ever called once.
      in (ExecFoldSeq' f g (moveCursor (clen c) x) acc', g acc')
    ExecStuple t ->
      let consT :: Atuple (ExecC senv) t -> (Atuple (ExecC senv) t, t)
          consT NilAtup        = (NilAtup, ())
          consT (SnocAtup t c) | (c', acc) <- consume c senv
                               , (t', acc') <- consT t
                               = (SnocAtup t' c', (acc', acc))
          (t', acc) = consT t
      in (ExecStuple t', toTuple ArraysProxy acc)

evalExtend :: Extend DelayedOpenAcc aenv aenv' -> Val aenv -> Val aenv'
evalExtend BaseEnv aenv = aenv
evalExtend (PushEnv ext1 ext2) aenv | aenv' <- (evalExtend ext1 aenv)
                                    = Push aenv' (evalOpenAcc (Manifest ext2) aenv')
