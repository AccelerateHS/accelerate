{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK prune #-}
-- |
-- Module      : Data.Array.Accelerate.Interpreter
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--               [2009..2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
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
  Arrays, run, run1, stream,

  -- Internal (hidden)
  evalPrim, evalPrimConst, evalPrj

) where

-- standard libraries
import Control.Monad
import Control.Monad.ST                                 ( ST )
import Data.Bits
import Data.Char                                        ( chr, ord )
import Prelude                                          hiding ( sum )

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Delayed
import Data.Array.Accelerate.Array.Representation       hiding ( sliceIndex )
import Data.Array.Accelerate.Array.Sugar (
  Z(..), (:.)(..), Array(..), Arrays, Scalar, Vector, Segments )
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Trafo.Substitution
import qualified Data.Array.Accelerate.Trafo.Sharing    as Sharing
import qualified Data.Array.Accelerate.Smart            as Sugar
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar

#include "accelerate.h"


-- Program execution
-- -----------------

-- | Run a complete embedded array program using the reference interpreter.
--
run :: Arrays a => Sugar.Acc a -> a
run = force . evalAcc . Sharing.convertAcc True True True


-- | Prepare and run an embedded array program of one argument
--
run1 :: (Arrays a, Arrays b) => (Sugar.Acc a -> Sugar.Acc b) -> a -> b
run1 = run'


-- | Prepare an n-ary embedded array program for execution, returning an n-ary
-- closure to do so.
--
run' :: Sharing.Afunction f => f -> Sharing.AfunctionR f
run' afun = let acc = Sharing.convertAfun True True True afun
            in  evalOpenAfun acc Empty


-- | Stream a lazily read list of input arrays through the given program,
-- collecting results as we go
--
stream :: (Arrays a, Arrays b) => (Sugar.Acc a -> Sugar.Acc b) -> [a] -> [b]
stream afun arrs = let go = run1 afun
                   in  map go arrs


-- Array expression evaluation
-- ---------------------------

-- Evaluate an open array function
--
evalOpenAfun :: OpenAfun aenv f -> Val aenv -> f
evalOpenAfun (Alam  f) aenv = \a -> evalOpenAfun f (aenv `Push` a)
evalOpenAfun (Abody b) aenv = force $ evalOpenAcc b aenv


-- Evaluate an open array expression
--
evalOpenAcc :: OpenAcc aenv a -> Val aenv -> Delayed a
evalOpenAcc (OpenAcc acc) = evalPreOpenAcc acc

evalPreOpenAcc :: forall aenv a. PreOpenAcc OpenAcc aenv a -> Val aenv -> Delayed a

evalPreOpenAcc (Alet acc1 acc2) aenv
  = let !arr1 = force $ evalOpenAcc acc1 aenv
    in evalOpenAcc acc2 (aenv `Push` arr1)

evalPreOpenAcc (Avar idx) aenv = delay $ prj idx aenv

evalPreOpenAcc (Atuple tup) aenv = delay (toTuple $ evalAtuple tup aenv :: a)

evalPreOpenAcc (Aprj ix (tup :: OpenAcc aenv arrs)) aenv =
  let tup'  = force $ evalOpenAcc tup aenv :: arrs
  in  delay $ evalPrj ix (fromTuple tup')

evalPreOpenAcc (Apply f acc) aenv =
  let !arr  = force $ evalOpenAcc acc aenv
  in  delay $ evalOpenAfun f aenv arr

evalPreOpenAcc (Acond cond acc1 acc2) aenv
  = if (evalExp cond aenv) then evalOpenAcc acc1 aenv else evalOpenAcc acc2 aenv

evalPreOpenAcc (Awhile cond body acc) aenv
  = let f       = evalOpenAfun body aenv
        p       = evalOpenAfun cond aenv
        go !x
          | (p x) Sugar.! Z = go (f x)
          | otherwise       = delay x
    in
    go . force $ evalOpenAcc acc aenv

evalPreOpenAcc (Use arr) _aenv = delay (Sugar.toArr arr :: a)

evalPreOpenAcc (Unit e) aenv = unitOp (evalExp e aenv)

evalPreOpenAcc (Generate sh f) aenv
  = generateOp (evalExp sh aenv) (evalFun f aenv)

evalPreOpenAcc (Transform sh ix f acc) aenv
  = transformOp (evalExp sh aenv) (evalFun ix aenv) (evalFun f aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (Reshape e acc) aenv
  = reshapeOp (evalExp e aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (Replicate sliceIndex slix acc) aenv
  = replicateOp sliceIndex (evalExp slix aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (Slice sliceIndex acc slix) aenv
  = sliceOp sliceIndex (evalOpenAcc acc aenv) (evalExp slix aenv)

evalPreOpenAcc (Map f acc) aenv = mapOp (evalFun f aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (ZipWith f acc1 acc2) aenv
  = zipWithOp (evalFun f aenv) (evalOpenAcc acc1 aenv) (evalOpenAcc acc2 aenv)

evalPreOpenAcc (Fold f e acc) aenv
  = foldOp (evalFun f aenv) (evalExp e aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (Fold1 f acc) aenv
  = fold1Op (evalFun f aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (FoldSeg f e acc1 acc2) aenv
  = foldSegOp integralType
              (evalFun f aenv) (evalExp e aenv)
              (evalOpenAcc acc1 aenv) (evalOpenAcc acc2 aenv)

evalPreOpenAcc (Fold1Seg f acc1 acc2) aenv
  = fold1SegOp integralType
               (evalFun f aenv) (evalOpenAcc acc1 aenv) (evalOpenAcc acc2 aenv)

evalPreOpenAcc (Scanl f e acc) aenv
  = scanlOp (evalFun f aenv) (evalExp e aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (Scanl' f e acc) aenv
  = scanl'Op (evalFun f aenv) (evalExp e aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (Scanl1 f acc) aenv
  = scanl1Op (evalFun f aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (Scanr f e acc) aenv
  = scanrOp (evalFun f aenv) (evalExp e aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (Scanr' f e acc) aenv
  = scanr'Op (evalFun f aenv) (evalExp e aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (Scanr1 f acc) aenv
  = scanr1Op (evalFun f aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (Permute f dftAcc p acc) aenv
  = permuteOp (evalFun f aenv) (evalOpenAcc dftAcc aenv)
              (evalFun p aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (Backpermute e p acc) aenv
  = backpermuteOp (evalExp e aenv) (evalFun p aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (Stencil sten bndy acc) aenv
  = stencilOp (evalFun sten aenv) bndy (evalOpenAcc acc aenv)

evalPreOpenAcc (Stencil2 sten bndy1 acc1 bndy2 acc2) aenv
  = stencil2Op (evalFun sten aenv) bndy1 (evalOpenAcc acc1 aenv) bndy2 (evalOpenAcc acc2 aenv)

-- The interpreter does not handle foreign functions so use the pure accelerate version
evalPreOpenAcc (Aforeign _ (Alam (Abody funAcc)) acc) aenv
  = let !arr = force $ evalOpenAcc acc aenv
    in evalOpenAcc funAcc (Empty `Push` arr)
evalPreOpenAcc (Aforeign _ _ _) _
  = error "This case is not possible"

-- Evaluate a closed array expressions
--
evalAcc :: Acc a -> Delayed a
evalAcc acc = evalOpenAcc acc Empty


-- Array tuple construction and projection
--
evalAtuple :: Atuple (OpenAcc aenv) t -> Val aenv -> t
evalAtuple NilAtup        _    = ()
evalAtuple (SnocAtup t a) aenv = (evalAtuple t aenv, force $ evalOpenAcc a aenv)


-- Array primitives
-- ----------------

unitOp :: Sugar.Elt e => e -> Delayed (Scalar e)
unitOp e
  = DelayedRpair DelayedRunit
  $ DelayedRarray {shapeDA = (), repfDA = const (Sugar.fromElt e)}

generateOp :: (Sugar.Shape dim, Sugar.Elt e)
      => dim
      -> (dim -> e)
      -> Delayed (Array dim e)
generateOp sh rf
  = DelayedRpair DelayedRunit
  $ DelayedRarray (Sugar.fromElt sh) (Sugar.sinkFromElt rf)

transformOp
      :: (Sugar.Shape sh', Sugar.Elt b)
      => sh'
      -> (sh' -> sh)
      -> (a -> b)
      -> Delayed (Array sh  a)
      -> Delayed (Array sh' b)
transformOp sh' ix f (DelayedRpair DelayedRunit (DelayedRarray _sh rf))
  = DelayedRpair DelayedRunit
  $ DelayedRarray (Sugar.fromElt sh')
                 (Sugar.sinkFromElt f . rf . Sugar.sinkFromElt ix)


reshapeOp :: Sugar.Shape dim
          => dim -> Delayed (Array dim' e) -> Delayed (Array dim e)
reshapeOp newShape darr@(DelayedRpair DelayedRunit (DelayedRarray {shapeDA = oldShape}))
  = let Array _ adata = force darr
    in
    BOUNDS_CHECK(check) "reshape" "shape mismatch" (Sugar.size newShape == size oldShape)
    $ delay $ Array (Sugar.fromElt newShape) adata

replicateOp :: (Sugar.Shape dim, Sugar.Elt slix)
            => SliceIndex (Sugar.EltRepr slix)
                          (Sugar.EltRepr sl)
                          co
                          (Sugar.EltRepr dim)
            -> slix
            -> Delayed (Array sl e)
            -> Delayed (Array dim e)
replicateOp sliceIndex slix (DelayedRpair DelayedRunit (DelayedRarray sh pf))
  = DelayedRpair DelayedRunit (DelayedRarray sh' (pf . pf'))
  where
    (sh', pf') = extend sliceIndex (Sugar.fromElt slix) sh

    extend :: SliceIndex slix sl co dim
           -> slix
           -> sl
           -> (dim, dim -> sl)
    extend (SliceNil)            ()        ()       = ((), const ())
    extend (SliceAll sliceIdx)   (slx, ()) (sl, sz)
      = let (dim', f') = extend sliceIdx slx sl
        in
        ((dim', sz), \(ix, i) -> (f' ix, i))
    extend (SliceFixed sliceIdx) (slx, sz) sl
      = let (dim', f') = extend sliceIdx slx sl
        in
        ((dim', sz), \(ix, _) -> f' ix)

sliceOp :: (Sugar.Shape sl, Sugar.Elt slix)
        => SliceIndex (Sugar.EltRepr slix)
                      (Sugar.EltRepr sl)
                      co
                      (Sugar.EltRepr dim)
        -> Delayed (Array dim e)
        -> slix
        -> Delayed (Array sl e)
sliceOp sliceIndex (DelayedRpair DelayedRunit (DelayedRarray sh pf)) slix
  = DelayedRpair DelayedRunit (DelayedRarray sh' (pf . pf'))
  where
    (sh', pf') = restrict sliceIndex (Sugar.fromElt slix) sh

    restrict :: SliceIndex slix sl co dim
             -> slix
             -> dim
             -> (sl, sl -> dim)
    restrict (SliceNil)            ()        ()       = ((), const ())
    restrict (SliceAll sliceIdx)   (slx, ()) (sl, sz)
      = let (sl', f') = restrict sliceIdx slx sl
        in
        ((sl', sz), \(ix, i) -> (f' ix, i))
    restrict (SliceFixed sliceIdx) (slx, i)  (sl, sz)
      = let (sl', f') = restrict sliceIdx slx sl
        in
        BOUNDS_CHECK(checkIndex) "slice" i sz $ (sl', \ix -> (f' ix, i))

mapOp :: Sugar.Elt e'
      => (e -> e')
      -> Delayed (Array dim e)
      -> Delayed (Array dim e')
mapOp f (DelayedRpair DelayedRunit (DelayedRarray sh rf))
  = DelayedRpair DelayedRunit
  $ DelayedRarray sh (Sugar.sinkFromElt f . rf)

zipWithOp :: Sugar.Elt e3
          => (e1 -> e2 -> e3)
          -> Delayed (Array dim e1)
          -> Delayed (Array dim e2)
          -> Delayed (Array dim e3)
zipWithOp f (DelayedRpair DelayedRunit (DelayedRarray sh1 rf1)) (DelayedRpair DelayedRunit (DelayedRarray sh2 rf2))
  = DelayedRpair DelayedRunit
  $ DelayedRarray (sh1 `intersect` sh2)
                 (\ix -> (Sugar.sinkFromElt2 f) (rf1 ix) (rf2 ix))

foldOp :: Sugar.Shape dim
       => (e -> e -> e)
       -> e
       -> Delayed (Array (dim:.Int) e)
       -> Delayed (Array dim e)
foldOp f e (DelayedRpair DelayedRunit (DelayedRarray (sh, n) rf))
  | size sh == 0
  = DelayedRpair DelayedRunit
  $ DelayedRarray (listToShape . map (max 1) . shapeToList $ sh)
      (\_ -> Sugar.fromElt e)
  --
  | otherwise
  = DelayedRpair DelayedRunit
  $ DelayedRarray sh
      (\ix -> iter ((), n) (\((), i) -> rf (ix, i)) (Sugar.sinkFromElt2 f) (Sugar.fromElt e))

fold1Op :: Sugar.Shape dim
        => (e -> e -> e)
        -> Delayed (Array (dim:.Int) e)
        -> Delayed (Array dim e)
fold1Op f (DelayedRpair DelayedRunit (DelayedRarray (sh, n) rf))
  = DelayedRpair DelayedRunit
  $ DelayedRarray sh (\ix -> iter1 ((), n) (\((), i) -> rf (ix, i)) (Sugar.sinkFromElt2 f))

foldSegOp :: IntegralType i
          -> (e -> e -> e)
          -> e
          -> Delayed (Array (dim:.Int) e)
          -> Delayed (Segments i)
          -> Delayed (Array (dim:.Int) e)
foldSegOp ty f e arr seg
  | IntegralDict <- integralDict ty = foldSegOp' f e arr seg

foldSegOp' :: forall i e dim. Integral i
           => (e -> e -> e)
           -> e
           -> Delayed (Array (dim:.Int) e)
           -> Delayed (Segments i)
           -> Delayed (Array (dim:.Int) e)
foldSegOp' f e (DelayedRpair DelayedRunit (DelayedRarray (sh, _n) rf)) seg@(DelayedRpair DelayedRunit (DelayedRarray shSeg rfSeg))
  = delay arr
  where
    DelayedRpair (DelayedRpair DelayedRunit (DelayedRarray _shSeg rfStarts)) _ = scanl'Op (+) 0 seg
    arr = Sugar.newArray (Sugar.toElt (sh, Sugar.toElt shSeg)) foldOne
    --
    foldOne :: dim:.Int -> e
    foldOne ix = let
                   (ix', i) = Sugar.fromElt ix
                   start    = fromIntegral ((Sugar.liftToElt rfStarts) i :: i)
                   len      = fromIntegral ((Sugar.liftToElt rfSeg)    i :: i)
                 in
                 fold ix' e start (start + len)

    fold :: Sugar.EltRepr dim -> e -> Int -> Int -> e
    fold ix' !v j end
      | j >= end  = v
      | otherwise = fold ix' (f v (Sugar.toElt . rf $ (ix', j))) (j + 1) end


fold1SegOp :: IntegralType i
           -> (e -> e -> e)
           -> Delayed (Array (dim:.Int) e)
           -> Delayed (Segments i)
           -> Delayed (Array (dim:.Int) e)
fold1SegOp ty f arr seg
  | IntegralDict <- integralDict ty = fold1SegOp' f arr seg


fold1SegOp' :: forall i e dim. Integral i
            => (e -> e -> e)
            -> Delayed (Array (dim:.Int) e)
            -> Delayed (Segments i)
            -> Delayed (Array (dim:.Int) e)
fold1SegOp' f (DelayedRpair DelayedRunit (DelayedRarray (sh, _n) rf)) seg@(DelayedRpair DelayedRunit (DelayedRarray shSeg rfSeg))
  = delay arr
  where
    DelayedRpair prefix _sum                                = scanl'Op (+) 0 seg
    DelayedRpair DelayedRunit (DelayedRarray _shSeg rfStarts) = prefix
    arr = Sugar.newArray (Sugar.toElt (sh, Sugar.toElt shSeg)) foldOne
    --
    foldOne :: dim:.Int -> e
    foldOne ix = let
                   (ix', i) = Sugar.fromElt ix
                   start    = fromIntegral ((Sugar.liftToElt rfStarts) i :: i)
                   len      = fromIntegral ((Sugar.liftToElt rfSeg)    i :: i)
                 in
                 if len == 0
                   then
                     BOUNDS_ERROR(error) "fold1Seg" "empty iteration space"
                   else
                     fold ix' (Sugar.toElt . rf $ (ix', start)) (start + 1) (start + len)

    fold :: Sugar.EltRepr dim -> e -> Int -> Int -> e
    fold ix' !v j end
      | j >= end  = v
      | otherwise = fold ix' (f v (Sugar.toElt . rf $ (ix', j))) (j + 1) end


scanlOp :: forall e. (e -> e -> e)
        -> e
        -> Delayed (Vector e)
        -> Delayed (Vector e)
scanlOp f e (DelayedRpair DelayedRunit (DelayedRarray sh rf))
  = delay $ adata `seq` Array ((), n + 1) adata
  where
    n  = size sh
    f' = Sugar.sinkFromElt2 f
    --
    (adata, _) = runArrayData $ do
                   arr   <- newArrayData (n + 1)
                   final <- traverse arr 0 (Sugar.fromElt e)
                   unsafeWriteArrayData arr n final
                   return (arr, undefined)

    traverse :: MutableArrayData s (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> ST s (Sugar.EltRepr e)
    traverse arr i v
      | i >= n    = return v
      | otherwise = do
                      unsafeWriteArrayData arr i v
                      traverse arr (i + 1) (f' v (rf ((), i)))

scanl'Op :: forall e. (e -> e -> e)
         -> e
         -> Delayed (Vector e)
         -> Delayed (Vector e, Scalar e)
scanl'Op f e (DelayedRpair DelayedRunit (DelayedRarray sh rf))
  = DelayedRpair (delay $ adata `seq` Array sh adata) final
  where
    n  = size sh
    f' = Sugar.sinkFromElt2 f
    --
    DelayedRpair DelayedRunit final = unitOp (Sugar.toElt asum)

    (adata, asum) = runArrayData $ do
                      arr <- newArrayData n
                      sum <- traverse arr 0 (Sugar.fromElt e)
                      return (arr, sum)

    traverse :: MutableArrayData s (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> ST s (Sugar.EltRepr e)
    traverse arr i v
      | i >= n    = return v
      | otherwise = do
                      unsafeWriteArrayData arr i v
                      traverse arr (i + 1) (f' v (rf ((), i)))

scanl1Op :: forall e. (e -> e -> e)
         -> Delayed (Vector e)
         -> Delayed (Vector e)
scanl1Op f (DelayedRpair DelayedRunit (DelayedRarray sh rf))
  = delay $ adata `seq` Array sh adata
  where
    n  = size sh
    f' = Sugar.sinkFromElt2 f
    --
    (adata, _) = runArrayData $ do
                   arr <- newArrayData n
                   traverse arr 0 undefined
                   return (arr, undefined)

    traverse :: MutableArrayData s (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> ST s ()
    traverse arr i v
      | i >= n    = return ()
      | i == 0    = do
                      let e = rf ((), i)
                      unsafeWriteArrayData arr i e
                      traverse arr (i + 1) e
      | otherwise = do
                      let e = f' v (rf ((), i))
                      unsafeWriteArrayData arr i e
                      traverse arr (i + 1) e

scanrOp :: forall e. (e -> e -> e)
        -> e
        -> Delayed (Vector e)
        -> Delayed (Vector e)
scanrOp f e (DelayedRpair DelayedRunit (DelayedRarray sh rf))
  = delay $ adata `seq` Array ((), n + 1) adata
  where
    n  = size sh
    f' = Sugar.sinkFromElt2 f
    --
    (adata, _) = runArrayData $ do
                   arr   <- newArrayData (n + 1)
                   final <- traverse arr n (Sugar.fromElt e)
                   unsafeWriteArrayData arr 0 final
                   return (arr, undefined)

    traverse :: MutableArrayData s (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> ST s (Sugar.EltRepr e)
    traverse arr i v
      | i == 0    = return v
      | otherwise = do
                      unsafeWriteArrayData arr i v
                      traverse arr (i - 1) (f' v (rf ((), i-1)))

scanr'Op :: forall e. (e -> e -> e)
         -> e
         -> Delayed (Vector e)
         -> Delayed (Vector e, Scalar e)
scanr'Op f e (DelayedRpair DelayedRunit (DelayedRarray sh rf))
  = DelayedRpair (delay $ adata `seq` Array sh adata) final
  where
    n  = size sh
    f' = Sugar.sinkFromElt2 f
    --
    DelayedRpair DelayedRunit final = unitOp (Sugar.toElt asum)

    (adata, asum) = runArrayData $ do
                      arr <- newArrayData n
                      sum <- traverse arr (n-1) (Sugar.fromElt e)
                      return (arr, sum)

    traverse :: MutableArrayData s (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> ST s (Sugar.EltRepr e)
    traverse arr i v
      | i < 0     = return v
      | otherwise = do
                      unsafeWriteArrayData arr i v
                      traverse arr (i - 1) (f' v (rf ((), i)))

scanr1Op :: forall e. (e -> e -> e)
         -> Delayed (Vector e)
         -> Delayed (Vector e)
scanr1Op f (DelayedRpair DelayedRunit (DelayedRarray sh rf))
  = delay $ adata `seq` Array sh adata
  where
    n  = size sh
    f' = Sugar.sinkFromElt2 f
    --
    (adata, _) = runArrayData $ do
                   arr <- newArrayData n
                   traverse arr (n - 1) undefined
                   return (arr, undefined)

    traverse :: MutableArrayData s (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> ST s ()
    traverse arr i v
      | i < 0        = return ()
      | i == (n - 1) = do
                         let e = rf ((), i)
                         unsafeWriteArrayData arr i e
                         traverse arr (i - 1) e
      | otherwise    = do
                         let e = f' v (rf ((), i))
                         unsafeWriteArrayData arr i e
                         traverse arr (i - 1) e

permuteOp :: (e -> e -> e)
          -> Delayed (Array dim' e)
          -> (dim -> dim')
          -> Delayed (Array dim e)
          -> Delayed (Array dim' e)
permuteOp f (DelayedRpair DelayedRunit (DelayedRarray dftsSh dftsPf))
          p (DelayedRpair DelayedRunit (DelayedRarray sh pf))
  = delay $ adata `seq` Array dftsSh adata
  where
    f' = Sugar.sinkFromElt2 f
    --
    (adata, _)
      = runArrayData $ do

            -- new array in target dimension
          arr <- newArrayData (size dftsSh)

            -- initialise it with the default values
          let write ix = unsafeWriteArrayData arr (toIndex dftsSh ix) (dftsPf ix)
          iter dftsSh write (>>) (return ())

            -- traverse the source dimension and project each element into
            -- the target dimension (where it gets combined with the current
            -- default)
          let update ix = do
                            let target = (Sugar.sinkFromElt p) ix
                            unless (target == ignore) $ do
                              let i = toIndex dftsSh target
                              e <- unsafeReadArrayData arr i
                              unsafeWriteArrayData arr i (pf ix `f'` e)
          iter sh update (>>) (return ())

            -- return the updated array
          return (arr, undefined)

backpermuteOp :: Sugar.Shape dim'
              => dim'
              -> (dim' -> dim)
              -> Delayed (Array dim e)
              -> Delayed (Array dim' e)
backpermuteOp sh' p (DelayedRpair DelayedRunit (DelayedRarray _sh rf))
  = DelayedRpair DelayedRunit
  $ DelayedRarray (Sugar.fromElt sh') (rf . Sugar.sinkFromElt p)

stencilOp :: forall dim e e' stencil. (Sugar.Elt e, Sugar.Elt e', Stencil dim e stencil)
          => (stencil -> e')
          -> Boundary (Sugar.EltRepr e)
          -> Delayed (Array dim e)
          -> Delayed (Array dim e')
stencilOp sten bndy (DelayedRpair DelayedRunit (DelayedRarray sh rf))
  = DelayedRpair DelayedRunit
  $ DelayedRarray sh rf'
  where
    rf' = Sugar.sinkFromElt (sten . stencilAccess rfBounded)

    -- add a boundary to the source array as specified by the boundary condition
    rfBounded :: dim -> e
    rfBounded ix = Sugar.toElt $ case Sugar.bound (Sugar.toElt sh) ix bndy of
                                    Left v    -> v
                                    Right ix' -> rf (Sugar.fromElt ix')

stencil2Op :: forall dim e1 e2 e' stencil1 stencil2.
              (Sugar.Elt e1, Sugar.Elt e2, Sugar.Elt e',
               Stencil dim e1 stencil1, Stencil dim e2 stencil2)
           => (stencil1 -> stencil2 -> e')
           -> Boundary (Sugar.EltRepr e1)
           -> Delayed (Array dim e1)
           -> Boundary (Sugar.EltRepr e2)
           -> Delayed (Array dim e2)
           -> Delayed (Array dim e')
stencil2Op sten bndy1 (DelayedRpair DelayedRunit (DelayedRarray sh1 rf1))
                bndy2 (DelayedRpair DelayedRunit (DelayedRarray sh2 rf2))
  = DelayedRpair DelayedRunit (DelayedRarray (sh1 `intersect` sh2) rf')
  where
    rf' = Sugar.sinkFromElt (\ix -> sten (stencilAccess rf1Bounded ix)
                                         (stencilAccess rf2Bounded ix))

    -- add a boundary to the source arrays as specified by the boundary conditions
    rf1Bounded :: dim -> e1
    rf1Bounded ix = Sugar.toElt $ case Sugar.bound (Sugar.toElt sh1) ix bndy1 of
                                     Left v    -> v
                                     Right ix' -> rf1 (Sugar.fromElt ix')

    rf2Bounded :: dim -> e2
    rf2Bounded ix = Sugar.toElt $ case Sugar.bound (Sugar.toElt sh2) ix bndy2 of
                                     Left v    -> v
                                     Right ix' -> rf2 (Sugar.fromElt ix')


-- Expression evaluation
-- ---------------------

-- Evaluate open function
--
evalOpenFun :: OpenFun env aenv t -> ValElt env -> Val aenv -> t
evalOpenFun (Body e) env aenv = evalOpenExp e env aenv
evalOpenFun (Lam f)  env aenv
  = \x -> evalOpenFun f (env `PushElt` Sugar.fromElt x) aenv

-- Evaluate a closed function
--
evalFun :: Fun aenv t -> Val aenv -> t
evalFun f aenv = evalOpenFun f EmptyElt aenv

-- Evaluate an open expression
--
-- NB: The implementation of 'Index' and 'Shape' demonstrate clearly why
--     array expressions must be hoisted out of scalar expressions before code
--     execution.  If these operations are in the body of a function that
--     gets mapped over an array, the array argument would be forced many times
--     leading to a large amount of wasteful recomputation.
--
evalOpenExp :: OpenExp env aenv a -> ValElt env -> Val aenv -> a

evalOpenExp (Let exp1 exp2) env aenv
  = let !v1 = evalOpenExp exp1 env aenv
    in evalOpenExp exp2 (env `PushElt` Sugar.fromElt v1) aenv

evalOpenExp (Var idx) env _
  = prjElt idx env

evalOpenExp (Const c) _ _
  = Sugar.toElt c

evalOpenExp (Tuple tup) env aenv
  = toTuple $ evalTuple tup env aenv

evalOpenExp (Prj idx e) env aenv
  = evalPrj idx (fromTuple $ evalOpenExp e env aenv)

evalOpenExp IndexNil _env _aenv
  = Z

evalOpenExp (IndexCons sh i) env aenv
  = evalOpenExp sh env aenv :. evalOpenExp i env aenv

evalOpenExp (IndexHead ix) env aenv
  = case evalOpenExp ix env aenv of _:.h -> h

evalOpenExp (IndexTail ix) env aenv
  = case evalOpenExp ix env aenv of t:._ -> t

evalOpenExp (IndexAny) _ _
  = Sugar.Any

evalOpenExp (IndexSlice sliceIndex slix sh) env aenv
  = Sugar.toElt
  $ restrict sliceIndex (Sugar.fromElt $ evalOpenExp slix env aenv)
                        (Sugar.fromElt $ evalOpenExp sh   env aenv)
  where
    restrict :: SliceIndex slix sl co sh -> slix -> sh -> sl
    restrict SliceNil              ()        ()       = ()
    restrict (SliceAll sliceIdx)   (slx, ()) (sl, sz)
      = let sl' = restrict sliceIdx slx sl
        in  (sl', sz)
    restrict (SliceFixed sliceIdx) (slx, _i)  (sl, _sz)
      = restrict sliceIdx slx sl

evalOpenExp (IndexFull sliceIndex slix sh) env aenv
  = Sugar.toElt
  $ extend sliceIndex (Sugar.fromElt $ evalOpenExp slix env aenv)
                      (Sugar.fromElt $ evalOpenExp sh   env aenv)
  where
    extend :: SliceIndex slix sl co sh -> slix -> sl -> sh
    extend SliceNil              ()        ()       = ()
    extend (SliceAll sliceIdx)   (slx, ()) (sl, sz)
      = let sh' = extend sliceIdx slx sl
        in  (sh', sz)
    extend (SliceFixed sliceIdx) (slx, sz) sl
      = let sh' = extend sliceIdx slx sl
        in  (sh', sz)

evalOpenExp (ToIndex sh ix) env aenv
  = Sugar.toIndex (evalOpenExp sh env aenv) (evalOpenExp ix env aenv)

evalOpenExp (FromIndex sh ix) env aenv
  = Sugar.fromIndex (evalOpenExp sh env aenv) (evalOpenExp ix env aenv)

evalOpenExp (Cond c t e) env aenv
  = if evalOpenExp c env aenv
    then evalOpenExp t env aenv
    else evalOpenExp e env aenv

evalOpenExp (While cond body seed) env aenv
  = let f       = evalOpenFun body env aenv
        p       = evalOpenFun cond env aenv
        go !x
          | p x         = go (f x)
          | otherwise   = x
    in
    go (evalOpenExp seed env aenv)

evalOpenExp (PrimConst c) _ _ = evalPrimConst c

evalOpenExp (PrimApp p arg) env aenv
  = evalPrim p (evalOpenExp arg env aenv)

evalOpenExp (Index acc ix) env aenv
  = case evalOpenAcc acc aenv of
      DelayedRpair DelayedRunit (DelayedRarray sh pf) ->
        let ix' = Sugar.fromElt $ evalOpenExp ix env aenv
        in
        toIndex sh ix' `seq` (Sugar.toElt $ pf ix')
                              -- FIXME: This is ugly, but (possibly) needed to
                              --       ensure bounds checking

evalOpenExp (LinearIndex acc i) env aenv
  = case evalOpenAcc acc aenv of
      DelayedRpair DelayedRunit (DelayedRarray sh pf) ->
        let i' = evalOpenExp i env aenv
            v  = pf (fromIndex sh i')
        in Sugar.toElt v

evalOpenExp (Shape acc) _ aenv
  = case evalOpenAcc acc aenv of
      DelayedRpair DelayedRunit (DelayedRarray sh _) -> Sugar.toElt sh

evalOpenExp (ShapeSize sh) env aenv
  = Sugar.size (evalOpenExp sh env aenv)

evalOpenExp (Intersect sh1 sh2) env aenv
  = Sugar.intersect (evalOpenExp sh1 env aenv) (evalOpenExp sh2 env aenv)

evalOpenExp (Foreign _ f e) env aenv
  = evalOpenExp e' env aenv
  where
    wExp :: Idx ((),a) t -> Idx (env,a) t
    wExp ZeroIdx = ZeroIdx
    wExp _       = INTERNAL_ERROR(error) "wExp" "unreachable case"

    e' = case f of
           (Lam (Body b)) -> Let e $ weakenEA rebuildOpenAcc undefined (weakenE wExp b)
           _              -> INTERNAL_ERROR(error) "travE" "unreachable case"

-- Evaluate a closed expression
--
evalExp :: Exp aenv t -> Val aenv -> t
evalExp e aenv = evalOpenExp e EmptyElt aenv


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

evalTuple :: Tuple (OpenExp env aenv) t -> ValElt env -> Val aenv -> t
evalTuple NilTup            _env _aenv = ()
evalTuple (tup `SnocTup` e) env  aenv  = (evalTuple tup env aenv, evalOpenExp e env aenv)

evalPrj :: TupleIdx t e -> t -> e
evalPrj ZeroTupIdx       (!_, v)   = v
evalPrj (SuccTupIdx idx) (tup, !_) = evalPrj idx tup
  -- FIXME: Strictly speaking, we ought to force all components of a tuples;
  --        not only those that we happen to encounter during the recursive
  --        walk.


-- Implementation of scalar primitives
-- -----------------------------------

evalLAnd :: (Bool, Bool) -> Bool
evalLAnd (!x, !y) = x && y

evalLOr  :: (Bool, Bool) -> Bool
evalLOr (!x, !y) = x || y

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
  , IntegralDict <- integralDict tb = fromIntegral
evalFromIntegral ta (FloatingNumType tb)
  | IntegralDict <- integralDict ta
  , FloatingDict <- floatingDict tb = fromIntegral


-- Extract methods from reified dictionaries
--

-- Constant methods of Bounded
--

evalMinBound :: BoundedType a -> a
evalMinBound (IntegralBoundedType ty)
  | IntegralDict <- integralDict ty = minBound
evalMinBound (NonNumBoundedType   ty)
  | NonNumDict   <- nonNumDict ty   = minBound

evalMaxBound :: BoundedType a -> a
evalMaxBound (IntegralBoundedType ty)
  | IntegralDict <- integralDict ty = maxBound
evalMaxBound (NonNumBoundedType   ty)
  | NonNumDict   <- nonNumDict ty   = maxBound

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
  , IntegralDict <- integralDict tb = truncate

evalRound :: FloatingType a -> IntegralType b -> (a -> b)
evalRound ta tb
  | FloatingDict <- floatingDict ta
  , IntegralDict <- integralDict tb = round

evalFloor :: FloatingType a -> IntegralType b -> (a -> b)
evalFloor ta tb
  | FloatingDict <- floatingDict ta
  , IntegralDict <- integralDict tb = floor

evalCeiling :: FloatingType a -> IntegralType b -> (a -> b)
evalCeiling ta tb
  | FloatingDict <- floatingDict ta
  , IntegralDict <- integralDict tb = ceiling

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
evalLt (NumScalarType (IntegralNumType ty))
  | IntegralDict <- integralDict ty = uncurry (<)
evalLt (NumScalarType (FloatingNumType ty))
  | FloatingDict <- floatingDict ty = uncurry (<)
evalLt (NonNumScalarType ty)
  | NonNumDict   <- nonNumDict ty   = uncurry (<)

evalGt :: ScalarType a -> ((a, a) -> Bool)
evalGt (NumScalarType (IntegralNumType ty))
  | IntegralDict <- integralDict ty = uncurry (>)
evalGt (NumScalarType (FloatingNumType ty))
  | FloatingDict <- floatingDict ty = uncurry (>)
evalGt (NonNumScalarType ty)
  | NonNumDict   <- nonNumDict ty   = uncurry (>)

evalLtEq :: ScalarType a -> ((a, a) -> Bool)
evalLtEq (NumScalarType (IntegralNumType ty))
  | IntegralDict <- integralDict ty = uncurry (<=)
evalLtEq (NumScalarType (FloatingNumType ty))
  | FloatingDict <- floatingDict ty = uncurry (<=)
evalLtEq (NonNumScalarType ty)
  | NonNumDict   <- nonNumDict ty   = uncurry (<=)

evalGtEq :: ScalarType a -> ((a, a) -> Bool)
evalGtEq (NumScalarType (IntegralNumType ty))
  | IntegralDict <- integralDict ty = uncurry (>=)
evalGtEq (NumScalarType (FloatingNumType ty))
  | FloatingDict <- floatingDict ty = uncurry (>=)
evalGtEq (NonNumScalarType ty)
  | NonNumDict   <- nonNumDict ty   = uncurry (>=)

evalEq :: ScalarType a -> ((a, a) -> Bool)
evalEq (NumScalarType (IntegralNumType ty))
  | IntegralDict <- integralDict ty = uncurry (==)
evalEq (NumScalarType (FloatingNumType ty))
  | FloatingDict <- floatingDict ty = uncurry (==)
evalEq (NonNumScalarType ty)
  | NonNumDict   <- nonNumDict ty   = uncurry (==)

evalNEq :: ScalarType a -> ((a, a) -> Bool)
evalNEq (NumScalarType (IntegralNumType ty))
  | IntegralDict <- integralDict ty = uncurry (/=)
evalNEq (NumScalarType (FloatingNumType ty))
  | FloatingDict <- floatingDict ty = uncurry (/=)
evalNEq (NonNumScalarType ty)
  | NonNumDict   <- nonNumDict ty   = uncurry (/=)

evalMax :: ScalarType a -> ((a, a) -> a)
evalMax (NumScalarType (IntegralNumType ty))
  | IntegralDict <- integralDict ty = uncurry max
evalMax (NumScalarType (FloatingNumType ty))
  | FloatingDict <- floatingDict ty = uncurry max
evalMax (NonNumScalarType ty)
  | NonNumDict   <- nonNumDict ty   = uncurry max

evalMin :: ScalarType a -> ((a, a) -> a)
evalMin (NumScalarType (IntegralNumType ty))
  | IntegralDict <- integralDict ty = uncurry min
evalMin (NumScalarType (FloatingNumType ty))
  | FloatingDict <- floatingDict ty = uncurry min
evalMin (NonNumScalarType ty)
  | NonNumDict   <- nonNumDict ty   = uncurry min

