{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE CPP, GADTs, BangPatterns, TypeOperators, PatternGuards #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}
-- |
-- Module      : Data.Array.Accelerate.Interpreter
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
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
  Arrays, run, stream,

  -- Internal (hidden)
  evalPrim, evalPrimConst, evalPrj

) where

-- standard libraries
import Control.Monad
import Control.Monad.ST                            (ST)
import Data.Bits
import Data.Char                                   (chr, ord)
import Prelude                                     hiding (sum)

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation  hiding (sliceIndex)
import Data.Array.Accelerate.Array.Sugar (
  Z(..), (:.)(..), Array(..), Scalar, Vector, Segments)
import Data.Array.Accelerate.Array.Delayed
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Smart       as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar

#include "accelerate.h"


-- Program execution
-- -----------------

-- |Run a complete embedded array program using the reference interpreter.
--
run :: Arrays a => Sugar.Acc a -> a
run = force . evalAcc . Sugar.convertAcc

-- | Stream a lazily read list of input arrays through the given program,
-- collecting results as we go
--
stream :: (Arrays a, Arrays b) => (Sugar.Acc a -> Sugar.Acc b) -> [a] -> [b]
stream afun = map (run1 acc)
  where
    acc = Sugar.convertAccFun1 afun

    run1 :: Delayable b => Afun (a -> b) -> a -> b
    run1 (Alam (Abody f)) = \a -> force (evalOpenAcc f (Empty `Push` a))
    run1 _                = error "Hey type checker! We can not get here!"


-- Array expression evaluation
-- ---------------------------

-- Evaluate an open array expression
--
evalOpenAcc :: Delayable a => OpenAcc aenv a -> Val aenv -> Delayed a
evalOpenAcc (OpenAcc acc) = evalPreOpenAcc acc

evalPreOpenAcc :: Delayable a => PreOpenAcc OpenAcc aenv a -> Val aenv -> Delayed a

evalPreOpenAcc (Let acc1 acc2) aenv 
  = let !arr1 = force $ evalOpenAcc acc1 aenv
    in evalOpenAcc acc2 (aenv `Push` arr1)

evalPreOpenAcc (Let2 acc1 acc2) aenv 
  = let (!arr1, !arr2) = force $ evalOpenAcc acc1 aenv
    in evalOpenAcc acc2 (aenv `Push` arr1 `Push` arr2)

evalPreOpenAcc (PairArrays acc1 acc2) aenv
  = DelayedPair (evalOpenAcc acc1 aenv) (evalOpenAcc acc2 aenv)

evalPreOpenAcc (Avar idx) aenv = delay $ prj idx aenv

evalPreOpenAcc (Apply (Alam (Abody funAcc)) acc) aenv
  = let !arr = force $ evalOpenAcc acc aenv
    in evalOpenAcc funAcc (Empty `Push` arr)
evalPreOpenAcc (Apply _afun _acc) _aenv
  = error "GHC's pattern match checker is too dumb to figure that this case is impossible"

evalPreOpenAcc (Acond cond acc1 acc2) aenv
  = if (evalExp cond aenv) then evalOpenAcc acc1 aenv else evalOpenAcc acc2 aenv

evalPreOpenAcc (Use arr) _aenv = delay arr

evalPreOpenAcc (Unit e) aenv = unitOp (evalExp e aenv)

evalPreOpenAcc (Generate sh f) aenv
  = generateOp (evalExp sh aenv) (evalFun f aenv)

evalPreOpenAcc (Reshape e acc) aenv 
  = reshapeOp (evalExp e aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (Replicate sliceIndex slix acc) aenv
  = replicateOp sliceIndex (evalExp slix aenv) (evalOpenAcc acc aenv)
  
evalPreOpenAcc (Index sliceIndex acc slix) aenv
  = indexOp sliceIndex (evalOpenAcc acc aenv) (evalExp slix aenv)

evalPreOpenAcc (Map f acc) aenv = mapOp (evalFun f aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (ZipWith f acc1 acc2) aenv
  = zipWithOp (evalFun f aenv) (evalOpenAcc acc1 aenv) (evalOpenAcc acc2 aenv)

evalPreOpenAcc (Fold f e acc) aenv
  = foldOp (evalFun f aenv) (evalExp e aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (Fold1 f acc) aenv
  = fold1Op (evalFun f aenv) (evalOpenAcc acc aenv)

evalPreOpenAcc (FoldSeg f e acc1 acc2) aenv
  = foldSegOp (evalFun f aenv) (evalExp e aenv) 
              (evalOpenAcc acc1 aenv) (evalOpenAcc acc2 aenv)

evalPreOpenAcc (Fold1Seg f acc1 acc2) aenv
  = fold1SegOp (evalFun f aenv) (evalOpenAcc acc1 aenv) (evalOpenAcc acc2 aenv)

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

-- Evaluate a closed array expressions
--
evalAcc :: Delayable a => Acc a -> Delayed a
evalAcc acc = evalOpenAcc acc Empty


-- Array primitives
-- ----------------

unitOp :: Sugar.Elt e => e -> Delayed (Scalar e)
unitOp e = DelayedArray {shapeDA = (), repfDA = const (Sugar.fromElt e)}

generateOp :: (Sugar.Shape dim, Sugar.Elt e)
      => dim
      -> (dim -> e)
      -> Delayed (Array dim e)
generateOp sh rf = DelayedArray (Sugar.fromElt sh) (Sugar.sinkFromElt rf)

reshapeOp :: Sugar.Shape dim 
          => dim -> Delayed (Array dim' e) -> Delayed (Array dim e)
reshapeOp newShape darr@(DelayedArray {shapeDA = oldShape})
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
replicateOp sliceIndex slix (DelayedArray sh pf)
  = DelayedArray sh' (pf . pf')
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

indexOp :: (Sugar.Shape sl, Sugar.Elt slix)
        => SliceIndex (Sugar.EltRepr slix)
                      (Sugar.EltRepr sl)
                      co
                      (Sugar.EltRepr dim)
        -> Delayed (Array dim e)
        -> slix
        -> Delayed (Array sl e)
indexOp sliceIndex (DelayedArray sh pf) slix
  = DelayedArray sh' (pf . pf')
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
        BOUNDS_CHECK(checkIndex) "index" i sz $ (sl', \ix -> (f' ix, i))

mapOp :: Sugar.Elt e' 
      => (e -> e') 
      -> Delayed (Array dim e) 
      -> Delayed (Array dim e')
mapOp f (DelayedArray sh rf) = DelayedArray sh (Sugar.sinkFromElt f . rf)

zipWithOp :: Sugar.Elt e3
          => (e1 -> e2 -> e3) 
          -> Delayed (Array dim e1) 
          -> Delayed (Array dim e2) 
          -> Delayed (Array dim e3)
zipWithOp f (DelayedArray sh1 rf1) (DelayedArray sh2 rf2) 
  = DelayedArray (sh1 `intersect` sh2) 
                 (\ix -> (Sugar.sinkFromElt2 f) (rf1 ix) (rf2 ix))

foldOp :: Sugar.Shape dim 
       => (e -> e -> e)
       -> e
       -> Delayed (Array (dim:.Int) e)
       -> Delayed (Array dim e)
foldOp f e (DelayedArray (sh, n) rf)
  = DelayedArray sh 
      (\ix -> iter ((), n) (\((), i) -> rf (ix, i)) (Sugar.sinkFromElt2 f) (Sugar.fromElt e))

fold1Op :: Sugar.Shape dim
        => (e -> e -> e)
        -> Delayed (Array (dim:.Int) e)
        -> Delayed (Array dim e)
fold1Op f (DelayedArray (sh, n) rf)
  = DelayedArray sh (\ix -> iter1 ((), n) (\((), i) -> rf (ix, i)) (Sugar.sinkFromElt2 f))
    
foldSegOp :: forall e dim.
             (e -> e -> e)
          -> e
          -> Delayed (Array (dim:.Int) e)
          -> Delayed Segments
          -> Delayed (Array (dim:.Int) e)
foldSegOp f e (DelayedArray (sh, _n) rf) seg@(DelayedArray shSeg rfSeg)
  = delay arr
  where
    DelayedPair (DelayedArray _shSeg rfStarts) _ = scanl'Op (+) 0 seg
    arr = Sugar.newArray (Sugar.toElt (sh, Sugar.toElt shSeg)) foldOne
    --
    foldOne :: dim:.Int -> e
    foldOne ix = let
                   (ix', i) = Sugar.fromElt ix
                   start    = (Sugar.liftToElt rfStarts) i
                   len      = (Sugar.liftToElt rfSeg) i
                 in
                 fold ix' e start (start + len)

    fold :: Sugar.EltRepr dim -> e -> Int -> Int -> e
    fold ix' v j end
      | j >= end  = v
      | otherwise = fold ix' (f v (Sugar.toElt . rf $ (ix', j))) (j + 1) end

fold1SegOp :: forall e dim.
              (e -> e -> e)
           -> Delayed (Array (dim:.Int) e)
           -> Delayed Segments
           -> Delayed (Array (dim:.Int) e)
fold1SegOp f (DelayedArray (sh, _n) rf) seg@(DelayedArray shSeg rfSeg)
  = delay arr
  where
    DelayedPair (DelayedArray _shSeg rfStarts) _ = scanl'Op (+) 0 seg
    arr = Sugar.newArray (Sugar.toElt (sh, Sugar.toElt shSeg)) foldOne
    --
    foldOne :: dim:.Int -> e
    foldOne ix = let
                   (ix', i) = Sugar.fromElt ix
                   start    = (Sugar.liftToElt rfStarts) i
                   len      = (Sugar.liftToElt rfSeg) i
                 in
                 if len == 0
                   then
                     BOUNDS_ERROR(error) "fold1Seg" "empty iteration space"
                   else
                     fold ix' (Sugar.toElt . rf $ (ix', start)) (start + 1) (start + len)

    fold :: Sugar.EltRepr dim -> e -> Int -> Int -> e
    fold ix' v j end
      | j >= end  = v
      | otherwise = fold ix' (f v (Sugar.toElt . rf $ (ix', j))) (j + 1) end

scanlOp :: forall e. (e -> e -> e)
        -> e
        -> Delayed (Vector e)
        -> Delayed (Vector e)
scanlOp f e (DelayedArray sh rf)
  = delay $ adata `seq` Array ((), n + 1) adata
  where
    n  = size sh
    f' = Sugar.sinkFromElt2 f
    --
    (adata, _) = runArrayData $ do
                   arr   <- newArrayData (n + 1)
                   final <- traverse arr 0 (Sugar.fromElt e)
                   writeArrayData arr n final
                   return (arr, undefined)

    traverse :: MutableArrayData s (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> ST s (Sugar.EltRepr e)
    traverse arr i v
      | i >= n    = return v
      | otherwise = do
                      writeArrayData arr i v
                      traverse arr (i + 1) (f' v (rf ((), i)))

scanl'Op :: forall e. (e -> e -> e)
         -> e
         -> Delayed (Vector e)
         -> Delayed (Vector e, Scalar e)
scanl'Op f e (DelayedArray sh rf)
  = DelayedPair (delay $ adata `seq` Array sh adata) 
                (unitOp (Sugar.toElt final))
  where
    n  = size sh
    f' = Sugar.sinkFromElt2 f
    --
    (adata, final) = runArrayData $ do
                       arr <- newArrayData n
                       sum <- traverse arr 0 (Sugar.fromElt e)
                       return (arr, sum)

    traverse :: MutableArrayData s (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> ST s (Sugar.EltRepr e)
    traverse arr i v
      | i >= n    = return v
      | otherwise = do
                      writeArrayData arr i v
                      traverse arr (i + 1) (f' v (rf ((), i)))

scanl1Op :: forall e. (e -> e -> e)
         -> Delayed (Vector e)
         -> Delayed (Vector e)
scanl1Op f (DelayedArray sh rf)
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
                      writeArrayData arr i e
                      traverse arr (i + 1) e
      | otherwise = do
                      let e = f' v (rf ((), i))
                      writeArrayData arr i e
                      traverse arr (i + 1) e

scanrOp :: forall e. (e -> e -> e)
        -> e
        -> Delayed (Vector e)
        -> Delayed (Vector e)
scanrOp f e (DelayedArray sh rf)
  = delay $ adata `seq` Array ((), n + 1) adata
  where
    n  = size sh
    f' = Sugar.sinkFromElt2 f
    --
    (adata, _) = runArrayData $ do
                   arr   <- newArrayData (n + 1)
                   final <- traverse arr n (Sugar.fromElt e)
                   writeArrayData arr 0 final
                   return (arr, undefined)

    traverse :: MutableArrayData s (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> ST s (Sugar.EltRepr e)
    traverse arr i v
      | i == 0    = return v
      | otherwise = do
                      writeArrayData arr i v
                      traverse arr (i - 1) (f' v (rf ((), i)))

scanr'Op :: forall e. (e -> e -> e)
         -> e
         -> Delayed (Vector e)
         -> Delayed (Vector e, Scalar e)
scanr'Op f e (DelayedArray sh rf)
  = DelayedPair (delay $ adata `seq` Array sh adata)
                (unitOp (Sugar.toElt final))
  where
    n  = size sh
    f' = Sugar.sinkFromElt2 f
    --
    (adata, final) = runArrayData $ do
                       arr <- newArrayData n
                       sum <- traverse arr (n-1) (Sugar.fromElt e)
                       return (arr, sum)

    traverse :: MutableArrayData s (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> ST s (Sugar.EltRepr e)
    traverse arr i v
      | i < 0     = return v
      | otherwise = do
                      writeArrayData arr i v
                      traverse arr (i - 1) (f' v (rf ((), i)))

scanr1Op :: forall e. (e -> e -> e)
         -> Delayed (Vector e)
         -> Delayed (Vector e)
scanr1Op f (DelayedArray sh rf)
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
                         writeArrayData arr i e
                         traverse arr (i - 1) e
      | otherwise    = do
                         let e = f' v (rf ((), i))
                         writeArrayData arr i e
                         traverse arr (i - 1) e

permuteOp :: (e -> e -> e)
          -> Delayed (Array dim' e)
          -> (dim -> dim')
          -> Delayed (Array dim e)
          -> Delayed (Array dim' e)
permuteOp f (DelayedArray dftsSh dftsPf) p (DelayedArray sh pf)
  = delay $ adata `seq` Array dftsSh adata
  where 
    f' = Sugar.sinkFromElt2 f
    --
    (adata, _) 
      = runArrayData $ do

            -- new array in target dimension
          arr <- newArrayData (size dftsSh)

            -- initialise it with the default values
          let write ix = writeArrayData arr (index dftsSh ix) (dftsPf ix)      
          iter dftsSh write (>>) (return ())

            -- traverse the source dimension and project each element into
            -- the target dimension (where it gets combined with the current
            -- default)
          let update ix = do
                            let target = (Sugar.sinkFromElt p) ix
                            unless (target == ignore) $ do
                              let i = index dftsSh target
                              e <- readArrayData arr i
                              writeArrayData arr i (pf ix `f'` e) 
          iter sh update (>>) (return ())
          
            -- return the updated array
          return (arr, undefined)

backpermuteOp :: Sugar.Shape dim'
              => dim'
              -> (dim' -> dim)
              -> Delayed (Array dim e)
              -> Delayed (Array dim' e)
backpermuteOp sh' p (DelayedArray _sh rf)
  = DelayedArray (Sugar.fromElt sh') (rf . Sugar.sinkFromElt p)

stencilOp :: forall dim e e' stencil. (Sugar.Elt e, Sugar.Elt e', Stencil dim e stencil)
          => (stencil -> e')
          -> Boundary (Sugar.EltRepr e)
          -> Delayed (Array dim e)
          -> Delayed (Array dim e')
stencilOp sten bndy (DelayedArray sh rf)
  = DelayedArray sh rf'
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
stencil2Op sten bndy1 (DelayedArray sh1 rf1) bndy2 (DelayedArray sh2 rf2)
  = DelayedArray (sh1 `intersect` sh2) rf'
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
evalOpenFun :: OpenFun env aenv t -> Val env -> Val aenv -> t
evalOpenFun (Body e) env aenv = evalOpenExp e env aenv
evalOpenFun (Lam f)  env aenv 
  = \x -> evalOpenFun f (env `Push` Sugar.fromElt x) aenv

-- Evaluate a closed function
--
evalFun :: Fun aenv t -> Val aenv -> t
evalFun f aenv = evalOpenFun f Empty aenv

-- Evaluate an open expression
--
-- NB: The implementation of 'IndexScalar' and 'Shape' demonstrate clearly why
--     array expressions must be hoisted out of scalar expressions before code
--     execution.  If these operations are in the body of a function that
--     gets mapped over an array, the array argument would be forced many times
--     leading to a large amount of wasteful recomputation.
--  
evalOpenExp :: OpenExp env aenv a -> Val env -> Val aenv -> a

evalOpenExp (Var idx) env _ = Sugar.toElt $ prj idx env
  
evalOpenExp (Const c) _ _ = Sugar.toElt c

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

evalOpenExp (Cond c t e) env aenv 
  = if evalOpenExp c env aenv
    then evalOpenExp t env aenv
    else evalOpenExp e env aenv

evalOpenExp (PrimConst c) _ _ = evalPrimConst c

evalOpenExp (PrimApp p arg) env aenv 
  = evalPrim p (evalOpenExp arg env aenv)

evalOpenExp (IndexScalar acc ix) env aenv 
  = case evalOpenAcc acc aenv of
      DelayedArray sh pf -> 
        let ix' = Sugar.fromElt $ evalOpenExp ix env aenv
        in
        index sh ix' `seq` (Sugar.toElt $ pf ix')
                              -- FIXME: This is ugly, but (possibly) needed to
                              --       ensure bounds checking

evalOpenExp (Shape acc) _ aenv 
  = case force $ evalOpenAcc acc aenv of
      Array sh _ -> Sugar.toElt sh

evalOpenExp (Size acc) _ aenv 
  = case force $ evalOpenAcc acc aenv of
      Array sh _ -> size sh

-- Evaluate a closed expression
--
evalExp :: Exp aenv t -> Val aenv -> t
evalExp e aenv = evalOpenExp e Empty aenv


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

evalTuple :: Tuple (OpenExp env aenv) t -> Val env -> Val aenv -> t
evalTuple NilTup            _env _aenv = ()
evalTuple (tup `SnocTup` e) env  aenv  = (evalTuple tup env aenv, 
                                          evalOpenExp e env aenv)

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

