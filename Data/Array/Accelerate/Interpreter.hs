{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
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
import Data.Bits
import Data.Char                                        ( chr, ord )
import Prelude                                          hiding ( sum )
import System.IO.Unsafe                                 ( unsafePerformIO )

-- friends
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Delayed
import Data.Array.Accelerate.Array.Representation       hiding ( sliceIndex, enumSlices, restrictSlice )
import Data.Array.Accelerate.Array.Sugar (
  Z(..), (:.)(..), Array(..), Arrays, Scalar, Vector, Segments, enumSlices, restrictSlice, Tuple(..), Atuple(..), CstProxy(..) )
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Trafo.Substitution
import qualified Data.Array.Accelerate.Trafo.Sharing    as Sharing
import qualified Data.Array.Accelerate.Smart            as Sugar
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar


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

evalPreOpenAcc (Atuple tup) aenv = delay (toTuple ArraysProxy $ evalAtuple tup aenv :: a)

evalPreOpenAcc (Aprj ix (tup :: OpenAcc aenv arrs)) aenv =
  let tup'  = force $ evalOpenAcc tup aenv :: arrs
  in  delay $ evalPrj ix (fromTuple ArraysProxy tup')

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

evalPreOpenAcc (Loop l) aenv = delay $ evalLoop defaultStreamConfig l aenv

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
    $boundsCheck "reshape" "shape mismatch" (Sugar.size newShape == size oldShape)
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
        $indexCheck "slice" i sz $ (sl', \ix -> (f' ix, i))

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
    DelayedRpair prefix _sum                                  = scanl'Op (+) 0 seg
    DelayedRpair DelayedRunit (DelayedRarray _shSeg rfStarts) = prefix
    arr = Sugar.newArray (Sugar.toElt (sh, Sugar.toElt shSeg)) foldOne
    --
    foldOne :: dim:.Int -> e
    foldOne ix =
      let
          (ix', i) = Sugar.fromElt ix
          start    = fromIntegral ((Sugar.liftToElt rfStarts) i :: i)
          len      = fromIntegral ((Sugar.liftToElt rfSeg)    i :: i)
      in
      if len == 0
         then $boundsError "fold1Seg" "empty iteration space"
         else fold ix' (Sugar.toElt . rf $ (ix', start)) (start + 1) (start + len)

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

    traverse :: MutableArrayData (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> IO (Sugar.EltRepr e)
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

    traverse :: MutableArrayData (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> IO (Sugar.EltRepr e)
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

    traverse :: MutableArrayData (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> IO ()
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

    traverse :: MutableArrayData (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> IO (Sugar.EltRepr e)
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

    traverse :: MutableArrayData (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> IO (Sugar.EltRepr e)
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

    traverse :: MutableArrayData (Sugar.EltRepr e) -> Int -> (Sugar.EltRepr e) -> IO ()
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

toStreamOp :: (Sugar.Elt slix, Sugar.Shape sl, Sugar.Shape dim, Sugar.Elt e)
           => SliceIndex (Sugar.EltRepr slix)
                         (Sugar.EltRepr sl)
                         co
                         (Sugar.EltRepr dim)
           -> slix
           -> Delayed (Array dim e)
           -> [Delayed (Array sl e)]
toStreamOp sliceIndex slix arr = map (sliceOp sliceIndex arr) (Sugar.enumSlices sliceIndex slix)

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
  = toTuple EltProxy $ evalTuple tup env aenv

evalOpenExp (Prj idx e) env aenv
  = evalPrj idx (fromTuple EltProxy $ evalOpenExp e env aenv)

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

evalOpenExp (Union sh1 sh2) env aenv
  = Sugar.union (evalOpenExp sh1 env aenv) (evalOpenExp sh2 env aenv)

evalOpenExp (Foreign _ f e) env aenv
  = evalOpenExp e' env aenv
  where
    wExp :: Idx ((),a) t -> Idx (env,a) t
    wExp ZeroIdx = ZeroIdx
    wExp _       = $internalError "wExp" "unreachable case"

    e' = case f of
           (Lam (Body b)) -> Let e $ weaken undefined (weakenE wExp b)
           _              -> $internalError "travE" "unreachable case"

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



-- Loop evaluation 
-- ---------------

-- Position in stream.
--
type StreamPos = Int

-- Configuration for stream evaluation.
--
data StreamConfig = StreamConfig 
  { chunkSize :: Int -- Allocation limit for a stream in words. Actaul
                     -- runtime allocation should be the maximum of
                     -- this size and the size of the largest element
                     -- in the stream.
  }

-- Default stream evaluation configuration for testing purposes.
--
defaultStreamConfig :: StreamConfig
defaultStreamConfig = StreamConfig { chunkSize = 2 }

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

elemsPerChunk :: StreamConfig -> Int -> Int
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
chunkShapes :: forall sh a. Sugar.Shape sh => Chunk (Array sh a) -> Vector sh
chunkShapes c = Sugar.fromList (Z :. clen c) (map Sugar.shape (elems c))

-- Get all the elements of a chunk of arrays. O(1).
--
chunkElems :: forall sh a. Sugar.Elt a => Chunk (Array sh a) -> Vector a
chunkElems c = 
  let xs = concatMap Sugar.toList (elems c)
  in Sugar.fromList (Z :. length xs) xs

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
-- fixed element-size streams.
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
-- element-size streams.
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

-- A window on a stream.
--
data Window a = Window 
  { chunk :: Chunk a   -- Current allocated chunk.
  , wpos  :: StreamPos -- Position of the window on the stream, given
                       -- in number of elements.
  }

-- The initial empty window.
--
window0 :: forall a. Window a
window0 = Window { chunk = emptyChunk, wpos = 0 }

-- Index the given window by the given index on the stream.
--
(!#) :: forall a. Window a -> StreamPos -> Chunk a
w !# i
  | j <- i - wpos w
  , j >= 0
  = cdrop j (chunk w)
  | otherwise = error "Window indexed before position."

-- Move the give window by supplying the next chunk.
--
moveWin :: forall a. Window a -> Chunk a -> Window a
moveWin w c = w { chunk = c
                , wpos = wpos w + clen (chunk w)
                }

-- A cursor on a stream.
--
data Cursor lenv a = Cursor 
  { ref  :: Idx lenv a -- Reference to the stream.
  , cpos :: StreamPos  -- Position of the cursor on the stream, given
                       -- in number of elements.
  }
  
-- Initial cursor.
--
cursor0 :: forall lenv a. Idx lenv a -> Cursor lenv a
cursor0 x = Cursor { ref = x, cpos = 0 }

-- Advance cursor by a relative amount.
--
moveCursor :: forall lenv a. Int -> Cursor lenv a -> Cursor lenv a
moveCursor k c = c { cpos = cpos c + k }

-- Valuation for an environment of stream windows.
--
data Val' lenv where
  Empty' :: Val' ()
  Push'  :: Val' lenv -> Window t -> Val' (lenv, t)  

-- Projection of a window from a window valuation using a de Bruijn
-- index.
--
prj' :: forall lenv t. Idx lenv t -> Val' lenv -> Window t
prj' ZeroIdx       (Push' _   v) = v
prj' (SuccIdx idx) (Push' val _) = prj' idx val
prj' _             _             = $internalError "prj" "inconsistent valuation"

-- Projection of a chunk from a window valuation using a stream
-- cursor.
--
prjChunk :: forall lenv a. Cursor lenv a -> Val' lenv -> Chunk a
prjChunk c lenv = prj' (ref c) lenv !# cpos c

-- An executable loop.
--
data ExecLoop lenv arrs where
  ExecEmpty :: ExecLoop lenv ()
  ExecP :: (Arrays a, Arrays arrs) => Window a -> ExecP      a -> ExecLoop (lenv, a) arrs -> ExecLoop lenv  arrs
  ExecT :: (Arrays a, Arrays arrs) => Window a -> ExecT lenv a -> ExecLoop (lenv, a) arrs -> ExecLoop lenv  arrs
  ExecC :: (Arrays a, Arrays arrs) =>             ExecC lenv a -> ExecLoop  lenv     arrs -> ExecLoop lenv (arrs, a)

-- An executable producer.
--
data ExecP a where
  ExecToStream :: Int -- Elements per chunk
               -> [Array sh e]
               -> ExecP (Array sh e)

  ExecUseLazy :: (Sugar.Elt slix, Sugar.Shape sl, Sugar.Elt e)
              => SliceIndex (Sugar.EltRepr slix)
                            (Sugar.EltRepr sl)
                            co
                            dim
              -> Int -- Elements per chunk
              -> [slix]
              -> (slix -> Array sl e)
              -> ExecP (Array sl e)

-- An executable transducer.
--
data ExecT lenv a where
  ExecMap :: (Chunk a -> Chunk b)
          -> Cursor lenv a
          -> ExecT lenv b

  ExecZipWith :: (Chunk a -> Chunk b -> Chunk c)
              -> Cursor lenv a
              -> Cursor lenv b
              -> ExecT lenv c

  ExecScanStream :: (a -> Chunk b -> (Chunk a, a))
                 -> Cursor lenv b
                 -> a
                 -> ExecT lenv a

-- An executable consumer.
--
data ExecC lenv a where
  ExecFoldStream' :: (acc -> Chunk b -> acc)
                  -> (acc -> res)
                  -> Cursor lenv b
                  -> acc
                  -> ExecC lenv res

minCursor :: ExecLoop lenv a -> StreamPos
minCursor l = travL l 0
  where
    travL :: ExecLoop lenv a -> Int -> StreamPos
    travL l i = 
      case l of
        ExecEmpty    -> maxBound
        ExecP _ _ l' -> travL l' (i+1)
        ExecT _ t l' -> travT t i `min` travL l' (i+1)
        ExecC   c l' -> travC c i `min` travL l' i

    k :: Cursor lenv a -> Int -> StreamPos
    k c i
      | i == idxToInt (ref c) = cpos c
      | otherwise             = maxBound

    travT :: ExecT lenv a -> Int -> StreamPos
    travT t i =
      case t of
        ExecMap _ c -> k c i
        ExecZipWith _ c1 c2 -> k c1 i `min` k c2 i
        ExecScanStream _ c _ -> k c i

    travC :: ExecC lenv a -> Int -> StreamPos
    travC c i =
      case c of
        ExecFoldStream' _ _ c _ -> k c i

evalLoop :: forall aenv arrs. 
            StreamConfig 
         -> PreOpenLoop OpenAcc aenv () arrs 
         -> Val aenv -> arrs
evalLoop conf l aenv | degenerate l = returnOut .        initLoop aenv $ l
                     | otherwise    = returnOut . loop . initLoop aenv $ l
  where
    -- A loop with no producers is degenerate since there is no
    -- halting condition, and should therefore not be iterated.
    -- Notice that the only degenerate closed loop is the empty loop.
    degenerate :: forall lenv arrs'. 
                  PreOpenLoop OpenAcc aenv lenv arrs'
               -> Bool
    degenerate l =
      case l of
        EmptyLoop    -> True
        Producer _ _ -> False
        Transducer _ l' -> degenerate l'
        Consumer   _ l' -> degenerate l'

    -- Initialize the producers and the accumulators of the consumers
    -- with the given array enviroment.
    initLoop :: forall lenv arrs'.
                Val aenv
             -> PreOpenLoop OpenAcc aenv lenv arrs'
             -> ExecLoop lenv arrs'
    initLoop aenv l =
      case l of
        EmptyLoop       -> ExecEmpty
        Producer   p l' -> ExecP window0 (initProducer   aenv p) (initLoop aenv l')
        Transducer t l' -> ExecT window0 (initTransducer aenv t) (initLoop aenv l')
        Consumer   c l' -> ExecC         (initConsumer   aenv c) (initLoop aenv l')

    -- Iterate the given loop until it terminates.
    -- A loop only terminates when one of the producers are exhausted.
    loop :: ExecLoop () arrs 
         -> ExecLoop () arrs
    loop l = 
      case step' l of
        Nothing -> l
        Just l' -> loop l'
      
      where 
        step' :: ExecLoop () arrs -> Maybe (ExecLoop () arrs)
        step' l = step l Empty'

    -- One iteration of a loop.
    step :: forall lenv arrs'.
            ExecLoop lenv arrs'
         -> Val' lenv
         -> Maybe (ExecLoop lenv arrs')
    step l lenv =
      case l of
        ExecEmpty -> return ExecEmpty
        ExecP w p l' -> checkWin w l' (ExecP w p) $       produce   p       >>= \ (a, p') -> step l' (lenv `Push'` moveWin w a) >>= \ l'' -> return (ExecP (moveWin w a) p' l'')
        ExecT w t l' -> checkWin w l' (ExecT w t) $ Just (transduce t lenv) >>= \ (a, t') -> step l' (lenv `Push'` moveWin w a) >>= \ l'' -> return (ExecT (moveWin w a) t' l'')
        ExecC   c l' ->                             Just (consume   c lenv) >>= \     c'  -> step l'  lenv                      >>= \ l'' -> return (ExecC c' l'')
      where
        checkWin :: forall a. -- TODO: Find a more elegant solution without using checkWin.
                    Window a
                 -> ExecLoop (lenv, a) arrs'
                 -> (ExecLoop (lenv, a) arrs' -> ExecLoop lenv arrs')
                 -> Maybe (ExecLoop lenv arrs')
                 -> Maybe (ExecLoop lenv arrs')
        checkWin w l' exec ml
          | null (elems (w !# minCursor l')) = ml
          | otherwise                        = step l' (lenv `Push'` w) >>= \ l'' -> Just (exec l'')

    -- Finalize and return the accumulators in the consumers of the loop.
    returnOut :: forall lenv arrs'. 
                 ExecLoop lenv arrs' -> arrs'
    returnOut l =
      case l of
        ExecEmpty -> ()
        ExecP _ _ l ->  returnOut l
        ExecT _ _ l ->  returnOut l
        ExecC   c l -> (returnOut l, readConsumer c)


    initProducer :: forall a.
                    Val aenv
                 -> Producer OpenAcc aenv a
                 -> ExecP a
    initProducer aenv p =
      case p of
        ToStream sliceIndex sl acc ->
          let arr = force $ evalOpenAcc acc aenv
              sl' = restrictSlice sliceIndex (Sugar.shape arr) (evalExp sl aenv)
              n   = size (sliceShape sliceIndex (Sugar.fromElt (Sugar.shape arr)))
              k   = elemsPerChunk conf n
          in ExecToStream k (map force (toStreamOp sliceIndex sl' (delay arr)))
        UseLazy sliceIndex sl arr ->
          let sl' = restrictSlice sliceIndex (Sugar.shape arr) (evalExp sl aenv)
              sls = enumSlices sliceIndex sl'
              n   = size (sliceShape sliceIndex (Sugar.fromElt (Sugar.shape arr)))
              k   = elemsPerChunk conf n
              f slix = force $ sliceOp sliceIndex (delay arr) slix
          in ExecUseLazy sliceIndex k sls f

    initTransducer :: forall a lenv.
                      Val aenv
                   -> Transducer OpenAcc aenv lenv a
                   -> ExecT lenv a
    initTransducer aenv t =
      case t of
        MapStream     f x       -> ExecMap     (mapChunk (evalOpenAfun f aenv)) (cursor0 x)
        ZipWithStream f x y     -> ExecZipWith (zipWithChunk (evalOpenAfun f aenv)) (cursor0 x) (cursor0 y)
        ScanStream    f acc x   -> initTransducer aenv (ScanStreamAct f f acc acc x)
        ScanStreamAct f g acc0 acc1 x -> ExecScanStream h (cursor0 x) a0
          where
            f' = evalOpenAfun f aenv
            g' = evalOpenAfun g aenv
            a0 = force (evalOpenAcc acc0 aenv)
            b0 = force (evalOpenAcc acc1 aenv)
            h a bs =
              let (bs', b) = scanReduceChunk g' b0 bs -- Scan and reduce chunk.
                  as = mapChunk (f' a) bs'            -- Add accumulator.
                  a' = f' a b                         -- Find new accumulator.
              in (as, a')

    initConsumer :: forall a lenv.
                    Val aenv
                 -> Consumer OpenAcc aenv lenv a
                 -> ExecC lenv a
    initConsumer aenv c =
      case c of
        FromStream x -> 
          ExecFoldStream' 
            appendChunk 
            (\ c -> (chunkShapes c, chunkElems c)) 
            (cursor0 x) 
            emptyChunk
        FoldStream f acc x ->
          let f' = evalOpenAfun f aenv
              a0 = force $ evalOpenAcc acc aenv
              k = elemsPerChunk conf (Sugar.size (Sugar.shape a0))
          in ExecFoldStream'
               (zipWithChunk' f')
               (reduceChunk f' a0)
               (cursor0 x)
               (replicateChunk k a0)
        FoldStreamAct f g acc1 acc2 x ->
          let f' = evalOpenAfun f aenv
              g' = evalOpenAfun g aenv
              a0 = force $ evalOpenAcc acc1 aenv
              b0 = force $ evalOpenAcc acc2 aenv
          in ExecFoldStream'
               (\ a bs -> f' a (reduceChunk g' b0 bs))
               id
               (cursor0 x)
               a0
        FoldStreamFlatten f acc x ->
          let f' = evalOpenAfun f aenv
              a0 = force $ evalOpenAcc acc aenv
          in ExecFoldStream' 
               (\ a bs -> f' a (chunkShapes bs) (chunkElems bs))
               id
               (cursor0 x)
               a0
        CollectStream f x -> 
          ExecFoldStream' (\ () xs -> unsafePerformIO $ mapM_ f (elems xs)) id (cursor0 x) ()

produce :: ExecP a -> Maybe (Chunk a, ExecP a)
produce p =
  case p of
    ExecToStream k xs ->
      if null xs
        then Nothing
        else 
          let (xs', xs'') = (take k xs, drop k xs)
          in Just ( Chunk { elems = xs' }
                  , ExecToStream k xs'')
    ExecUseLazy sliceIndex k slixs f ->
      if null slixs
        then Nothing
        else
          let (slixs', slixs'') = (take k slixs, drop k slixs)
          in Just ( Chunk { elems = map f slixs' }
                  , ExecUseLazy sliceIndex k slixs'' f)

transduce :: ExecT lenv a -> Val' lenv -> (Chunk a, ExecT lenv a)
transduce t lenv =
  case t of
    ExecMap f x -> 
      let c = prjChunk x lenv
      in (f c, ExecMap f (moveCursor (clen c) x))
    ExecZipWith f x y -> 
      let c1 = prjChunk x lenv
          c2 = prjChunk y lenv
          k = clen c1 `min` clen c2
      in (f c1 c2, ExecZipWith f (moveCursor k x) (moveCursor k y))
    ExecScanStream f x a   -> 
      let c = prjChunk x lenv
          (as, a') = f a c
      in  (as, ExecScanStream f (moveCursor (clen c) x) a')

consume :: ExecC lenv a -> Val' lenv -> ExecC lenv a
consume c lenv =
  case c of
    ExecFoldStream' f g x acc  ->
      let c = prjChunk x lenv
      in ExecFoldStream' f g (moveCursor (clen c) x) (f acc c)

readConsumer :: ExecC lenv a -> a
readConsumer c =
  case c of
    ExecFoldStream'   _ g _ acc -> g acc
