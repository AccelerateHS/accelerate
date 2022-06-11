{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK prune #-}
-- |
-- Module      : Data.Array.Accelerate.Interpreter
-- Description : Reference backend (interpreted)
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This interpreter is meant to be a reference implementation of the
-- semantics of the embedded array language. The emphasis is on defining
-- the semantics clearly, not on performance.
--

module Data.Array.Accelerate.Interpreter (

  Smart.Acc, Sugar.Arrays,
  Afunction, AfunctionR,

  -- * Interpret an array expression
  run, run1, runN,

  -- Internal (hidden)
  evalPrim, evalCoerceScalar, atraceOp,

) where

import Data.Array.Accelerate.AST                                    hiding ( Boundary(..) )
import Data.Array.Accelerate.AST.Environment
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Slice
import Data.Array.Accelerate.Representation.Stencil
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Trafo.Delayed                          ( DelayedOpenAfun, DelayedOpenAcc )
import Data.Array.Accelerate.Trafo.Sharing                          ( AfunctionR, AfunctionRepr(..), afunctionRepr )
import Data.Array.Accelerate.Type
import Data.Primitive.Bit                                           as Bit
import Data.Primitive.Vec                                           as Vec
import Data.Primitive.Vec                                           as Prim
import qualified Data.Array.Accelerate.AST                          as AST
import qualified Data.Array.Accelerate.Debug.Internal.Flags         as Debug
import qualified Data.Array.Accelerate.Debug.Internal.Graph         as Debug
import qualified Data.Array.Accelerate.Debug.Internal.Stats         as Debug
import qualified Data.Array.Accelerate.Debug.Internal.Timed         as Debug
import qualified Data.Array.Accelerate.Interpreter.Arithmetic       as A
import qualified Data.Array.Accelerate.Smart                        as Smart
import qualified Data.Array.Accelerate.Sugar.Array                  as Sugar
import qualified Data.Array.Accelerate.Sugar.Elt                    as Sugar
import qualified Data.Array.Accelerate.Trafo.Delayed                as AST

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Data.Text.Lazy.Builder
import Formatting
import System.IO
import System.IO.Unsafe                                             ( unsafePerformIO )
import Unsafe.Coerce
import Prelude                                                      hiding ( (!!), sum )
import qualified Data.Text.IO                                       as T

import GHC.TypeLits


-- Program execution
-- -----------------

-- | Run a complete embedded array program using the reference interpreter.
--
run :: (HasCallStack, Sugar.Arrays a) => Smart.Acc a -> a
run a = unsafePerformIO execute
  where
    !acc    = convertAcc a
    execute = do
      Debug.dumpGraph $!! acc
      Debug.dumpSimplStats
      res <- phase "execute" Debug.elapsed $ evaluate $ evalOpenAcc acc Empty
      return $ Sugar.toArr $ snd res

-- | This is 'runN' specialised to an array program of one argument.
--
run1 :: (HasCallStack, Sugar.Arrays a, Sugar.Arrays b) => (Smart.Acc a -> Smart.Acc b) -> a -> b
run1 = runN

-- | Prepare and execute an embedded array program.
--
runN :: forall f. (HasCallStack, Afunction f) => f -> AfunctionR f
runN f = go
  where
    !acc    = convertAfun f
    !afun   = unsafePerformIO $ do
                Debug.dumpGraph $!! acc
                Debug.dumpSimplStats
                return acc
    !go     = eval (afunctionRepr @f) afun Empty
    --
    eval :: AfunctionRepr g (AfunctionR g) (ArraysFunctionR g)
         -> DelayedOpenAfun aenv (ArraysFunctionR g)
         -> Val aenv
         -> AfunctionR g
    eval (AfunctionReprLam reprF) (Alam lhs f) aenv = \a -> eval reprF f $ aenv `push` (lhs, Sugar.fromArr a)
    eval AfunctionReprBody        (Abody b)    aenv = unsafePerformIO $ phase "execute" Debug.elapsed (Sugar.toArr . snd <$> evaluate (evalOpenAcc b aenv))
    eval _                        _aenv        _    = error "Two men say they're Jesus; one of them must be wrong"


-- Debugging
-- ---------

phase :: Builder -> Format Builder (Double -> Double -> Builder) -> IO a -> IO a
phase n fmt go = Debug.timed Debug.dump_phases (now ("phase " <> n <> ": ") % fmt) go


-- Delayed Arrays
-- --------------

-- Note that in contrast to the representation used in the optimised AST, the
-- delayed array representation used here is _only_ for delayed arrays --- we do
-- not require an optional Manifest|Delayed data type to evaluate the program.
--
data Delayed a where
  Delayed :: ArrayR (Array sh e)
          -> sh
          -> (sh -> e)
          -> (INT -> e)
          -> Delayed (Array sh e)


-- Array expression evaluation
-- ---------------------------

type WithReprs acc = (ArraysR acc, acc)

fromFunction' :: ArrayR (Array sh e) -> sh -> (sh -> e) -> WithReprs (Array sh e)
fromFunction' repr sh f = (TupRsingle repr, fromFunction repr sh f)

-- Evaluate an open array function
--
evalOpenAfun :: HasCallStack => DelayedOpenAfun aenv f -> Val aenv -> f
evalOpenAfun (Alam lhs f) aenv = \a -> evalOpenAfun f $ aenv `push` (lhs, a)
evalOpenAfun (Abody b)    aenv = snd $ evalOpenAcc b aenv


-- The core interpreter for optimised array programs
--
evalOpenAcc
    :: forall aenv a. HasCallStack
    => DelayedOpenAcc aenv a
    -> Val aenv
    -> WithReprs a
evalOpenAcc AST.Delayed{}       _    = internalError "expected manifest array"
evalOpenAcc (AST.Manifest pacc) aenv =
  let
      manifest :: forall a'. HasCallStack => DelayedOpenAcc aenv a' -> WithReprs a'
      manifest acc =
        let (repr, a') = evalOpenAcc acc aenv
        in  rnfArraysR repr a' `seq` (repr, a')

      delayed :: DelayedOpenAcc aenv (Array sh e) -> Delayed (Array sh e)
      delayed AST.Delayed{..} = Delayed reprD (evalE extentD) (evalF indexD) (evalF linearIndexD)
      delayed a' = Delayed aR (shape a) (indexArray aR a) (linearIndexArray (arrayRtype aR) a)
        where
          (TupRsingle aR, a) = manifest a'

      evalE :: Exp aenv t -> t
      evalE exp = evalExp exp aenv

      evalF :: Fun aenv f -> f
      evalF fun = evalFun fun aenv

      evalB :: AST.Boundary aenv t -> Boundary t
      evalB bnd = evalBoundary bnd aenv

      dir :: Direction -> t -> t -> t
      dir LeftToRight l _ = l
      dir RightToLeft _ r = r
  in
  case pacc of
    Avar (Var repr ix)            -> (TupRsingle repr, prj ix aenv)
    Alet lhs acc1 acc2            -> evalOpenAcc acc2 $ aenv `push` (lhs, snd $ manifest acc1)
    Apair acc1 acc2               -> let (r1, a1) = manifest acc1
                                         (r2, a2) = manifest acc2
                                     in
                                     (TupRpair r1 r2, (a1, a2))
    Anil                          -> (TupRunit, ())
    Atrace msg as bs              -> unsafePerformIO $ manifest bs <$ atraceOp msg (snd $ manifest as)
    Apply repr afun acc           -> (repr, evalOpenAfun afun aenv $ snd $ manifest acc)
    Aforeign repr _ afun acc      -> (repr, evalOpenAfun afun Empty $ snd $ manifest acc)
    Acond p acc1 acc2
      | toBool (evalE p)          -> manifest acc1
      | otherwise                 -> manifest acc2

    Awhile cond body acc          -> (repr, go initial)
      where
        (repr, initial) = manifest acc
        p               = evalOpenAfun cond aenv
        f               = evalOpenAfun body aenv
        go !x
          | toBool (linearIndexArray (Sugar.eltR @Bool) (p x) 0) = go (f x)
          | otherwise                                             = x

    Use repr arr                  -> (TupRsingle repr, arr)
    Unit tp e                     -> unitOp tp (evalE e)
    -- Collect s                     -> evalSeq defaultSeqConfig s aenv

    -- Producers
    -- ---------
    Map tp f acc                  -> mapOp tp (evalF f) (delayed acc)
    Generate repr sh f            -> generateOp repr (evalE sh) (evalF f)
    Transform repr sh p f acc     -> transformOp repr (evalE sh) (evalF p) (evalF f) (delayed acc)
    Backpermute shr sh p acc      -> backpermuteOp shr (evalE sh) (evalF p) (delayed acc)
    Reshape shr sh acc            -> reshapeOp shr (evalE sh) (manifest acc)

    ZipWith tp f acc1 acc2        -> zipWithOp tp (evalF f) (delayed acc1) (delayed acc2)
    Replicate slice slix acc      -> replicateOp slice (evalE slix) (manifest acc)
    Slice slice acc slix          -> sliceOp slice (manifest acc) (evalE slix)

    -- Consumers
    -- ---------
    Fold f (Just z) acc           -> foldOp (evalF f) (evalE z) (delayed acc)
    Fold f Nothing  acc           -> fold1Op (evalF f) (delayed acc)
    FoldSeg i f (Just z) acc seg  -> foldSegOp i (evalF f) (evalE z) (delayed acc) (delayed seg)
    FoldSeg i f Nothing acc seg   -> fold1SegOp i (evalF f) (delayed acc) (delayed seg)
    Scan  d f (Just z) acc        -> dir d scanlOp  scanrOp  (evalF f) (evalE z) (delayed acc)
    Scan  d f Nothing  acc        -> dir d scanl1Op scanr1Op (evalF f)           (delayed acc)
    Scan' d f z acc               -> dir d scanl'Op scanr'Op (evalF f) (evalE z) (delayed acc)
    Permute f def p acc           -> permuteOp (evalF f) (manifest def) (evalF p) (delayed acc)
    Stencil s tp sten b acc       -> stencilOp s tp (evalF sten) (evalB b) (delayed acc)
    Stencil2 s1 s2 tp sten b1 a1 b2 a2
                                  -> stencil2Op s1 s2 tp (evalF sten) (evalB b1) (delayed a1) (evalB b2) (delayed a2)


-- Array primitives
-- ----------------

unitOp :: TypeR e -> e -> WithReprs (Scalar e)
unitOp tp e = fromFunction' (ArrayR ShapeRz tp) () (const e)


generateOp
    :: ArrayR (Array sh e)
    -> sh
    -> (sh -> e)
    -> WithReprs (Array sh e)
generateOp = fromFunction'


transformOp
    :: ArrayR (Array sh' b)
    -> sh'
    -> (sh' -> sh)
    -> (a -> b)
    -> Delayed (Array sh a)
    -> WithReprs (Array sh' b)
transformOp repr sh' p f (Delayed _ _ xs _)
  = fromFunction' repr sh' (\ix -> f (xs $ p ix))


reshapeOp
    :: HasCallStack
    => ShapeR sh
    -> sh
    -> WithReprs (Array sh' e)
    -> WithReprs (Array sh  e)
reshapeOp newShapeR newShape (TupRsingle (ArrayR shr tp), (Array sh adata))
  = boundsCheck "shape mismatch" (size newShapeR newShape == size shr sh)
    ( TupRsingle (ArrayR newShapeR tp)
    , Array newShape adata
    )


replicateOp
    :: SliceIndex slix sl co sh
    -> slix
    -> WithReprs (Array sl e)
    -> WithReprs (Array sh e)
replicateOp slice slix (TupRsingle repr@(ArrayR _ tp), arr)
  = fromFunction' repr' sh (\ix -> (repr, arr) ! pf ix)
  where
    repr' = ArrayR (sliceDomainR slice) tp
    (sh, pf) = extend slice slix (shape arr)

    extend :: SliceIndex slix sl co dim
           -> slix
           -> sl
           -> (dim, dim -> sl)
    extend SliceNil              ()        ()
      = ((), const ())
    extend (SliceAll sliceIdx)   (slx, ()) (sl, sz)
      = let (dim', f') = extend sliceIdx slx sl
        in  ((dim', sz), \(ix, i) -> (f' ix, i))
    extend (SliceFixed sliceIdx) (slx, sz) sl
      = let (dim', f') = extend sliceIdx slx sl
        in  ((dim', sz), \(ix, _) -> f' ix)


sliceOp
    :: SliceIndex slix sl co sh
    -> WithReprs (Array sh e)
    -> slix
    -> WithReprs (Array sl e)
sliceOp slice (TupRsingle repr@(ArrayR _ tp), arr) slix
  = fromFunction' repr' sh' (\ix -> (repr, arr) ! pf ix)
  where
    repr' = ArrayR (sliceShapeR slice) tp
    (sh', pf) = restrict slice slix (shape arr)

    restrict
        :: HasCallStack
        => SliceIndex slix sl co sh
        -> slix
        -> sh
        -> (sl, sl -> sh)
    restrict SliceNil              ()        ()
      = ((), const ())
    restrict (SliceAll sliceIdx)   (slx, ()) (sl, sz)
      = let (sl', f') = restrict sliceIdx slx sl
        in  ((sl', sz), \(ix, i) -> (f' ix, i))
    restrict (SliceFixed sliceIdx) (slx, i)  (sl, sz)
      = let (sl', f') = restrict sliceIdx slx sl
        in  indexCheck i sz $ (sl', \ix -> (f' ix, i))


mapOp :: TypeR b
      -> (a -> b)
      -> Delayed   (Array sh a)
      -> WithReprs (Array sh b)
mapOp tp f (Delayed (ArrayR shr _) sh xs _)
  = fromFunction' (ArrayR shr tp) sh (\ix -> f (xs ix))


zipWithOp
    :: TypeR c
    -> (a -> b -> c)
    -> Delayed   (Array sh a)
    -> Delayed   (Array sh b)
    -> WithReprs (Array sh c)
zipWithOp tp f (Delayed (ArrayR shr _) shx xs _) (Delayed _ shy ys _)
  = fromFunction' (ArrayR shr tp) (intersect shr shx shy) (\ix -> f (xs ix) (ys ix))


foldOp
    :: (e -> e -> e)
    -> e
    -> Delayed (Array (sh, INT) e)
    -> WithReprs (Array sh e)
foldOp f z (Delayed (ArrayR (ShapeRsnoc shr) tp) (sh, n) arr _)
  = fromFunction' (ArrayR shr tp) sh (\ix -> iter (ShapeRsnoc ShapeRz) ((), n) (\((), i) -> arr (ix, i)) f z)


fold1Op
    :: HasCallStack
    => (e -> e -> e)
    -> Delayed (Array (sh, INT) e)
    -> WithReprs (Array sh e)
fold1Op f (Delayed (ArrayR (ShapeRsnoc shr) tp) (sh, n) arr _)
  = boundsCheck "empty array" (n > 0)
  $ fromFunction' (ArrayR shr tp) sh (\ix -> iter1 (ShapeRsnoc ShapeRz) ((), n) (\((), i) -> arr (ix, i)) f)


foldSegOp
    :: HasCallStack
    => SingleIntegralType i
    -> (e -> e -> e)
    -> e
    -> Delayed (Array (sh, INT) e)
    -> Delayed (Segments i)
    -> WithReprs (Array (sh, INT) e)
foldSegOp itp f z (Delayed repr (sh, _) arr _) (Delayed _ ((), n) _ seg)
  | IntegralDict <- integralDict itp
  = boundsCheck "empty segment descriptor" (n > 0)
  $ fromFunction' repr (sh, n-1)
  $ \(sz, ix) -> let start = fromIntegral $ seg ix
                     end   = fromIntegral $ seg (ix+1)
                 in
                 boundsCheck "empty segment" (end >= start)
                 $ iter (ShapeRsnoc ShapeRz) ((), end-start) (\((), i) -> arr (sz, start+i)) f z


fold1SegOp
    :: HasCallStack
    => SingleIntegralType i
    -> (e -> e -> e)
    -> Delayed (Array (sh, INT) e)
    -> Delayed (Segments i)
    -> WithReprs (Array (sh, INT) e)
fold1SegOp itp f (Delayed repr (sh, _) arr _) (Delayed _ ((), n) _ seg)
  | IntegralDict <- integralDict itp
  = boundsCheck "empty segment descriptor" (n > 0)
  $ fromFunction' repr (sh, n-1)
  $ \(sz, ix)   -> let start = fromIntegral $ seg ix
                       end   = fromIntegral $ seg (ix+1)
                   in
                   boundsCheck "empty segment" (end > start)
                   $ iter1 (ShapeRsnoc ShapeRz) ((), end-start) (\((), i) -> arr (sz, start+i)) f


scanl1Op
    :: forall sh e. HasCallStack
    => (e -> e -> e)
    -> Delayed (Array (sh, INT) e)
    -> WithReprs (Array (sh, INT) e)
scanl1Op f (Delayed (ArrayR shr tp) sh ain _)
  = ( TupRsingle $ ArrayR shr tp
    , adata `seq` Array sh adata
    )
  where
    --
    (adata, _)  = runArrayData @e $ do
      aout <- newArrayData tp (fromIntegral $ size shr sh)

      let write (sz, 0) = writeArrayData tp aout (fromIntegral $ toIndex shr sh (sz, 0)) (ain (sz, 0))
          write (sz, i) = do
            x <- readArrayData tp aout (fromIntegral $ toIndex shr sh (sz, i-1))
            let y = ain (sz, i)
            writeArrayData tp aout (fromIntegral $ toIndex shr sh (sz, i)) (f x y)

      iter shr sh write (>>) (return ())
      return (aout, undefined)


scanlOp
    :: forall sh e.
       (e -> e -> e)
    -> e
    -> Delayed (Array (sh, INT) e)
    -> WithReprs (Array (sh, INT) e)
scanlOp f z (Delayed (ArrayR shr tp) (sh, n) ain _)
  = ( TupRsingle $ ArrayR shr tp
    , adata `seq` Array sh' adata
    )
  where
    sh'         = (sh, n+1)
    --
    (adata, _)  = runArrayData @e $ do
      aout <- newArrayData tp (fromIntegral $ size shr sh')

      let write (sz, 0) = writeArrayData tp aout (fromIntegral $ toIndex shr sh' (sz, 0)) z
          write (sz, i) = do
            x <- readArrayData tp aout (fromIntegral $ toIndex shr sh' (sz, i-1))
            let y = ain (sz, i-1)
            writeArrayData tp aout (fromIntegral $ toIndex shr sh' (sz, i)) (f x y)

      iter shr sh' write (>>) (return ())
      return (aout, undefined)


scanl'Op
    :: forall sh e.
       (e -> e -> e)
    -> e
    -> Delayed (Array (sh, INT) e)
    -> WithReprs (Array (sh, INT) e, Array sh e)
scanl'Op f z (Delayed (ArrayR shr@(ShapeRsnoc shr') tp) (sh, n) ain _)
  = ( TupRsingle (ArrayR shr tp) `TupRpair` TupRsingle (ArrayR shr' tp)
    , aout `seq` asum `seq` ( Array (sh, n) aout, Array sh asum )
    )
  where
    ((aout, asum), _) = runArrayData @(e, e) $ do
      aout <- newArrayData tp (fromIntegral $ size shr  (sh, n))
      asum <- newArrayData tp (fromIntegral $ size shr' sh)

      let write (sz, 0)
            | n == 0    = writeArrayData tp asum (fromIntegral $ toIndex shr' sh sz) z
            | otherwise = writeArrayData tp aout (fromIntegral $ toIndex shr  (sh, n) (sz, 0)) z
          write (sz, i) = do
            x <- readArrayData tp aout (fromIntegral $ toIndex shr (sh, n) (sz, i-1))
            let y = ain (sz, i-1)
            if i == n
              then writeArrayData tp asum (fromIntegral $ toIndex shr' sh      sz)      (f x y)
              else writeArrayData tp aout (fromIntegral $ toIndex shr  (sh, n) (sz, i)) (f x y)

      iter shr (sh, n+1) write (>>) (return ())
      return ((aout, asum), undefined)


scanrOp
    :: forall sh e.
       (e -> e -> e)
    -> e
    -> Delayed (Array (sh, INT) e)
    -> WithReprs (Array (sh, INT) e)
scanrOp f z (Delayed (ArrayR shr tp) (sz, n) ain _)
  = ( TupRsingle (ArrayR shr tp)
    , adata `seq` Array sh' adata
    )
  where
    sh'         = (sz, n+1)
    --
    (adata, _)  = runArrayData @e $ do
      aout <- newArrayData tp (fromIntegral $ size shr sh')

      let write (sz, 0) = writeArrayData tp aout (fromIntegral $ toIndex shr sh' (sz, n)) z
          write (sz, i) = do
            let x = ain (sz, n-i)
            y <- readArrayData tp aout (fromIntegral $ toIndex shr sh' (sz, n-i+1))
            writeArrayData tp aout (fromIntegral $ toIndex shr sh' (sz, n-i)) (f x y)

      iter shr sh' write (>>) (return ())
      return (aout, undefined)


scanr1Op
    :: forall sh e. HasCallStack
    => (e -> e -> e)
    -> Delayed (Array (sh, INT) e)
    -> WithReprs (Array (sh, INT) e)
scanr1Op f (Delayed (ArrayR shr tp) sh@(_, n) ain _)
  = ( TupRsingle $ ArrayR shr tp
    , adata `seq` Array sh adata
    )
  where
    (adata, _)  = runArrayData @e $ do
      aout <- newArrayData tp (fromIntegral $ size shr sh)

      let write (sz, 0) = writeArrayData tp aout (fromIntegral $ toIndex shr sh (sz, n-1)) (ain (sz, n-1))
          write (sz, i) = do
            let x = ain (sz, n-i-1)
            y <- readArrayData tp aout (fromIntegral $ toIndex shr sh (sz, n-i))
            writeArrayData tp aout (fromIntegral $ toIndex shr sh (sz, n-i-1)) (f x y)

      iter shr sh write (>>) (return ())
      return (aout, undefined)


scanr'Op
    :: forall sh e.
       (e -> e -> e)
    -> e
    -> Delayed (Array (sh, INT) e)
    -> WithReprs (Array (sh, INT) e, Array sh e)
scanr'Op f z (Delayed (ArrayR shr@(ShapeRsnoc shr') tp) (sh, n) ain _)
  = ( TupRsingle (ArrayR shr tp) `TupRpair` TupRsingle (ArrayR shr' tp)
    , aout `seq` asum `seq` ( Array (sh, n) aout, Array sh asum )
    )
  where
    ((aout, asum), _) = runArrayData @(e, e) $ do
      aout <- newArrayData tp (fromIntegral $ size shr  (sh, n))
      asum <- newArrayData tp (fromIntegral $ size shr' sh)

      let write (sz, 0)
            | n == 0    = writeArrayData tp asum (fromIntegral $ toIndex shr' sh sz) z
            | otherwise = writeArrayData tp aout (fromIntegral $ toIndex shr  (sh, n) (sz, n-1)) z

          write (sz, i) = do
            let x = ain (sz, n-i)
            y <- readArrayData tp aout (fromIntegral $ toIndex shr (sh, n) (sz, n-i))
            if i == n
              then writeArrayData tp asum (fromIntegral $ toIndex shr' sh      sz)          (f x y)
              else writeArrayData tp aout (fromIntegral $ toIndex shr  (sh, n) (sz, n-i-1)) (f x y)

      iter shr (sh, n+1) write (>>) (return ())
      return ((aout, asum), undefined)


permuteOp
    :: forall sh sh' e. HasCallStack
    => (e -> e -> e)
    -> WithReprs (Array sh' e)
    -> (sh -> PrimMaybe sh')
    -> Delayed   (Array sh  e)
    -> WithReprs (Array sh' e)
permuteOp f (TupRsingle (ArrayR shr' _), def@(Array _ adef)) p (Delayed (ArrayR shr tp) sh _ ain)
  = (TupRsingle $ ArrayR shr' tp, adata `seq` Array sh' adata)
  where
    sh'         = shape def
    n'          = size shr' sh'
    --
    (adata, _)  = runArrayData @e $ do
      aout <- newArrayData tp (fromIntegral n')

      let -- initialise array with default values
          init i
            | i >= n'   = return ()
            | otherwise = do
                x <- readArrayData tp adef (fromIntegral i)
                writeArrayData tp aout (fromIntegral i) x
                init (i+1)

          -- project each element onto the destination array and update
          update src
            = case p src of
                (0,_)        -> return ()
                (1,((),dst)) -> do
                  let i = toIndex shr  sh  src
                      j = toIndex shr' sh' dst
                      x = ain i
                  --
                  y <- readArrayData tp aout (fromIntegral j)
                  writeArrayData tp aout (fromIntegral j) (f x y)
                _            -> internalError "unexpected tag"

      init 0
      iter shr sh update (>>) (return ())
      return (aout, undefined)


backpermuteOp
    :: ShapeR sh'
    -> sh'
    -> (sh' -> sh)
    -> Delayed (Array sh e)
    -> WithReprs (Array sh' e)
backpermuteOp shr sh' p (Delayed (ArrayR _ tp) _ arr _)
  = fromFunction' (ArrayR shr tp) sh' (\ix -> arr $ p ix)


stencilOp
    :: HasCallStack
    => StencilR sh a stencil
    -> TypeR b
    -> (stencil -> b)
    -> Boundary (Array sh a)
    -> Delayed  (Array sh a)
    -> WithReprs (Array sh b)
stencilOp stencil tp f bnd arr@(Delayed _ sh _ _)
  = fromFunction' (ArrayR shr tp) sh
  $ f . stencilAccess stencil (bounded shr bnd arr)
  where
    shr = stencilShapeR stencil


stencil2Op
    :: HasCallStack
    => StencilR sh a stencil1
    -> StencilR sh b stencil2
    -> TypeR c
    -> (stencil1 -> stencil2 -> c)
    -> Boundary (Array sh a)
    -> Delayed  (Array sh a)
    -> Boundary (Array sh b)
    -> Delayed  (Array sh b)
    -> WithReprs (Array sh c)
stencil2Op s1 s2 tp stencil bnd1 arr1@(Delayed _ sh1 _ _) bnd2 arr2@(Delayed _ sh2 _ _)
  = fromFunction' (ArrayR shr tp) (intersect shr sh1 sh2) f
  where
    f ix  = stencil (stencilAccess s1 (bounded shr bnd1 arr1) ix)
                    (stencilAccess s2 (bounded shr bnd2 arr2) ix)
    shr = stencilShapeR s1

stencilAccess
    :: StencilR sh e stencil
    -> (sh -> e)
    -> sh
    -> stencil
stencilAccess stencil = goR (stencilShapeR stencil) stencil
  where
    -- Base cases, nothing interesting to do here since we know the lower
    -- dimension is Z.
    --
    goR :: ShapeR sh -> StencilR sh e stencil -> (sh -> e) -> sh -> stencil
    goR _ (StencilRunit3 _) rf ix =
      let
          (z, i) = ix
          rf' d  = rf (z, i+d)
      in
      ((( ()
      , rf' (-1))
      , rf'   0 )
      , rf'   1 )

    goR _ (StencilRunit5 _) rf ix =
      let (z, i) = ix
          rf' d  = rf (z, i+d)
      in
      ((((( ()
      , rf' (-2))
      , rf' (-1))
      , rf'   0 )
      , rf'   1 )
      , rf'   2 )

    goR _ (StencilRunit7 _) rf ix =
      let (z, i) = ix
          rf' d  = rf (z, i+d)
      in
      ((((((( ()
      , rf' (-3))
      , rf' (-2))
      , rf' (-1))
      , rf'   0 )
      , rf'   1 )
      , rf'   2 )
      , rf'   3 )

    goR _ (StencilRunit9 _) rf ix =
      let (z, i) = ix
          rf' d  = rf (z, i+d)
      in
      ((((((((( ()
      , rf' (-4))
      , rf' (-3))
      , rf' (-2))
      , rf' (-1))
      , rf'   0 )
      , rf'   1 )
      , rf'   2 )
      , rf'   3 )
      , rf'   4 )

    -- Recursive cases. Note that because the stencil pattern is defined with
    -- cons ordering, whereas shapes (and indices) are defined as a snoc-list,
    -- when we recurse on the stencil structure we must manipulate the
    -- _left-most_ index component.
    --
    goR (ShapeRsnoc shr) (StencilRtup3 s1 s2 s3) rf ix =
      let (i, ix') = uncons shr ix
          rf' d ds = rf (cons shr (i+d) ds)
      in
      ((( ()
      , goR shr s1 (rf' (-1)) ix')
      , goR shr s2 (rf'   0)  ix')
      , goR shr s3 (rf'   1)  ix')

    goR (ShapeRsnoc shr) (StencilRtup5 s1 s2 s3 s4 s5) rf ix =
      let (i, ix') = uncons shr ix
          rf' d ds = rf (cons shr (i+d) ds)
      in
      ((((( ()
      , goR shr s1 (rf' (-2)) ix')
      , goR shr s2 (rf' (-1)) ix')
      , goR shr s3 (rf'   0)  ix')
      , goR shr s4 (rf'   1)  ix')
      , goR shr s5 (rf'   2)  ix')

    goR (ShapeRsnoc shr) (StencilRtup7 s1 s2 s3 s4 s5 s6 s7) rf ix =
      let (i, ix') = uncons shr ix
          rf' d ds = rf (cons shr (i+d) ds)
      in
      ((((((( ()
      , goR shr s1 (rf' (-3)) ix')
      , goR shr s2 (rf' (-2)) ix')
      , goR shr s3 (rf' (-1)) ix')
      , goR shr s4 (rf'   0)  ix')
      , goR shr s5 (rf'   1)  ix')
      , goR shr s6 (rf'   2)  ix')
      , goR shr s7 (rf'   3)  ix')

    goR (ShapeRsnoc shr) (StencilRtup9 s1 s2 s3 s4 s5 s6 s7 s8 s9) rf ix =
      let (i, ix') = uncons shr ix
          rf' d ds = rf (cons shr (i+d) ds)
      in
      ((((((((( ()
      , goR shr s1 (rf' (-4)) ix')
      , goR shr s2 (rf' (-3)) ix')
      , goR shr s3 (rf' (-2)) ix')
      , goR shr s4 (rf' (-1)) ix')
      , goR shr s5 (rf'   0)  ix')
      , goR shr s6 (rf'   1)  ix')
      , goR shr s7 (rf'   2)  ix')
      , goR shr s8 (rf'   3)  ix')
      , goR shr s9 (rf'   4)  ix')

    -- Add a left-most component to an index
    --
    cons :: ShapeR sh -> INT -> sh -> (sh, INT)
    cons ShapeRz          ix ()       = ((), ix)
    cons (ShapeRsnoc shr) ix (sh, sz) = (cons shr ix sh, sz)

    -- Remove the left-most index of an index, and return the remainder
    --
    uncons :: ShapeR sh -> (sh, INT) -> (INT, sh)
    uncons ShapeRz          ((), v)  = (v, ())
    uncons (ShapeRsnoc shr) (v1, v2) = let (i, v1') = uncons shr v1
                                       in  (i, (v1', v2))


bounded
    :: HasCallStack
    => ShapeR sh
    -> Boundary (Array sh e)
    -> Delayed (Array sh e)
    -> sh
    -> e
bounded shr bnd (Delayed _ sh f _) ix =
  if inside shr sh ix
    then f ix
    else
      case bnd of
        Function g -> g ix
        Constant v -> v
        _          -> f (bound shr sh ix)

  where
    -- Whether the index (second argument) is inside the bounds of the given
    -- shape (first argument).
    --
    inside :: ShapeR sh -> sh -> sh -> Bool
    inside ShapeRz          ()       ()       = True
    inside (ShapeRsnoc shr) (sh, sz) (ih, iz) = iz >= 0 && iz < sz && inside shr sh ih

    -- Return the index (second argument), updated to obey the given boundary
    -- conditions when outside the bounds of the given shape (first argument)
    --
    bound :: HasCallStack => ShapeR sh -> sh -> sh -> sh
    bound ShapeRz () () = ()
    bound (ShapeRsnoc shr) (sh, sz) (ih, iz) = (bound shr sh ih, ih')
      where
        ih'
          | iz < 0 = case bnd of
                          Clamp  -> 0
                          Mirror -> -iz
                          Wrap   -> sz + iz
                          _      -> internalError "unexpected boundary condition"
          | iz >= sz  = case bnd of
                          Clamp  -> sz - 1
                          Mirror -> sz - (iz - sz + 2)
                          Wrap   -> iz - sz
                          _      -> internalError "unexpected boundary condition"
          | otherwise = iz


-- Stencil boundary conditions
-- ---------------------------

data Boundary t where
  Clamp    :: Boundary t
  Mirror   :: Boundary t
  Wrap     :: Boundary t
  Constant :: t -> Boundary (Array sh t)
  Function :: (sh -> e) -> Boundary (Array sh e)


evalBoundary :: HasCallStack => AST.Boundary aenv t -> Val aenv -> Boundary t
evalBoundary bnd aenv =
  case bnd of
    AST.Clamp      -> Clamp
    AST.Mirror     -> Mirror
    AST.Wrap       -> Wrap
    AST.Constant v -> Constant v
    AST.Function f -> Function (evalFun f aenv)

atraceOp :: Message as -> as -> IO ()
atraceOp (Message show _ msg) as =
  let str = show as
   in do
     if null str
        then T.hPutStrLn stderr msg
        else hprint stderr (stext % ": " % string % "\n") msg str
     hFlush stderr


-- Scalar expression evaluation
-- ----------------------------

-- Evaluate a closed scalar expression
--
evalExp :: HasCallStack => Exp aenv t -> Val aenv -> t
evalExp e aenv = evalOpenExp e Empty aenv

-- Evaluate a closed scalar function
--
evalFun :: HasCallStack => Fun aenv t -> Val aenv -> t
evalFun f aenv = evalOpenFun f Empty aenv

-- Evaluate an open scalar function
--
evalOpenFun :: HasCallStack => OpenFun env aenv t -> Val env -> Val aenv -> t
evalOpenFun (Body e)    env aenv = evalOpenExp e env aenv
evalOpenFun (Lam lhs f) env aenv =
  \x -> evalOpenFun f (env `push` (lhs, x)) aenv


-- Evaluate an open scalar expression
--
-- NB: The implementation of 'Index' and 'Shape' demonstrate clearly why
--     array expressions must be hoisted out of scalar expressions before code
--     execution. If these operations are in the body of a function that gets
--     mapped over an array, the array argument would be evaluated many times
--     leading to a large amount of wasteful recomputation.
--
evalOpenExp
    :: forall env aenv t. HasCallStack
    => OpenExp env aenv t
    -> Val env
    -> Val aenv
    -> t
evalOpenExp pexp env aenv =
  let
      evalE :: OpenExp env aenv t' -> t'
      evalE e = evalOpenExp e env aenv

      evalF :: OpenFun env aenv f' -> f'
      evalF f = evalOpenFun f env aenv

      evalA :: ArrayVar aenv a -> WithReprs a
      evalA (Var repr ix) = (TupRsingle repr, prj ix aenv)
  in
  case pexp of
    Let lhs exp1 exp2           -> let !v1  = evalE exp1
                                       env' = env `push` (lhs, v1)
                                   in  evalOpenExp exp2 env' aenv
    Evar (Var _ ix)             -> prj ix env
    Const _ c                   -> c
    Undef tp                    -> undefElt (TupRsingle tp)
    PrimApp f x                 -> evalPrim f (evalE x)
    Nil                         -> ()
    Pair e1 e2                  -> let !x1 = evalE e1
                                       !x2 = evalE e2
                                   in  (x1, x2)
    Extract vR iR v i           -> evalExtract vR iR (evalE v) (evalE i)
    Insert vR iR v i x          -> evalInsert vR iR (evalE v) (evalE i) (evalE x)
    Shuffle rR iR x y i         -> let TupRsingle eR = expType x
                                    in evalShuffle eR rR iR (evalE x) (evalE y) (evalE i)
    Select m x y                -> let TupRsingle eR = expType x
                                    in evalSelect eR (evalE m) (evalE x) (evalE y)
    IndexSlice slice slix sh    -> restrict slice (evalE slix) (evalE sh)
      where
        restrict :: SliceIndex slix sl co sh -> slix -> sh -> sl
        restrict SliceNil              ()        ()         = ()
        restrict (SliceAll sliceIdx)   (slx, ()) (sl, sz)   =
          let sl' = restrict sliceIdx slx sl
          in  (sl', sz)
        restrict (SliceFixed sliceIdx) (slx, _i)  (sl, _sz) =
          restrict sliceIdx slx sl

    IndexFull slice slix sh     -> extend slice (evalE slix) (evalE sh)
      where
        extend :: SliceIndex slix sl co sh -> slix -> sl -> sh
        extend SliceNil              ()        ()       = ()
        extend (SliceAll sliceIdx)   (slx, ()) (sl, sz) =
          let sh' = extend sliceIdx slx sl
          in  (sh', sz)
        extend (SliceFixed sliceIdx) (slx, sz) sl       =
          let sh' = extend sliceIdx slx sl
          in  (sh', sz)

    ToIndex shr sh ix           -> toIndex shr (evalE sh) (evalE ix)
    FromIndex shr sh ix         -> fromIndex shr (evalE sh) (evalE ix)
    Case _ e rhs def            -> evalE (caseof (evalE e) rhs)
      where
        caseof :: TAG -> [(TAG, OpenExp env aenv t)] -> OpenExp env aenv t
        caseof tag = go
          where
            go ((t,c):cs)
              | tag == t  = c
              | otherwise = go cs
            go []
              | Just d <- def = d
              | otherwise     = internalError "unmatched case"

    Cond c t e
      | toBool (evalE c)        -> evalE t
      | otherwise               -> evalE e

    While cond body seed        -> go (evalE seed)
      where
        f       = evalF body
        p       = evalF cond
        go !x
          | toBool (p x) = go (f x)
          | otherwise    = x

    Index acc ix                -> let (TupRsingle repr, a) = evalA acc
                                   in (repr, a) ! evalE ix
    LinearIndex acc i           -> let (TupRsingle repr, a) = evalA acc
                                       ix   = fromIndex (arrayRshape repr) (shape a) (evalE i)
                                   in (repr, a) ! ix
    Shape acc                   -> shape $ snd $ evalA acc
    ShapeSize shr sh            -> size shr (evalE sh)
    Foreign _ _ f e             -> evalOpenFun f Empty Empty $ evalE e
    Coerce t1 t2 e              -> evalCoerceScalar t1 t2 (evalE e)


-- Coercions
-- ---------

-- Coercion between two scalar types. We require that the size of the source and
-- destination values are equal (this is not checked at this point).
--
evalCoerceScalar :: ScalarType a -> ScalarType b -> a -> b
evalCoerceScalar = scalar
  where
    scalar :: ScalarType a -> ScalarType b -> a -> b
    scalar (NumScalarType a) = num a
    scalar (BitScalarType a) = bit a

    bit :: BitType a -> ScalarType b -> a -> b
    bit TypeBit = \case
      BitScalarType TypeBit -> id
      _                     -> internalError "evalCoerceScalar @Bit"
    bit (TypeMask _) = \case
      NumScalarType b -> num' b
      BitScalarType b -> bit' b
      where
        bit' :: BitType b -> Vec n Bit -> b
        bit' TypeMask{} = unsafeCoerce
        bit' TypeBit    = internalError "evalCoerceScalar @Bit"

        num' :: NumType b -> Vec n Bit -> b
        num' (IntegralNumType b) = integral' b
        num' (FloatingNumType b) = floating' b

        integral' :: IntegralType b -> Vec n Bit -> b
        integral' (VectorIntegralType _ _) = unsafeCoerce
        integral' (SingleIntegralType   b)
          | IntegralDict <- integralDict b
          = peek

        floating' :: FloatingType b -> Vec n Bit -> b
        floating' (VectorFloatingType _ _) = unsafeCoerce
        floating' (SingleFloatingType   b)
          | FloatingDict <- floatingDict b
          = peek

    num :: NumType a -> ScalarType b -> a -> b
    num (IntegralNumType a) = integral a
    num (FloatingNumType t) = floating t

    integral :: IntegralType a -> ScalarType b -> a -> b
    integral (SingleIntegralType a) = \case
      NumScalarType b -> num' b a
      BitScalarType b -> bit' b a
      where
        bit' :: BitType b -> SingleIntegralType a -> a -> b
        bit' TypeBit    _ = unsafeCoerce
        bit' TypeMask{} a
          | IntegralDict <- integralDict a
          = poke

        num' :: NumType b -> SingleIntegralType a -> a -> b
        num' (IntegralNumType b) = integral' b
        num' (FloatingNumType b) = floating' b

        integral' :: IntegralType b -> SingleIntegralType a -> a -> b
        integral' (SingleIntegralType   _) _ = unsafeCoerce
        integral' (VectorIntegralType _ b) a
          | IntegralDict <- integralDict a
          = case b of
              TypeInt8    -> poke
              TypeInt16   -> poke
              TypeInt32   -> poke
              TypeInt64   -> poke
              TypeInt128  -> poke
              TypeWord8   -> poke
              TypeWord16  -> poke
              TypeWord32  -> poke
              TypeWord64  -> poke
              TypeWord128 -> poke

        floating' :: FloatingType b -> SingleIntegralType a -> a -> b
        floating' (SingleFloatingType   _) _ = unsafeCoerce
        floating' (VectorFloatingType _ b) a
          | IntegralDict <- integralDict a
          = case b of
              TypeFloat16  -> poke
              TypeFloat32  -> poke
              TypeFloat64  -> poke
              TypeFloat128 -> poke

    integral (VectorIntegralType _ a) = \case
      NumScalarType b -> num' b a
      BitScalarType b -> bit' b a
      where
        bit' :: BitType b -> SingleIntegralType a -> Vec n a -> b
        bit' TypeBit    _ = unsafeCoerce
        bit' TypeMask{} _ = unsafeCoerce

        num' :: NumType b -> SingleIntegralType a -> Vec n a -> b
        num' (IntegralNumType b) = integral' b
        num' (FloatingNumType b) = floating' b

        integral' :: IntegralType b -> SingleIntegralType a -> Vec n a -> b
        integral' (VectorIntegralType _ _) _ = unsafeCoerce
        integral' (SingleIntegralType   b) _
          | IntegralDict <- integralDict b
          = peek

        floating' :: FloatingType b -> SingleIntegralType a -> Vec n a -> b
        floating' (VectorFloatingType _ _) _ = unsafeCoerce
        floating' (SingleFloatingType   b) _
          | FloatingDict <- floatingDict b
          = peek

    floating :: FloatingType a -> ScalarType b -> a -> b
    floating (SingleFloatingType a) = \case
      NumScalarType b -> num' b a
      BitScalarType b -> bit' b a
      where
        bit' :: BitType b -> SingleFloatingType a -> a -> b
        bit' TypeBit    _ = unsafeCoerce
        bit' TypeMask{} _ = unsafeCoerce

        num' :: NumType b -> SingleFloatingType a -> a -> b
        num' (IntegralNumType b) = integral' b
        num' (FloatingNumType b) = floating' b

        integral' :: IntegralType b -> SingleFloatingType a -> a -> b
        integral' (SingleIntegralType   _) _ = unsafeCoerce
        integral' (VectorIntegralType _ b) a
          | FloatingDict <- floatingDict a
          = case b of
              TypeInt8    -> poke
              TypeInt16   -> poke
              TypeInt32   -> poke
              TypeInt64   -> poke
              TypeInt128  -> poke
              TypeWord8   -> poke
              TypeWord16  -> poke
              TypeWord32  -> poke
              TypeWord64  -> poke
              TypeWord128 -> poke

        floating' :: FloatingType b -> SingleFloatingType a -> a -> b
        floating' (SingleFloatingType   _) _ = unsafeCoerce
        floating' (VectorFloatingType _ b) a
          | FloatingDict <- floatingDict a
          = case b of
              TypeFloat16  -> poke
              TypeFloat32  -> poke
              TypeFloat64  -> poke
              TypeFloat128 -> poke

    floating (VectorFloatingType _ a) = \case
      NumScalarType b -> num' b a
      BitScalarType b -> bit' b a
      where
        bit' :: BitType b -> SingleFloatingType a -> Vec n a -> b
        bit' TypeBit    _ = unsafeCoerce
        bit' TypeMask{} _ = unsafeCoerce

        num' :: NumType b -> SingleFloatingType a -> Vec n a -> b
        num' (IntegralNumType b) = integral' b
        num' (FloatingNumType b) = floating' b

        integral' :: IntegralType b -> SingleFloatingType a -> Vec n a -> b
        integral' (VectorIntegralType _ _) _ = unsafeCoerce
        integral' (SingleIntegralType   b) _
          | IntegralDict <- integralDict b
          = peek

        floating' :: FloatingType b -> SingleFloatingType a -> Vec n a -> b
        floating' (VectorFloatingType _ _) _ = unsafeCoerce
        floating' (SingleFloatingType   b) _
          | FloatingDict <- floatingDict b
          = peek

    {-# INLINE poke #-}
    poke :: forall a b n. Prim a => a -> Vec n b
    poke x = runST $ do
      mba <- newByteArray (sizeOf (undefined::a))
      writeByteArray mba 0 x
      ByteArray ba# <- unsafeFreezeByteArray mba
      return $ Vec ba#

    {-# INLINE peek #-}
    peek :: Prim b => Vec n a -> b
    peek (Vec ba#) = indexByteArray (ByteArray ba#) 0

{--
evalCoerceScalar SingleScalarType{}    SingleScalarType{} a = unsafeCoerce a
evalCoerceScalar VectorScalarType{}    VectorScalarType{} a = unsafeCoerce a  -- XXX: or just unpack/repack the (Vec ba#)
evalCoerceScalar (SingleScalarType ta) VectorScalarType{} a = vector ta a
  where
    vector :: SingleType a -> a -> Vec n b
    vector (NumSingleType t) = num t

    num :: NumType a -> a -> Vec n b
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType a -> a -> Vec n b
    integral TypeInt{}     = poke
    integral TypeInt8{}    = poke
    integral TypeInt16{}   = poke
    integral TypeInt32{}   = poke
    integral TypeInt64{}   = poke
    integral TypeWord{}    = poke
    integral TypeWord8{}   = poke
    integral TypeWord16{}  = poke
    integral TypeWord32{}  = poke
    integral TypeWord64{}  = poke

    floating :: FloatingType a -> a -> Vec n b
    floating TypeHalf{}    = poke
    floating TypeFloat{}   = poke
    floating TypeDouble{}  = poke

    {-# INLINE poke #-}
    poke :: forall a b n. Prim a => a -> Vec n b
    poke x = runST $ do
      mba <- newByteArray (sizeOf (undefined::a))
      writeByteArray mba 0 x
      ByteArray ba# <- unsafeFreezeByteArray mba
      return $ Vec ba#

evalCoerceScalar VectorScalarType{} (SingleScalarType tb) a = scalar tb a
  where
    scalar :: SingleType b -> Vec n a -> b
    scalar (NumSingleType t) = num t

    num :: NumType b -> Vec n a -> b
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType b -> Vec n a -> b
    integral TypeInt{}     = peek
    integral TypeInt8{}    = peek
    integral TypeInt16{}   = peek
    integral TypeInt32{}   = peek
    integral TypeInt64{}   = peek
    integral TypeWord{}    = peek
    integral TypeWord8{}   = peek
    integral TypeWord16{}  = peek
    integral TypeWord32{}  = peek
    integral TypeWord64{}  = peek

    floating :: FloatingType b -> Vec n a -> b
    floating TypeHalf{}    = peek
    floating TypeFloat{}   = peek
    floating TypeDouble{}  = peek

    {-# INLINE peek #-}
    peek :: Prim a => Vec n b -> a
    peek (Vec ba#) = indexByteArray (ByteArray ba#) 0
--}

-- Scalar primitives
-- -----------------

evalPrim :: PrimFun (a -> r) -> (a -> r)
evalPrim (PrimAdd t)                = A.add t
evalPrim (PrimSub t)                = A.sub t
evalPrim (PrimMul t)                = A.mul t
evalPrim (PrimNeg t)                = A.negate t
evalPrim (PrimAbs t)                = A.abs t
evalPrim (PrimSig t)                = A.signum t
evalPrim (PrimQuot t)               = A.quot t
evalPrim (PrimRem t)                = A.rem t
evalPrim (PrimQuotRem t)            = A.quotRem t
evalPrim (PrimIDiv t)               = A.div t
evalPrim (PrimMod t)                = A.mod t
evalPrim (PrimDivMod t)             = A.divMod t
evalPrim (PrimBAnd t)               = A.band t
evalPrim (PrimBOr t)                = A.bor t
evalPrim (PrimBXor t)               = A.xor t
evalPrim (PrimBNot t)               = A.complement t
evalPrim (PrimBShiftL t)            = A.shiftL t
evalPrim (PrimBShiftR t)            = A.shiftR t
evalPrim (PrimBRotateL t)           = A.rotateL t
evalPrim (PrimBRotateR t)           = A.rotateR t
evalPrim (PrimPopCount t)           = A.popCount t
evalPrim (PrimCountLeadingZeros t)  = A.countLeadingZeros t
evalPrim (PrimCountTrailingZeros t) = A.countTrailingZeros t
evalPrim (PrimFDiv t)               = A.fdiv t
evalPrim (PrimRecip t)              = A.recip t
evalPrim (PrimSin t)                = A.sin t
evalPrim (PrimCos t)                = A.cos t
evalPrim (PrimTan t)                = A.tan t
evalPrim (PrimAsin t)               = A.asin t
evalPrim (PrimAcos t)               = A.acos t
evalPrim (PrimAtan t)               = A.atan t
evalPrim (PrimSinh t)               = A.sinh t
evalPrim (PrimCosh t)               = A.cosh t
evalPrim (PrimTanh t)               = A.tanh t
evalPrim (PrimAsinh t)              = A.asinh t
evalPrim (PrimAcosh t)              = A.acosh t
evalPrim (PrimAtanh t)              = A.atanh t
evalPrim (PrimExpFloating t)        = A.exp t
evalPrim (PrimSqrt t)               = A.sqrt t
evalPrim (PrimLog t)                = A.log t
evalPrim (PrimFPow t)               = A.pow t
evalPrim (PrimLogBase t)            = A.logBase t
evalPrim (PrimTruncate ta tb)       = A.truncate ta tb
evalPrim (PrimRound ta tb)          = A.round ta tb
evalPrim (PrimFloor ta tb)          = A.floor ta tb
evalPrim (PrimCeiling ta tb)        = A.ceiling ta tb
evalPrim (PrimAtan2 t)              = A.atan2 t
evalPrim (PrimIsNaN t)              = A.isNaN t
evalPrim (PrimIsInfinite t)         = A.isInfinite t
evalPrim (PrimLt t)                 = A.lt t
evalPrim (PrimGt t)                 = A.gt t
evalPrim (PrimLtEq t)               = A.lte t
evalPrim (PrimGtEq t)               = A.gte t
evalPrim (PrimEq t)                 = A.eq t
evalPrim (PrimNEq t)                = A.neq t
evalPrim (PrimMax t)                = A.max t
evalPrim (PrimMin t)                = A.min t
evalPrim (PrimLAnd t)               = A.land t
evalPrim (PrimLOr t)                = A.lor t
evalPrim (PrimLNot t)               = A.lnot t
evalPrim (PrimFromIntegral ta tb)   = A.fromIntegral ta tb
evalPrim (PrimToFloating ta tb)     = A.toFloating ta tb
evalPrim (PrimToBool i b)           = A.toBool i b
evalPrim (PrimFromBool b i)         = A.fromBool b i


-- Vector primitives
-- -----------------

evalExtract :: ScalarType (Prim.Vec n a) -> SingleIntegralType i -> Prim.Vec n a -> i -> a
evalExtract vR iR v i = scalar vR v
  where
    scalar :: ScalarType (Prim.Vec n t) -> Prim.Vec n t -> t
    scalar (NumScalarType t) = num t
    scalar (BitScalarType t) = bit t

    num :: NumType (Prim.Vec n t) -> Prim.Vec n t -> t
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    bit :: BitType (Prim.Vec n t) -> Prim.Vec n t -> t
    bit TypeMask{} v
      | IntegralDict <- integralDict iR
      = Bit.extract (BitMask v) (fromIntegral i)

    integral :: IntegralType (Prim.Vec n t) -> Prim.Vec n t -> t
    integral (SingleIntegralType   tR) _ = case tR of
    integral (VectorIntegralType _ tR) v
      | IntegralDict <- integralDict tR
      , IntegralDict <- integralDict iR
      = Vec.extract v (fromIntegral i)

    floating :: FloatingType (Prim.Vec n t) -> Prim.Vec n t -> t
    floating (SingleFloatingType   tR) _ = case tR of
    floating (VectorFloatingType _ tR) v
      | FloatingDict <- floatingDict tR
      , IntegralDict <- integralDict iR
      = Vec.extract v (fromIntegral i)

evalInsert
    :: ScalarType (Prim.Vec n a)
    -> SingleIntegralType i
    -> Prim.Vec n a
    -> i
    -> a
    -> Prim.Vec n a
evalInsert vR iR v i x = scalar vR v x
  where
    scalar :: ScalarType (Prim.Vec n t) -> Prim.Vec n t -> t -> Prim.Vec n t
    scalar (NumScalarType t) = num t
    scalar (BitScalarType t) = bit t

    num :: NumType (Prim.Vec n t) -> Prim.Vec n t -> t -> Prim.Vec n t
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    bit :: BitType (Prim.Vec n t) -> Prim.Vec n t -> t -> Prim.Vec n t
    bit TypeMask{} v x
      | IntegralDict <- integralDict iR
      = unMask $ Bit.insert (BitMask v) (fromIntegral i) x

    integral :: IntegralType (Prim.Vec n t) -> Prim.Vec n t -> t -> Prim.Vec n t
    integral (SingleIntegralType   tR) _ _ = case tR of
    integral (VectorIntegralType _ tR) v x
      | IntegralDict <- integralDict tR
      , IntegralDict <- integralDict iR
      = Vec.insert v (fromIntegral i) x

    floating :: FloatingType (Prim.Vec n t) -> Prim.Vec n t -> t -> Prim.Vec n t
    floating (SingleFloatingType   tR) _ _ = case tR of
    floating (VectorFloatingType _ tR) v x
      | FloatingDict <- floatingDict tR
      , IntegralDict <- integralDict iR
      = Vec.insert v (fromIntegral i) x

evalShuffle
    :: ScalarType (Prim.Vec n a)
    -> ScalarType (Prim.Vec m a)
    -> SingleIntegralType i
    -> Prim.Vec n a
    -> Prim.Vec n a
    -> Prim.Vec m i
    -> Prim.Vec m a
evalShuffle = scalar
  where
    scalar :: ScalarType (Prim.Vec n t)
           -> ScalarType (Prim.Vec m t)
           -> SingleIntegralType i
           -> Prim.Vec n t
           -> Prim.Vec n t
           -> Prim.Vec m i
           -> Prim.Vec m t
    scalar (NumScalarType s) (NumScalarType t) = num s t
    scalar (BitScalarType s) (BitScalarType t) = bit s t
    scalar _ _ = internalError "unexpected vector encoding"

    num :: NumType (Prim.Vec n t)
        -> NumType (Prim.Vec m t)
        -> SingleIntegralType i
        -> Prim.Vec n t
        -> Prim.Vec n t
        -> Prim.Vec m i
        -> Prim.Vec m t
    num (IntegralNumType s) (IntegralNumType t) = integral s t
    num (FloatingNumType s) (FloatingNumType t) = floating s t
    num _ _ = internalError "unexpected vector encoding"

    bit :: BitType (Prim.Vec n t)
        -> BitType (Prim.Vec m t)
        -> SingleIntegralType i
        -> Prim.Vec n t
        -> Prim.Vec n t
        -> Prim.Vec m i
        -> Prim.Vec m t
    bit (TypeMask n#) TypeMask{} iR x y i
      | IntegralDict <- integralDict iR
      = let n = fromInteger (natVal' n#)
         in unMask
          $ Bit.fromList [ boundsCheck "vector index" (j >= 0 && j < 2*n)
                         $ if j < n then Bit.extract (BitMask x) j
                                    else Bit.extract (BitMask y) (j - n)
                         | j <- map fromIntegral (Vec.toList i) ]

    integral :: IntegralType (Prim.Vec n t)
             -> IntegralType (Prim.Vec m t)
             -> SingleIntegralType i
             -> Prim.Vec n t
             -> Prim.Vec n t
             -> Prim.Vec m i
             -> Prim.Vec m t
    integral (SingleIntegralType s) _ _ _ _ _ = case s of
    integral _ (SingleIntegralType t) _ _ _ _ = case t of
    integral (VectorIntegralType n# sR) (VectorIntegralType _ tR) iR x y i
      | IntegralDict <- integralDict iR
      , IntegralDict <- integralDict sR
      , IntegralDict <- integralDict tR
      = let n = fromInteger (natVal' n#)
         in Vec.fromList [ boundsCheck "vector index" (j >= 0 && j < 2*n)
                         $ if j < n then Vec.extract x j
                                    else Vec.extract y (j - n)
                         | j <- map fromIntegral (Vec.toList i) ]

    floating :: FloatingType (Prim.Vec n t)
             -> FloatingType (Prim.Vec m t)
             -> SingleIntegralType i
             -> Prim.Vec n t
             -> Prim.Vec n t
             -> Prim.Vec m i
             -> Prim.Vec m t
    floating (SingleFloatingType s) _ _ _ _ _ = case s of
    floating _ (SingleFloatingType t) _ _ _ _ = case t of
    floating (VectorFloatingType n# sR) (VectorFloatingType _ tR) iR x y i
      | IntegralDict <- integralDict iR
      , FloatingDict <- floatingDict sR
      , FloatingDict <- floatingDict tR
      = let n = fromInteger (natVal' n#)
         in Vec.fromList [ boundsCheck "vector index" (j >= 0 && j < 2*n)
                         $ if j < n then Vec.extract x j
                                    else Vec.extract y (j - n)
                         | j <- map fromIntegral (Vec.toList i) ]

evalSelect
    :: ScalarType (Prim.Vec n a)
    -> Prim.Vec n Bit
    -> Prim.Vec n a
    -> Prim.Vec n a
    -> Prim.Vec n a
evalSelect = scalar
  where
    scalar :: ScalarType (Prim.Vec n t) -> Prim.Vec n Bit -> Prim.Vec n t -> Prim.Vec n t -> Prim.Vec n t
    scalar (NumScalarType t) = num t
    scalar (BitScalarType t) = bit t

    num :: NumType (Prim.Vec n t) -> Prim.Vec n Bit -> Prim.Vec n t -> Prim.Vec n t -> Prim.Vec n t
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    bit :: BitType (Prim.Vec n t) -> Prim.Vec n Bit -> Prim.Vec n t -> Prim.Vec n t -> Prim.Vec n t
    bit TypeMask{} m x y
      = unMask
      $ Bit.fromList [ if unBit b then Bit.extract (BitMask x) i
                                  else Bit.extract (BitMask y) i
                     | b <- Bit.toList (BitMask m)
                     | i <- [0..]
                     ]

    integral :: IntegralType (Prim.Vec n t) -> Prim.Vec n Bit -> Prim.Vec n t -> Prim.Vec n t -> Prim.Vec n t
    integral (SingleIntegralType   tR) _ _ _ = case tR of
    integral (VectorIntegralType _ tR) m x y
      | IntegralDict <- integralDict tR
      = Vec.fromList [ if unBit b then Vec.extract x i
                                  else Vec.extract y i
                     | b <- Bit.toList (BitMask m)
                     | i <- [0..]
                     ]

    floating :: FloatingType (Prim.Vec n t) -> Prim.Vec n Bit -> Prim.Vec n t -> Prim.Vec n t -> Prim.Vec n t
    floating (SingleFloatingType   tR) _ _ _ = case tR of
    floating (VectorFloatingType _ tR) m x y
      | FloatingDict <- floatingDict tR
      = Vec.fromList [ if unBit b then Vec.extract x i
                                  else Vec.extract y i
                     | b <- Bit.toList (BitMask m)
                     | i <- [0..]
                     ]


-- Utilities
-- ---------

toBool :: PrimBool -> Bool
toBool = unBit

data IntegralDict t where
  IntegralDict :: (Integral t, Prim t) => IntegralDict t

data FloatingDict t where
  FloatingDict :: (RealFloat t, Prim t) => FloatingDict t

{-# INLINE integralDict #-}
integralDict :: SingleIntegralType t -> IntegralDict t
integralDict TypeInt8    = IntegralDict
integralDict TypeInt16   = IntegralDict
integralDict TypeInt32   = IntegralDict
integralDict TypeInt64   = IntegralDict
integralDict TypeInt128  = IntegralDict
integralDict TypeWord8   = IntegralDict
integralDict TypeWord16  = IntegralDict
integralDict TypeWord32  = IntegralDict
integralDict TypeWord64  = IntegralDict
integralDict TypeWord128 = IntegralDict

{-# INLINE floatingDict #-}
floatingDict :: SingleFloatingType t -> FloatingDict t
floatingDict TypeFloat16  = FloatingDict
floatingDict TypeFloat32  = FloatingDict
floatingDict TypeFloat64  = FloatingDict
floatingDict TypeFloat128 = FloatingDict

