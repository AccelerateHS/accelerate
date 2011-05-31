{-# LANGUAGE BangPatterns, CPP, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes, TupleSections, TypeOperators, TypeSynonymInstances #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Execute
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Execute (

  -- * Execute a computation under a CUDA environment
  executeAcc, executeAfun1

) where


-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Representation               hiding (Shape, sliceIndex)
import Data.Array.Accelerate.Array.Sugar                        hiding
  (dim, size, index, newArray, shapeToList, sliceIndex)
import qualified Data.Array.Accelerate.Interpreter              as I
import qualified Data.Array.Accelerate.Array.Data               as AD
import qualified Data.Array.Accelerate.Array.Sugar              as Sugar
import qualified Data.Array.Accelerate.Array.Representation     as R

import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Compile
import Data.Array.Accelerate.CUDA.CodeGen
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.Analysis.Launch

-- libraries
import Prelude                                                  hiding (sum)
import Control.Applicative                                      hiding (Const)
import Control.Monad
import Control.Monad.Trans
import System.IO.Unsafe

import Foreign.Ptr (Ptr)
import qualified Foreign.CUDA.Driver                            as CUDA

#include "accelerate.h"


-- Array expression evaluation
-- ---------------------------

-- Computations are evaluated by traversing the AST bottom-up, and for each node
-- distinguishing between three cases:
--
-- 1. If it is a Use node, return a reference to the device memory holding the
--    array data
--
-- 2. If it is a non-skeleton node, such as a let-binding or shape conversion,
--    this is executed directly by updating the environment or similar
--
-- 3. If it is a skeleton node, the associated binary object is retrieved,
--    memory allocated for the result, and the kernel(s) that implement the
--    skeleton are invoked
--

-- Evaluate a closed array expression
--
executeAcc :: Arrays a => ExecAcc a -> CIO a
executeAcc acc = executeOpenAcc acc Empty

-- Evaluate an expression with free array variables
--
executeAfun1 :: (Arrays a, Arrays b) => ExecAcc (a -> b) -> a -> CIO b
executeAfun1 (ExecAfun (R1 c) (Alam (Abody f))) arrs =
  applyArraysR uploadArray arrays arrs *>
  executeOpenAcc f (Empty `Push` arrs) <*
  applyArraysR deleteArray arrays arrs
  where
    uploadArray :: (Shape sh, Elt e) => Array sh e -> CIO ()
    uploadArray (Array sh ad) =
      let n = size sh
      in do mallocArray    ad (Just c) (max 1 n)
            pokeArrayAsync ad n Nothing

executeAfun1 _ _ = error "the sword comes out after you swallow it, right?"


-- Evaluate an open array expression
--
executeOpenAcc :: ExecOpenAcc aenv a -> Val aenv -> CIO a
executeOpenAcc (ExecAcc count kernel bindings acc) aenv =
  let R1 c     = count
      R2 c1 c0 = count
  in case acc of
    --
    -- (1) Array introduction
    --
    Use arr@(Array _ ad) -> do
      when (c > 1) $ touchArray ad (c-1)
      return arr

    --
    -- (2) Environment manipulation
    --
    Avar ix  -> return (prj ix aenv)

    Let  a b -> do
      a0 <- executeOpenAcc a aenv
      executeOpenAcc b (aenv `Push` a0) <* applyArraysR deleteArray arrays a0

    Let2 a b -> do
      (a1, a0) <- executeOpenAcc a aenv
      executeOpenAcc b (aenv `Push` a1 `Push` a0) -- <* applyArraysR deleteArray arrays a0
                                                  -- <* applyArraysR deleteArray arrays a1

    PairArrays a b ->
      (,) <$> executeOpenAcc a aenv
          <*> executeOpenAcc b aenv

    Apply (Alam (Abody f)) a -> do
      a0 <- executeOpenAcc a aenv
      executeOpenAcc f (Empty `Push` a0) <* applyArraysR deleteArray arrays a0
    Apply _ _   -> error "Awww... the sky is crying"

    Acond p t e -> do
      cond <- executeExp p aenv
      if cond then executeOpenAcc t aenv
              else executeOpenAcc e aenv

    Reshape e a -> do
      ix <- executeExp e aenv
      a0 <- executeOpenAcc a aenv
      reshapeOp c ix a0

    Unit e ->
      unitOp c =<< executeExp e aenv

    --
    -- (3) Array computations
    --
    Generate e _        ->
      generateOp c kernel bindings acc aenv =<< executeExp e aenv

    Replicate sliceIndex e a -> do
      slix <- executeExp e aenv
      a0   <- executeOpenAcc a aenv
      replicateOp c kernel bindings acc aenv sliceIndex slix a0

    Index sliceIndex a e -> do
      slix <- executeExp e aenv
      a0   <- executeOpenAcc a aenv
      indexOp c kernel bindings acc aenv sliceIndex a0 slix

    Map _ a             -> do
      a0 <- executeOpenAcc a aenv
      mapOp c kernel bindings acc aenv a0

    ZipWith _ a b       -> do
      a1 <- executeOpenAcc a aenv
      a0 <- executeOpenAcc b aenv
      zipWithOp c kernel bindings acc aenv a1 a0

    Fold _ _ a          -> do
      a0 <- executeOpenAcc a aenv
      foldOp c kernel bindings acc aenv a0

    Fold1 _ a           -> do
      a0 <- executeOpenAcc a aenv
      foldOp c kernel bindings acc aenv a0

    FoldSeg _ _ a s     -> do
      a0 <- executeOpenAcc a aenv
      s0 <- executeOpenAcc s aenv
      foldSegOp c kernel bindings acc aenv a0 s0

    Fold1Seg _ a s      -> do
      a0 <- executeOpenAcc a aenv
      s0 <- executeOpenAcc s aenv
      foldSegOp c kernel bindings acc aenv a0 s0

    Scanl _ _ a         -> do
      a0 <- executeOpenAcc a aenv
      scanOp c kernel bindings acc aenv a0

    Scanl' _ _ a        -> do
      a0 <- executeOpenAcc a aenv
      scan'Op (c1,c0) kernel bindings acc aenv a0

    Scanl1 _ a          -> do
      a0 <- executeOpenAcc a aenv
      scan1Op c kernel bindings acc aenv a0

    Scanr _ _ a         -> do
      a0 <- executeOpenAcc a aenv
      scanOp c kernel bindings acc aenv a0

    Scanr' _ _ a        -> do
      a0 <- executeOpenAcc a aenv
      scan'Op (c1,c0) kernel bindings acc aenv a0

    Scanr1 _ a          -> do
      a0 <- executeOpenAcc a aenv
      scan1Op c kernel bindings acc aenv a0

    Permute _ a _ b     -> do
      a0 <- executeOpenAcc a aenv
      a1 <- executeOpenAcc b aenv
      permuteOp c kernel bindings acc aenv a0 a1

    Backpermute e _ a   -> do
      sh <- executeExp e aenv
      a0 <- executeOpenAcc a aenv
      backpermuteOp c kernel bindings acc aenv sh a0

    Stencil _ _ a       -> do
      a0 <- executeOpenAcc a aenv
      stencilOp c kernel bindings acc aenv a0

    Stencil2 _ _ a _ b  -> do
      a1 <- executeOpenAcc a aenv
      a0 <- executeOpenAcc b aenv
      stencil2Op c kernel bindings acc aenv a1 a0

executeOpenAcc (ExecAfun _ _) _ =
  INTERNAL_ERROR(error) "executeOpenAcc" "impossible evaluation"


-- Implementation of primitive array operations
-- --------------------------------------------

reshapeOp :: Shape dim
          => Int
          -> dim
          -> Array dim' e
          -> CIO (Array dim e)
reshapeOp rc newShape (Array oldShape adata)
  = BOUNDS_CHECK(check) "reshape" "shape mismatch" (Sugar.size newShape == size oldShape)
  $ do when (rc-1 > 0) $ touchArray adata (rc-1)
       return          $ Array (fromElt newShape) adata


unitOp :: Elt e
       => Int
       -> e
       -> CIO (Scalar e)
unitOp rc v = do
  let (!ad,_) = AD.runArrayData $ do
        arr  <- AD.newArrayData 1024  -- FIXME: small arrays moved by the GC
        AD.writeArrayData arr 0 (fromElt v)
        return (arr, undefined)
  mallocArray ad (Just rc) 1
  pokeArrayAsync ad 1 Nothing
  return $ Array () ad


generateOp :: (Shape dim, Elt e)
           => Int
           -> AccKernel a
           -> [AccBinding aenv]
           -> PreOpenAcc ExecOpenAcc aenv (Array dim e)
           -> Val aenv
           -> dim
           -> CIO (Array dim e)
generateOp c kernel bindings acc aenv sh = do
  res@(Array s out) <- newArray c sh
  execute kernel bindings acc aenv (Sugar.size sh) (((),out),convertIx s)
  return res


replicateOp :: (Shape dim, Elt slix)
            => Int
            -> AccKernel (Array dim e)
            -> [AccBinding aenv]
            -> PreOpenAcc ExecOpenAcc aenv (Array dim e)
            -> Val aenv
            -> SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr dim)
            -> slix
            -> Array sl e
            -> CIO (Array dim e)
replicateOp c kernel bindings acc aenv sliceIndex slix (Array sh0 in0) = do
  res@(Array sh out) <- newArray c (toElt $ extend sliceIndex (fromElt slix) sh0)
  execute kernel bindings acc aenv (size sh) (((((),out),in0),convertIx sh0),convertIx sh)
  freeArray in0
  return res
  where
    extend :: SliceIndex slix sl co dim -> slix -> sl -> dim
    extend (SliceNil)            ()       ()      = ()
    extend (SliceAll sliceIdx)   (slx,()) (sl,sz) = (extend sliceIdx slx sl, sz)
    extend (SliceFixed sliceIdx) (slx,sz) sl      = (extend sliceIdx slx sl, sz)


indexOp :: (Shape sl, Elt slix)
        => Int
        -> AccKernel (Array dim e)
        -> [AccBinding aenv]
        -> PreOpenAcc ExecOpenAcc aenv (Array sl e)
        -> Val aenv
        -> SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr dim)
        -> Array dim e
        -> slix
        -> CIO (Array sl e)
indexOp c kernel bindings acc aenv sliceIndex (Array sh0 in0) slix = do
  res@(Array sh out) <- newArray c (toElt $ restrict sliceIndex (fromElt slix) sh0)
  execute kernel bindings acc aenv (size sh)
    ((((((),out),in0),convertIx sh),convertSlix sliceIndex (fromElt slix)),convertIx sh0)
  freeArray in0
  return res
  where
    restrict :: SliceIndex slix sl co dim -> slix -> dim -> sl
    restrict (SliceNil)            ()       ()      = ()
    restrict (SliceAll sliceIdx)   (slx,()) (sh,sz) = (restrict sliceIdx slx sh, sz)
    restrict (SliceFixed sliceIdx) (slx,i)  (sh,sz)
      = BOUNDS_CHECK(checkIndex) "slice" i sz $ restrict sliceIdx slx sh
    --
    convertSlix :: SliceIndex slix sl co dim -> slix -> [Int32]
    convertSlix (SliceNil)            ()     = []
    convertSlix (SliceAll   sliceIdx) (s,()) = convertSlix sliceIdx s
    convertSlix (SliceFixed sliceIdx) (s,i)  = fromIntegral i : convertSlix sliceIdx s


mapOp :: Elt e
      => Int
      -> AccKernel (Array dim e)
      -> [AccBinding aenv]
      -> PreOpenAcc ExecOpenAcc aenv (Array dim e)
      -> Val aenv
      -> Array dim e'
      -> CIO (Array dim e)
mapOp c kernel bindings acc aenv (Array sh0 in0) = do
  res@(Array _ out) <- newArray c (toElt sh0)
  execute kernel bindings acc aenv (size sh0) ((((),out),in0),size sh0)
  freeArray in0
  return res

zipWithOp :: Elt c
          => Int
          -> AccKernel (Array dim c)
          -> [AccBinding aenv]
          -> PreOpenAcc ExecOpenAcc aenv (Array dim c)
          -> Val aenv
          -> Array dim a
          -> Array dim b
          -> CIO (Array dim c)
zipWithOp c kernel bindings acc aenv (Array sh1 in1) (Array sh0 in0) = do
  res@(Array sh out) <- newArray c $ toElt (sh1 `intersect` sh0)
  execute kernel bindings acc aenv (size sh) (((((((),out),in1),in0),convertIx sh),convertIx sh1),convertIx sh0)
  freeArray in1
  freeArray in0
  return res

foldOp :: forall dim e aenv. Shape dim
       => Int
       -> AccKernel (Array dim e)
       -> [AccBinding aenv]
       -> PreOpenAcc ExecOpenAcc aenv (Array dim e)
       -> Val aenv
       -> Array (dim:.Int) e
       -> CIO (Array dim e)
foldOp c kernel bindings acc aenv (Array sh0 in0)
  -- A recursive multi-block reduction when collapsing to a single value
  --
  -- TLM: multiple bind/free of arrays in scalar expressions in the recursive
  --      case, which probably breaks reference counting.
  --
  | dim sh0 == 1 = do
      cfg@(_,_,(_,g,_)) <- configure kernel acc (size sh0)
      res@(Array _ out) <- newArray (bool c 1 (g > 1)) (toElt (fst sh0,g)) :: CIO (Array (dim:.Int) e)
      dispatch cfg bindings aenv ((((),out),in0),size sh0)
      freeArray in0
      if g > 1 then foldOp c kernel bindings acc aenv res
               else return (Array (fst sh0) out)
  --
  -- Reduction over the innermost dimension of an array (single pass operation)
  --
  | otherwise    = do
      res@(Array sh out) <- newArray c $ toElt (fst sh0)
      execute kernel bindings acc aenv (size (fst sh0)) (((((),out),in0),convertIx sh),convertIx sh0)
      freeArray in0
      return res

foldSegOp :: Shape dim
          => Int
          -> AccKernel (Array dim e)
          -> [AccBinding aenv]
          -> PreOpenAcc ExecOpenAcc aenv (Array (dim:.Int) e)
          -> Val aenv
          -> Array (dim:.Int) e
          -> Segments
          -> CIO (Array (dim:.Int) e)
foldSegOp c kernel bindings acc aenv (Array sh0 in0) (Array shs seg) = do
  res@(Array sh out) <- newArray c $ toElt (fst sh0, size shs-1)
  execute kernel bindings acc aenv (size sh) ((((((),out),in0),seg),convertIx sh),convertIx sh0)
  freeArray in0
  freeArray seg
  return res


scanOp :: forall aenv e. Elt e
       => Int
       -> AccKernel (Vector e)
       -> [AccBinding aenv]
       -> PreOpenAcc ExecOpenAcc aenv (Vector e)
       -> Val aenv
       -> Vector e
       -> CIO (Vector e)
scanOp c kernel bindings acc aenv (Array sh0 in0) = do
  (mdl,fscan,(t,g,m)) <- configure kernel acc (size sh0)
  fadd                <- liftIO $ CUDA.getFun mdl "exclusive_update"
  res@(Array _ out)   <- newArray c (Z :. size sh0 + 1)
  (Array _ bks)       <- newArray 1 (Z :. g) :: CIO (Vector e)
  (Array _ sum)       <- newArray 1 Z        :: CIO (Scalar e)
  let n   = size sh0
      itv = (n + g - 1) `div` g
  --
  bindLifted mdl aenv bindings
  launch (t,g,m) fscan ((((((),out),in0),bks),n),itv)   -- inclusive scan of input array
  launch (t,1,m) fscan ((((((),bks),bks),sum),g),itv)   -- inclusive scan block-level sums
  launch (t,g,m) fadd  ((((((),out),bks),sum),n),itv)   -- distribute partial results
  freeLifted aenv bindings
  freeArray in0
  freeArray bks
  freeArray sum
  return res

scan'Op :: forall aenv e. Elt e
        => (Int,Int)
        -> AccKernel (Vector e)
        -> [AccBinding aenv]
        -> PreOpenAcc ExecOpenAcc aenv (Vector e, Scalar e)
        -> Val aenv
        -> Vector e
        -> CIO (Vector e, Scalar e)
scan'Op (c1,c0) kernel bindings acc aenv (Array sh0 in0) = do
  (mdl,fscan,(t,g,m)) <- configure kernel acc (size sh0)
  fadd                <- liftIO $ CUDA.getFun mdl "exclusive_update"
  res1@(Array _ out)  <- newArray c1 (toElt sh0)
  res2@(Array _ sum)  <- newArray c0 Z
  (Array _ bks)       <- newArray 1  (Z :. g) :: CIO (Vector e)
  let n   = size sh0
      itv = (n + g - 1) `div` g
  --
  bindLifted mdl aenv bindings
  launch (t,g,m) fscan ((((((),out),in0),bks),n),itv)   -- inclusive scan of input array
  launch (t,1,m) fscan ((((((),bks),bks),sum),g),itv)   -- inclusive scan block-level sums
  launch (t,g,m) fadd  ((((((),out),bks),sum),n),itv)   -- distribute partial results
  freeLifted aenv bindings
  freeArray in0
  freeArray bks
  when (c1 == 0) $ freeArray out
  when (c0 == 0) $ freeArray sum
  return (res1,res2)

scan1Op :: forall aenv e. Elt e
        => Int
        -> AccKernel (Vector e)
        -> [AccBinding aenv]
        -> PreOpenAcc ExecOpenAcc aenv (Vector e)
        -> Val aenv
        -> Vector e
        -> CIO (Vector e)
scan1Op c kernel bindings acc aenv (Array sh0 in0) = do
  (mdl,fscan,(t,g,m)) <- configure kernel acc (size sh0)
  fadd                <- liftIO $ CUDA.getFun mdl "inclusive_update"
  res@(Array _ out)   <- newArray c (toElt sh0)
  (Array _ bks)       <- newArray 1 (Z :. g) :: CIO (Vector e)
  (Array _ sum)       <- newArray 1 Z        :: CIO (Scalar e)
  let n   = size sh0
      itv = (n + g - 1) `div` g
  --
  bindLifted mdl aenv bindings
  launch (t,g,m) fscan ((((((),out),in0),bks),n),itv)   -- inclusive scan of input array
  launch (t,1,m) fscan ((((((),bks),bks),sum),g),itv)   -- inclusive scan block-level sums
  launch (t,g,m) fadd  (((((),out),bks),n),itv)         -- distribute partial results
  freeLifted aenv bindings
  freeArray in0
  freeArray bks
  freeArray sum
  return res

permuteOp :: Elt e
          => Int
          -> AccKernel (Array dim e)
          -> [AccBinding aenv]
          -> PreOpenAcc ExecOpenAcc aenv (Array dim' e)
          -> Val aenv
          -> Array dim' e       -- default values
          -> Array dim e        -- permuted array
          -> CIO (Array dim' e)
permuteOp c kernel bindings acc aenv (Array sh0 in0) (Array sh1 in1) = do
  res@(Array _ out) <- newArray c (toElt sh0)
  copyArray in0 out (size sh0)
  execute kernel bindings acc aenv (size sh0) (((((),out),in1),convertIx sh0),convertIx sh1)
  freeArray in0
  freeArray in1
  return res

backpermuteOp :: (Shape dim', Elt e)
              => Int
              -> AccKernel (Array dim e)
              -> [AccBinding aenv]
              -> PreOpenAcc ExecOpenAcc aenv (Array dim' e)
              -> Val aenv
              -> dim'
              -> Array dim e
              -> CIO (Array dim' e)
backpermuteOp c kernel bindings acc aenv dim' (Array sh0 in0) = do
  res@(Array sh out) <- newArray c dim'
  execute kernel bindings acc aenv (size sh) (((((),out),in0),convertIx sh),convertIx sh0)
  freeArray in0
  return res

stencilOp :: Elt e
          => Int
          -> AccKernel (Array dim e)
          -> [AccBinding aenv]
          -> PreOpenAcc ExecOpenAcc aenv (Array dim e)
          -> Val aenv
          -> Array dim e'
          -> CIO (Array dim e)
stencilOp c kernel bindings acc aenv sten0@(Array sh0 in0) = do
  res@(Array _ out)  <- newArray c (toElt sh0)
  (mdl,fstencil,cfg) <- configure kernel acc (size sh0)
  bindLifted mdl aenv bindings
  bindStencil 0 mdl sten0
  launch cfg fstencil (((),out),convertIx sh0)
  freeLifted aenv bindings
  freeArray in0
  return res

stencil2Op :: Elt e
           => Int
           -> AccKernel (Array dim e)
           -> [AccBinding aenv]
           -> PreOpenAcc ExecOpenAcc aenv (Array dim e)
           -> Val aenv
           -> Array dim e1
           -> Array dim e2
           -> CIO (Array dim e)
stencil2Op c kernel bindings acc aenv sten1@(Array sh1 in1) sten0@(Array sh0 in0) = do
  res@(Array sh out) <- newArray c $ toElt (sh1 `intersect` sh0)
  (mdl,fstencil,cfg) <- configure kernel acc (size sh)
  bindLifted mdl aenv bindings
  bindStencil 0 mdl sten0
  bindStencil 1 mdl sten1
  launch cfg fstencil (((((),out),convertIx sh),convertIx sh1),convertIx sh0)
  freeLifted aenv bindings
  freeArray in0
  freeArray in1
  return res


-- Expression evaluation
-- ---------------------

-- Evaluate an open expression
--
executeOpenExp :: PreOpenExp ExecOpenAcc env aenv t -> Val env -> Val aenv -> CIO t
executeOpenExp (Var idx)         env _    = return . toElt $ prj idx env
executeOpenExp (Const c)         _   _    = return $ toElt c
executeOpenExp (PrimConst c)     _   _    = return $ I.evalPrimConst c
executeOpenExp (PrimApp fun arg) env aenv = I.evalPrim fun <$> executeOpenExp arg env aenv
executeOpenExp (Tuple tup)       env aenv = toTuple                   <$> executeTuple tup env aenv
executeOpenExp (Prj idx e)       env aenv = I.evalPrj idx . fromTuple <$> executeOpenExp e env aenv
executeOpenExp IndexAny          _   _    = INTERNAL_ERROR(error) "executeOpenExp" "IndexAny: not implemented yet"
executeOpenExp IndexNil          _   _    = return Z
executeOpenExp (IndexCons sh i)  env aenv = (:.) <$> executeOpenExp sh env aenv <*> executeOpenExp i env aenv
executeOpenExp (IndexHead ix)    env aenv = (\(_:.h) -> h) <$> executeOpenExp ix env aenv
executeOpenExp (IndexTail ix)    env aenv = (\(t:._) -> t) <$> executeOpenExp ix env aenv
executeOpenExp (IndexScalar a e) env aenv = do
  (Array sh ad) <- executeOpenAcc a aenv
  ix            <- executeOpenExp e env aenv
  res           <- toElt <$> ad `indexArray` index sh (fromElt ix)
  freeArray ad
  return res

executeOpenExp (Shape a) _ aenv = do
  (Array sh ad) <- executeOpenAcc a aenv
  freeArray ad
  return (toElt sh)

executeOpenExp (Size a) _ aenv = do
  (Array sh ad) <- executeOpenAcc a aenv
  freeArray ad
  return (size sh)

executeOpenExp (Cond c t e) env aenv = do
  p <- executeOpenExp c env aenv
  if p then executeOpenExp t env aenv
       else executeOpenExp e env aenv


-- Evaluate a closed expression
--
executeExp :: PreExp ExecOpenAcc aenv t -> Val aenv -> CIO t
executeExp e = executeOpenExp e Empty


-- Tuple evaluation
--
executeTuple :: Tuple (PreOpenExp ExecOpenAcc env aenv) t -> Val env -> Val aenv -> CIO t
executeTuple NilTup          _   _    = return ()
executeTuple (t `SnocTup` e) env aenv = (,) <$> executeTuple   t env aenv
                                            <*> executeOpenExp e env aenv


-- Array references in scalar code
-- -------------------------------

bindLifted :: CUDA.Module -> Val aenv -> [AccBinding aenv] -> CIO ()
bindLifted mdl aenv = mapM_ (bindAcc mdl aenv)

freeLifted :: Val aenv -> [AccBinding aenv] -> CIO ()
freeLifted aenv = mapM_ free
  where
    free (ArrayVar idx) =
      let Array _ ad = prj idx aenv
      in  freeArray ad


bindAcc :: CUDA.Module
        -> Val aenv
        -> AccBinding aenv
        -> CIO ()
bindAcc mdl aenv (ArrayVar idx) =
  let idx'        = show $ idxToInt idx
      Array sh ad = prj idx aenv
      --
      bindDim = liftIO $
        CUDA.getPtr mdl ("sh" ++ idx') >>=
        CUDA.pokeListArray (convertIx sh) . fst
      --
      arr n   = "arr" ++ idx' ++ "_a" ++ show (n::Int)
      tex     = CUDA.getTex mdl . arr
      bindTex =
        marshalTextureData ad (size sh) =<< liftIO (sequence' $ map tex [0..])
  in
  bindDim >> bindTex


bindStencil :: Int
            -> CUDA.Module
            -> Array dim e
            -> CIO ()
bindStencil s mdl (Array sh ad) =
  let sten n = "stencil" ++ show s ++ "_a" ++ show (n::Int)
      tex    = CUDA.getTex mdl . sten
  in
  marshalTextureData ad (size sh) =<< liftIO (sequence' $ map tex [0..])


-- Kernel execution
-- ----------------

-- Data which can be marshalled as arguments to a kernel invocation. For Int and
-- Word, we match the device bit-width of these types.
--
class Marshalable a where
  marshal :: a -> CIO [CUDA.FunParam]

instance Marshalable () where
  marshal _ = return []

instance Marshalable Int where
  marshal x = marshal (fromIntegral x :: Int32)         -- TLM: this isn't so good...

instance Marshalable Word where
  marshal x = marshal (fromIntegral x :: Word32)

#define primMarshalable(ty)                                                    \
instance Marshalable ty where {                                                \
  marshal x = return [CUDA.VArg x] }

primMarshalable(Int8)
primMarshalable(Int16)
primMarshalable(Int32)
primMarshalable(Int64)
primMarshalable(Word8)
primMarshalable(Word16)
primMarshalable(Word32)
primMarshalable(Word64)
primMarshalable(Float)
primMarshalable(Double)
primMarshalable((Ptr a))
primMarshalable((CUDA.DevicePtr a))

instance Marshalable CUDA.FunParam where
  marshal x = return [x]

instance AD.ArrayElt e => Marshalable (AD.ArrayData e) where
  marshal = marshalArrayData    -- Marshalable (DevicePtrs a) does not type )=

instance Marshalable a => Marshalable [a] where
  marshal = concatMapM marshal

instance (Marshalable a, Marshalable b) => Marshalable (a,b) where
  marshal (a,b) = (++) <$> marshal a <*> marshal b


-- Link the binary object implementing the computation, configure the kernel
-- launch parameters, and initiate the computation. This also handles lifting
-- and binding of array references from scalar expressions.
--
execute :: Marshalable args
        => AccKernel a          -- The binary module implementing this kernel
        -> [AccBinding aenv]    -- Array variables embedded in scalar expressions
        -> PreOpenAcc ExecOpenAcc aenv a
        -> Val aenv
        -> Int
        -> args
        -> CIO ()
execute kernel bindings acc aenv n args =
  configure kernel acc n >>= \cfg ->
  dispatch cfg bindings aenv args

-- Pre-execution configuration and kernel linking
--
configure :: AccKernel a
          -> PreOpenAcc ExecOpenAcc aenv a
          -> Int
          -> CIO (CUDA.Module, CUDA.Fun, (Int,Int,Integer))
configure (name, kernel) acc n = do
  mdl <- kernel
  fun <- liftIO $ CUDA.getFun mdl name
  cfg <- launchConfig acc n fun
  return (mdl, fun, cfg)


-- Binding of lifted array expressions and kernel invocation
--
dispatch :: Marshalable args
         => (CUDA.Module, CUDA.Fun, (Int,Int,Integer))
         -> [AccBinding aenv]
         -> Val aenv
         -> args
         -> CIO ()
dispatch (mdl, fun, cfg) fvs aenv args = do
  bindLifted mdl aenv fvs
  launch cfg fun args
  freeLifted aenv fvs

-- Execute a device function, with the given thread configuration and function
-- parameters. The tuple contains (threads per block, grid size, shared memory)
--
launch :: Marshalable args => (Int,Int,Integer) -> CUDA.Fun -> args -> CIO ()
launch (cta,grid,smem) fn a = do
  args <- marshal a
  liftIO $ do
    CUDA.setParams     fn args
    CUDA.setSharedSize fn smem
    CUDA.setBlockShape fn (cta,1,1)
    CUDA.launch        fn (grid,1) Nothing


-- Memory management
-- -----------------

-- Allocate a new device array to accompany the given host-side Accelerate
-- array, of given shape and reference count.
--
newArray :: (Shape sh, Elt e)
         => Int                         -- use/reference count
         -> sh                          -- shape
         -> CIO (Array sh e)
newArray rc sh = do
  ad `seq` mallocArray ad (Just rc) (1 `max` n)
  return $ Array (fromElt sh) ad
  where
    n      = Sugar.size sh
    (ad,_) = AD.runArrayData $ (,undefined) `fmap` AD.newArrayData (1024 `max` n)
      -- FIXME: small arrays moved by the GC
      -- FIXME: only the final output array needs to be allocated to full size


-- Auxiliary functions
-- -------------------

-- Fold over a boolean value, analogous to 'maybe' and 'either'
--
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True  = y

-- Like 'when' but in teh monadz
--
whenM :: Monad m => m Bool -> m () -> m ()
whenM predicate action = do
  doit <- predicate
  when doit action

-- Generalise concatMap to arbitrary monads
--
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat `liftM` mapM f xs

-- A lazier version of 'Control.Monad.sequence'
--
sequence' :: [IO a] -> IO [a]
sequence' = foldr k (return [])
  where k m ms = do { x <- m; xs <- unsafeInterleaveIO ms; return (x:xs) }

-- Extract shape dimensions as a list of 32-bit integers (the base integer width
-- of the device, and used for index calculations). Singleton dimensions are
-- considered to be of unit size.
--
-- Internally, Accelerate uses snoc-based tuple projection, while the data
-- itself is stored in reading order. Ensure we match the behaviour of regular
-- tuples and code generation thereof.
--
-- TLM: keep native integer sizes, now that we have conversion functions
--
convertIx :: R.Shape sh => sh -> [Int32]
convertIx = post . map fromIntegral . shapeToList
  where post [] = [1]
        post xs = reverse xs

-- Cautiously delete a array, checking if it still exists on the device first.
-- This is because we have over zealous reference counting.
--
deleteArray :: Elt e => Array sh e -> CIO ()
deleteArray (Array _ ad) = whenM (existsArrayData ad) (freeArray ad)

-- Apply a function to all components of an Arrays structure
--
applyArraysR
    :: (forall sh e. (Shape sh, Elt e) => Array sh e -> CIO ())
    -> ArraysR arrs
    -> arrs
    -> CIO ()
applyArraysR _  ArraysRunit         ()       = return ()
applyArraysR go (ArraysRpair r1 r0) (a1, a0) = applyArraysR go r1 a1 >> applyArraysR go r0 a0
applyArraysR go ArraysRarray        arr      = go arr

