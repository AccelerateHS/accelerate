{-# LANGUAGE BangPatterns, CPP, GADTs, PatternGuards, ScopedTypeVariables #-}
{-# LANGUAGE TupleSections, TypeOperators, TypeSynonymInstances #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Execute
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Execute (executeAcc, executeAfun1)
  where

import Prelude                                                  hiding (sum)
import Data.Int
import Data.Word
import Data.Maybe
import Data.Typeable
import Control.Monad
import Control.Monad.Trans
import Control.Applicative                                      hiding (Const)
import qualified Data.HashTable                                 as Hash

import System.FilePath                                          hiding (combine)
import System.Posix.Process
import System.Exit                                              (ExitCode(..))
import System.Posix.Types                                       (ProcessID)
import System.Mem.StableName
import System.IO.Unsafe

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Representation               hiding (sliceIndex)
import Data.Array.Accelerate.Array.Sugar (
  Z(..), (:.)(..), Array(..), Scalar, Vector, Segments)
import qualified Data.Array.Accelerate.Array.Data               as AD
import qualified Data.Array.Accelerate.Array.Sugar              as Sugar
import qualified Data.Array.Accelerate.Interpreter              as I

import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.Analysis.Hash
import Data.Array.Accelerate.CUDA.Analysis.Launch

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
executeAcc :: Arrays a => Acc a -> CIO a
executeAcc acc = executeOpenAcc acc Empty

-- Evaluate an expression with free array variables
--
-- TLM: we do not have reference counting for the free array variables, so they
--      remain active for the entire computation.
--
executeAfun1 :: Arrays b => Afun (a -> b) -> a -> CIO b
executeAfun1 (Alam (Abody f)) arrs = do
  copyArraysR arrays arrs
  executeOpenAcc f (Empty `Push` arrs) <* freeArraysR arrays arrs
  where
    copyArraysR :: ArraysR arrs -> arrs -> CIO ()
    copyArraysR ArraysRunit         ()      = return ()
    copyArraysR ArraysRarray        arr     = copy arr
    copyArraysR (ArraysRpair r1 r0) (a1,a0) = copyArraysR r1 a1 >> copyArraysR r0 a0
    --
    copy :: (Sugar.Shape dim, Sugar.Elt e) => Array dim e -> CIO ()
    copy (Array sh ad) =
      let n = size sh
          c = Nothing   -- no reference counting yet; need to keep active )=
      in do mallocArray    ad c (max 1 n)
            pokeArrayAsync ad n Nothing
    --
    freeArraysR :: ArraysR arrs -> arrs -> CIO ()
    freeArraysR ArraysRunit         ()           = return ()
    freeArraysR ArraysRarray        (Array _ ad) = unbindArray ad    >> freeArray ad
    freeArraysR (ArraysRpair r1 r0) (a1,a0)      = freeArraysR r1 a1 >> freeArraysR r0 a0

executeAfun1 _ _                = error "we can never get here"


-- Evaluate an open array expression
--
executeOpenAcc :: (Typeable aenv, Typeable a) => OpenAcc aenv a -> Val aenv -> CIO a

---- (1) Array introduction ----
executeOpenAcc (Use a)    _    = return a

---- (2) Non-skeleton nodes ----
executeOpenAcc (Avar ix)  aenv = return (prj ix aenv)

executeOpenAcc (Let  a b) aenv = do
  a0 <- executeOpenAcc a aenv
  executeOpenAcc b (aenv `Push` a0)

executeOpenAcc (Let2 a b) aenv = do
  (a1,a0) <- executeOpenAcc a aenv
  executeOpenAcc b (aenv `Push` a1 `Push` a0)

executeOpenAcc (Reshape e a) aenv = do
  ix <- executeExp e aenv
  a0 <- executeOpenAcc a aenv
  reshapeOp ix a0

executeOpenAcc acc@(Unit e) aenv =
  unitOp acc =<< executeExp e aenv

---- (3) Array computations ----
executeOpenAcc acc@(Generate e _) aenv =
  generateOp acc aenv =<< executeExp e aenv

executeOpenAcc acc@(Replicate sliceIndex e a) aenv = do
  slix <- executeExp e aenv
  a0   <- executeOpenAcc a aenv
  replicateOp acc aenv sliceIndex slix a0

executeOpenAcc acc@(Index sliceIndex a e) aenv = do
  slix <- executeExp e aenv
  a0   <- executeOpenAcc a aenv
  indexOp acc aenv sliceIndex a0 slix

executeOpenAcc acc@(Map _ a) aenv = do
  a0 <- executeOpenAcc a aenv
  mapOp acc aenv a0

executeOpenAcc acc@(ZipWith _ a b) aenv = do
  a1 <- executeOpenAcc a aenv
  a0 <- executeOpenAcc b aenv
  zipWithOp acc aenv a1 a0

executeOpenAcc acc@(Fold _ _ a) aenv = do
  a0 <- executeOpenAcc a aenv
  foldOp acc aenv a0

executeOpenAcc acc@(Fold1 _ a) aenv = do
  a0 <- executeOpenAcc a aenv
  foldOp acc aenv a0

executeOpenAcc acc@(FoldSeg _ _ a s) aenv = do
  a0 <- executeOpenAcc a aenv
  s0 <- executeOpenAcc s aenv
  foldSegOp acc aenv a0 s0

executeOpenAcc acc@(Fold1Seg _ a s) aenv = do
  a0 <- executeOpenAcc a aenv
  s0 <- executeOpenAcc s aenv
  foldSegOp acc aenv a0 s0

executeOpenAcc acc@(Scanl _ _ a) aenv = do
  a0 <- executeOpenAcc a aenv
  scanOp acc aenv a0

executeOpenAcc acc@(Scanl' _ _ a) aenv = do
  a0 <- executeOpenAcc a aenv
  scan'Op acc aenv a0

executeOpenAcc acc@(Scanl1 _ a) aenv = do
  a0 <- executeOpenAcc a aenv
  scan1Op acc aenv a0

executeOpenAcc acc@(Scanr _ _ a) aenv = do
  a0 <- executeOpenAcc a aenv
  scanOp acc aenv a0

executeOpenAcc acc@(Scanr' _ _ a) aenv = do
  a0 <- executeOpenAcc a aenv
  scan'Op acc aenv a0

executeOpenAcc acc@(Scanr1 _ a) aenv = do
  a0 <- executeOpenAcc a aenv
  scan1Op acc aenv a0

executeOpenAcc acc@(Permute _ a _ b) aenv = do
  a0 <- executeOpenAcc a aenv
  a1 <- executeOpenAcc b aenv
  permuteOp acc aenv a0 a1

executeOpenAcc acc@(Backpermute e _ a) aenv = do
  sh <- executeExp e aenv
  a0 <- executeOpenAcc a aenv
  backpermuteOp acc aenv sh a0

executeOpenAcc acc@(Stencil _ _ a) aenv = do
  a0 <- executeOpenAcc a aenv
  stencilOp acc aenv a0

executeOpenAcc acc@(Stencil2 _ _ a _ b) aenv = do
  a1 <- executeOpenAcc a aenv
  a0 <- executeOpenAcc b aenv
  stencil2Op acc aenv a1 a0



-- Implementation of primitive array operations
-- --------------------------------------------

reshapeOp :: Sugar.Shape dim
          => dim
          -> Array dim' e
          -> CIO (Array dim e)
reshapeOp newShape (Array oldShape adata)
  = BOUNDS_CHECK(check) "reshape" "shape mismatch" (Sugar.size newShape == size oldShape)
  $ return (Array (Sugar.fromElt newShape) adata)

unitOp :: (Sugar.Elt e, Typeable aenv)
       => OpenAcc aenv (Scalar e)
       -> e
       -> CIO (Scalar e)
unitOp acc v = do
  rc <- Just `fmap` getUseCount acc
  let (!ad,_) = AD.runArrayData $ do
        arr  <- AD.newArrayData 1024                    -- FIXME: small arrays moved by the GC
        AD.writeArrayData arr 0 (Sugar.fromElt v)
        return (arr, undefined)
  mallocArray ad rc 1
  pokeArrayAsync ad 1 Nothing
  return $ Array () ad

generateOp :: (Sugar.Shape dim, Sugar.Elt e, Typeable aenv)
           => OpenAcc aenv (Array dim e)
           -> Val aenv
           -> dim
           -> CIO (Array dim e)
generateOp acc aenv sh = do
  res@(Array s out) <- allocResult acc sh
  execute "generate" acc aenv (Sugar.size sh) (((),out),convertIx s)
  return res

replicateOp :: (Sugar.Shape dim, Sugar.Elt slix, Typeable aenv)
            => OpenAcc aenv (Array dim e)
            -> Val aenv
            -> SliceIndex (Sugar.EltRepr slix)
                          (Sugar.EltRepr sl)
                          co
                          (Sugar.EltRepr dim)
            -> slix
            -> Array sl e
            -> CIO (Array dim e)
replicateOp acc aenv sliceIndex slix (Array sh0 in0) = do
  res@(Array sh out) <- allocResult acc (Sugar.toElt $ extend sliceIndex (Sugar.fromElt slix) sh0)
  execute "replicate" acc aenv (size sh) (((((),out),in0),convertIx sh0),convertIx sh)
  freeArray in0
  return res
  where
    extend :: SliceIndex slix sl co dim -> slix -> sl -> dim
    extend (SliceNil)            ()       ()      = ()
    extend (SliceAll sliceIdx)   (slx,()) (sl,sz) = (extend sliceIdx slx sl, sz)
    extend (SliceFixed sliceIdx) (slx,sz) sl      = (extend sliceIdx slx sl, sz)

indexOp :: (Sugar.Shape sl, Sugar.Elt slix, Typeable aenv)
        => OpenAcc aenv (Array sl e)
        -> Val aenv
        -> SliceIndex (Sugar.EltRepr slix)
                      (Sugar.EltRepr sl)
                      co
                      (Sugar.EltRepr dim)
        -> Array dim e
        -> slix
        -> CIO (Array sl e)
indexOp acc aenv sliceIndex (Array sh0 in0) slix = do
  res@(Array sh out) <- allocResult acc (Sugar.toElt $ restrict sliceIndex (Sugar.fromElt slix) sh0)
  execute "slice" acc aenv (size sh)
    ((((((),out),in0),convertIx sh),convertSlix sliceIndex (Sugar.fromElt slix)),convertIx sh0)
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


mapOp :: (Sugar.Elt e, Typeable aenv)
      => OpenAcc aenv (Array dim e)
      -> Val aenv
      -> Array dim e'
      -> CIO (Array dim e)
mapOp acc aenv (Array sh0 in0) = do
  res@(Array _ out) <- allocResult acc (Sugar.toElt sh0)
  execute "map" acc aenv (size sh0) ((((),out),in0),size sh0)
  freeArray in0
  return res

zipWithOp :: (Sugar.Elt c, Typeable aenv)
          => OpenAcc aenv (Array dim c)
          -> Val aenv
          -> Array dim a
          -> Array dim b
          -> CIO (Array dim c)
zipWithOp acc aenv (Array sh1 in1) (Array sh0 in0) = do
  res@(Array sh out) <- allocResult acc $ Sugar.toElt (sh1 `intersect` sh0)
  execute "zipWith" acc aenv (size sh) (((((((),out),in1),in0),convertIx sh),convertIx sh1),convertIx sh0)
  freeArray in1
  freeArray in0
  return res

foldOp :: (Sugar.Shape dim, Typeable aenv)
       => OpenAcc aenv (Array dim e)
       -> Val aenv
       -> Array (dim:.Int) e
       -> CIO (Array dim e)
foldOp acc aenv (Array sh0 in0)
  -- A recursive multi-block reduction when collapsing to a single value
  --
  | dim sh0 == 1 = do
      cfg@(_,_,_,(_,g,_)) <- configure "fold" acc aenv (size sh0)
      rc                  <- getUseCount acc
      res@(Array _ out)   <- newArray (bool rc 1 (g > 1)) (Sugar.toElt (fst sh0,g))
      dispatch cfg ((((),out),in0),size sh0)
      freeArray in0
      if g > 1 then foldOp acc aenv res
               else return (Array (fst sh0) out)
  --
  -- Reduction over the innermost dimension of an array (single pass operation)
  --
  | otherwise    = do
      res@(Array sh out) <- allocResult acc $ Sugar.toElt (fst sh0)
      execute "fold" acc aenv (size (fst sh0)) (((((),out),in0),convertIx sh),convertIx sh0)
      freeArray in0
      return res

foldSegOp :: (Sugar.Shape dim, Typeable aenv)
          => OpenAcc aenv (Array (dim:.Int) e)
          -> Val aenv
          -> Array (dim:.Int) e
          -> Segments
          -> CIO (Array (dim:.Int) e)
foldSegOp acc aenv (Array sh0 in0) seg' = do
  (Array shs seg)    <- scanOp scan aenv seg'           -- transform segment descriptor into offset indices
  res@(Array sh out) <- allocResult acc $ Sugar.toElt (fst sh0, size shs-1)
  execute "foldSeg" acc aenv (size sh) ((((((),out),in0),seg),convertIx sh),convertIx sh0)
  freeArray in0
  freeArray seg
  return res
  where
    scan = Scanl add (Const ((),0)) (Use (Array undefined undefined :: Segments))
    add  = Lam (Lam (Body (PrimAdd numType
                          `PrimApp`
                          Tuple (NilTup `SnocTup` Var (SuccIdx ZeroIdx)
                                        `SnocTup` Var ZeroIdx))))

scanOp :: forall aenv e. (Sugar.Elt e, Typeable aenv)
       => OpenAcc aenv (Vector e)
       -> Val aenv
       -> Vector e
       -> CIO (Vector e)
scanOp acc aenv (Array sh0 in0) = do
  (fvs,mdl,fscan,(t,g,m)) <- configure "inclusive_scan" acc aenv (size sh0)
  fadd                    <- liftIO $ CUDA.getFun mdl "exclusive_update"
  c                       <- getUseCount acc
  res@(Array _ out)       <- newArray c (Z :. size sh0 + 1)
  (Array _ bks)           <- newArray 1 (Z :. g) :: CIO (Vector e)
  (Array _ sum)           <- newArray 1 Z        :: CIO (Scalar e)
  let n   = size sh0
      itv = (n + g - 1) `div` g
  --
  bindLifted mdl fvs
  launch (t,g,m) fscan ((((((),out),in0),bks),n),itv)   -- inclusive scan of input array
  launch (t,1,m) fscan ((((((),bks),bks),sum),g),itv)   -- inclusive scan block-level sums
  launch (t,g,m) fadd  ((((((),out),bks),sum),n),itv)   -- distribute partial results
  freeLifted fvs
  freeArray in0
  freeArray bks
  freeArray sum
  return res

scan'Op :: forall aenv e. (Sugar.Elt e, Typeable aenv)
        => OpenAcc aenv (Vector e, Scalar e)
        -> Val aenv 
        -> Vector e
        -> CIO (Vector e, Scalar e)
scan'Op acc aenv (Array sh0 in0) = do
  (fvs,mdl,fscan,(t,g,m)) <- configure "inclusive_scan" acc aenv (size sh0)
  fadd                    <- liftIO $ CUDA.getFun mdl "exclusive_update"
  (c1,c2)                 <- getUseCount2 acc
  res1@(Array _ out)      <- newArray c1 (Sugar.toElt sh0)
  res2@(Array _ sum)      <- newArray c2 Z
  (Array _ bks)           <- newArray 1  (Z :. g) :: CIO (Vector e)
  let n   = size sh0
      itv = (n + g - 1) `div` g
  --
  bindLifted mdl fvs
  launch (t,g,m) fscan ((((((),out),in0),bks),n),itv)   -- inclusive scan of input array   
  launch (t,1,m) fscan ((((((),bks),bks),sum),g),itv)   -- inclusive scan block-level sums
  launch (t,g,m) fadd  ((((((),out),bks),sum),n),itv)   -- distribute partial results
  freeLifted fvs
  freeArray in0
  freeArray bks
  when (c1 == 0) $ freeArray out
  when (c2 == 0) $ freeArray sum
  return (res1,res2)

scan1Op :: forall aenv e. (Sugar.Elt e, Typeable aenv)
        => OpenAcc aenv (Vector e)
        -> Val aenv
        -> Vector e
        -> CIO (Vector e)
scan1Op acc aenv (Array sh0 in0) = do
  (fvs,mdl,fscan,(t,g,m)) <- configure "inclusive_scan" acc aenv (size sh0)
  fadd                    <- liftIO $ CUDA.getFun mdl "inclusive_update"
  c                       <- getUseCount acc
  res@(Array _ out)       <- newArray c (Sugar.toElt sh0)
  (Array _ bks)           <- newArray 1 (Z :. g) :: CIO (Vector e)
  (Array _ sum)           <- newArray 1 Z        :: CIO (Scalar e)
  let n   = size sh0
      itv = (n + g - 1) `div` g
  --
  bindLifted mdl fvs
  launch (t,g,m) fscan ((((((),out),in0),bks),n),itv)   -- inclusive scan of input array
  launch (t,1,m) fscan ((((((),bks),bks),sum),g),itv)   -- inclusive scan block-level sums
  launch (t,g,m) fadd  (((((),out),bks),n),itv)         -- distribute partial results
  freeLifted fvs
  freeArray in0
  freeArray bks
  freeArray sum
  return res

permuteOp :: (Sugar.Elt e, Typeable aenv)
          => OpenAcc aenv (Array dim' e)
          -> Val aenv
          -> Array dim' e       -- default values
          -> Array dim e        -- permuted array
          -> CIO (Array dim' e)
permuteOp acc aenv (Array sh0 in0) (Array sh1 in1) = do
  res@(Array _ out) <- allocResult acc (Sugar.toElt sh0)
  copyArray in0 out (size sh0)
  execute "permute" acc aenv (size sh0) (((((),out),in1),convertIx sh0),convertIx sh1)
  freeArray in0
  freeArray in1
  return res

backpermuteOp :: (Sugar.Shape dim', Sugar.Elt e, Typeable aenv)
              => OpenAcc aenv (Array dim' e)
              -> Val aenv
              -> dim'
              -> Array dim e
              -> CIO (Array dim' e)
backpermuteOp acc aenv dim' (Array sh0 in0) = do
  res@(Array sh out) <- allocResult acc dim'
  execute "backpermute" acc aenv (size sh) (((((),out),in0),convertIx sh),convertIx sh0)
  freeArray in0
  return res

stencilOp :: (Sugar.Elt e, Typeable aenv)
          => OpenAcc aenv (Array dim e)
          -> Val aenv
          -> Array dim e'
          -> CIO (Array dim e)
stencilOp acc aenv ain0@(Array sh0 _) = do
  res@(Array _ out)      <- allocResult acc (Sugar.toElt sh0)
  (fvs,mdl,fstencil,cfg) <- configure "stencil1" acc aenv (size sh0)
  let fvs' = FreeArray ain0 : fvs                       -- input arrays read via texture references
  bindLifted mdl fvs'
  launch cfg fstencil (((),out),convertIx sh0)
  freeLifted fvs'
  return res

stencil2Op :: (Sugar.Elt e, Typeable aenv)
           => OpenAcc aenv (Array dim e)
           -> Val aenv
           -> Array dim e1
           -> Array dim e2
           -> CIO (Array dim e)
stencil2Op acc aenv ain1@(Array sh1 _) ain0@(Array sh0 _) = do
  res@(Array sh out)     <- allocResult acc $ Sugar.toElt (sh1 `intersect` sh0)
  (fvs,mdl,fstencil,cfg) <- configure "stencil2" acc aenv (size sh)
  let fvs' = FreeArray ain0 : FreeArray ain1 : fvs      -- input arrays read via texture references
  bindLifted mdl fvs'
  launch cfg fstencil (((((),out),convertIx sh),convertIx sh0),convertIx sh1)
  freeLifted fvs'
  return res



-- Expression evaluation
-- ---------------------

-- Evaluate an open expression
--
executeOpenExp :: Typeable aenv => OpenExp env aenv t -> Val env -> Val aenv -> CIO t
executeOpenExp (Var idx)         env _    = return . Sugar.toElt $ prj idx env
executeOpenExp (Const c)         _   _    = return $ Sugar.toElt c
executeOpenExp (PrimConst c)     _   _    = return $ I.evalPrimConst c
executeOpenExp (PrimApp fun arg) env aenv = I.evalPrim fun <$> executeOpenExp arg env aenv
executeOpenExp (Tuple tup)       env aenv = toTuple                   <$> executeTuple tup env aenv
executeOpenExp (Prj idx e)       env aenv = I.evalPrj idx . fromTuple <$> executeOpenExp e env aenv
executeOpenExp IndexNil          _   _    = return Z
executeOpenExp (IndexCons sh i)  env aenv = (:.) <$> executeOpenExp sh env aenv <*> executeOpenExp i env aenv
executeOpenExp (IndexHead ix)    env aenv = (\(_:.h) -> h) <$> executeOpenExp ix env aenv
executeOpenExp (IndexTail ix)    env aenv = (\(t:._) -> t) <$> executeOpenExp ix env aenv
executeOpenExp (IndexScalar a e) env aenv = do
  (Array sh ad) <- executeOpenAcc a aenv
  ix            <- executeOpenExp e env aenv
  res           <- Sugar.toElt <$> ad `indexArray` index sh (Sugar.fromElt ix)
  freeArray ad
  return res

executeOpenExp (Shape a) _ aenv = do
  (Array sh ad) <- executeOpenAcc a aenv
  freeArray ad
  return (Sugar.toElt sh)

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
executeExp :: Typeable aenv => Exp aenv t -> Val aenv -> CIO t
executeExp e = executeOpenExp e Empty


-- Tuple evaluation
--

executeTuple :: Typeable aenv => Tuple (OpenExp env aenv) t -> Val env -> Val aenv -> CIO t
executeTuple NilTup          _   _    = return ()
executeTuple (t `SnocTup` e) env aenv = (,) <$> executeTuple   t env aenv
                                            <*> executeOpenExp e env aenv

-- Array references in scalar code
-- -------------------------------

-- Lift array valued variables out of scalar computations. Returns a list of the
-- arrays in the order that they were encountered, corresponding to the order in
-- which the should be bound to the appropriate device module references; c.f.
-- code generation stage
--
data Lifted where
  FreeShape :: Shape sh => sh          -> Lifted
  FreeArray ::             Array dim e -> Lifted


liftAcc :: Typeable aenv => OpenAcc aenv a -> Val aenv -> CIO [Lifted]
liftAcc (Let  a b)           aenv = do
  a0 <- executeOpenAcc a aenv
  liftAcc b (aenv `Push` a0)

liftAcc (Let2 a b)           aenv = do
  (a1,a0) <- executeOpenAcc a aenv
  liftAcc b (aenv `Push` a1 `Push` a0)

liftAcc (Avar ix)            aenv = return $ applyR arrays (prj ix aenv)        -- TLM ??
  where
    applyR :: ArraysR arrs -> arrs -> [Lifted]
    applyR ArraysRunit         ()      = []
    applyR ArraysRarray        arr     = [FreeArray arr]
    applyR (ArraysRpair r1 r0) (a1,a0) = applyR r1 a1 ++ applyR r0 a0

liftAcc (Use _)              _    = return []
liftAcc (Unit _)             _    = return []
liftAcc (Reshape _ _)        _    = return []
liftAcc (Replicate _ _ _)    _    = return []
liftAcc (Index _ _ _)        _    = return []
liftAcc (Generate _ f)       aenv = liftFun f aenv
liftAcc (Map f _)            aenv = liftFun f aenv
liftAcc (ZipWith f _ _)      aenv = liftFun f aenv
liftAcc (Fold1 f _)          aenv = liftFun f aenv
liftAcc (Fold1Seg f _ _)     aenv = liftFun f aenv
liftAcc (Scanl1 f _)         aenv = liftFun f aenv
liftAcc (Scanr1 f _)         aenv = liftFun f aenv
liftAcc (Fold f e _)         aenv = concatM [liftExp e aenv, liftFun f aenv]
liftAcc (FoldSeg f e _ _)    aenv = concatM [liftExp e aenv, liftFun f aenv]
liftAcc (Scanl f e _)        aenv = concatM [liftExp e aenv, liftFun f aenv]
liftAcc (Scanr f e _)        aenv = concatM [liftExp e aenv, liftFun f aenv]
liftAcc (Scanl' f e _)       aenv = concatM [liftExp e aenv, liftFun f aenv]
liftAcc (Scanr' f e _)       aenv = concatM [liftExp e aenv, liftFun f aenv]
liftAcc (Permute f _ g _)    aenv = concatM [liftFun f aenv, liftFun g aenv]
liftAcc (Backpermute _ f _)  aenv = liftFun f aenv
liftAcc (Stencil f _ _)      aenv = liftFun f aenv
liftAcc (Stencil2 f _ _ _ _) aenv = liftFun f aenv


liftFun :: Typeable aenv => OpenFun env aenv a -> Val aenv -> CIO [Lifted]
liftFun (Lam  lam)  = liftFun lam
liftFun (Body body) = liftExp body

liftTup :: Typeable aenv => Tuple (OpenExp env aenv) t -> Val aenv -> CIO [Lifted]
liftTup NilTup          _    = return []
liftTup (t `SnocTup` e) aenv = (++) <$> liftTup t aenv <*> liftExp e aenv

liftExp :: Typeable aenv => OpenExp env aenv a -> Val aenv -> CIO [Lifted]
liftExp (Var _)           _    = return []
liftExp (Const _)         _    = return []
liftExp (PrimConst _)     _    = return []
liftExp (IndexNil)        _    = return []
liftExp (Tuple t)         aenv = liftTup t aenv
liftExp (Prj _ e)         aenv = liftExp e aenv
liftExp (IndexCons sh i)  aenv = concatM [liftExp sh aenv, liftExp i aenv]
liftExp (IndexHead ix)    aenv = liftExp ix aenv
liftExp (IndexTail ix)    aenv = liftExp ix aenv
liftExp (PrimApp _ e)     aenv = liftExp e aenv
liftExp (Cond p t e)      aenv = concatM [liftExp p aenv, liftExp t aenv, liftExp e aenv]
liftExp (Shape a)         aenv = do
  (Array sh _) <- executeOpenAcc a aenv
  return [FreeShape sh]

liftExp (IndexScalar a e) aenv = do
  vs               <- liftExp e aenv
  arr@(Array sh _) <- executeOpenAcc a aenv
  return $ vs ++ [FreeArray arr, FreeShape sh]
--  return $ FreeArray arr : FreeShape sh : vs

liftExp (Size a)          aenv = liftExp (Shape a) aenv


-- Bind array variables to the appropriate module references, where binding
-- names are simply derived "in order", c.f. code generation.
--
bindLifted :: CUDA.Module -> [Lifted] -> CIO ()
bindLifted mdl = foldM_ go (0,0) {-- . reverse --}
  where
    go :: (Int,Int) -> Lifted -> CIO (Int,Int)
    go (n,m) (FreeShape sh)            = bindDim n sh    >>  return (n+1,m)
    go (n,m) (FreeArray (Array sh ad)) = bindTex m sh ad >>= \m' -> return (n,m+m')

    bindDim :: Shape sh => Int -> sh -> CIO ()
    bindDim n sh = liftIO $
      CUDA.getPtr mdl ("sh"++show n) >>= \(p,_) ->
      CUDA.pokeListArray (convertIx sh) p

    bindTex :: (Shape sh, ArrayElt e) => Int -> sh -> AD.ArrayData e -> CIO Int
    bindTex m sh ad
      = let textures = sequence' $ map (CUDA.getTex mdl . ("tex"++) . show) [m..]
        in  marshalTextureData ad (size sh) =<< liftIO textures

-- Release arrays lifted from scalar expressions
--
freeLifted :: [Lifted] -> CIO ()
freeLifted = mapM_ go
  where go :: Lifted -> CIO ()
        go (FreeShape _)            = return ()
        go (FreeArray (Array _ ad)) = freeArray ad



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
  marshal x = marshal (fromIntegral x :: Int32)

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

instance ArrayElt e => Marshalable (AD.ArrayData e) where
  marshal = marshalArrayData    -- Marshalable (DevicePtrs a) does not type )=

instance Marshalable a => Marshalable [a] where
  marshal = concatMapM marshal

instance (Marshalable a, Marshalable b) => Marshalable (a,b) where
  marshal (a,b) = (++) <$> marshal a <*> marshal b


-- Link the binary object implementing the computation, configure the kernel
-- launch parameters, and initiate the computation. This also handles lifting
-- and binding of array references from scalar expressions.
--
execute :: (Typeable a, Typeable aenv, Marshalable args)
        => String
        -> OpenAcc aenv a
        -> Val aenv
        -> Int
        -> args
        -> CIO ()
execute name acc aenv n args =
  configure name acc aenv n >>= flip dispatch args

-- Pre-execution configuration and kernel linking
--
configure :: (Typeable a, Typeable aenv)
          => String
          -> OpenAcc aenv a
          -> Val aenv
          -> Int
          -> CIO ([Lifted], CUDA.Module, CUDA.Fun, (Int,Int,Integer))
configure name acc aenv n = do
  fvs <- liftAcc acc aenv
  mdl <- loadKernel acc
  fun <- liftIO $ CUDA.getFun mdl name
  cfg <- launchConfig acc n fun
  return (fvs, mdl, fun, cfg)


-- Binding of lifted array expressions and kernel invocation
--
dispatch :: Marshalable args
         => ([Lifted], CUDA.Module, CUDA.Fun, (Int,Int,Integer))
         -> args
         -> CIO ()
dispatch (fvs, mdl, fun, cfg) args = do
  bindLifted mdl fvs
  launch cfg fun args
  freeLifted fvs

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



-- Dynamic kernel loading
-- ----------------------

-- Retrieve the binary object associated with an AST node
--
loadKernel :: (Typeable a, Typeable aenv) => OpenAcc aenv a -> CIO CUDA.Module
loadKernel acc = do
  n <- lookupAccTable acc
  case n of
    Just e | Just mdl <- getL executable e -> return mdl
    Just e                                 -> cache e
    Nothing                                -> cache $ AccNode (1,0) Nothing
  where
    cache e =
      linkKernel acc >>= \mdl -> do
      updateAccTable acc $ setL executable (Just mdl) e
      return mdl


-- Link the CUDA binary object implementing the kernel for the given array
-- computation. This may entail waiting for the external compilation process.
--
linkKernel :: OpenAcc aenv a -> CIO CUDA.Module
linkKernel acc =
  let key           = accToKey acc
      either' e r l = either l r e
      intErr        = INTERNAL_ERROR(error) "loadKernel" "code generation failed"
  in do
    tab <- getM kernelTable
    krn <- fromMaybe intErr <$> liftIO (Hash.lookup tab key)
    either' (getL kernelStatus krn) return $ \pid -> liftIO $ do
      waitFor pid
      mdl <- CUDA.loadFile (getL kernelName krn `replaceExtension` ".cubin")
      Hash.insert tab key (setL kernelStatus (Right mdl) krn)
      return mdl

-- Wait for the compilation process to finish
--
waitFor :: ProcessID -> IO ()
waitFor pid = do
  status <- getProcessStatus True True pid
  case status of
       Just (Exited ExitSuccess) -> return ()
       _                         -> error  $ "nvcc (" ++ show pid ++ ") terminated abnormally"



-- Memory management
-- -----------------

-- Return the number of times the result of the given computation will be used.
-- The use map only contains entries for (let-bound) arrays that are used more
-- than once.
--
getUseCount :: (Sugar.Shape sh, Sugar.Elt e, Typeable aenv)
            => OpenAcc aenv (Array sh e)
            -> CIO Int
getUseCount acc = maybe 1 (fst . getL usecount) `fmap` lookupAccTable acc

-- Get the use count for each of the results of an array computation yielding
-- two arrays. The use map should definitely have an entry for computations of
-- this type.
--
getUseCount2 :: (Sugar.Shape sh1, Sugar.Shape sh2, Sugar.Elt a, Sugar.Elt b, Typeable aenv)
             => OpenAcc aenv (Array sh1 a, Array sh2 b)
             -> CIO (Int, Int)
getUseCount2 acc =
  let err = INTERNAL_ERROR(error) "getUseCount2" "assumption failed"
  in  maybe err (getL usecount) `fmap` lookupAccTable acc


-- Convenience function for allocating the output array for a given computation,
-- with appropriate reference counting information
--
allocResult :: (Sugar.Shape sh, Sugar.Elt e, Typeable aenv)
            => OpenAcc aenv (Array sh e)
            -> sh
            -> CIO (Array sh e)
allocResult acc sh = flip newArray sh =<< getUseCount acc


-- Allocate a new device array to accompany the given host-side Accelerate
-- array, of given shape and reference count.
--
newArray :: (Sugar.Shape sh, Sugar.Elt e)
         => Int                         -- use/reference count
         -> sh                          -- shape
         -> CIO (Array sh e)
newArray rc sh = do
  ad `seq` mallocArray ad (Just rc) (1 `max` n)
  return $ Array (Sugar.fromElt sh) ad
  where
    n      = Sugar.size sh
    (ad,_) = AD.runArrayData $ (,undefined) `fmap` AD.newArrayData (1024 `max` n)
      -- FIXME: small arrays moved by the GC



-- Stable names
-- ------------

lookupAccTable :: (Typeable aenv, Typeable a) => OpenAcc aenv a -> CIO (Maybe AccNode)
lookupAccTable acc = do
  table <- getM computeTable
  liftIO $ Hash.lookup table =<< makeStableAcc acc

updateAccTable :: (Typeable aenv, Typeable a) => OpenAcc aenv a -> AccNode -> CIO ()
updateAccTable acc val = do
  tab <- getM computeTable
  key <- liftIO $ makeStableAcc acc
  liftIO $ Hash.update tab key val >> return ()

makeStableAcc :: (Typeable aenv, Typeable a) => OpenAcc aenv a -> IO StableAccName
makeStableAcc a = StableAccName `fmap` makeStableName a


-- Auxiliary functions
-- -------------------

-- Fold over a boolean value, analogous to 'maybe' and 'either'
--
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True  = y

-- Special version of msum, which doesn't behave as we would like for CIO [a]
--
concatM :: Monad m => [m [a]] -> m [a]
concatM ms = concat `liftM` sequence ms

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
convertIx :: Shape sh => sh -> [Int32]
convertIx = post . map fromIntegral . shapeToList
  where post [] = [1]
        post xs = reverse xs

