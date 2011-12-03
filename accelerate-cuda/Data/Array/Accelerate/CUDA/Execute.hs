{-# LANGUAGE BangPatterns, CPP, GADTs, ScopedTypeVariables, FlexibleInstances #-}
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
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Representation               hiding (Shape, sliceIndex)
import qualified Data.Array.Accelerate.Interpreter              as I
import qualified Data.Array.Accelerate.Array.Data               as AD
import qualified Data.Array.Accelerate.Array.Representation     as R

import Data.Array.Accelerate.CUDA.AST
import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.Array.Sugar                   hiding
   (dim, size, index, shapeToList, sliceIndex)
import Data.Array.Accelerate.CUDA.Analysis.Launch
import qualified Data.Array.Accelerate.CUDA.Array.Sugar         as Sugar

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
executeAfun1 :: (Arrays a, Arrays b) => ExecAfun (a -> b) -> a -> CIO b
executeAfun1 (Alam (Abody f)) arrs = do
  applyArraysR useArray arrays arrs
  executeOpenAcc f (Empty `Push` arrs)

executeAfun1 _ _                   =
  error "the sword comes out after you swallow it, right?"


-- Evaluate an open array expression
--
executeOpenAcc :: ExecOpenAcc aenv a -> Val aenv -> CIO a
executeOpenAcc (ExecAcc kernel _ bindings acc) aenv =
  case acc of
    --
    -- (1) Array introduction
    --
    Use arr -> return arr

    --
    -- (2) Environment manipulation
    --
    Avar ix  -> return (prj ix aenv)

    Alet  a b -> do
      a0 <- executeOpenAcc a aenv
      executeOpenAcc b (aenv `Push` a0)

    Alet2 a b -> do
      (a1, a0) <- executeOpenAcc a aenv
      executeOpenAcc b (aenv `Push` a1 `Push` a0)

    PairArrays a b ->
      (,) <$> executeOpenAcc a aenv
          <*> executeOpenAcc b aenv

    Apply (Alam (Abody f)) a -> do
      a0 <- executeOpenAcc a aenv
      executeOpenAcc f (Empty `Push` a0)
    Apply _ _   -> error "Awww... the sky is crying"

    Acond p t e -> do
      cond <- executeExp p aenv
      if cond then executeOpenAcc t aenv
              else executeOpenAcc e aenv

    Reshape e a -> do
      ix <- executeExp e aenv
      a0 <- executeOpenAcc a aenv
      reshapeOp ix a0

    Unit e ->
      unitOp =<< executeExp e aenv

    --
    -- (3) Array computations
    --
    Generate e _        ->
      generateOp kernel bindings acc aenv =<< executeExp e aenv

    Replicate sliceIndex e a -> do
      slix <- executeExp e aenv
      a0   <- executeOpenAcc a aenv
      replicateOp kernel bindings acc aenv sliceIndex slix a0

    Index sliceIndex a e -> do
      slix <- executeExp e aenv
      a0   <- executeOpenAcc a aenv
      indexOp kernel bindings acc aenv sliceIndex a0 slix

    Map _ a             -> do
      a0 <- executeOpenAcc a aenv
      mapOp kernel bindings acc aenv a0

    ZipWith _ a b       -> do
      a1 <- executeOpenAcc a aenv
      a0 <- executeOpenAcc b aenv
      zipWithOp kernel bindings acc aenv a1 a0

    Fold _ _ a          -> do
      a0 <- executeOpenAcc a aenv
      foldOp kernel bindings acc aenv a0

    Fold1 _ a           -> do
      a0 <- executeOpenAcc a aenv
      foldOp kernel bindings acc aenv a0

    FoldSeg _ _ a s     -> do
      a0 <- executeOpenAcc a aenv
      s0 <- executeOpenAcc s aenv
      foldSegOp kernel bindings acc aenv a0 s0

    Fold1Seg _ a s      -> do
      a0 <- executeOpenAcc a aenv
      s0 <- executeOpenAcc s aenv
      foldSegOp kernel bindings acc aenv a0 s0

    Scanl _ _ a         -> do
      a0 <- executeOpenAcc a aenv
      scanOp kernel bindings acc aenv a0

    Scanl' _ _ a        -> do
      a0 <- executeOpenAcc a aenv
      scan'Op kernel bindings acc aenv a0

    Scanl1 _ a          -> do
      a0 <- executeOpenAcc a aenv
      scan1Op kernel bindings acc aenv a0

    Scanr _ _ a         -> do
      a0 <- executeOpenAcc a aenv
      scanOp kernel bindings acc aenv a0

    Scanr' _ _ a        -> do
      a0 <- executeOpenAcc a aenv
      scan'Op kernel bindings acc aenv a0

    Scanr1 _ a          -> do
      a0 <- executeOpenAcc a aenv
      scan1Op kernel bindings acc aenv a0

    Permute _ a _ b     -> do
      a0 <- executeOpenAcc a aenv
      a1 <- executeOpenAcc b aenv
      permuteOp kernel bindings acc aenv a0 a1

    Backpermute e _ a   -> do
      sh <- executeExp e aenv
      a0 <- executeOpenAcc a aenv
      backpermuteOp kernel bindings acc aenv sh a0

    Stencil _ _ a       -> do
      a0 <- executeOpenAcc a aenv
      stencilOp kernel bindings acc aenv a0

    Stencil2 _ _ a _ b  -> do
      a1 <- executeOpenAcc a aenv
      a0 <- executeOpenAcc b aenv
      stencil2Op kernel bindings acc aenv a1 a0


-- Implementation of primitive array operations
-- --------------------------------------------

reshapeOp :: Shape dim
          => dim
          -> Array dim' e
          -> CIO (Array dim e)
reshapeOp newShape (Array oldShape adata)
  = BOUNDS_CHECK(check) "reshape" "shape mismatch" (Sugar.size newShape == size oldShape)
  $ return $ Array (fromElt newShape) adata


unitOp :: Elt e
       => e
       -> CIO (Scalar e)
unitOp v = newArray Z (const v)


generateOp :: (Shape dim, Elt e)
           => AccKernel a
           -> [AccBinding aenv]
           -> PreOpenAcc ExecOpenAcc aenv (Array dim e)
           -> Val aenv
           -> dim
           -> CIO (Array dim e)
generateOp kernel bindings acc aenv sh = do
  res@(Array s out) <- allocateArray sh
  execute kernel bindings acc aenv (Sugar.size sh) (((),out),convertIx s)
  return res


replicateOp :: (Shape dim, Elt slix)
            => AccKernel (Array dim e)
            -> [AccBinding aenv]
            -> PreOpenAcc ExecOpenAcc aenv (Array dim e)
            -> Val aenv
            -> SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr dim)
            -> slix
            -> Array sl e
            -> CIO (Array dim e)
replicateOp kernel bindings acc aenv sliceIndex slix (Array sh0 in0) = do
  res@(Array sh out) <- allocateArray (toElt $ extend sliceIndex (fromElt slix) sh0)
  execute kernel bindings acc aenv (size sh) (((((),out),in0),convertIx sh0),convertIx sh)
  return res
  where
    extend :: SliceIndex slix sl co dim -> slix -> sl -> dim
    extend (SliceNil)            ()       ()      = ()
    extend (SliceAll sliceIdx)   (slx,()) (sl,sz) = (extend sliceIdx slx sl, sz)
    extend (SliceFixed sliceIdx) (slx,sz) sl      = (extend sliceIdx slx sl, sz)


indexOp :: (Shape sl, Elt slix)
        => AccKernel (Array dim e)
        -> [AccBinding aenv]
        -> PreOpenAcc ExecOpenAcc aenv (Array sl e)
        -> Val aenv
        -> SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr dim)
        -> Array dim e
        -> slix
        -> CIO (Array sl e)
indexOp kernel bindings acc aenv sliceIndex (Array sh0 in0) slix = do
  res@(Array sh out) <- allocateArray (toElt $ restrict sliceIndex (fromElt slix) sh0)
  execute kernel bindings acc aenv (size sh)
    ((((((),out),in0),convertIx sh),convertSlix sliceIndex (fromElt slix)),convertIx sh0)
  return res
  where
    restrict :: SliceIndex slix sl co dim -> slix -> dim -> sl
    restrict (SliceNil)            ()       ()      = ()
    restrict (SliceAll sliceIdx)   (slx,()) (sh,sz) = (restrict sliceIdx slx sh, sz)
    restrict (SliceFixed sliceIdx) (slx,i)  (sh,sz)
      = BOUNDS_CHECK(checkIndex) "slice" i sz $ restrict sliceIdx slx sh
    --
    convertSlix :: SliceIndex slix sl co dim -> slix -> [Int]
    convertSlix (SliceNil)            ()     = []
    convertSlix (SliceAll   sliceIdx) (s,()) = convertSlix sliceIdx s
    convertSlix (SliceFixed sliceIdx) (s,i)  = i : convertSlix sliceIdx s


mapOp :: Elt e
      => AccKernel (Array dim e)
      -> [AccBinding aenv]
      -> PreOpenAcc ExecOpenAcc aenv (Array dim e)
      -> Val aenv
      -> Array dim e'
      -> CIO (Array dim e)
mapOp kernel bindings acc aenv (Array sh0 in0) = do
  res@(Array _ out) <- allocateArray (toElt sh0)
  execute kernel bindings acc aenv (size sh0) ((((),out),in0),size sh0)
  return res

zipWithOp :: Elt c
          => AccKernel (Array dim c)
          -> [AccBinding aenv]
          -> PreOpenAcc ExecOpenAcc aenv (Array dim c)
          -> Val aenv
          -> Array dim a
          -> Array dim b
          -> CIO (Array dim c)
zipWithOp kernel bindings acc aenv (Array sh1 in1) (Array sh0 in0) = do
  res@(Array sh out) <- allocateArray $ toElt (sh1 `intersect` sh0)
  execute kernel bindings acc aenv (size sh) (((((((),out),in1),in0),convertIx sh),convertIx sh1),convertIx sh0)
  return res

foldOp :: forall dim e aenv. Shape dim
       => AccKernel (Array dim e)
       -> [AccBinding aenv]
       -> PreOpenAcc ExecOpenAcc aenv (Array dim e)
       -> Val aenv
       -> Array (dim:.Int) e
       -> CIO (Array dim e)
foldOp kernel bindings acc aenv (Array sh0 in0)
  -- A recursive multi-block reduction when collapsing to a single value
  --
  | dim sh0 == 1 = do
      cfg@(_,_,(_,g,_)) <- configure kernel acc (size sh0)
      res@(Array _ out) <- allocateArray (toElt (fst sh0,g)) :: CIO (Array (dim:.Int) e)
      dispatch cfg bindings aenv ((((),out),in0),size sh0)
      if g > 1 then foldOp kernel bindings acc aenv res
               else return (Array (fst sh0) out)
  --
  -- Reduction over the innermost dimension of an array (single pass operation)
  --
  | otherwise    = do
      res@(Array sh out) <- allocateArray $ toElt (fst sh0)
      execute kernel bindings acc aenv (size (fst sh0)) (((((),out),in0),convertIx sh),convertIx sh0)
      return res

foldSegOp :: Shape dim
          => AccKernel (Array dim e)
          -> [AccBinding aenv]
          -> PreOpenAcc ExecOpenAcc aenv (Array (dim:.Int) e)
          -> Val aenv
          -> Array (dim:.Int) e
          -> Segments
          -> CIO (Array (dim:.Int) e)
foldSegOp kernel bindings acc aenv (Array sh0 in0) (Array shs seg) = do
  res@(Array sh out) <- allocateArray $ toElt (fst sh0, size shs-1)
  execute kernel bindings acc aenv (size sh) ((((((),out),in0),seg),convertIx sh),convertIx sh0)
  return res


scanOp :: forall aenv e. Elt e
       => AccKernel (Vector e)
       -> [AccBinding aenv]
       -> PreOpenAcc ExecOpenAcc aenv (Vector e)
       -> Val aenv
       -> Vector e
       -> CIO (Vector e)
scanOp kernel bindings acc aenv (Array sh0 in0) = do
  (mdl,fscan,(t,g,m)) <- configure kernel acc (size sh0)
  fadd                <- liftIO $ CUDA.getFun mdl "exclusive_update"
  res@(Array _ out)   <- allocateArray (Z :. size sh0 + 1)
  (Array _ bks)       <- allocateArray (Z :. g) :: CIO (Vector e)
  (Array _ sum)       <- allocateArray Z        :: CIO (Scalar e)
  let n   = size sh0
      itv = (n + g - 1) `div` g
  --
  bindLifted mdl aenv bindings
  launch (t,g,m) fscan ((((((),out),in0),bks),n),itv)   -- inclusive scan of input array
  launch (t,1,m) fscan ((((((),bks),bks),sum),g),itv)   -- inclusive scan block-level sums
  launch (t,g,m) fadd  ((((((),out),bks),sum),n),itv)   -- distribute partial results
  return res

scan'Op :: forall aenv e. Elt e
        => AccKernel (Vector e)
        -> [AccBinding aenv]
        -> PreOpenAcc ExecOpenAcc aenv (Vector e, Scalar e)
        -> Val aenv
        -> Vector e
        -> CIO (Vector e, Scalar e)
scan'Op kernel bindings acc aenv (Array sh0 in0) = do
  (mdl,fscan,(t,g,m)) <- configure kernel acc (size sh0)
  fadd                <- liftIO $ CUDA.getFun mdl "exclusive_update"
  res1@(Array _ out)  <- allocateArray (toElt sh0)
  res2@(Array _ sum)  <- allocateArray Z
  (Array _ bks)       <- allocateArray  (Z :. g) :: CIO (Vector e)
  let n   = size sh0
      itv = (n + g - 1) `div` g
  --
  bindLifted mdl aenv bindings
  launch (t,g,m) fscan ((((((),out),in0),bks),n),itv)   -- inclusive scan of input array
  launch (t,1,m) fscan ((((((),bks),bks),sum),g),itv)   -- inclusive scan block-level sums
  launch (t,g,m) fadd  ((((((),out),bks),sum),n),itv)   -- distribute partial results
  return (res1,res2)

scan1Op :: forall aenv e. Elt e
        => AccKernel (Vector e)
        -> [AccBinding aenv]
        -> PreOpenAcc ExecOpenAcc aenv (Vector e)
        -> Val aenv
        -> Vector e
        -> CIO (Vector e)
scan1Op kernel bindings acc aenv (Array sh0 in0) = do
  (mdl,fscan,(t,g,m)) <- configure kernel acc (size sh0)
  fadd                <- liftIO $ CUDA.getFun mdl "inclusive_update"
  res@(Array _ out)   <- allocateArray (toElt sh0)
  (Array _ bks)       <- allocateArray (Z :. g) :: CIO (Vector e)
  (Array _ sum)       <- allocateArray Z        :: CIO (Scalar e)
  let n   = size sh0
      itv = (n + g - 1) `div` g
  --
  bindLifted mdl aenv bindings
  launch (t,g,m) fscan ((((((),out),in0),bks),n),itv)   -- inclusive scan of input array
  launch (t,1,m) fscan ((((((),bks),bks),sum),g),itv)   -- inclusive scan block-level sums
  launch (t,g,m) fadd  (((((),out),bks),n),itv)         -- distribute partial results
  return res

permuteOp :: Elt e
          => AccKernel (Array dim e)
          -> [AccBinding aenv]
          -> PreOpenAcc ExecOpenAcc aenv (Array dim' e)
          -> Val aenv
          -> Array dim' e       -- default values
          -> Array dim e        -- permuted array
          -> CIO (Array dim' e)
permuteOp kernel bindings acc aenv in0@(Array sh0 _) (Array sh1 in1) = do
  res@(Array _ out) <- allocateArray (toElt sh0)
  copyArray in0 res
  execute kernel bindings acc aenv (size sh0) (((((),out),in1),convertIx sh0),convertIx sh1)
  return res

backpermuteOp :: (Shape dim', Elt e)
              => AccKernel (Array dim e)
              -> [AccBinding aenv]
              -> PreOpenAcc ExecOpenAcc aenv (Array dim' e)
              -> Val aenv
              -> dim'
              -> Array dim e
              -> CIO (Array dim' e)
backpermuteOp kernel bindings acc aenv dim' (Array sh0 in0) = do
  res@(Array sh out) <- allocateArray dim'
  execute kernel bindings acc aenv (size sh) (((((),out),in0),convertIx sh),convertIx sh0)
  return res

stencilOp :: Elt e
          => AccKernel (Array dim e)
          -> [AccBinding aenv]
          -> PreOpenAcc ExecOpenAcc aenv (Array dim e)
          -> Val aenv
          -> Array dim e'
          -> CIO (Array dim e)
stencilOp kernel bindings acc aenv in0@(Array sh0 _) = do
  res@(Array _ out)  <- allocateArray (toElt sh0)
  (mdl,fstencil,cfg) <- configure kernel acc (size sh0)
  bindLifted mdl aenv bindings
  bindStencil 0 mdl in0
  launch cfg fstencil (((),out),convertIx sh0)
  return res

stencil2Op :: Elt e
           => AccKernel (Array dim e)
           -> [AccBinding aenv]
           -> PreOpenAcc ExecOpenAcc aenv (Array dim e)
           -> Val aenv
           -> Array dim e1
           -> Array dim e2
           -> CIO (Array dim e)
stencil2Op kernel bindings acc aenv in1@(Array sh1 _) in0@(Array sh0 _) = do
  res@(Array sh out) <- allocateArray $ toElt (sh1 `intersect` sh0)
  (mdl,fstencil,cfg) <- configure kernel acc (size sh)
  bindLifted mdl aenv bindings
  bindStencil 0 mdl in0
  bindStencil 1 mdl in1
  launch cfg fstencil (((((),out),convertIx sh),convertIx sh1),convertIx sh0)
  return res


-- Expression evaluation
-- ---------------------

-- Evaluate an open expression
--
executeOpenExp :: PreOpenExp ExecOpenAcc env aenv t -> Val env -> Val aenv -> CIO t
executeOpenExp (Let _ _)         _   _    = INTERNAL_ERROR(error) "executeOpenExp" "Let: not implemented yet"
executeOpenExp (Var idx)         env _    = return $ prj idx env
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
  arr <- executeOpenAcc a aenv
  ix  <- executeOpenExp e env aenv
  indexArray arr ix

executeOpenExp (Shape a) _ aenv = do
  (Array sh _) <- executeOpenAcc a aenv
  return (toElt sh)

executeOpenExp (ShapeSize e) env aenv = do
  sh <- executeOpenExp e env aenv
  return (size $ fromElt sh)

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


bindAcc :: CUDA.Module
        -> Val aenv
        -> AccBinding aenv
        -> CIO ()
bindAcc mdl aenv (ArrayVar idx) =
  let idx'        = show $ deBruijnToInt idx
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

#define primMarshalable(ty)                                                    \
instance Marshalable (ty) where {                                              \
  marshal x = return [CUDA.VArg x] }

primMarshalable(Int)
primMarshalable(Int8)
primMarshalable(Int16)
primMarshalable(Int32)
primMarshalable(Int64)
primMarshalable(Word)
primMarshalable(Word8)
primMarshalable(Word16)
primMarshalable(Word32)
primMarshalable(Word64)
primMarshalable(Float)
primMarshalable(Double)
primMarshalable(Ptr a)
primMarshalable(CUDA.DevicePtr a)

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
          -> CIO (CUDA.Module, CUDA.Fun, (Int,Int,Int))
configure (name, kernel) acc n = do
  mdl <- kernel
  fun <- liftIO $ CUDA.getFun mdl name
  cfg <- launchConfig acc n fun
  return (mdl, fun, cfg)


-- Binding of lifted array expressions and kernel invocation
--
dispatch :: Marshalable args
         => (CUDA.Module, CUDA.Fun, (Int,Int,Int))
         -> [AccBinding aenv]
         -> Val aenv
         -> args
         -> CIO ()
dispatch (mdl, fun, cfg) fvs aenv args = do
  bindLifted mdl aenv fvs
  launch cfg fun args

-- Execute a device function, with the given thread configuration and function
-- parameters. The tuple contains (threads per block, grid size, shared memory)
--
launch :: Marshalable args => (Int,Int,Int) -> CUDA.Fun -> args -> CIO ()
launch (cta,grid,smem) fn a = do
  args  <- marshal a
  liftIO $ CUDA.launchKernel' fn (grid,1,1) (cta,1,1) smem Nothing args


-- Auxiliary functions
-- -------------------

-- Generalise concatMap for teh monadz
--
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat `liftM` mapM f xs

-- A lazier version of 'Control.Monad.sequence'
--
sequence' :: [IO a] -> IO [a]
sequence' = foldr k (return [])
  where k m ms = do { x <- m; xs <- unsafeInterleaveIO ms; return (x:xs) }

-- Extract shape dimensions as a list of integers. Singleton dimensions are
-- considered to be of unit size.
--
-- Internally, Accelerate uses snoc-based tuple projection, while the data
-- itself is stored in reading order. Ensure we match the behaviour of regular
-- tuples and code generation thereof.
--
convertIx :: R.Shape sh => sh -> [Int]
convertIx = post . shapeToList
  where post [] = [1]
        post xs = reverse xs


-- Apply a function to all components of an Arrays structure
--
applyArraysR
    :: (forall sh e. (Shape sh, Elt e) => Array sh e -> CIO ())
    -> ArraysR arrs
    -> arrs
    -> CIO ()
applyArraysR _ ArraysRunit         ()       = return ()
applyArraysR f (ArraysRpair r1 r0) (a1, a0) = applyArraysR f r1 a1 >> applyArraysR f r0 a0
applyArraysR f ArraysRarray        arr      = f arr

