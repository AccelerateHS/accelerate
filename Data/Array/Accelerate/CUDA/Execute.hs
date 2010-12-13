{-# LANGUAGE CPP, GADTs, TypeSynonymInstances, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeOperators  #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Execute
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Execute (executeAcc)
  where

import Prelude hiding (id, (.), sum)
import Control.Category

import Data.Int
import Data.Word
import Data.Maybe
import Control.Monad
import Control.Monad.Trans                              (liftIO)
import Control.Applicative                              hiding (Const)
import qualified Data.HashTable                         as Hash

import System.FilePath                                  hiding (combine)
import System.Posix.Process
import System.Exit                                      (ExitCode(..))
import System.Posix.Types                               (ProcessID)
import System.IO.Unsafe

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Analysis.Shape
import Data.Array.Accelerate.Array.Representation       hiding (sliceIndex)
import Data.Array.Accelerate.Array.Sugar                (Array(..),Segments,Scalar,Vector,Z(..),(:.)(..))
import qualified Data.Array.Accelerate.Array.Data       as AD
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar
import qualified Data.Array.Accelerate.Interpreter      as I

import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.Analysis.Hash
import Data.Array.Accelerate.CUDA.Analysis.Launch

import Foreign.Ptr (Ptr)
import qualified Foreign.CUDA.Driver                    as CUDA

#include "accelerate.h"


-- Array evaluation
-- ----------------

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
executeAcc :: Acc a -> CIO a
executeAcc acc = executeOpenAcc acc Empty

executeOpenAcc :: OpenAcc aenv a -> Val aenv -> CIO a
---- (1) Array introduction ----
executeOpenAcc (Use a)    _    = return a

---- (2) Non-skeleton nodes ----
executeOpenAcc (Avar ix)  aenv = return (prj ix aenv)

executeOpenAcc (Let  x y) aenv = do
  a0  <- executeOpenAcc x aenv
  acc <- withBoundArrays a0 $ executeOpenAcc y (aenv `Push` a0)
  withBoundArrays acc       $ freeArraysR a0
  return acc

executeOpenAcc (Let2 x y) aenv = do
  (a1,a0) <- executeOpenAcc x aenv
  acc     <- withBoundArrays2 a1 a0 $ executeOpenAcc y (aenv `Push` a1 `Push` a0)
  withBoundArrays acc               $ freeArraysR a1 >> freeArraysR a0
  return acc

executeOpenAcc (Reshape e a) aenv = do
  ix            <- executeExp e aenv
  (Array sh ad) <- executeOpenAcc a aenv
  BOUNDS_CHECK(check) "reshape" "shape mismatch" (Sugar.size ix == size sh)
    $ return (Array (Sugar.fromElt ix) ad)

executeOpenAcc (Unit e) aenv = do
  v <- executeExp e aenv
  let ad = fst . AD.runArrayData $ (,undefined) <$> do
        arr <- AD.newArrayData 1024    -- FIXME: small arrays moved by the GC
        AD.writeArrayData arr 0 (Sugar.fromElt v)
        return arr
  mallocArray    ad 1
  pokeArrayAsync ad 1 Nothing
  return (Array (Sugar.fromElt ()) ad)

---- (3) Array computations ----
executeOpenAcc acc@(Generate e _) aenv = do
  sh              <- executeExp e aenv
  r@(Array s out) <- newArray sh
  let n = size s
  execute "generate" acc aenv n ((((),out),convertIx s),n)
  return r

executeOpenAcc acc@(Map _ a0) aenv = do
  (Array sh0 in0) <- executeOpenAcc a0 aenv
  r@(Array _ out) <- newArray (Sugar.toElt sh0)
  let n = size sh0
  execute "map" acc aenv n ((((),out),in0),n)
  freeArray in0
  return r

executeOpenAcc acc@(ZipWith _ a1 a0) aenv = do
  (Array sh1 in1) <- executeOpenAcc a1 aenv
  (Array sh0 in0) <- executeOpenAcc a0 aenv
  r@(Array s out) <- newArray (Sugar.toElt (sh1 `intersect` sh0))
  execute "zipWith" acc aenv (size s) (((((((),out),in1),in0),convertIx s),convertIx sh1),convertIx sh0)
  freeArray in1
  freeArray in0
  return r

executeOpenAcc acc@(Fold _ _ a0) aenv =
  if accDim a0 == 1 then executeFoldAll acc a0 aenv
                    else executeFold    acc a0 aenv

executeOpenAcc acc@(Fold1 _ a0) aenv =
  if accDim a0 == 1 then executeFoldAll acc a0 aenv
                    else executeFold    acc a0 aenv

executeOpenAcc acc@(FoldSeg _ _ a0 s0) aenv = executeFoldSeg acc a0 s0 aenv
executeOpenAcc acc@(Fold1Seg  _ a0 s0) aenv = executeFoldSeg acc a0 s0 aenv

executeOpenAcc acc@(Scanr  _ _ a0) aenv = executeScan  acc a0 aenv
executeOpenAcc acc@(Scanr' _ _ a0) aenv = executeScan' acc a0 aenv
executeOpenAcc acc@(Scanr1 _ a0)   aenv = executeScan1 acc a0 aenv

executeOpenAcc acc@(Scanl  _ _ a0) aenv = executeScan  acc a0 aenv
executeOpenAcc acc@(Scanl' _ _ a0) aenv = executeScan' acc a0 aenv
executeOpenAcc acc@(Scanl1 _ a0)   aenv = executeScan1 acc a0 aenv

executeOpenAcc acc@(Permute _ a0 _ a1) aenv = do
  (Array sh0 in0) <- executeOpenAcc a0 aenv     -- default values
  (Array sh1 in1) <- executeOpenAcc a1 aenv     -- permuted array
  r@(Array _ out) <- newArray (Sugar.toElt sh0)
  copyArray in0 out (size sh0)
  execute "permute" acc aenv (size sh0) (((((),out),in1),convertIx sh0),convertIx sh1)
  freeArray in0
  freeArray in1
  return r

executeOpenAcc acc@(Backpermute e _ a0) aenv = do
  dim'            <- executeExp e aenv
  (Array sh0 in0) <- executeOpenAcc a0 aenv
  r@(Array s out) <- newArray dim'
  execute "backpermute" acc aenv (size s) (((((),out),in0),convertIx s),convertIx sh0)
  freeArray in0
  return r

executeOpenAcc acc@(Replicate sliceIndex e a0) aenv = do
  let extend :: SliceIndex slix sl co dim -> slix -> sl -> dim
      extend (SliceNil)            ()       ()      = ()
      extend (SliceAll sliceIdx)   (slx,()) (sl,sz) = (extend sliceIdx slx sl, sz)
      extend (SliceFixed sliceIdx) (slx,sz) sl      = (extend sliceIdx slx sl, sz)

  slix            <- executeExp e aenv
  (Array sh0 in0) <- executeOpenAcc a0 aenv
  r@(Array s out) <- newArray (Sugar.toElt $ extend sliceIndex (Sugar.fromElt slix) sh0)
  execute "replicate" acc aenv (size s) (((((),out),in0),convertIx sh0),convertIx s)
  freeArray in0
  return r

executeOpenAcc acc@(Index sliceIndex a0 e) aenv = do
  let restrict :: SliceIndex slix sl co dim -> slix -> dim -> sl
      restrict (SliceNil)            ()       ()      = ()
      restrict (SliceAll sliceIdx)   (slx,()) (sh,sz) = (restrict sliceIdx slx sh, sz)
      restrict (SliceFixed sliceIdx) (slx,i)  (sh,sz)
        = BOUNDS_CHECK(checkIndex) "slice" i sz $ restrict sliceIdx slx sh

      convertSlix :: SliceIndex slix sl co dim -> slix -> [Int32]
      convertSlix (SliceNil)            ()     = []
      convertSlix (SliceAll   sliceIdx) (s,()) = convertSlix sliceIdx s
      convertSlix (SliceFixed sliceIdx) (s,i)  = fromIntegral i : convertSlix sliceIdx s

  slix            <- executeExp e aenv
  (Array sh0 in0) <- executeOpenAcc a0 aenv
  r@(Array s out) <- newArray (Sugar.toElt $ restrict sliceIndex (Sugar.fromElt slix) sh0)
  execute "slice" acc aenv (size s)
    ((((((),out),in0),convertIx s),convertSlix sliceIndex (Sugar.fromElt slix)),convertIx sh0)
  freeArray in0
  return r

executeOpenAcc _acc@(Stencil _ _ _a0) _aenv
  = INTERNAL_ERROR(error) "executeOpenAcc" "Stencil NOT YET IMPLEMENTED"
executeOpenAcc _acc@(Stencil2 _ _ _ _ _a0) _aenv
  = INTERNAL_ERROR(error) "executeOpenAcc" "Stencil2 NOT YET IMPLEMENTED"


-- Reduction
--
executeFoldAll :: Sugar.Shape dim
  => OpenAcc aenv (Array dim e)         -- dim ~ Z
  -> OpenAcc aenv (Array (dim:.Int) e)
  -> Val aenv
  -> CIO (Array dim e)
executeFoldAll acc a0 aenv = do
  (Array sh0 in0)   <- executeOpenAcc a0 aenv
  c@(_,_,_,(_,g,_)) <- configure "fold" acc aenv (size sh0)
  r@(Array _ out)   <- newArray (Sugar.toElt (fst sh0,g))
  dispatch c ((((),out),in0),size sh0)
  freeArray in0
  if g > 1 then executeFoldAll acc (Use r) aenv
           else return (Array (fst sh0) out)

executeFold :: Sugar.Shape dim
  => OpenAcc aenv (Array dim e)
  -> OpenAcc aenv (Array (dim:.Int) e)
  -> Val aenv
  -> CIO (Array dim e)
executeFold acc a0 aenv = do
  (Array sh0 in0) <- executeOpenAcc a0 aenv
  r@(Array s out) <- newArray (Sugar.toElt $ fst sh0)
  execute "fold" acc aenv (size (fst sh0)) (((((),out),in0),convertIx s),convertIx sh0)
  freeArray in0
  return r

-- Segmented Reduction
--
executeFoldSeg
  :: Sugar.Shape dim
  => OpenAcc aenv (Array (dim:.Int) e)
  -> OpenAcc aenv (Array (dim:.Int) e)
  -> OpenAcc aenv Segments
  -> Val aenv
  -> CIO (Array (dim:.Int) e)
executeFoldSeg acc a0 s0 aenv = do
  (Array sh0 in0) <- executeOpenAcc a0 aenv
  (Array shs seg) <- executeOpenAcc s0 aenv >>= flip executeOpenAcc aenv . scan
  r@(Array s out) <- newArray (Sugar.toElt (fst sh0,size shs - 1))
  execute "foldSeg" acc aenv (size s) ((((((),out),in0),seg),convertIx s),convertIx sh0)
  freeArray in0
  freeArray seg
  return r
  where
    scan = Scanl add (Const ((),0)) . Use
    add  = Lam (Lam (Body (PrimAdd numType
                          `PrimApp`
                          Tuple (NilTup `SnocTup` (Var (SuccIdx ZeroIdx))
                                        `SnocTup` (Var ZeroIdx)))))


-- Left and right scan variants
--
executeScan :: forall aenv e.
     OpenAcc aenv (Vector e)
  -> OpenAcc aenv (Vector e)
  -> Val aenv
  -> CIO (Vector e)
executeScan acc a0 aenv = do
  (Array sh0 in0)         <- executeOpenAcc a0 aenv
  (fvs,mdl,fscan,(t,g,m)) <- configure "inclusive_scan" acc aenv (size sh0)
  fadd                    <- liftIO $ CUDA.getFun mdl "exclusive_update"
  a@(Array _ out)         <- newArray (Z :. size sh0 + 1)
  (Array _ bks)           <- newArray (Z :. g) :: CIO (Vector e)
  (Array _ sum)           <- newArray Z        :: CIO (Scalar e)
  let n   = size sh0
      itv = (n + g - 1) `div` g

  bindLifted mdl fvs
  launch (t,g,m) fscan ((((((),out),in0),bks),n),itv)   -- inclusive scan of input array
  launch (t,1,m) fscan ((((((),bks),bks),sum),g),itv)   -- inclusive scan block-level sums
  launch (t,g,m) fadd  ((((((),out),bks),sum),n),itv)   -- distribute partial results
  freeLifted fvs
  freeArray in0
  freeArray bks
  freeArray sum
  return a

executeScan' :: forall aenv e.
     OpenAcc aenv (Vector e, Scalar e)
  -> OpenAcc aenv (Vector e)
  -> Val aenv 
  -> CIO (Vector e, Scalar e)
executeScan' acc a0 aenv = do
  (Array sh0 in0)         <- executeOpenAcc a0 aenv
  (fvs,mdl,fscan,(t,g,m)) <- configure "inclusive_scan" acc aenv (size sh0)
  fadd                    <- liftIO $ CUDA.getFun mdl "exclusive_update"
  a@(Array _ out)         <- newArray (Sugar.toElt sh0)
  s@(Array _ sum)         <- newArray Z
  (Array _ bks)           <- newArray (Z :. g) :: CIO (Vector e)
  let n   = size sh0
      itv = (n + g - 1) `div` g

  bindLifted mdl fvs
  launch (t,g,m) fscan ((((((),out),in0),bks),n),itv)   -- inclusive scan of input array   
  launch (t,1,m) fscan ((((((),bks),bks),sum),g),itv)   -- inclusive scan block-level sums
  launch (t,g,m) fadd  ((((((),out),bks),sum),n),itv)   -- distribute partial results
  freeLifted fvs
  freeArray in0
  freeArray bks
  return (a,s)

executeScan1 :: forall aenv e.
     OpenAcc aenv (Vector e)
  -> OpenAcc aenv (Vector e)
  -> Val aenv
  -> CIO (Vector e)
executeScan1 acc a0 aenv = do
  (Array sh0 in0)         <- executeOpenAcc a0 aenv
  (fvs,mdl,fscan,(t,g,m)) <- configure "inclusive_scan" acc aenv (size sh0)
  fadd                    <- liftIO $ CUDA.getFun mdl "inclusive_update"
  a@(Array _ out)         <- newArray (Sugar.toElt sh0)
  (Array _ bks)           <- newArray (Z :. g) :: CIO (Vector e)
  (Array _ sum)           <- newArray Z        :: CIO (Scalar e)
  let n   = size sh0
      itv = (n + g - 1) `div` g

  bindLifted mdl fvs
  launch (t,g,m) fscan ((((((),out),in0),bks),n),itv)   -- inclusive scan of input array
  launch (t,1,m) fscan ((((((),bks),bks),sum),g),itv)   -- inclusive scan block-level sums
  launch (t,g,m) fadd  (((((),out),bks),n),itv)         -- distribute partial results
  freeLifted fvs
  freeArray in0
  freeArray bks
  freeArray sum
  return a

-- Apply a function to a set of Arrays
--
applyArraysR :: Arrays arrs => (forall e. ArrayElt e => AD.ArrayData e -> CIO ()) -> arrs -> CIO ()
applyArraysR f arrs = applyR arrays arrs
  where
    applyR :: ArraysR arrs -> arrs -> CIO ()
    applyR ArraysRunit         ()           = return ()
    applyR ArraysRarray        (Array _ ad) = f ad
    applyR (ArraysRpair r1 r0) (a1,a0)      = applyR r1 a1 >> applyR r0 a0

-- Execute an action under which the given set of Arrays will not be released by
-- a call to 'freeArray'. This is used to ensure that let-bound arrays remain
-- active for the duration of the sub-computation.
--
withBoundArrays :: Arrays arrs => arrs -> CIO a -> CIO a
withBoundArrays arrs action =
  applyArraysR bindArray   arrs >> action >>= \r ->
  applyArraysR unbindArray arrs >> return r

withBoundArrays2 :: (Arrays arrs1, Arrays arrs2) => arrs1 -> arrs2 -> CIO a -> CIO a
withBoundArrays2 arrs1 arrs2 action =
  withBoundArrays arrs1 $ withBoundArrays arrs2 action

freeArraysR :: Arrays arrs => arrs -> CIO ()
freeArraysR = applyArraysR freeArray


-- Scalar expression evaluation
-- ----------------------------

-- Evaluate a closed scalar expression. Expressions are evaluated on the host,
-- but may require some interaction with the device, such as array indexing
--
executeExp :: Exp aenv t -> Val aenv -> CIO t
executeExp e = executeOpenExp e Empty

executeOpenExp :: OpenExp env aenv t -> Val env -> Val aenv -> CIO t
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


executeTuple :: Tuple (OpenExp env aenv) t -> Val env -> Val aenv -> CIO t
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
  Shapes :: Shape sh => sh          -> Lifted
  Arrays ::             Array dim e -> Lifted

liftAcc :: forall a aenv. OpenAcc aenv a -> Val aenv -> CIO [Lifted]
liftAcc (Let _ _)            _    = INTERNAL_ERROR(error) "liftAcc" "let-binding?"
liftAcc (Let2 _ _)           _    = INTERNAL_ERROR(error) "liftAcc" "let-binding?"
liftAcc (Avar _)             _    = return []
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


liftFun :: OpenFun env aenv a -> Val aenv -> CIO [Lifted]
liftFun (Lam  lam)  = liftFun lam
liftFun (Body body) = liftExp body

liftTup :: Tuple (OpenExp env aenv) t -> Val aenv -> CIO [Lifted]
liftTup NilTup          _    = return []
liftTup (t `SnocTup` e) aenv = (++) <$> liftTup t aenv <*> liftExp e aenv

liftExp :: OpenExp env aenv a -> Val aenv -> CIO [Lifted]
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
  return [Shapes sh]

liftExp (IndexScalar a e) aenv = do
  vs               <- liftExp e aenv
  arr@(Array sh _) <- executeOpenAcc a aenv
  return $ Arrays arr : Shapes sh : vs

liftExp (Size a)          aenv = liftExp (Shape a) aenv


-- Bind array variables to the appropriate module references, where binding
-- names are simply derived "in order", c.f. code generation.
--
bindLifted :: CUDA.Module -> [Lifted] -> CIO ()
bindLifted mdl = foldM_ go (0,0)
  where
    go :: (Int,Int) -> Lifted -> CIO (Int,Int)
    go (n,m) (Shapes sh)            = bindDim n sh    >>  return (n+1,m)
    go (n,m) (Arrays (Array sh ad)) = bindTex m sh ad >>= \m' -> return (n,m+m')

    bindDim n sh = liftIO $
      CUDA.getPtr mdl ("sh"++show n) >>= \(p,_) ->
      CUDA.pokeListArray (convertIx sh) p

    bindTex m sh ad
      = let textures = sequence' $ map (CUDA.getTex mdl . ("tex"++) . show) [m..]
        in  marshalTextureData ad (size sh) =<< liftIO textures

-- Release arrays lifted from scalar expressions
--
freeLifted :: [Lifted] -> CIO ()
freeLifted = mapM_ go
  where go :: Lifted -> CIO ()
        go (Shapes _)            = return ()
        go (Arrays (Array _ ad)) = freeArray ad


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
execute :: Marshalable args => String -> OpenAcc aenv a -> Val aenv -> Int -> args -> CIO ()
execute name acc aenv n args =
  configure name acc aenv n >>= flip dispatch args

-- Pre-execution configuration and kernel linking
--
configure :: String -> OpenAcc aenv a -> Val aenv -> Int -> CIO ([Lifted], CUDA.Module, CUDA.Fun, (Int,Int,Integer))
configure name acc aenv n = do
  fvs <- liftAcc acc aenv
  mdl <- loadKernel acc
  fun <- liftIO $ CUDA.getFun mdl name
  cfg <- launchConfig acc n fun
  return (fvs, mdl, fun, cfg)

-- Binding of lifted array expressions and kernel invocation
--
dispatch :: Marshalable args => ([Lifted], CUDA.Module, CUDA.Fun, (Int,Int,Integer)) -> args -> CIO ()
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
{-
-- Hash value for an Acc node
--
makeStableAcc :: OpenAcc aenv a -> IO Int32
makeStableAcc acc = combine accID . fromIntegral . hashStableName <$> makeStableName acc
  where
    combine a b = (a `rotate` 1) `xor` b
    accID       = Hash.hashInt . ord . head $ accToKey acc

-- Kernel module lookup with fast association to particular AST nodes
--
loadKernel :: OpenAcc aenv a -> CIO CUDA.Module
loadKernel acc = do
  tab <- getM computeTable
  key <- liftIO $ makeStableAcc acc
  mdl <- liftIO $ Hash.lookup tab key
  case mdl of
    Just e  -> INTERNAL_ASSERT "loadKernel" (getL accKey e == accToKey acc) $ return (getL accKernel e)
    Nothing -> do
      m <- linkKernel acc
#ifdef ACCELERATE_INTERNAL_CHECKS
      liftIO $ Hash.insert tab key (AccEntry (accToKey acc) m)
#else
      liftIO $ Hash.insert tab key (AccEntry undefined m)
#endif
      return m
-}

-- Link the CUDA binary object implementing the kernel for the given array
-- computation. This may entail waiting for the external compilation process.
--
loadKernel :: OpenAcc aenv a -> CIO CUDA.Module
loadKernel acc =
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


-- Auxiliary functions
-- -------------------

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


-- Create a new host array, and associated device memory area
-- FIXME: small arrays are relocated by the GC
--
newArray :: (Sugar.Shape sh, Sugar.Elt e) => sh -> CIO (Array sh e)
newArray sh =
  let ad = fst . AD.runArrayData $ (,undefined) <$> AD.newArrayData (1024 `max` Sugar.size sh)
  in do
    ad `seq` mallocArray ad (1 `max` Sugar.size sh)
    return $ Array (Sugar.fromElt sh) ad


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

