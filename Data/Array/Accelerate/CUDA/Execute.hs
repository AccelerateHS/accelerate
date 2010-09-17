{-# LANGUAGE CPP, GADTs, TupleSections #-}
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

import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Applicative                              hiding (Const)
import qualified Data.HashTable                         as HT

import System.FilePath
import System.Posix.Process
import System.Exit                                      (ExitCode(..))
import System.Posix.Types                               (ProcessID)
import Text.PrettyPrint

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation       hiding (sliceIndex)
import Data.Array.Accelerate.Array.Sugar                (Array(..))
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar
import qualified Data.Array.Accelerate.Interpreter      as I

import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.Analysis.Hash
import Data.Array.Accelerate.CUDA.Analysis.Launch

import qualified Foreign.CUDA.Driver                    as CUDA

#include "accelerate.h"


-- Expression evaluation
-- ~~~~~~~~~~~~~~~~~~~~~

-- Evaluate a closed scalar expression. Expressions are evaluated on the host,
-- but may require some interaction with the device (such as array indexing).
--
-- TLM 2010-07-13:
--   We sneakily use the Interpreter backend for primitive operations, but maybe
--   we don't want to...?
--
executeExp :: Exp aenv t -> Val aenv -> CIO t
executeExp e = executeOpenExp e Empty

executeOpenExp :: OpenExp env aenv t -> Val env -> Val aenv -> CIO t
executeOpenExp (Var idx) env _            = return . Sugar.toElem $ prj idx env
executeOpenExp (Const c) _ _              = return $ Sugar.toElem c
executeOpenExp (PrimConst c) _ _          = return $ I.evalPrimConst c
executeOpenExp (PrimApp fun arg) env aenv = I.evalPrim fun <$> executeOpenExp arg env aenv
executeOpenExp (Prj idx e) env aenv       = I.evalPrj idx . fromTuple <$> executeOpenExp e env aenv
executeOpenExp (Tuple tup) env aenv       = toTuple                   <$> executeTuple tup env aenv
executeOpenExp (IndexScalar a e) env aenv = do
  (Array sh ad) <- executeOpenAcc a aenv
  ix            <- executeOpenExp e env aenv
  Sugar.toElem <$> ad `indexArray` index sh (Sugar.fromElem ix)

executeOpenExp (Shape a) _ aenv = do
  (Array sh _)  <- executeOpenAcc a aenv
  return (Sugar.toElem sh)

executeOpenExp (Cond c t e) env aenv = do
  p <- executeOpenExp c env aenv
  if p then executeOpenExp t env aenv
       else executeOpenExp e env aenv


executeTuple :: Tuple (OpenExp env aenv) t -> Val env -> Val aenv -> CIO t
executeTuple NilTup            _    _     = return ()
executeTuple (tup `SnocTup` e) env  aenv  = (,) <$> executeTuple tup env aenv <*> executeOpenExp e env aenv


-- Array evaluation
-- ~~~~~~~~~~~~~~~

-- | Execute a closed array expression
--
executeAcc :: Acc a -> CIO a
executeAcc acc = executeOpenAcc acc Empty

-- |
-- Execute an embedded array program using the CUDA backend. The function will
-- block if the compilation has not yet completed, but subsequent invocations of
-- the same kernel will be able to extract the loaded kernel directly.
--
executeOpenAcc :: OpenAcc aenv a -> Val aenv -> CIO a
executeOpenAcc (Use  xs) _env = return xs
executeOpenAcc (Avar ix)  env = return $ prj ix env
executeOpenAcc (Let  x y) env = do
  ax <- executeOpenAcc x env
  executeOpenAcc y (env `Push` ax)

executeOpenAcc (Let2 x y) env = do
  (ax1,ax2) <- executeOpenAcc x env
  executeOpenAcc y (env `Push` ax1 `Push` ax2)

executeOpenAcc (Reshape e a) env = do
  ix            <- executeExp e env
  (Array sh ad) <- executeOpenAcc a env
  BOUNDS_CHECK(check) "reshape" "shape mismatch" (Sugar.size ix == size sh)
    $ return (Array (Sugar.fromElem ix) ad)

executeOpenAcc (Unit e) env = do
  v  <- executeExp e env
  let ad = fst . runArrayData $ (,undefined) <$> do
        arr <- newArrayData 1024    -- FIXME: small arrays moved by the GC
        writeArrayData arr 0 (Sugar.fromElem v)
        return arr
  mallocArray    ad 1
  pokeArrayAsync ad 1 Nothing
  return (Array (Sugar.fromElem ()) ad)


executeOpenAcc acc env = do
  tab <- getM kernelTable
  krn <- fromMaybe (error "code generation failed") <$> liftIO (HT.lookup tab key)
  mdl <- either' (getL kernelStatus krn) return $ \pid -> do
    liftIO (waitFor pid)
    mdl <- liftIO $ CUDA.loadFile (getL kernelName krn `replaceExtension` ".cubin")
    liftIO        $ HT.insert tab key (setL kernelStatus (Right mdl) krn)
    return mdl

  -- determine dispatch pattern, extract parameters, allocate storage, run
  --
  dispatch acc env mdl

  where
    key           = accToKey acc
    either' e r l = either l r e


-- Dispatch
-- ~~~~~~~~

data FVar where
  FArr :: Array dim e -> FVar

-- Lift free array variables out of scalar computations. Returns a list of the
-- arrays in the order that they are encountered, which also corresponds to the
-- texture reference index they should be bound to.
--
liftFun :: OpenFun env aenv a -> Val aenv -> CIO [FVar]
liftFun (Body e) = liftExp e
liftFun (Lam f)  = liftFun f

liftExp :: OpenExp env aenv a -> Val aenv -> CIO [FVar]
liftExp (Tuple t)         aenv = liftTup t aenv
liftExp (Prj _ e)         aenv = liftExp e aenv
liftExp (PrimApp _ e)     aenv = liftExp e aenv
liftExp (Cond e1 e2 e3)   aenv = concat <$> sequence [liftExp e1 aenv, liftExp e2 aenv, liftExp e3 aenv]
liftExp (IndexScalar a e) aenv = (:) . FArr <$> executeOpenAcc a aenv <*> liftExp e aenv
liftExp _ _ =
  return []

liftTup :: Tuple (OpenExp env aenv) a -> Val aenv -> CIO [FVar]
liftTup NilTup _             = return []
liftTup (t `SnocTup` e) aenv = (++) <$> liftTup t aenv <*> liftExp e aenv


-- Extract texture references from the compiled module and bind an array to it
--
bind :: CUDA.Module -> [FVar] -> CIO [CUDA.FunParam]
bind mdl var =
  let tex n (FArr (Array sh ad)) = textureRefs ad mdl (size sh) n
  in  foldM (\texs farr -> (texs ++) <$> tex (length texs) farr) [] var

release :: [FVar] -> CIO ()
release = mapM_ (\(FArr (Array _ ad)) -> freeArray ad)


-- Setup and initiate the computation. This may require several kernel launches.
--
-- A NOTE ON TUPLES TYPES
--
--   The astute reader may be wondering, if arrays of tuples are stored as a
--   tuple of arrays, how exactly are we telling the kernel function about this?
--
--   From Haskell land, the tuple components are passed to the kernel function
--   individually. The C function, however, interprets these into a structure of
--   pointers. While this is a nasty sleight-of-hand, it should indeed be safe.
--   Pointer types will all have the same size and alignment, and C structures
--   are defined to keep their fields adjacent in memory (modulo alignment
--   restrictions, which don't concern us).
--
dispatch :: OpenAcc aenv a -> Val aenv -> CUDA.Module -> CIO a
dispatch acc@(Map f ad) env mdl = do
  fn             <- liftIO $ CUDA.getFun mdl "map"
  (Array sh in0) <- executeOpenAcc ad env
  let res@(Array sh' out) = newArray (Sugar.toElem sh)
      n                   = size sh'

  mallocArray out n
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  f_var <- liftFun f env
  t_var <- bind mdl f_var

  launch acc n fn (d_out ++ d_in0 ++ t_var ++ [CUDA.IArg n])
  freeArray in0
  release f_var
  return res

dispatch acc@(ZipWith f ad1 ad0) env mdl = do
  fn              <- liftIO $ CUDA.getFun mdl "zipWith"
  (Array sh1 in1) <- executeOpenAcc ad1 env
  (Array sh0 in0) <- executeOpenAcc ad0 env
  let res@(Array sh' out) = newArray (Sugar.toElem (sh0 `intersect` sh1))
      n                   = size sh'

  mallocArray out n
  d_out <- devicePtrs out
  d_in1 <- devicePtrs in1
  d_in0 <- devicePtrs in0
  f_var <- liftFun f env
  t_var <- bind mdl f_var

  launch acc n fn (d_out ++ d_in1 ++ d_in0 ++ t_var ++ convertIx sh' ++ convertIx sh1 ++ convertIx sh0)
  freeArray in0
  freeArray in1
  release f_var
  return res

dispatch acc@(Fold f x ad) env mdl = do
  fn              <- liftIO $ CUDA.getFun mdl "fold"
  (Array sh in0)  <- executeOpenAcc ad env
  (cta,grid,smem) <- launchConfig acc (size sh) fn
  let res@(Array _ out) = newArray grid

  mallocArray out grid
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  f_arr <- (++) <$> liftExp x env <*> liftFun f env
  t_var <- bind mdl f_arr

  launch' (cta,grid,smem) fn (d_out ++ d_in0 ++ t_var ++ [CUDA.IArg (size sh)])
  freeArray in0
  release f_arr
  if grid > 1 then dispatch (Fold f x (Use res)) env mdl
              else return (Array (Sugar.fromElem ()) out)

dispatch acc@(FoldSeg f x ad sd) env mdl = do
  fn              <- liftIO $ CUDA.getFun mdl "fold_segmented"
  (Array sh  in0) <- executeOpenAcc ad env
  (Array sh' seg) <- executeOpenAcc sd env
  (cta,grid,smem) <- launchConfig acc (size sh') fn
  let res@(Array _ out) = newArray (size sh')

  mallocArray out (size sh')
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  d_seg <- devicePtrs seg
  f_arr <- (++) <$> liftExp x env <*> liftFun f env
  t_var <- bind mdl f_arr

  launch' (cta,grid,smem) fn (d_out ++ d_in0 ++ d_seg ++ t_var ++ map (CUDA.IArg . size) [sh', sh])
  freeArray in0
  freeArray seg
  release f_arr
  return res

-- Unified dispatch handler for left/right scan.
--
-- TLM 2010-07-02:
--   This is a little awkward. At its core we have an inclusive scan routine,
--   whereas accelerate actually returns an exclusive scan result. Multi-block
--   arrays require the inclusive scan when calculating the partial block sums,
--   which are then added to every element of an interval. Single-block arrays
--   on the other hand need to be "converted" to an exclusive result in the
--   second pass.
--
--   This optimised could be implemented by enabling some some extra code to the
--   skeleton, and dispatching accordingly.
--
-- TLM 2010-08-19:
--   On the other hand, the inclusive scan core provides a way in which we could
--   add support for non-identic seed elements.
--
dispatch     (Scanr f x ad) env mdl = dispatch (Scanl f x ad) env mdl
dispatch acc@(Scanl f x ad) env mdl = do
  fscan           <- liftIO $ CUDA.getFun mdl "inclusive_scan"
  fadd            <- liftIO $ CUDA.getFun mdl "exclusive_update"
  (Array sh in0)  <- executeOpenAcc ad env
  (cta,grid,smem) <- launchConfig acc (size sh) fscan
  let a_out@(Array _ out) = newArray (Sugar.toElem sh)
      a_sum@(Array _ sum) = newArray ()
      a_bks@(Array _ bks) = newArray grid
      n                   = size sh
      interval            = (n + grid - 1) `div` grid

      unify :: Array dim e -> Array dim e -> CIO ()
      unify _ _ = return ()

  unify a_out a_bks -- TLM: *cough*

  mallocArray out n
  mallocArray sum 1
  mallocArray bks grid
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  d_bks <- devicePtrs bks
  d_sum <- devicePtrs sum
  f_arr <- (++) <$> liftExp x env <*> liftFun f env
  t_var <- bind mdl f_arr

  launch' (cta,grid,smem) fscan (d_out ++ d_in0 ++ d_bks ++ t_var ++ map CUDA.IArg [n,interval])
  launch' (cta,1,smem)    fscan (d_bks ++ d_bks ++ d_sum ++ map CUDA.IArg [grid,interval])
  launch' (cta,grid,smem) fadd  (d_out ++ d_bks ++ map CUDA.IArg [n,interval])

  freeArray in0
  freeArray bks
  release f_arr
  return (a_out, a_sum)

dispatch acc@(Permute f1 df f2 ad) env mdl = do
  fn              <- liftIO $ CUDA.getFun mdl "permute"
  (Array sh  def) <- executeOpenAcc df env
  (Array sh0 in0) <- executeOpenAcc ad env
  let res@(Array _ out) = newArray (Sugar.toElem sh)
      n                 = size sh0

  mallocArray out n
  copyArray def out n
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  f_arr <- (++) <$> liftFun f1 env <*> liftFun f2 env
  t_var <- bind mdl f_arr

  launch acc n fn (d_out ++ d_in0 ++ t_var ++ convertIx sh ++ convertIx sh0)
  freeArray def
  freeArray in0
  release f_arr
  return res

dispatch acc@(Backpermute e f ad) env mdl = do
  fn              <- liftIO $ CUDA.getFun mdl "backpermute"
  sh              <- executeExp e env
  (Array sh0 in0) <- executeOpenAcc ad env
  let res@(Array sh' out) = newArray sh
      n                   = size sh'

  mallocArray out n
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  f_arr <- liftFun f env
  t_var <- bind mdl f_arr

  launch acc n fn (d_out ++ d_in0 ++ t_var ++ convertIx sh' ++ convertIx sh0)
  freeArray in0
  release f_arr
  return res

dispatch acc@(Replicate sliceIndex e ad) env mdl = do
  fn              <- liftIO $ CUDA.getFun mdl "replicate"
  slix            <- executeExp e env
  (Array sh0 in0) <- executeOpenAcc ad env

  let block               = extend sliceIndex (Sugar.fromElem slix) sh0
      res@(Array sh' out) = newArray (Sugar.toElem block)
      n                   = size sh'

      extend :: SliceIndex slix sl co dim -> slix -> sl -> dim
      extend (SliceNil)            ()       ()      = ()
      extend (SliceAll sliceIdx)   (slx,()) (sl,sz) = (extend sliceIdx slx sl, sz)
      extend (SliceFixed sliceIdx) (slx,sz) sl      = (extend sliceIdx slx sl, sz)

  mallocArray out n
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  f_arr <- liftExp e env
  t_var <- bind mdl f_arr

  launch acc n fn (d_out ++ d_in0 ++ t_var ++ convertIx sh0 ++ convertIx sh')
  freeArray in0
  release f_arr
  return res

dispatch acc@(Index sliceIndex ad e) env mdl = do
  fn              <- liftIO $ CUDA.getFun mdl "slice"
  slix            <- executeExp e env
  (Array sh0 in0) <- executeOpenAcc ad env
  let slice               = restrict sliceIndex (Sugar.fromElem slix) sh0
      slix'               = convertSliceIndex sliceIndex (Sugar.fromElem slix)
      res@(Array sh' out) = newArray (Sugar.toElem slice)
      n                   = size sh'

      restrict :: SliceIndex slix sl co dim -> slix -> dim -> sl
      restrict (SliceNil)            ()       ()      = ()
      restrict (SliceAll sliceIdx)   (slx,()) (sh,sz) = (restrict sliceIdx slx sh, sz)
      restrict (SliceFixed sliceIdx) (slx,i)  (sh,sz)
        = BOUNDS_CHECK(checkIndex) "slice" i sz $ restrict sliceIdx slx sh

  mallocArray out n
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  f_arr <- liftExp e env
  t_var <- bind mdl f_arr

  launch acc n fn (d_out ++ d_in0 ++ t_var ++ convertIx sh' ++ slix' ++ convertIx sh0)
  freeArray in0
  release f_arr
  return res

dispatch x _ _ =
  INTERNAL_ERROR(error) "dispatch"
  (unlines ["unsupported array primitive", render . nest 2 $ text (show x)])


-- Initiate the device computation. The first version selects launch parameters
-- automatically, the second requires them explicitly. This tuple contains
-- threads per block, grid size, and dynamic shared memory, respectively.
--
launch :: OpenAcc aenv a -> Int -> CUDA.Fun -> [CUDA.FunParam] -> CIO ()
launch acc n fn args =
  launchConfig acc n fn >>= \cfg ->
  launch' cfg fn args

launch' :: (Int,Int,Integer) -> CUDA.Fun -> [CUDA.FunParam] -> CIO ()
launch' (cta,grid,smem) fn args =
  liftIO $ do
    CUDA.setParams     fn args
    CUDA.setSharedSize fn smem
    CUDA.setBlockShape fn (cta,1,1)
    CUDA.launch        fn (grid,1) Nothing


-- Create a new array (obviously...)
--
{-# INLINE newArray #-}
newArray :: (Sugar.Ix dim, Sugar.Elem e) => dim -> Array dim e
newArray sh = ad `seq` Array (Sugar.fromElem sh) ad
  where
    -- FIXME: small arrays are relocated by the GC
    ad = fst . runArrayData $ (,undefined) <$> newArrayData (1024 `max` Sugar.size sh)

-- Extract shape dimensions as a list of function parameters. Note that this
-- will convert to the base integer width of the device, namely, 32-bits.
-- Singleton dimensions are considered to be of unit size.
--
-- Internally, Accelerate uses snoc-based tuple projection, while the data
-- itself is stored in reading order. Ensure we match the behaviour of regular
-- tuples and code generation thereof.
--
convertIx :: Ix dim => dim -> [CUDA.FunParam]
convertIx = post . map CUDA.IArg . shapeToList
  where post [] = [CUDA.IArg 1]
        post xs = reverse xs

-- Convert a slice specification into storable index projection components.
-- Note: implicit conversion Int -> Int32
--
convertSliceIndex :: SliceIndex slix sl co dim -> slix -> [CUDA.FunParam]
convertSliceIndex (SliceNil)            ()     = []
convertSliceIndex (SliceAll   sliceIdx) (s,()) = convertSliceIndex sliceIdx s
convertSliceIndex (SliceFixed sliceIdx) (s,i)  = CUDA.IArg i : convertSliceIndex sliceIdx s

-- | Wait for the compilation process to finish
--
waitFor :: ProcessID -> IO ()
waitFor pid = do
  status <- getProcessStatus True True pid
  case status of
       Just (Exited ExitSuccess) -> return ()
       _                         -> error  $ "nvcc (" ++ shows pid ") terminated abnormally"

