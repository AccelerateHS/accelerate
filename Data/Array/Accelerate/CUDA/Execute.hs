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

import Prelude hiding (id, (.), mod, sum)
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
import Data.Array.Accelerate.Array.Representation
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
executeOpenAcc (Use xs)  _env = return xs
executeOpenAcc (Avar ix)  env = return $ prj ix env
executeOpenAcc (Let  x y) env = do
  ax <- executeOpenAcc x env
  executeOpenAcc y (env `Push` ax)

executeOpenAcc (Let2 x y) env = do
  (ax1,ax2) <- executeOpenAcc x env
  executeOpenAcc y (env `Push` ax1 `Push` ax2)

executeOpenAcc (Unit e)   env = do
  v  <- executeExp e env
  let ad = fst . runArrayData $ (,undefined) <$> do
        arr <- newArrayData 1
        writeArrayData  arr 0 (Sugar.fromElem v)
        return arr

  mallocArray    ad 1
  pokeArrayAsync ad 1 Nothing
  return (Array (Sugar.fromElem ()) ad)


executeOpenAcc acc env = do
  tab <- getM kernelTable
  krn <- fromMaybe (error "code generation failed") <$> liftIO (HT.lookup tab key)
  mdl <- either' (get kernelStatus krn) return $ \pid -> do
    liftIO (waitFor pid)
    mdl <- liftIO $ CUDA.loadFile (get kernelName krn `replaceExtension` ".cubin")
    liftIO        $ HT.insert tab key (set kernelStatus (Right mdl) krn)
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

  launch acc n fn (d_out ++ d_in1 ++ d_in0 ++ t_var ++ [CUDA.IArg n])
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
  f_arr <- liftFun f env
  t_var <- bind mdl f_arr

  launch' (cta,grid,smem) fn (d_out ++ d_in0 ++ t_var ++ [CUDA.IArg (size sh)])
  freeArray in0
  release f_arr
  if grid > 1 then dispatch (Fold f x (Use res)) env mdl
              else return (Array (Sugar.fromElem ()) out)

dispatch acc@(FoldSeg f _ ad sd) env mdl = do
  fn              <- liftIO $ CUDA.getFun mdl "fold_segmented"
  (Array sh  in0) <- executeOpenAcc ad env
  (Array sh' seg) <- executeOpenAcc sd env
  (cta,grid,smem) <- launchConfig acc (size sh') fn
  let res@(Array _ out) = newArray (size sh')

  mallocArray out (size sh')
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  d_seg <- devicePtrs seg
  f_arr <- liftFun f env
  t_var <- bind mdl f_arr

  launch' (cta,grid,smem) fn (d_out ++ d_in0 ++ d_seg ++ t_var ++ map (CUDA.IArg . size) [sh', sh])
  freeArray in0
  freeArray seg
  release f_arr
  return res


dispatch acc@(Scanl _ _ _) env mdl = dispatchScan acc env mdl
dispatch acc@(Scanr _ _ _) env mdl = dispatchScan acc env mdl

dispatch acc@(Permute f1 df f2 ad) env mdl = do
  fn              <- liftIO $ CUDA.getFun mdl "permute"
  (Array sh  def) <- executeOpenAcc df env
  (Array sh' in0) <- executeOpenAcc ad env
  let res@(Array _ out) = newArray (Sugar.toElem sh)
      n                 = size sh'

  mallocArray out n
  copyArray def out n
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  f_arr <- (++) <$> liftFun f1 env <*> liftFun f2 env
  t_var <- bind mdl f_arr

  launch acc n fn (d_out ++ d_in0 ++ t_var ++ [CUDA.IArg n])
  freeArray def
  freeArray in0
  release f_arr
  return res

dispatch acc@(Backpermute e f ad) env mdl = do
  fn            <- liftIO $ CUDA.getFun mdl "backpermute"
  sh            <- executeExp e env
  (Array _ in0) <- executeOpenAcc ad env
  let res@(Array sh' out) = newArray sh
      n                   = size sh'

  mallocArray out n
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  f_arr <- liftFun f env
  t_var <- bind mdl f_arr

  launch acc n fn (d_out ++ d_in0 ++ t_var ++ [CUDA.IArg n])
  freeArray in0
  release f_arr
  return res

dispatch x _ _ =
  INTERNAL_ERROR(error) "dispatch"
  (unlines ["unsupported array primitive", render . nest 2 $ text (show x)])


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
dispatchScan :: OpenAcc aenv a -> Val aenv -> CUDA.Module -> CIO a
dispatchScan     (Scanr f x ad) env mdl = dispatchScan (Scanl f x ad) env mdl
dispatchScan acc@(Scanl f _ ad) env mdl = do
  fscan           <- liftIO $ CUDA.getFun mdl "inclusive_scan"
  fadd            <- liftIO $ CUDA.getFun mdl "exclusive_update"
  (Array sh in0)  <- executeOpenAcc ad env
  (cta,grid,smem) <- launchConfig acc (size sh) fscan
  let a_out@(Array _ out) = newArray (Sugar.toElem sh)
      a_sum@(Array _ sum) = newArray ()
      a_bks@(Array _ bks) = newArray grid
      n                   = size sh
      interval            = (n + grid - 1) `div` grid

      unify :: Array dim e -> Array dim' e -> CIO ()
      unify _ _ = return ()

  unify a_sum a_bks -- TLM: *cough*

  mallocArray out n
  mallocArray sum 1
  mallocArray bks grid
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  d_bks <- devicePtrs bks
  d_sum <- devicePtrs sum
  f_arr <- liftFun f env
  t_var <- bind mdl f_arr

  launch' (cta,grid,smem) fscan (d_out ++ d_in0 ++ d_bks ++ t_var ++ map CUDA.IArg [n,interval])
  launch' (cta,1,smem)    fscan (d_bks ++ d_bks ++ d_sum ++ map CUDA.IArg [grid,interval])
  launch' (cta,grid,smem) fadd  (d_out ++ d_bks ++ map CUDA.IArg [n,interval])

  freeArray in0
  freeArray bks
  return (a_out, a_sum)

dispatchScan _ _ _ =
  error "we can never get here"


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


-- | Wait for the compilation process to finish
--
waitFor :: ProcessID -> IO ()
waitFor pid = do
  status <- getProcessStatus True True pid
  case status of
       Just (Exited ExitSuccess) -> return ()
       _                         -> error  $ "nvcc (" ++ shows pid ") terminated abnormally"

