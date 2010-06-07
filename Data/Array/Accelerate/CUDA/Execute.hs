{-# LANGUAGE GADTs, TupleSections #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Execute
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
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
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.Map                               as M

import System.FilePath
import System.Posix.Process
import System.Exit                                      (ExitCode(..))
import System.Posix.Types                               (ProcessID)

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Sugar                (Array(..))
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar

import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Compile
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.Analysis.Launch

import qualified Foreign.CUDA.Driver                    as CUDA


-- Environments
-- ~~~~~~~~~~~~

-- Valuation for an environment
--
data Val env where
  Empty :: Val ()
  Push  :: Val env -> t -> Val (env, t)

-- Projection of a value from a valuation using a de Bruijn index
--
prj :: Idx env t -> Val env -> t
prj ZeroIdx       (Push _   v) = v
prj (SuccIdx idx) (Push val _) = prj idx val
prj _             _            =
  error "Data.Array.Accelerate.CUDA: prj: inconsistent valuation"


-- Array expression evaluation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- |
-- Evaluate a closed array expression
--
executeAcc :: Acc a -> CIO a
executeAcc acc = executeOpenAcc acc Empty

-- |
-- Execute an embedded array program using the CUDA backend. The function will
-- block if the compilation has not yet completed, but subsequent invocations of
-- the same kernel will be able to extract the loaded kernel directly.
--
executeOpenAcc :: OpenAcc aenv a -> Val aenv -> CIO a
executeOpenAcc (Let  x y) env = do
  ax <- executeOpenAcc x env
  executeOpenAcc y (env `Push` ax)

executeOpenAcc (Let2 x y) env = do
  (ax1,ax2) <- executeOpenAcc x env
  executeOpenAcc y (env `Push` ax1 `Push` ax2)

executeOpenAcc (Avar ix)  env = return $ prj ix env
executeOpenAcc (Unit _)  _env = error "executeOpenAcc: unit"
executeOpenAcc (Use xs)  _env = return xs

executeOpenAcc acc env = do
  krn <- fromMaybe (error "code generation failed") . M.lookup key <$> getM kernelEntry
  mdl <- either' (get kernelStatus krn) return $ \pid -> do
    liftIO (waitFor pid)
    mdl <- liftIO    $ CUDA.loadFile (get kernelName krn `replaceExtension` ".cubin")
    modM kernelEntry $ M.insert key  (set kernelStatus (Right mdl) krn)
    return mdl
    --
    -- TLM 2010-06-02: delete the temporary files??

  -- determine dispatch pattern, extract parameters, allocate storage, run
  --
  dispatch acc env mdl

  where
    key           = accToKey acc
    either' e r l = either l r e


-- Setup and initiate the computation. This may require several kernel launches.
--
dispatch :: OpenAcc aenv a -> Val aenv -> CUDA.Module -> CIO a
dispatch acc@(Map _ ad) env mdl = do
  fn             <- liftIO $ CUDA.getFun mdl "map"
  (Array sh in0) <- executeOpenAcc ad env
  let res@(Array sh' out) = newArray (Sugar.toElem sh)
      n                   = size sh'

  mallocArray out n
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0

  launch acc n fn (d_out ++ d_in0 ++ [CUDA.IArg n])
  free   in0
  return res

dispatch acc@(ZipWith _ ad0 ad1) env mdl = do
  fn              <- liftIO $ CUDA.getFun mdl "zipWith"
  (Array sh0 in0) <- executeOpenAcc ad0 env
  (Array sh1 in1) <- executeOpenAcc ad1 env
  let res@(Array sh' out) = newArray (Sugar.toElem (sh0 `intersect` sh1))
      n    = size sh'

  mallocArray out n
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  d_in1 <- devicePtrs in1

  launch acc n fn (d_out ++ d_in0 ++ d_in1 ++ [CUDA.IArg n])
  free   in0
  free   in1
  return res

dispatch acc@(Fold _ x ad) env mdl = do
  fn              <- liftIO $ CUDA.getFun mdl "fold"
  (Array sh in0)  <- executeOpenAcc ad env
  (cta,grid,smem) <- launchConfig acc (size sh) fn
  let res@(Array _ out) = newArray grid

  mallocArray out grid
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0

  launch' (cta,grid,smem) fn (d_out ++ d_in0 ++ [CUDA.IArg (size sh)])
  free in0
  if grid > 1 then dispatch (Fold undefined x (Use res)) env mdl
              else return (Array (Sugar.fromElem ()) out)

dispatch acc@(Scanl _ _ _) env mdl = dispatchScan acc env mdl
dispatch acc@(Scanr _ _ _) env mdl = dispatchScan acc env mdl

dispatch _ _ _ =
  error "Data.Array.Accelerate.CUDA: dispatch: internal error"


-- Unified dispatch handler for left/right scan. This is a little awkward, but
-- the execution semantics between the two are the same, and all differences
-- have been established during code generation.
--
dispatchScan :: OpenAcc aenv a -> Val aenv -> CUDA.Module -> CIO a
dispatchScan     (Scanr _ x ad) env mdl = dispatchScan (Scanl undefined x ad) env mdl
dispatchScan acc@(Scanl _ x ad) env mdl = do
  fscan           <- liftIO $ CUDA.getFun mdl "scan"
  fadd            <- liftIO $ CUDA.getFun mdl "vectorAddUniform4"
  (Array sh in0)  <- executeOpenAcc ad env
  (cta,grid,smem) <- launchConfig acc (size sh) fscan
  let arr@(Array _ out) = newArray (Sugar.toElem sh)
      bks@(Array _ sum) = newArray grid
      n                 = size sh

  mallocArray out (size sh)
  mallocArray sum grid
  d_out <- devicePtrs out
  d_in0 <- devicePtrs in0
  d_bks <- devicePtrs sum

  -- Single row, multi-block, non-full block scan
  --
  launch' (cta,grid,smem) fscan (d_out ++ d_in0 ++ d_bks ++ map CUDA.IArg [n,1,1])
  free in0

  -- Now, take the last value of all of the sub-blocks and scan those. This will
  -- give a new value that must be added to each block to get the final result
  --
  if grid <= 1
     then return (arr, Array () sum)
     else do
       (Array _ sum', r) <- dispatchScan (Scanl undefined x (Use bks)) env mdl
       d_bks'            <- devicePtrs sum'

       launch' (cta,grid,0) fadd (d_out ++ d_bks' ++ map CUDA.IArg [n,4,4,0,0])
       free sum'
       return (arr,r)

dispatchScan _ _ _ =
  error "Data.Array.Accelerate.CUDA: internal error"


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


-- |
-- Wait for the compilation process to finish
--
waitFor :: ProcessID -> IO ()
waitFor pid = do
  status <- getProcessStatus True True pid
  case status of
       Just (Exited ExitSuccess) -> return ()
       _                         -> error  $ "nvcc (" ++ shows pid ") terminated abnormally"

