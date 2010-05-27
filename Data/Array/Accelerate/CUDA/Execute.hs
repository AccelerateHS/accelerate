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

module Data.Array.Accelerate.CUDA.Execute (compile, execute)
  where

import Prelude hiding (id, (.), mod)
import Control.Category

import Data.Maybe
import Control.Arrow
import Control.Applicative
import Control.Monad.IO.Class
import Control.Exception.Extensible
import qualified Data.Map                               as M

import System.Directory
import System.FilePath
import System.Posix.Process
import System.Exit                                      (ExitCode(..))
import System.Posix.Types                               (ProcessID)

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Pretty ()
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Sugar                (Array(..))
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar

import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.CodeGen
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.Analysis.Launch

import Foreign.Marshal.Utils
import Foreign.CUDA.Analysis.Device
import qualified Foreign.CUDA.Driver                    as CUDA


-- This is going to be annoying to maintain...
--
accToInt :: OpenAcc aenv a -> Int
accToInt (Let  _ _)          = 0
accToInt (Let2 _ _)          = 1
accToInt (Avar _)            = 2
accToInt (Use  _)            = 3
accToInt (Unit _)            = 4
accToInt (Reshape _ _)       = 5
accToInt (Replicate _ _ _)   = 6
accToInt (Index _ _ _)       = 7
accToInt (Map _ _)           = 8
accToInt (ZipWith _ _ _)     = 9
accToInt (Fold _ _ _)        = 10
accToInt (FoldSeg _ _ _ _)   = 11
accToInt (Scan _ _ _)        = 12
--accToInt (Scanr _ _ _)       = 13
accToInt (Permute _ _ _ _)   = 14
accToInt (Backpermute _ _ _) = 15


-- Maybe the sub-expression (for example, the function argument to `fold`) is
-- sufficient, and/or accToInt is unnecessary.
--
accToKey :: OpenAcc aenv a -> Key
accToKey = accToInt &&& show


-- |
-- Generate and compile code for an array expression
--
compile :: OpenAcc aenv a -> CIO ()
compile acc = do
  nvcc   <- fromMaybe (error "nvcc: command not found") <$> liftIO (findExecutable "nvcc")
  dir    <- liftIO outputDir
  cufile <- outputName acc (dir </> "dragon.cu")
  flags  <- compileFlags nvcc
  pid    <- liftIO . withFilePath dir $ do
              writeFile cufile . show $ codeGenAcc acc (takeBaseName cufile)
              forkProcess $ executeFile nvcc False (takeFileName cufile : flags) Nothing

  modM kernelEntry $ M.insert (accToKey acc) (KernelEntry cufile (Left pid))


-- Determine the appropriate command line flags to pass to the compiler process
--
-- TLM: the system include is probably automatic?
--
compileFlags :: FilePath -> CIO [String]
compileFlags nvcc = do
  arch <- computeCapability <$> getM deviceProps
  return [ "-I.", "-I" ++ dropFileName nvcc </> ".." </> "include"
         , "-O3", "-m32", "--compiler-options", "-fno-strict-aliasing"
         , "-arch=sm_" ++ show (round (arch * 10) :: Int)
         , "-DUNIX"
         , "-cubin" ]


-- Execute the IO action under the given directory
--
withFilePath :: FilePath -> IO a -> IO a
withFilePath fp action =
  bracket getCurrentDirectory setCurrentDirectory . const $ do
    setCurrentDirectory fp
    action


-- Return the output filename of the generated CUDA code
--
-- In future, we hope that this is a unique filename based on the function
-- itself, but for now it is just a unique filename.
--
outputName :: OpenAcc aenv a -> FilePath -> CIO FilePath
outputName acc cufile = do
  n <- freshVar
  x <- liftIO $ doesFileExist (base ++ show n <.> suffix)
  if x then outputName acc cufile
       else return (base ++ show n <.> suffix)
  where
    (base,suffix) = splitExtension cufile
    freshVar      = getM unique <* modM unique (+1)


-- Return the output directory for compilation by-products. This currently maps
-- to a temporary directory, but could be mode to point towards a persistent
-- cache (eg: getAppUserDataDirectory)
--
outputDir :: IO FilePath
outputDir = do
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  dir <- canonicalizePath $ tmp </> "accelerate-cuda-" ++ show pid
  createDirectoryIfMissing True dir
  return dir


-- |
-- Execute an embedded array program using the CUDA backend. The function will
-- block if the compilation has not yet completed, but subsequent invocations of
-- the same kernel will be able to extract the loaded kernel directly.
--
execute :: OpenAcc aenv a -> CIO a
execute (Use xs) = return xs
execute acc      = do
  krn <- fromMaybe (error "code generation failed") . M.lookup (accToKey acc) <$> getM kernelEntry
  fun <- either' (get kernelStatus krn) return $ \pid -> do
    let name = get kernelName krn

    liftIO (waitFor pid)
    mdl <- liftIO $ CUDA.loadFile   (replaceExtension name ".cubin")
    fun <- liftIO $ CUDA.getFun mdl (takeBaseName name)

    modM kernelEntry $ M.insert (accToKey acc) (set kernelStatus (Right fun) krn)
    return fun

  -- determine dispatch pattern, extract parameters, allocate storage
  --
  dispatch acc fun

  where
    either' :: Either a b -> (b -> c) -> (a -> c) -> c
    either' e r l = either l r e


-- Setup and initiate the computation. This may require several kernel launches.
--
-- TLM: focus more on dispatch patterns (reduction, permutation, map, etc),
-- rather than the actual functions. So ZipWith and Map should resolve to the
-- same procedure, for example (more or less, hopefully ... *vague*).
--
dispatch :: OpenAcc aenv a -> CUDA.Fun -> CIO a
dispatch acc@(Map _ ad) fn = do
  (Array sh xs) <- execute ad
  let res@(Array sh' ys) = newArray (Sugar.toElem sh)
      n    = size sh'
      full = fromBool False     -- TLM: totally useless parameter

  mallocArray ys n
  d_xs <- devicePtrs xs
  d_ys <- devicePtrs ys

  launch acc fn (d_xs ++ d_ys ++ [CUDA.IArg n, CUDA.IArg full])
  free   xs
  return res

dispatch acc@(ZipWith _ ad1 ad2) fn = do
  (Array sh1 xs) <- execute ad1
  (Array sh2 ys) <- execute ad2
  let res@(Array sh' zs) = newArray (Sugar.toElem (sh1 `intersect` sh2))
      n    = size sh'
      full = fromBool False

  mallocArray zs n
  d_xs <- devicePtrs xs
  d_ys <- devicePtrs ys
  d_zs <- devicePtrs zs

  launch acc fn (d_xs ++ d_ys ++ d_zs ++ [CUDA.IArg n, CUDA.IArg full])
  free   xs
  free   ys
  return res


-- Initiate the device computation. First parameter is the work size, typically
-- something like (array size / elements per thread)
--
launch :: OpenAcc aenv a -> CUDA.Fun -> [CUDA.FunParam] -> CIO ()
launch acc fn args = do
  (cta,grid,smem) <- launchConfig acc fn

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

