{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Compile
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Compile (accToKey, compile)
  where

import Prelude hiding (id, (.), mod)
import Control.Category

import Data.Maybe
import Control.Arrow
import Control.Applicative
import Control.Monad.IO.Class
import Control.Exception.Extensible
import Language.C
import Text.PrettyPrint
import qualified Data.Map                               as M

import System.Directory
import System.FilePath
import System.Posix.Process
import System.IO

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.CodeGen

import Foreign.CUDA.Analysis.Device


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
  cufile <- outputName acc (dir </> "dragon.cu")        -- here be dragons!
  flags  <- compileFlags nvcc
  pid    <- liftIO . withFilePath dir $ do
              writeCode cufile $ codeGenAcc acc
              forkProcess      $ executeFile nvcc False (takeFileName cufile : flags) Nothing

  modM kernelEntry $ M.insert (accToKey acc) (KernelEntry cufile (Left pid))


-- Write the generated code to file, exporting C symbols
--
-- TLM: The nasty __global__ attribute resolution issue is only present for
--      cuda < 3.0, might want to add #undefs?
--
writeCode :: FilePath -> CTranslUnit -> IO ()
writeCode f code
  = withFile f WriteMode $ \hdl -> do
      hPutStrLn hdl "extern \"C\" {"
      printDoc PageMode hdl (pretty code)
      hPutStrLn hdl "}"

-- stolen from $fptools/ghc/compiler/utils/Pretty.lhs
--
-- This code has a BSD-style license
--
printDoc :: Mode -> Handle -> Doc -> IO ()
printDoc m hdl doc = do
  fullRender m cols 1.5 put done doc
  hFlush hdl
  where
    put (Chr c)  next = hPutChar hdl c >> next
    put (Str s)  next = hPutStr  hdl s >> next
    put (PStr s) next = hPutStr  hdl s >> next

    done = hPutChar hdl '\n'
    cols = 100


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


-- Return the output directory for compilation by-products. This currently maps
-- to a temporary directory, but could be mode to point towards a persistent
-- cache (eg: getAppUserDataDirectory)
--
outputDir :: IO FilePath
outputDir = do
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  dir <- canonicalizePath $ tmp </> "ac" ++ show pid
  createDirectoryIfMissing True dir
  return dir

