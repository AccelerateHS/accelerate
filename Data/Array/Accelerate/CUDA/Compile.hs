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
import Control.Monad
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


-- | Generate a unique key for each kernel computation
--
accToKey :: OpenAcc aenv a -> Key
--accToKey (Let  _ _)          = 0
--accToKey (Let2 _ _)          = 1
--accToKey (Avar _)            = 2
--accToKey (Use  _)            = 3
--accToKey (Unit _)            = 4
--accToKey (Reshape _ _)       = 5
accToKey (Replicate _ e _)   = (6,  show e)
--accToKey (Index _ _ _)       = 7
accToKey (Map f _)           = (8,  show f)
accToKey (ZipWith f _ _)     = (9,  show f)
accToKey (Fold f e _)        = (10, show f ++ show e)
accToKey (FoldSeg f e _ _)   = (11, show f ++ show e)
accToKey (Scanl f e _)       = (12, show f ++ show e)
accToKey (Scanr f e _)       = (13, show f ++ show e)
accToKey (Permute c e p _)   = (14, show c ++ show e ++ show p)
accToKey (Backpermute p _ _) = (15, show p)

accToKey _ = error "Data.Array.Accelerate.CUDA.accToKey: internal error"


-- | Generate and compile code for an array expression
--
compile :: OpenAcc aenv a -> CIO ()
compile acc = do
  let key = accToKey acc
  compiled <- M.member key <$> getM kernelEntry
  when (not compiled) $ do
    nvcc   <- fromMaybe (error "nvcc: command not found") <$> liftIO (findExecutable "nvcc")
    dir    <- liftIO outputDir
    cufile <- outputName acc (dir </> "dragon.cu")        -- here be dragons!
    flags  <- compileFlags nvcc
    pid    <- liftIO . withFilePath dir $ do
                writeCode cufile $ codeGenAcc acc
                forkProcess      $ executeFile nvcc False (takeFileName cufile : flags) Nothing

    modM kernelEntry $ M.insert key (KernelEntry cufile (Left pid))


-- Write the generated code to file, exporting C symbols
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

