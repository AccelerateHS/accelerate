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

module Data.Array.Accelerate.CUDA.Compile (compileAcc)
  where

import Prelude   hiding (id, (.), mod)
import Control.Category

import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Control.Applicative
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
import Data.Array.Accelerate.CUDA.Analysis.Hash

import Foreign.CUDA.Analysis.Device



compileAcc :: OpenAcc aenv a -> CIO ()
compileAcc = compile


-- | Generate and compile code for an array expression
--
compile :: OpenAcc aenv a -> CIO ()
compile acc = do
  let key = accToKey acc
  compiled <- M.member key <$> getM kernelEntry
  unless compiled $ do
    nvcc   <- fromMaybe (error "nvcc: command not found") <$> liftIO (findExecutable "nvcc")
    dir    <- liftIO outputDir
    cufile <- outputName acc (dir </> "dragon.cu")        -- here be dragons!
    flags  <- compileFlags cufile
    pid    <- liftIO . withFilePath dir $ do
                writeCode cufile $ codeGenAcc acc
                forkProcess      $ executeFile nvcc False flags Nothing

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
compileFlags :: FilePath -> CIO [String]
compileFlags cufile = do
  arch <- computeCapability <$> getM deviceProps
  return [ "-O3", "-m32", "--compiler-options", "-fno-strict-aliasing"
         , "-arch=sm_" ++ show (round (arch * 10) :: Int)
         , "-DUNIX"
         , "-cubin"
         , takeFileName cufile ]


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

