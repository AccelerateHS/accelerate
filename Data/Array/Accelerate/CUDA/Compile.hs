{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Compile
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
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
import Control.Applicative                              hiding (Const)
import Language.C
import Text.PrettyPrint
import qualified Data.HashTable                         as HT

import System.Directory
import System.FilePath
import System.Posix.Process
import System.IO

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Sugar                (Array(..))
import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.CodeGen
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.Analysis.Hash

import Foreign.CUDA.Analysis.Device

import Paths_accelerate                                 (getDataDir)


-- | Initiate code generation, compilation, and data transfer for an array
-- expression
--
compileAcc :: OpenAcc aenv a -> CIO ()
compileAcc (Use (Array sh ad)) =
  let n = size sh
  in  when (n > 0) $ do
        mallocArray    ad n
        pokeArrayAsync ad n Nothing

compileAcc (Let  a1 a2)    = compileAcc a1 >> compileAcc a2
compileAcc (Let2 a1 a2)    = compileAcc a1 >> compileAcc a2
compileAcc (Avar _)        = return ()
compileAcc (Unit e1)       = compileExp e1
compileAcc (Reshape e1 a1) = compileExp e1 >> compileAcc a1

compileAcc op@(Replicate _ e1 a1)    = compileExp e1 >> compileAcc a1 >> compile op
compileAcc op@(Index _ a1 e1)        = compileAcc a1 >> compileExp e1 >> compile op
compileAcc op@(Map f1 a1)            = compileFun f1 >> compileAcc a1 >> compile op
compileAcc op@(ZipWith f1 a1 a2)     = compileFun f1 >> compileAcc a1 >> compileAcc a2 >> compile op
compileAcc op@(Fold f1 e1 a1)        = compileFun f1 >> compileExp e1 >> compileAcc a1 >> compile op
compileAcc op@(Scanl f1 e1 a1)       = compileFun f1 >> compileExp e1 >> compileAcc a1 >> compile op
compileAcc op@(Scanr f1 e1 a1)       = compileFun f1 >> compileExp e1 >> compileAcc a1 >> compile op
compileAcc op@(Backpermute e1 f1 a1) = compileExp e1 >> compileFun f1 >> compileAcc a1 >> compile op
compileAcc op@(Permute f1 a1 f2 a2)  = compileFun f1 >> compileAcc a1 >> compileFun f2 >> compileAcc a2 >> compile op
compileAcc op@(FoldSeg f1 e1 a1 a2)  = compileFun f1 >> compileExp e1 >> compileAcc a1 >> compileAcc a2 >> compile op
compileAcc (Stencil _ _ _)           = 
  error "D.A.Accelerate.CUDA.Compile: the CUDA backend does not support 'stencil' operations yet"
compileAcc (Stencil2 _ _ _ _ _)      = 
  error "D.A.Accelerate.CUDA.Compile: the CUDA backend does not support 'stencil2' operations yet"


-- | Lift array expressions out of closed functions for code generation
--
compileFun :: OpenFun env aenv t -> CIO ()
compileFun (Body e) = compileExp e
compileFun (Lam f)  = compileFun f


-- | Lift array computations out of scalar expressions for code generation. The
-- array expression must not contain any free scalar variables.
--
compileExp :: OpenExp env aenv a -> CIO ()
compileExp (Tuple t)           = compileTup t
compileExp (Prj _ e1)          = compileExp e1
compileExp (PrimApp _ e1)      = compileExp e1
compileExp (Cond e1 e2 e3)     = compileExp e1 >> compileExp e2 >> compileExp e3
compileExp (IndexScalar a1 e1) = compileAcc a1 >> compileExp e1
compileExp (Shape a1)          = compileAcc a1
compileExp _                   = return ()

compileTup :: Tuple (OpenExp env aenv) a -> CIO ()
compileTup NilTup          = return ()
compileTup (t `SnocTup` e) = compileExp e >> compileTup t


-- | Generate and compile code for a single open array expression
--
compile :: OpenAcc aenv a -> CIO ()
compile acc = do
  let key = accToKey acc
  table    <- getM kernelTable
  compiled <- isJust <$> liftIO (HT.lookup table key)
  unless compiled $ do
    nvcc   <- fromMaybe (error "nvcc: command not found") <$> liftIO (findExecutable "nvcc")
    dir    <- getM outputDir
    cufile <- outputName acc (dir </> "dragon.cu")        -- here be dragons!
    flags  <- compileFlags cufile
    pid    <- liftIO . withFilePath dir $ do
                writeCode cufile (codeGenAcc acc)
                forkProcess $ executeFile nvcc False flags Nothing

    liftIO $ HT.insert table key (KernelEntry cufile (Left pid))


-- Write the generated code to file
--
writeCode :: FilePath -> CUTranslSkel -> IO ()
writeCode f code =
  withFile f WriteMode $ \hdl ->
  printDoc PageMode hdl (pretty code)


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
  ddir <- liftIO getDataDir
  return [ "-I.", "-I", ddir
         , "-O2", "--compiler-options", "-fno-strict-aliasing"
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

