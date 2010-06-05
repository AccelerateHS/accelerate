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

import Prelude   hiding (id, (.), mod)
import qualified Prelude
import Control.Category

import Data.Char
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Exception.Extensible
import Language.C
import Text.PrettyPrint
import qualified Data.IntMap                            as IM

import System.Directory
import System.FilePath
import System.Posix.Process
import System.IO

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.CodeGen

import Foreign.CUDA.Analysis.Device


-- |
-- Generate a unique key for each kernel computation (not extensively tested...)
--
accToKey :: OpenAcc aenv a -> Int
accToKey = quad . accKey
  where
    accKey :: OpenAcc aenv a -> String
    accKey (Unit e)            = chr 2   : showTy (expType e) ++ show e
    accKey (Reshape _ a)       = chr 7   : showTy (accType a)
    accKey (Replicate _ e a)   = chr 17  : showTy (accType a) ++ show e
    accKey (Index _ a e)       = chr 29  : showTy (accType a) ++ show e
    accKey (Map f a)           = chr 41  : showTy (accType a) ++ show f
    accKey (ZipWith f x y)     = chr 53  : showTy (accType x) ++ showTy (accType y) ++ show f
    accKey (Fold f e a)        = chr 67  : showTy (accType a) ++ show f ++ show e
    accKey (FoldSeg f e _ a)   = chr 79  : showTy (accType a) ++ show f ++ show e
    accKey (Scanl f e a)       = chr 97  : showTy (accType a) ++ show f ++ show e
    accKey (Scanr f e a)       = chr 107 : showTy (accType a) ++ show f ++ show e
    accKey (Permute c e p a)   = chr 127 : showTy (accType a) ++ show c ++ show e ++ show p
    accKey (Backpermute p _ a) = chr 139 : showTy (accType a) ++ show p
    accKey _ =
      error "we should never get here"

    showTy :: TupleType a -> String
    showTy UnitTuple = []
    showTy (SingleTuple ty) = show ty
    showTy (PairTuple a b)  = showTy a ++ showTy b


-- hash function from the dragon book pp437; assumes 7 bit characters and needs
-- the (nearly) full range of values guaranteed for `Int' by the Haskell
-- language definition; can handle 8 bit characters provided we have 29 bit for
-- the `Int's without sign
--
quad :: String -> Int
quad (c1:c2:c3:c4:s)  = (( ord c4 * bits21
                         + ord c3 * bits14
                         + ord c2 * bits7
                         + ord c1)
                         `Prelude.mod` bits28)
                        + (quad s `Prelude.mod` bits28)
quad (c1:c2:c3:[]  )  = ord c3 * bits14 + ord c2 * bits7 + ord c1
quad (c1:c2:[]     )  = ord c2 * bits7 + ord c1
quad (c1:[]        )  = ord c1
quad ([]           )  = 0

bits7, bits14, bits21, bits28 :: Int
bits7  = 2^(7 ::Int)
bits14 = 2^(14::Int)
bits21 = 2^(21::Int)
bits28 = 2^(28::Int)


-- | Generate and compile code for an array expression
--
compile :: OpenAcc aenv a -> CIO ()
compile acc = do
  let key = accToKey acc
  compiled <- IM.member key <$> getM kernelEntry
  when (not compiled) $ do
    nvcc   <- fromMaybe (error "nvcc: command not found") <$> liftIO (findExecutable "nvcc")
    dir    <- liftIO outputDir
    cufile <- outputName acc (dir </> "dragon.cu")        -- here be dragons!
    flags  <- compileFlags nvcc
    pid    <- liftIO . withFilePath dir $ do
                writeCode cufile $ codeGenAcc acc
                forkProcess      $ executeFile nvcc False (takeFileName cufile : flags) Nothing

    modM kernelEntry $ IM.insert key (KernelEntry cufile (Left pid))


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

