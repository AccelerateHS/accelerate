{-# LANGUAGE GADTs, RankNTypes #-}
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

import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Control.Applicative                              hiding (Const)
import Language.C
import Text.PrettyPrint
import qualified Data.HashTable                         as Hash

import System.Directory
import System.FilePath
import System.Posix.Process
import System.IO

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar                (Array(..), Segments)
import Data.Array.Accelerate.Array.Representation
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
compileAcc = travA k
  where
    k :: OpenAcc aenv a -> CIO ()
    k (Use (Array sh ad))   = let n = size sh
                              in do mallocArray    ad (max 1 n)
                                    pokeArrayAsync ad n Nothing
    k acc@(FoldSeg _ _ _ _) = compile scan >> compile acc
    k acc@(Fold1Seg  _ _ _) = compile scan >> compile acc
    k acc                   = compile acc

    scan = Scanl add (Const ((),0)) (Use (Array undefined undefined :: Segments))
    add  = Lam (Lam (Body (PrimAdd numType
                          `PrimApp`
                          Tuple (NilTup `SnocTup` (Var (SuccIdx ZeroIdx))
                                        `SnocTup` (Var ZeroIdx)))))


-- Depth-first traversal of the term tree, searching for array expressions to
-- apply the given function to.
--
travA :: (forall a' aenv'. OpenAcc aenv' a' -> CIO ()) -> OpenAcc aenv a -> CIO ()
travA _ (Avar _)                 = return ()
travA f (Let a b)                = travA f a >> travA f b
travA f (Let2 a b)               = travA f a >> travA f b
travA f (Unit e)                 = travE f e
travA f acc@(Generate e g)       = travE f e >> travF f g >> f acc
travA f (Reshape e a)            = travE f e >> travA f a
travA f acc@(Use _)              = f acc
travA f acc@(Replicate _ e a)    = travE f e >> travA f a >> f acc
travA f acc@(Index _ a e)        = travA f a >> travE f e >> f acc
travA f acc@(Map g a)            = travF f g >> travA f a >> f acc
travA f acc@(ZipWith g a b)      = travF f g >> travA f a >> travA f b >> f acc
travA f acc@(Fold g e a)         = travF f g >> travE f e >> travA f a >> f acc
travA f acc@(FoldSeg g e a s)    = travF f g >> travE f e >> travA f a >> travA f s >> f acc
travA f acc@(Fold1 g a)          = travF f g >> travA f a >> f acc
travA f acc@(Fold1Seg g a s)     = travF f g >> travA f a >> travA f s >> f acc
travA f acc@(Scanl g e a)        = travF f g >> travE f e >> travA f a >> f acc
travA f acc@(Scanl' g e a)       = travF f g >> travE f e >> travA f a >> f acc
travA f acc@(Scanl1 g a)         = travF f g >> travA f a >> f acc
travA f acc@(Scanr g e a)        = travF f g >> travE f e >> travA f a >> f acc
travA f acc@(Scanr' g e a)       = travF f g >> travE f e >> travA f a >> f acc
travA f acc@(Scanr1 g a)         = travF f g >> travA f a >> f acc
travA f acc@(Permute g a h b)    = travF f g >> travA f a >> travF f h >> travA f b >> f acc
travA f acc@(Backpermute e g a)  = travE f e >> travF f g >> travA f a >> f acc
travA f acc@(Stencil g _ a)      = travF f g >> travA f a >> f acc
travA f acc@(Stencil2 g _ a _ b) = travF f g >> travA f a >> travA f b >> f acc

travE :: (forall a' aenv'. OpenAcc aenv' a' -> CIO ()) -> OpenExp env aenv e -> CIO ()
travE _ (Var _)           = return ()
travE _ (Const _)         = return ()
travE f (Tuple t)         = travT f t
travE f (Prj _ e)         = travE f e
travE _ (IndexNil)        = return ()
travE f (IndexCons ix i)  = travE f ix >> travE f i
travE f (IndexHead ix)    = travE f ix
travE f (IndexTail ix)    = travE f ix
travE _ (PrimConst _)     = return ()
travE f (PrimApp _ e)     = travE f e
travE f (Cond p t e)      = travE f p >> travE f t >> travE f e
travE f (IndexScalar a e) = travA f a >> travE f e
travE f (Shape a)         = travA f a
travE f (Size a)          = travA f a

travT :: (forall a' aenv'. OpenAcc aenv' a' -> CIO ()) -> Tuple (OpenExp env aenv) t -> CIO ()
travT _ NilTup        = return ()
travT f (SnocTup t e) = travE f e >> travT f t

travF :: (forall a' aenv'. OpenAcc aenv' a' -> CIO ()) -> OpenFun env aenv t -> CIO ()
travF f (Body e) = travE f e
travF f (Lam b)  = travF f b


-- | Generate and compile code for a single open array expression
--
compile :: OpenAcc aenv a -> CIO ()
compile acc = do
  let key = accToKey acc
  kernels  <- getM kernelTable
  compiled <- isJust <$> liftIO (Hash.lookup kernels key)
  unless compiled $ do
    nvcc   <- fromMaybe (error "nvcc: command not found") <$> liftIO (findExecutable "nvcc")
    dir    <- getM outputDir
    cufile <- outputName acc (dir </> "dragon.cu")        -- rawr!
    flags  <- compileFlags cufile
    pid    <- liftIO . withFilePath dir $ do
                writeCode cufile (codeGenAcc acc)
                forkProcess $ executeFile nvcc False flags Nothing

    liftIO $ Hash.insert kernels key (KernelEntry cufile (Left pid))


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
  return [ "-I.", "-I", ddir </> "cubits"
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
  x <- liftIO $ doesFileExist (filename n)
  if x then outputName acc cufile
       else return (filename n)
  where
    (base,suffix) = splitExtension cufile
    filename n    = base ++ pad (show n) <.> suffix
    pad s         = replicate (4-length s) '0' ++ s

