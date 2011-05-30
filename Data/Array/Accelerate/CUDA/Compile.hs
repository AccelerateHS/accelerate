{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns, CPP, GADTs, TupleSections, TypeSynonymInstances #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Compile
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Compile (

  -- * Types parameterising our annotated computation form
  AccKernel, ExecAcc, ExecOpenAcc(..),

  -- * generate and compile kernels to realise a computation
  compileAcc, compileAfun1

) where

#include "accelerate.h"

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.AST                        hiding (Val(..))
import Data.Array.Accelerate.Pretty.Print

import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.CodeGen
import Data.Array.Accelerate.CUDA.Array.Sugar
import Data.Array.Accelerate.CUDA.Analysis.Hash

-- libraries
import Prelude                                          hiding (exp)
import Control.Applicative                              hiding (Const)
import Control.Monad.Trans
import Control.Monad
import Control.Concurrent.MVar
import Data.Maybe
import Data.Label.PureM
import Language.C
import System.FilePath
import System.Directory
import System.IO
import System.Exit                                      (ExitCode(..))
import System.Posix.Types                               (ProcessID)
import System.Posix.Process
import Text.PrettyPrint
import Foreign.Storable
import qualified Data.HashTable                         as Hash
import qualified Foreign.CUDA.Driver                    as CUDA

import Paths_accelerate                                 (getDataDir)


-- A binary object that will be used to execute a kernel
--
type AccKernel a = (String, CIO CUDA.Module)

noKernel :: AccKernel a
noKernel = INTERNAL_ERROR(error) "ExecAcc" "no kernel module for this node"


-- Interleave execution state annotations into an open array computation AST
--
data ExecOpenAcc aenv a where
  ExecAfun :: PreOpenAfun ExecOpenAcc () t
           -> ExecOpenAcc aenv t

  ExecAcc  :: AccKernel a                       -- an executable binary object
           -> [AccBinding aenv]                 -- auxiliary arrays from the environment the kernel needs access to
           -> PreOpenAcc ExecOpenAcc aenv a     -- the actual computation
           -> ExecOpenAcc aenv a

-- An annotated AST suitable for execution in the CUDA environment
--
type ExecAcc a = ExecOpenAcc () a

instance Show (ExecOpenAcc aenv a) where
  show = render . prettyExecAcc 0 noParens


-- |Initiate code generation, compilation, and data transfer for an array
-- expression. If we are in `streaming' mode, then the arrays are marked so that
-- they will be retained between iterations.
--
-- The returned array computation is annotated so to be suitable for execution
-- in the CUDA environment. This includes:
--
--   1. The kernel module that can be used to execute the computation, and the
--      list of array variables that were embedded within scalar expressions
--      (TLM: todo)
--
--   2. Array reference counts (TLM: not accurate in the presence of branches)
--
--   3. Wrap the segment descriptor of FoldSeg and similar in 'Scanl (+) 0', to
--      transform the segment lengths into global offset indices.
--
compileAcc :: Acc a -> CIO (ExecAcc a)
compileAcc acc = prepareAcc acc


compileAfun1 :: Afun (a -> b) -> CIO (ExecAcc (a -> b))
compileAfun1 (Alam (Abody b)) = do
  b' <- prepareAcc b
  return $ ExecAfun (Alam (Abody b'))

compileAfun1 _ =
  error "Hope (noun): something that happens to facts when the world refuses to agree"


prepareAcc :: OpenAcc aenv a -> CIO (ExecOpenAcc aenv a)
prepareAcc rootAcc = do
  travA rootAcc
  where
    -- Traverse an open array expression in depth-first order
    --
    travA :: OpenAcc aenv a -> CIO (ExecOpenAcc aenv a)
    travA acc@(OpenAcc pacc) =
      case pacc of

        -- Environment manipulations
        --
        Avar ix -> return $ node (Avar ix)

        -- Let bindings
        --
        Alet2 a b -> do
          a' <- travA a
          b' <- travA b
          return $ node (Alet2 a' b')

        Alet a b  -> do
          a' <- travA a
          b' <- travA b
          return $ node (Alet a' b')

        Apply (Alam (Abody b)) a -> do
          a' <- travA a
          b' <- travA b
          return $ node (Apply (Alam (Abody b')) a')
        Apply _                _ -> error "I made you a cookie, but I eated it"

        PairArrays arr1 arr2 -> do
          arr1' <- travA arr1
          arr2' <- travA arr2
          return $ node (PairArrays arr1' arr2')

        Acond c t e -> do
          (c', _) <- travE c []
          t'      <- travA t
          e'      <- travA e
          return $ node (Acond c' t' e')

        -- Array injection
        --
        Use arr@(Array _ _) -> do
	  useArray arr
	  return $ node (Use arr)

        -- Computation nodes
        --
        Reshape sh a -> do
          (sh', _) <- travE sh []
          a'       <- travA a
          return $ node (Reshape sh' a')

        Unit e  -> do
          (e', _) <- travE e []
          return $ node (Unit e')

        Generate e f -> do
          (e', _)    <- travE e []
          (f', var1) <- travF f []
          kernel     <- build "generate" acc var1
          return $ ExecAcc kernel var1 (Generate e' f')

        Replicate slix e a -> do
          (e', _) <- travE e []
          a'      <- travA a
          kernel  <- build "replicate" acc []
          return $ ExecAcc kernel [] (Replicate slix e' a')

        Index slix a e -> do
          a'      <- travA a
          (e', _) <- travE e []
          kernel  <- build "slice" acc []
          return $ ExecAcc kernel [] (Index slix a' e')

        Map f a -> do
          (f', var1) <- travF f []
          a'         <- travA a
          kernel     <- build "map" acc var1
          return $ ExecAcc kernel var1 (Map f' a')

        ZipWith f a b -> do
          (f', var1) <- travF f []
          a'         <- travA a
          b'         <- travA b
          kernel     <- build "zipWith" acc var1
          return $ ExecAcc kernel var1 (ZipWith f' a' b')

        Fold f e a -> do
          (f', var1) <- travF f []
          (e', var2) <- travE e var1
          a'         <- travA a
          kernel     <- build "fold" acc var2
          return $ ExecAcc kernel var2 (Fold f' e' a')

        Fold1 f a -> do
          (f', var1) <- travF f []
          a'         <- travA a
          kernel     <- build "fold" acc var1
          return $ ExecAcc kernel var1 (Fold1 f' a')

        FoldSeg f e a s -> do
          (f', var1) <- travF f []
          (e', var2) <- travE e var1
          a'         <- travA a
          s'         <- travA (scan s)
          kernel     <- build "foldSeg" acc var2
          return $ ExecAcc kernel var2 (FoldSeg f' e' a' s')

        Fold1Seg f a s -> do
          (f', var1) <- travF f []
          a'         <- travA a
          s'         <- travA (scan s)
          kernel     <- build "foldSeg" acc var1
          return $ ExecAcc kernel var1 (Fold1Seg f' a' s')

        Scanl f e a -> do
          (f', var1) <- travF f []
          (e', var2) <- travE e var1
          a'         <- travA a
          kernel     <- build "inclusive_scan" acc var2
          return $ ExecAcc kernel var2 (Scanl f' e' a')

        Scanl' f e a -> do
          (f', var1) <- travF f []
          (e', var2) <- travE e var1
          a'         <- travA a
          kernel     <- build "inclusive_scan" acc var2
          return $ ExecAcc kernel var2 (Scanl' f' e' a')

        Scanl1 f a -> do
          (f', var1) <- travF f []
          a'         <- travA a
          kernel     <- build "inclusive_scan" acc var1
          return $ ExecAcc kernel var1 (Scanl1 f' a')

        Scanr f e a -> do
          (f', var1) <- travF f []
          (e', var2) <- travE e var1
          a'         <- travA a
          kernel     <- build "inclusive_scan" acc var2
          return $ ExecAcc kernel var2 (Scanr f' e' a')

        Scanr' f e a -> do
          (f', var1) <- travF f []
          (e', var2) <- travE e var1
          a'         <- travA a
          kernel     <- build "inclusive_scan" acc var2
          return $ ExecAcc kernel var2 (Scanr' f' e' a')

        Scanr1 f a -> do
          (f', var1) <- travF f []
          a'         <- travA a
          kernel     <- build "inclusive_scan" acc var1
          return $ ExecAcc kernel var1 (Scanr1 f' a')

        Permute f a g b -> do
          (f', var1) <- travF f []
          (g', var2) <- travF g var1
          a'         <- travA a
          b'         <- travA b
          kernel     <- build "permute" acc var2
          return $ ExecAcc kernel var2 (Permute f' a' g' b')

        Backpermute e f a -> do
          (e', _)    <- travE e []
          (f', var2) <- travF f []
          a'         <- travA a
          kernel     <- build "backpermute" acc var2
          return $ ExecAcc kernel var2 (Backpermute e' f' a')

        Stencil f b a -> do
          (f', var1) <- travF f []
          a'         <- travA a
          kernel     <- build "stencil" acc var1
          return $ ExecAcc kernel var1 (Stencil f' b a')

        Stencil2 f b1 a1 b2 a2 -> do
          (f', var1) <- travF f []
          a1'        <- travA a1
          a2'        <- travA a2
          kernel     <- build "stencil2" acc var1
          return $ ExecAcc kernel var1 (Stencil2 f' b1 a1' b2 a2')


    -- Traverse a scalar expression
    --
    travE :: OpenExp env aenv e
          -> [AccBinding aenv]
          -> CIO (PreOpenExp ExecOpenAcc env aenv e, [AccBinding aenv])
    travE exp vars =
      case exp of
        Let _ _         -> INTERNAL_ERROR(error) "prepareAcc" "Let: not implemented yet"
        Var ix          -> return (Var ix, vars)
        Const c         -> return (Const c, vars)
        PrimConst c     -> return (PrimConst c, vars)
        IndexAny        -> INTERNAL_ERROR(error) "prepareAcc" "IndexAny: not implemented yet"
        IndexNil        -> return (IndexNil, vars)
        IndexCons ix i  -> do
          (ix', var1) <- travE ix vars
          (i',  var2) <- travE i  var1
          return (IndexCons ix' i', var2)

        IndexHead ix    -> do
          (ix', var1) <- travE ix vars
          return (IndexHead ix', var1)

        IndexTail ix    -> do
          (ix', var1) <- travE ix vars
          return (IndexTail ix', var1)

        Tuple t         -> do
          (t', var1) <- travT t vars
          return (Tuple t', var1)

        Prj idx e       -> do
          (e', var1) <- travE e vars
          return (Prj idx e', var1)

        Cond p t e      -> do
          (p', var1) <- travE p vars
          (t', var2) <- travE t var1
          (e', var3) <- travE e var2
          return (Cond p' t' e', var3)

        PrimApp f e     -> do
          (e', var1) <- travE e vars
          return (PrimApp f e', var1)

        IndexScalar a e -> do
          a'         <- travA a
          (e', var2) <- travE e vars
          return (IndexScalar a' e', bind a' `cons` var2)

        Shape a         -> do
          a' <- travA a
          return (Shape a', bind a' `cons` vars)

        ShapeSize e     -> do
          (e', var1) <- travE e vars
          return (ShapeSize e', var1)


    travT :: Tuple (OpenExp env aenv) t
          -> [AccBinding aenv]
          -> CIO (Tuple (PreOpenExp ExecOpenAcc env aenv) t, [AccBinding aenv])
    travT NilTup        vars = return (NilTup, vars)
    travT (SnocTup t e) vars = do
      (e', var1) <- travE e vars
      (t', var2) <- travT t var1
      return (SnocTup t' e', var2)

    travF :: OpenFun env aenv t
          -> [AccBinding aenv]
          -> CIO (PreOpenFun ExecOpenAcc env aenv t, [AccBinding aenv])
    travF (Body b) vars = do
      (b', var1) <- travE b vars
      return (Body b', var1)
    travF (Lam  f) vars = do
      (f', var1) <- travF f vars
      return (Lam f', var1)


    -- Auxiliary
    --
    scan :: OpenAcc aenv Segments -> OpenAcc aenv Segments
    scan = OpenAcc . Scanl plus (Const ((),0))

    plus :: PreOpenFun OpenAcc () aenv (Int -> Int -> Int)
    plus = Lam (Lam (Body (PrimAdd numType
                          `PrimApp`
                          Tuple (NilTup `SnocTup` Var (SuccIdx ZeroIdx)
                                        `SnocTup` Var ZeroIdx))))

    node :: PreOpenAcc ExecOpenAcc aenv a -> ExecOpenAcc aenv a
    node = ExecAcc noKernel []

    cons :: AccBinding aenv -> [AccBinding aenv] -> [AccBinding aenv]
    cons x xs | x `notElem` xs = x : xs
              | otherwise      = xs

    bind :: (Shape sh, Elt e) => ExecOpenAcc aenv (Array sh e) -> AccBinding aenv
    bind (ExecAcc _ _ (Avar ix)) = ArrayVar ix
    bind _                       =
     INTERNAL_ERROR(error) "bind" "expected array variable"


-- Compilation
-- -----------

-- Initiate compilation and provide a closure to later link the compiled module
-- when it is required.
--
-- TLM: should get name(s) from code generation
--
build :: String -> OpenAcc aenv a -> [AccBinding aenv] -> CIO (AccKernel a)
build name acc fvar =
  let key = accToKey acc
  in do
    mvar   <- liftIO newEmptyMVar
    table  <- gets kernelTable
    cached <- isJust `fmap` liftIO (Hash.lookup table key)
    unless cached $ compile table key acc fvar
    return . (name,) . liftIO $ memo mvar (link table key)

-- A simple memoisation routine
-- TLM: maybe we can be a bit clever than this...
--
memo :: MVar a -> IO a -> IO a
memo mvar fun = do
  full <- not `fmap` isEmptyMVar mvar
  if full
     then readMVar mvar
     else do a <- fun
             putMVar mvar a
             return a


-- Link a compiled binary and update the associated kernel entry in the hash
-- table. This may entail waiting for the external compilation process to
-- complete. If successfully, the temporary files are removed.
--
link :: KernelTable -> AccKey -> IO CUDA.Module
link table key =
  let intErr = INTERNAL_ERROR(error) "link" "missing kernel entry"
  in do
    (KernelEntry cufile stat) <- fromMaybe intErr `fmap` Hash.lookup table key
    case stat of
      Right mdl -> return mdl
      Left  pid -> do
        -- wait for compiler to finish and load binary object
        --
        waitFor pid
        mdl <- CUDA.loadFile (replaceExtension cufile ".cubin")

#ifndef ACCELERATE_CUDA_PERSISTENT_CACHE
        -- remove build products
        --
        removeFile      cufile
        removeFile      (replaceExtension cufile ".cubin")
        removeDirectory (dropFileName cufile)
          `catch` \_ -> return ()       -- directory not empty
#endif

        -- update hash table
        --
        Hash.insert table key (KernelEntry cufile (Right mdl))
        return mdl


-- Generate and compile code for a single open array expression
--
compile :: KernelTable -> AccKey -> OpenAcc aenv a -> [AccBinding aenv] -> CIO ()
compile table key acc fvar = do
  dir     <- outputDir
  nvcc    <- fromMaybe (error "nvcc: command not found") <$> liftIO (findExecutable "nvcc")
  cufile  <- outputName acc (dir </> "dragon.cu")        -- rawr!
  flags   <- compileFlags cufile
  pid     <- liftIO $ do
               writeCode cufile (codeGenAcc acc fvar)
               forkProcess $ executeFile nvcc False flags Nothing
  --
  liftIO $ Hash.insert table key (KernelEntry cufile (Left pid))


-- Wait for the compilation process to finish
--
waitFor :: ProcessID -> IO ()
waitFor pid = do
  status <- getProcessStatus True True pid
  case status of
    Just (Exited ExitSuccess) -> return ()
    _                         -> error  $ "nvcc (" ++ show pid ++ ") terminated abnormally"


-- Determine the appropriate command line flags to pass to the compiler process.
-- This is dependent on the host architecture and device capabilities.
--
compileFlags :: FilePath -> CIO [String]
compileFlags cufile =
  let machine = case sizeOf (undefined :: Int) of
                  4 -> "-m32"
                  8 -> "-m64"
                  _ -> error "huh? non 32-bit or 64-bit architecture"
  in do
  arch <- CUDA.computeCapability <$> gets deviceProps
  ddir <- liftIO getDataDir
  return [ "-I", ddir </> "cubits"
         , "-O2", "--compiler-options", "-fno-strict-aliasing"
         , "-arch=sm_" ++ show (round (arch * 10) :: Int)
         , "-DUNIX"
         , "-cubin"
         , "-o", cufile `replaceExtension` "cubin"
         , machine
         , cufile ]


-- Return a unique output filename for the generated CUDA code
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
    freshVar      = gets unique <* modify unique (+1)


-- Return the output directory for compilation by-products, creating if it does
-- not exist.
--
outputDir :: CIO FilePath
outputDir = liftIO $ do
#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
  tmp <- getDataDir
  let dir = tmp </> "cache"
#else
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  let dir = tmp </> "accelerate-cuda-" ++ show pid
#endif
  createDirectoryIfMissing True dir
  canonicalizePath dir


-- Pretty printing
-- ---------------

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


-- Display the annotated AST
--
prettyExecAcc :: PrettyAcc ExecOpenAcc
prettyExecAcc alvl wrap ecc =
  case ecc of
    ExecAfun pfun      -> prettyPreAfun prettyExecAcc alvl pfun
    ExecAcc  _ fv pacc ->
      let base = prettyPreAcc prettyExecAcc alvl wrap pacc
          ann  = braces (freevars fv)
      in case pacc of
           Avar _         -> base
           Alet  _ _      -> base
           Alet2 _ _      -> base
           Apply _ _      -> base
           PairArrays _ _ -> base
           Acond _ _ _    -> base
           _              -> ann <+> base
  where
    freevars = (text "fv=" <>) . brackets . hcat . punctuate comma
                               . map (\(ArrayVar ix) -> char 'a' <> int (deBruijnToInt ix))

