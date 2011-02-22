{-# LANGUAGE CPP, GADTs #-}
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

  -- * generate and compile kernels to realise a computation
  compileAcc, compileAccFun

) where

import Prelude                                          hiding (exp)
import Data.Maybe
import Data.Typeable
import Data.Record.Label
import Control.Arrow                                    (first, second)
import Control.Applicative                              hiding (Const)
import Control.Monad
import Control.Monad.IO.Class
import Language.C
import Text.PrettyPrint
import qualified Data.HashTable                         as Hash

import System.Directory
import System.FilePath
import System.Posix.Process
import System.Mem.StableName
import System.IO
import System.IO.Unsafe

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar                (Array(..), Elt, Shape, Segments)
import Data.Array.Accelerate.Array.Representation       (size)
import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.CodeGen
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.Analysis.Hash

import Foreign.CUDA.Analysis.Device

import Paths_accelerate                                 (getDataDir)

#include "accelerate.h"


-- TLM:
--   These two passes could be combined into one (some overlap with Use nodes),
--   but for now they are kept separate for clarity's sake.
--
--   It might be appropriate to "tag" each node of the AST, with its stable name
--   and accumulate to this tag any meta-information required for execution /
--   dependency tracking, such as array use counts and any inputs which need to
--   bound to scalar expressions (and the associated ordering!).
--

-- Memory management
-- -----------------

-- | Build a hash table of array usage counts. This only contains entries for
-- let-bound arrays that are referenced more than once.
--
-- TLM: this does not include the free variables of function abstractions over
--      array computations.
--
generateUseMap :: (Typeable aenv, Typeable a) => OpenAcc aenv a -> CIO ()
generateUseMap acc' = do
  useMap <- liftIO newAccHashTable
  liftIO $  traverseAcc useMap acc' []
  setM computeTable useMap
  where
    -- Traverse an open expression term
    --
    traverseExp :: Typeable aenv
                => AccHashTable AccNode
                -> OpenExp env aenv e
                -> [(StableAccName, Maybe AccNode -> AccNode)]
                -> IO ()
    traverseExp useMap exp aenv =
      case exp of
        Var _           -> return ()
        Const _         -> return ()
        Tuple t         -> travT t
        Prj _ e         -> travE e
        IndexNil        -> return ()
        IndexCons ix i  -> travE ix >> travE i
        IndexHead ix    -> travE ix
        IndexTail ix    -> travE ix
        PrimConst _     -> return ()
        PrimApp _ e     -> travE e
        Cond p t e      -> travE p >> travE t >> travE e
        IndexScalar a e -> travA a >> travE e
        Shape a         -> travA a
        Size a          -> travA a

      where
        travT :: Typeable aenv => Tuple (OpenExp env aenv) t -> IO ()
        travT NilTup        = return ()
        travT (SnocTup t e) = travE e >> travT t

        travE :: Typeable aenv => OpenExp env aenv e -> IO ()
        travE e = traverseExp useMap e aenv

        travA :: (Typeable aenv, Shape dim, Elt e) => OpenAcc aenv (Array dim e) -> IO ()
        travA a = traverseAcc useMap a aenv

    -- Traverse an open array computation
    --
    -- We keep track of an environment of currently bound sharing variables,
    -- stored in reverse chronological order (outermost variable is at the end
    -- of the list). This is a StableName associated with the bound AST node,
    -- together with the procedure of incrementing that node's reference count.
    -- The latter is required because of those nodes which produce a pair of
    -- output arrays (namely, Scanl' and Scanr').
    --
    traverseAcc :: (Typeable aenv, Typeable a)
                => AccHashTable AccNode
                -> OpenAcc aenv a
                -> [(StableAccName, Maybe AccNode -> AccNode)]
                -> IO ()
    traverseAcc useMap acc aenv =
      case acc of
        -- If the bound array is itself a bound variable, be sure to refer to
        -- the reference count of the real array.
        Let2 (Avar ix) b   ->
          let sn = fst $ prjEnv ix aenv
          in  travA' b ((sn, incSnd) : (sn, incFst) : aenv)

        Let2 a b           -> do
          sn <- makeStableAcc a
          travA  a
          travA' b ((sn, incSnd) : (sn, incFst) : aenv)

        Let (Avar _) _     -> INTERNAL_ERROR(error) "generateUseMap" "assumption failed"
        Let a b            -> do
          sn <- makeStableAcc a
          travA a
          if isAcc2 b then travA' b ((sn, noInc)  : aenv) -- should never get added to the hash table
                      else travA' b ((sn, incFst) : aenv)

        Avar ix            -> updateUseMap (prjEnv ix aenv)
        Apply _f _a        -> INTERNAL_ERROR(error) "generateUseMap" "Apply: not yet implemented"
        Use _              -> return ()
        Unit e             -> travE e
        Reshape e a        -> travE e >> travA a
        Generate e f       -> travE e >> travF f
        Replicate _ e a    -> travE e >> travA a
        Index _ a e        -> travE e >> travA a
        Map f a            -> travF f >> travA a
        ZipWith f a b      -> travF f >> travA a >> travA b
        Fold f e a         -> travF f >> travE e >> travA a
        Fold1 f a          -> travF f >> travA a
        FoldSeg f e a s    -> travF f >> travE e >> travA a >> travA s
        Fold1Seg f a s     -> travF f >> travA a >> travA s
        Scanl f e a        -> travF f >> travE e >> travA a
        Scanl' f e a       -> travF f >> travE e >> travA a
        Scanl1 f a         -> travF f >> travA a
        Scanr f e a        -> travF f >> travE e >> travA a
        Scanr' f e a       -> travF f >> travE e >> travA a
        Scanr1 f a         -> travF f >> travA a
        Permute f a g b    -> travF f >> travA a >> travF g >> travA b
        Backpermute e f a  -> travE e >> travF f >> travA a
        Stencil f _ a      -> travF f >> travA a
        Stencil2 f _ a _ b -> travF f >> travA a >> travA b

      where
        prjEnv :: Idx env t -> [a] -> a
        prjEnv ZeroIdx       (x:_)  = x
        prjEnv (SuccIdx idx) (_:xs) = prjEnv idx xs
        prjEnv _             _      = INTERNAL_ERROR(error) "prjEnv" "inconsistent valuation"

        incFst = maybe (AccNode (1,0) Nothing) (modL usecount (first  (+1)))
        incSnd = maybe (AccNode (0,1) Nothing) (modL usecount (second (+1)))
        noInc  = INTERNAL_ERROR(error) "generateUseMap" "assumption failed"
          -- when let-binding the paired result of a computation (c.f. isAcc2)
          -- that is later rebound and unpacked by a let2.

        isAcc2 :: OpenAcc aenv a -> Bool
        isAcc2 (Scanl' _ _ _) = True
        isAcc2 (Scanr' _ _ _) = True
        isAcc2 _              = False

        travA' :: (Typeable aenv, Typeable a)
                 => OpenAcc aenv a
                 -> [(StableAccName, Maybe AccNode -> AccNode)]
                 -> IO ()
        travA' = traverseAcc useMap

        travA :: (Typeable aenv, Typeable a) => OpenAcc aenv a -> IO ()
        travA a = traverseAcc useMap a aenv

        travE :: Typeable aenv => OpenExp env aenv e -> IO ()
        travE e = traverseExp useMap e aenv

        travF :: Typeable aenv => OpenFun env aenv t -> IO ()
        travF (Lam  f) = travF f
        travF (Body b) = travE b

        updateUseMap (sn,f) = do
          e <- Hash.lookup useMap sn
          _ <- Hash.update useMap sn (f e)
          return ()

makeStableAcc :: (Typeable aenv, Typeable a) => OpenAcc aenv a -> IO StableAccName
makeStableAcc a = StableAccName `fmap` makeStableName a


-- Compilation
-- -----------

-- | Initiate code generation, compilation, and data transfer for an array
-- expression. If we are in `streaming' mode, the input arrays are marked so
-- that they will be retained between each iteration.
--
compileAccFun :: Typeable t => Afun t -> CIO ()
compileAccFun = compileOpenAfun

compileOpenAfun :: (Typeable aenv, Typeable t) => OpenAfun aenv t -> CIO ()
compileOpenAfun (Alam  b) = compileOpenAfun b
compileOpenAfun (Abody f) = generateUseMap f >> generateCode True f

compileAcc :: Arrays a => Acc a -> CIO ()
compileAcc acc = generateUseMap acc >> generateCode False acc

generateCode :: (Typeable aenv, Typeable a) => Bool -> OpenAcc aenv a -> CIO ()
generateCode iss acc' = do
  memMap <- liftIO newAccMemoryTable
  setM memoryTable memMap
  travA acc'
  where
    -- Traverse an open array expression in depth-first order
    --
    travA :: (Typeable aenv, Typeable a) => OpenAcc aenv a -> CIO ()
    travA acc =
      case acc of
        Avar _             -> return ()
        Let a b            -> travA a >> travA b
        Let2 a b           -> travA a >> travA b
        Apply _f _a        -> INTERNAL_ERROR(error) "generateCode" "Apply: not yet implemented"
        Unit e             -> travE e
        Reshape e a        -> travE e >> travA a
        Use arr            -> upload arr
        Generate e f       -> travE e >> travF f                       >> compile acc
        Replicate _ e a    -> travE e >> travA a                       >> compile acc
        Index _ a e        -> travE e >> travA a                       >> compile acc
        Map f a            -> travF f >> travA a                       >> compile acc
        ZipWith f a b      -> travF f >> travA a >> travA b            >> compile acc
        Fold f e a         -> travF f >> travE e >> travA a            >> compile acc
        Fold1 f a          -> travF f >> travA a                       >> compile acc
        FoldSeg f e a s    -> travF f >> travE e >> travA a >> travA s >> compile scan >> compile acc
        Fold1Seg f a s     -> travF f >> travA a >> travA s            >> compile scan >> compile acc
        Scanl f e a        -> travF f >> travE e >> travA a            >> compile acc
        Scanl' f e a       -> travF f >> travE e >> travA a            >> compile acc
        Scanl1 f a         -> travF f >> travA a                       >> compile acc
        Scanr f e a        -> travF f >> travE e >> travA a            >> compile acc
        Scanr' f e a       -> travF f >> travE e >> travA a            >> compile acc
        Scanr1 f a         -> travF f >> travA a                       >> compile acc
        Permute f a g b    -> travF f >> travA a >> travF g >> travA b >> compile acc
        Backpermute e f a  -> travE e >> travF f >> travA a            >> compile acc
        Stencil f _ a      -> travF f >> travA a                       >> compile acc
        Stencil2 f _ a _ b -> travF f >> travA a >> travA b            >> compile acc

      where
        -- TLM: could rewrite the tree to include this additional step; the
        --      stable-name operations would then work appropriately.
        scan = Scanl add (Const ((),0)) (Use (Array undefined undefined :: Segments))
        add  = Lam (Lam (Body (PrimAdd numType
                              `PrimApp`
                              Tuple (NilTup `SnocTup` Var (SuccIdx ZeroIdx)
                                            `SnocTup` Var ZeroIdx))))

        -- Retrieve array use information. If we are under the "streaming"
        -- paradigm, all Use nodes must be retained over successive iterations
        --
        lookupUseCount :: CIO (Maybe Int)
        lookupUseCount
          | iss       = return Nothing
          | otherwise = do
              tab <- getM computeTable
              val <- liftIO $ Hash.lookup tab =<< makeStableAcc acc
              return . Just $ maybe 1 (fst . getL usecount) val

        -- Copy an input array (Use node) to the device
        --
        upload :: Array dim e -> CIO ()
        upload (Array sh ad) = do
          let n = size sh
          c <- lookupUseCount
          mallocArray    ad c (max 1 n)
          pokeArrayAsync ad n Nothing

    -- Traverse scalar expressions looking for embedded array computations
    --
    travE :: Typeable aenv => OpenExp env aenv e -> CIO ()
    travE exp =
      case exp of
        Var _           -> return ()
        Const _         -> return ()
        Tuple t         -> travT t
        Prj _ e         -> travE e
        IndexNil        -> return ()
        IndexCons ix i  -> travE ix >> travE i
        IndexHead ix    -> travE ix
        IndexTail ix    -> travE ix
        PrimConst _     -> return ()
        PrimApp _ e     -> travE e
        Cond p t e      -> travE p >> travE t >> travE e
        IndexScalar a e -> travA a >> travE e
        Shape a         -> travA a
        Size a          -> travA a

    travT :: Typeable aenv => Tuple (OpenExp env aenv) t -> CIO ()
    travT NilTup        = return ()
    travT (SnocTup t e) = travE e >> travT t

    travF :: Typeable aenv => OpenFun env aenv t -> CIO ()
    travF (Body e) = travE e
    travF (Lam b)  = travF b


-- | Generate and compile code for a single open array expression
--
compile :: OpenAcc aenv a -> CIO ()
compile acc = do
  let key = accToKey acc
  kernels  <- getM kernelTable
  compiled <- isJust <$> liftIO (Hash.lookup kernels key)
  unless compiled $ do
    nvcc   <- fromMaybe (error "nvcc: command not found") <$> liftIO (findExecutable "nvcc")
    cufile <- outputName acc (outputDir </> "dragon.cu")        -- rawr!
    flags  <- compileFlags cufile
    pid    <- liftIO $ do
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
  return [ "-I", ddir </> "cubits"
         , "-O2", "--compiler-options", "-fno-strict-aliasing"
         , "-arch=sm_" ++ show (round (arch * 10) :: Int)
         , "-DUNIX"
         , "-cubin"
         , "-o", cufile `replaceExtension` "cubin"
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
    freshVar      = getM unique <* modM unique (+1)


-- Return the output directory for compilation by-products, creating if it does
-- not exist. This is safe in the sense that the result will never change for a
-- given program invocation (TLM: a little excessive, really...)
--
outputDir :: FilePath
{-# NOINLINE outputDir #-}
outputDir = unsafePerformIO $ do
#ifdef ACCELERATE_CUDA_PERSISTENT_CACHE
  tmp <- getDataDir
  let dir = tmp </> "cache"
#else
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  let dir = tmp </> "ac" ++ show pid
#endif
  createDirectoryIfMissing True dir
  canonicalizePath dir

