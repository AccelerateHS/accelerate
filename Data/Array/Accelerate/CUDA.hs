{-# LANGUAGE FlexibleContexts, GADTs, PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies      #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA
-- Copyright   : [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module is the CUDA backend for the embedded array language.

module Data.Array.Accelerate.CUDA (

  -- * Generate CUDA code and execute it
  Arrays, run

) where

-- standard libraries
import Control.Monad
import Control.Monad.State
import Data.Maybe           (fromMaybe)
import Foreign
import System.Exit          (ExitCode(ExitSuccess))
import System.Posix.Process (ProcessStatus(..), executeFile, forkProcess, getProcessStatus)
import System.Posix.Types   (ProcessID)
import Control.Exception

import qualified Data.Map              as M (empty, insert, lookup)
import qualified Data.ByteString.Char8 as B

-- friends
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Sugar (Array(..))
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Smart        as Sugar
import qualified Data.Array.Accelerate.Array.Sugar  as Sugar

import qualified Data.Array.Accelerate.CUDA.Data    as CUDA
import qualified Data.Array.Accelerate.CUDA.Monad   as CUDA
import qualified Data.Array.Accelerate.CUDA.Scalar  as CUDA
import qualified Data.Array.Accelerate.CUDA.Syntax  as CUDA
import qualified Data.Array.Accelerate.CUDA.Fold    as CUDA
import qualified Data.Array.Accelerate.CUDA.Map     as CUDA
import qualified Data.Array.Accelerate.CUDA.ZipWith as CUDA

import qualified Foreign.CUDA.Driver                as CUDA
  (
    ContextFlag(..), DeviceProperties(..), FunParam(..),
    create, destroy, device, devPtrToWordPtr, getFun, initialise, launch,
    loadDataEx, maxGridSize, nullDevPtr, props, setBlockShape, setParams,
    setSharedSize, sync
  )


-- Program execution
-- -----------------

-- |Characterises the types that may be returned when running an array program.
--
class Arrays as where

instance Arrays ()
instance Arrays (Array dim e)
instance (Arrays as1, Arrays as2) => Arrays (as1, as2)

-- |Compiles and runs a complete embedded array program using the CUDA backend.
--
run :: Arrays a => Sugar.Acc a -> IO a
run acc =
  bracket (initialise Nothing) finalise $ \_ ->
    execStateT (memHtoD    ast) ist >>=
    execStateT (codeGenAcc ast)     >>=
    runStateT  (execute    ast)     >>= \(a,s) ->
    execStateT (memDtoH    ast a) s >>  return a
  where
    ast = Sugar.convertAcc acc
    ist = CUDA.CGState 0 0 M.empty M.empty M.empty M.empty

    finalise     = CUDA.destroy . snd
    initialise n = do
      CUDA.initialise []
      dev <- CUDA.device (fromMaybe 0 n)
      ctx <- CUDA.create dev [CUDA.SchedAuto]
      return (dev, ctx)


-- |Executes the generated code
--
execute :: OpenAcc aenv a -> CUDA.CGIO a
execute op@(Fold fun def xs) = do
  currentState <- get
  case M.lookup (show fun) (CUDA.foldMap currentState) of
    Nothing -> error $ "Code generation for \n\t" ++ show op ++ "\nfailed."
    Just v  -> do
      _      <- getCompilerProcessStatus (CUDA.compilerPID v)
      props  <- liftIO $ CUDA.device 0 >>= CUDA.props
      ptx    <- liftIO $ B.readFile (CUDA.progName v ++ ".ptx")
      (m, _) <- liftIO $ CUDA.loadDataEx ptx []
      fun    <- liftIO $ CUDA.getFun m ("_" ++ CUDA.progName v ++ "Scan4")
      Array sh rf <- execute xs
      let n              = size sh
          ctaSize        = 128
          numElemsPerCTA = ctaSize * 8
          execute' e rf' n' = do
            let ys'@(Array _newSh' newRf') = newArray_ (Sugar.toElem ())
                isFullBlock = if n' `mod` numElemsPerCTA == 0 then 1 else 0
                numBlocks   = (n' + numElemsPerCTA - 1) `div` numElemsPerCTA
                gridDim     = getGridDim props numBlocks
            CUDA.mallocArray newRf' numBlocks
            xsFunParams <- CUDA.toFunParams rf'
            ysFunParams <- CUDA.toFunParams newRf'
            liftIO $ CUDA.setSharedSize
              fun (fromIntegral $ 256 * sizeOf (undefined::Float))
            liftIO $ CUDA.setParams fun $
              [CUDA.IArg e] ++ toFunParams def ++ xsFunParams ++ ysFunParams ++
              [CUDA.IArg n', CUDA.IArg isFullBlock]
            liftIO $ CUDA.setBlockShape fun (ctaSize, 1, 1)
            liftIO $ CUDA.launch fun gridDim Nothing
            liftIO $ CUDA.sync
            CUDA.decUse rf' >>= \ decUse -> when (decUse == 0) $ CUDA.free rf'
            if numBlocks == 1
              then return ys'
              else execute' 0 newRf' numBlocks
      execute' 1 rf (n + 1)

execute op@(Map fun xs) = do
  currentState <- get
  case M.lookup (show fun) (CUDA.mapMap currentState) of
    Nothing -> error $ "Code generation for \n\t" ++ show op ++ "\nfailed."
    Just v  -> do
      _      <- getCompilerProcessStatus (CUDA.compilerPID v)
      props  <- liftIO $ CUDA.device 0 >>= CUDA.props
      ptx    <- liftIO $ B.readFile (CUDA.progName v ++ ".ptx")
      (m, _) <- liftIO $ CUDA.loadDataEx ptx []
      fun    <- liftIO $ CUDA.getFun m ('_' : CUDA.progName v)
      (Array sh rf) <- execute xs
      let ys@(Array newSh newRf) = newArray_ (Sugar.toElem sh)
          n              = size newSh
          ctaSize        = 128
          numElemsPerCTA = ctaSize * 2
          numBlocks      = (n + numElemsPerCTA - 1) `div` numElemsPerCTA
          gridDim        = getGridDim props numBlocks
          isFullBlock    = if n `mod` numElemsPerCTA == 0 then 1 else 0
      CUDA.mallocArray newRf n
      xsFunParams <- CUDA.toFunParams rf
      ysFunParams <- CUDA.toFunParams newRf
      liftIO $ CUDA.setParams fun $
        xsFunParams ++ ysFunParams ++ [CUDA.IArg n, CUDA.IArg isFullBlock]
      liftIO $ CUDA.setBlockShape fun (ctaSize, 1, 1)
      liftIO $ CUDA.launch fun gridDim Nothing
      liftIO $ CUDA.sync
      CUDA.decUse rf >>= \ decUse -> when (decUse == 0) $ CUDA.free rf
      return ys

execute op@(ZipWith fun xs ys) = do
  currentState <- get
  case M.lookup (show fun) (CUDA.zipWithMap currentState) of
    Nothing -> error $ "Code generation for \n\t" ++ show op ++ "\nfailed."
    Just v  -> do
      _      <- getCompilerProcessStatus (CUDA.compilerPID v)
      props  <- liftIO $ CUDA.device 0 >>= CUDA.props
      ptx    <- liftIO $ B.readFile (CUDA.progName v ++ ".ptx")
      (m, _) <- liftIO $ CUDA.loadDataEx ptx []
      fun    <- liftIO $ CUDA.getFun m ('_' : CUDA.progName v)
      (Array sh1 rf1) <- execute xs
      (Array sh2 rf2) <- execute ys
      let zs@(Array newSh newRf) = newArray_ (Sugar.toElem $ intersect sh1 sh2)
          n              = size newSh
          ctaSize        = 128
          numElemsPerCTA = ctaSize * 2
          numBlocks      = (n + numElemsPerCTA - 1) `div` numElemsPerCTA
          gridDim        = getGridDim props numBlocks
          isFullBlock    = if n `mod` numElemsPerCTA == 0 then 1 else 0
      CUDA.mallocArray newRf n
      xsFunParams <- CUDA.toFunParams rf1
      ysFunParams <- CUDA.toFunParams rf2
      zsFunParams <- CUDA.toFunParams newRf
      liftIO $ CUDA.setParams fun $
        xsFunParams ++ ysFunParams ++ zsFunParams ++
        [CUDA.IArg n, CUDA.IArg isFullBlock]
      liftIO $ CUDA.setBlockShape fun (ctaSize, 1, 1)
      liftIO $ CUDA.launch fun gridDim Nothing
      liftIO $ CUDA.sync
      CUDA.decUse rf1 >>= \ decUse -> when (decUse == 0) $ CUDA.free rf1
      CUDA.decUse rf2 >>= \ decUse -> when (decUse == 0) $ CUDA.free rf2
      return zs

execute (Use arr) =
  return arr

getGridDim :: CUDA.DeviceProperties -> Int -> (Int, Int)
getGridDim props numBlocks =
  let (maxX, maxY, _) = CUDA.maxGridSize props
  in  (min maxX numBlocks, (numBlocks + maxY - 1) `div` maxY)

-- Create an array for output
newArray_ :: (Sugar.Ix dim, Sugar.Elem e) => dim -> Array dim e
{-# INLINE newArray_ #-}
newArray_ sh
  = adata `seq` Array (Sugar.fromElem sh) adata
  where
    (adata, _) = runArrayData $ do
                   -- FIXME: (max 1024 $ Sugar.size sh), because small arrays
                   -- are relocated by the GC.
                   arr <- newArrayData (max 1024 $ Sugar.size sh)
                   return (arr, undefined)

getCompilerProcessStatus :: ProcessID -> CUDA.CGIO ProcessStatus
getCompilerProcessStatus pid = do
  status <- liftIO $ getProcessStatus True True pid
  case status of
    Just s@(Exited ExitSuccess) -> return s
    _ -> error $ "An nvcc process (PID=" ++ show pid
              ++ ") terminated abnormally."

--
-- Host To GPU data transfer
-- -------------------------

-- |Allocates device memory and triggers asynchronous data transfer.
--
memHtoD :: OpenAcc aenv a -> CUDA.CGIO ()
memHtoD (Map     _ xs)          = memHtoD xs
memHtoD (ZipWith _ xs ys)       = memHtoD xs >> memHtoD ys
memHtoD (Fold    _ _  xs)       = memHtoD xs
memHtoD (Use     (Array sh rf)) = do
  let numElems  = size sh
--      allocFlag = [CUDA.Portable, CUDA.WriteCombined]
--  hptrs <- CUDA.mallocHostArray e allocFlag numElems
  CUDA.mallocArray    rf numElems
  CUDA.pokeArrayAsync rf numElems Nothing

memHtoD op = error $ shows op " not supported yet by the code generator."

--
-- GPU To Host data transfer
-- -------------------------

-- |Transfers the result from GPU
--
memDtoH :: OpenAcc aenv a -> a -> CUDA.CGIO ()
memDtoH (Fold _ _ _)    (Array sh rf) = CUDA.peekArray rf (size sh) >> CUDA.free rf
memDtoH (Map _ _)       (Array sh rf) = CUDA.peekArray rf (size sh) >> CUDA.free rf
memDtoH (ZipWith _ _ _) (Array sh rf) = CUDA.peekArray rf (size sh) >> CUDA.free rf

--
-- CUDA code compilation flags
-- ---------------------------

cuCompileFlags :: String -> [String]
cuCompileFlags progName =
  [ "-m32", "--compiler-options", "-fno-strict-aliasing", "-DUNIX"
  , "-O2", "-o", progName ++ ".ptx", "-ptx", progName ++ ".cu"]

--
-- CUDA/PTX code generation and compilation
-- ----------------------------------------

-- Generate CUDA code and PTX code for an array expression
--
codeGenAcc :: OpenAcc aenv a -> CUDA.CGIO ()
codeGenAcc op@(Fold fun left xs) = do
  currentState <- get
  let uniqueID = CUDA.uniqueID currentState
      progName = "CUDAFold" ++ show uniqueID
      foldMap      = CUDA.foldMap currentState
      foldMapKey   = show fun
      foldMapValue = case M.lookup foldMapKey foldMap of
        Just  v -> v
        Nothing -> CUDA.OperationValue 0 progName (CUDA.devPtrToWordPtr CUDA.nullDevPtr)
      scalar   = CUDA.Scalar
        { CUDA.params   =
          [ (codeGenTupleType $ expType left, "x1")
          , (codeGenTupleType $ accType xs,   "x0")]
        , CUDA.outTy    = codeGenTupleType $ accType op
        , CUDA.comp     = codeGenFun fun
        , CUDA.identity = Nothing}
  liftIO $ CUDA.foldGen progName scalar (codeGenTupleType $ expType left, "left")
  pid <- liftIO $ forkProcess $
    executeFile "nvcc" True (cuCompileFlags progName) Nothing
  let foldMapValue' = foldMapValue
        {CUDA.compilerPID = pid, CUDA.progName  = progName}
  put $ currentState
    { CUDA.uniqueID = uniqueID + 1
    , CUDA.foldMap  =
      M.insert foldMapKey foldMapValue' foldMap}
codeGenAcc op@(Map fun xs) = do
  currentState <- get
  let uniqueID    = CUDA.uniqueID currentState
      progName    = "CUDAMap" ++ show uniqueID
      mapMap      = CUDA.mapMap currentState
      mapMapKey   = show fun
      mapMapValue = case M.lookup mapMapKey mapMap of
        Just  v -> v
        Nothing -> CUDA.OperationValue 0 progName (CUDA.devPtrToWordPtr CUDA.nullDevPtr)
      scalar   = CUDA.Scalar
        { CUDA.params   =
          [ (codeGenTupleType $ accType xs, "x0")]
        , CUDA.outTy    = codeGenTupleType $ accType op
        , CUDA.comp     = codeGenFun fun
        , CUDA.identity = Nothing}
  liftIO $ CUDA.mapGen progName scalar
  pid <- liftIO $ forkProcess $
    executeFile "nvcc" True (cuCompileFlags progName) Nothing
  let mapMapValue' = mapMapValue
        {CUDA.compilerPID = pid, CUDA.progName  = progName}
  put $ currentState
    { CUDA.uniqueID = uniqueID + 1
    , CUDA.mapMap   =
      M.insert mapMapKey mapMapValue' mapMap}
codeGenAcc op@(ZipWith fun xs ys) = do
  currentState <- get
  let uniqueID        = CUDA.uniqueID currentState
      progName        = "CUDAZipWith" ++ show uniqueID
      zipWithMap      = CUDA.zipWithMap currentState
      zipWithMapKey   = show fun
      zipWithMapValue = case M.lookup zipWithMapKey zipWithMap of
        Just  v -> v
        Nothing -> CUDA.OperationValue 0 progName (CUDA.devPtrToWordPtr CUDA.nullDevPtr)
      scalar   = CUDA.Scalar
        { CUDA.params   =
          [ (codeGenTupleType $ accType xs, "x1")
          , (codeGenTupleType $ accType ys, "x0")]
        , CUDA.outTy    = codeGenTupleType $ accType op
        , CUDA.comp     = codeGenFun fun
        , CUDA.identity = Nothing}
  liftIO $ CUDA.zipWithGen progName scalar
  pid <- liftIO $ forkProcess $
    executeFile "nvcc" True (cuCompileFlags progName) Nothing
  let zipWithMapValue' = zipWithMapValue
        {CUDA.compilerPID = pid, CUDA.progName  = progName}
  put $ currentState
    { CUDA.uniqueID   = uniqueID + 1
    , CUDA.zipWithMap =
      M.insert zipWithMapKey zipWithMapValue' zipWithMap}
codeGenAcc (Use _   ) = return ()
codeGenAcc op = error $ shows op " not supported yet by the code generator."

-- Scalar function
-- ---------------

codeGenFun :: OpenFun env aenv t -> [CUDA.BlkItem]
codeGenFun (Lam lam)   = codeGenFun lam
codeGenFun (Body body) =
  [CUDA.StmtItem $ CUDA.JumpStmt $ CUDA.Return $ Just $ codeGenExp body]

-- Expression
-- ----------

codeGenExp :: forall t env aenv . OpenExp env aenv t -> CUDA.Exp
codeGenExp (Var idx) =
  let idxToInt :: Idx env' t' -> Int
      idxToInt ZeroIdx        = 0
      idxToInt (SuccIdx idx') = 1 + idxToInt idx'
  in  CUDA.Exp [CUDA.toAssignExp $ CUDA.Ident $ 'x' : show (idxToInt idx)]
codeGenExp e@(Const c) =
  let valStr = show (Sugar.toElem c :: t)
      getCUDAConst (CUDA.Float)        = CUDA.FloatConst   $ read valStr
      getCUDAConst (CUDA.Double)       = CUDA.DoubleConst  $ read valStr
      getCUDAConst (CUDA.Char Nothing) = CUDA.CharConst    $ read valStr
      getCUDAConst (CUDA.Int  Nothing) | valStr == "True"  = CUDA.IntegerConst 1
                                       | valStr == "False" = CUDA.IntegerConst 0
                                       | otherwise = CUDA.IntegerConst $ read valStr
      getCUDAConst t' = error $ shows t' " not supported yet as a const type"
  in  CUDA.Exp [CUDA.toAssignExp $ getCUDAConst $ codeGenTupleType $ expType e]
codeGenExp e@(Tuple _) = error $ "the expression, " ++ shows e ", not supported yet by the code generator."
codeGenExp e@(Prj _ _) = error $ "the expression, " ++ shows e ", not supported yet by the code generator."
codeGenExp (Cond e1 e2 e3) =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.NestedExp $ codeGenExp e1)
    (codeGenExp e2)
    (CUDA.toCondExp $ CUDA.NestedExp $ codeGenExp e3)]
codeGenExp (PrimConst c) = codeGenPrimConst c
codeGenExp (PrimApp f (Tuple (NilTup `SnocTup` e1 `SnocTup` e2))) =
  codeGenPrim f $ Prelude.map CUDA.NestedExp [codeGenExp e1, codeGenExp e2]
codeGenExp e@(IndexScalar _ _) = error $ "the expression, " ++ shows e ", not supported yet by the code generator."
codeGenExp e@(Shape _)         = error $ "the expression, " ++ shows e ", not supported yet by the code generator."

toFunParams :: forall env aenv t . OpenExp env aenv t -> [CUDA.FunParam]
toFunParams e@(Const c) =
  let valStr = show (Sugar.toElem c :: t)
      toFunParams' (CUDA.Float)        = [CUDA.FArg (read valStr :: Float)]
      toFunParams' (CUDA.Double)       = [CUDA.VArg (read valStr :: Double)]
      toFunParams' (CUDA.Char Nothing) = [CUDA.VArg (read valStr :: Char)]
      toFunParams' (CUDA.Int  Nothing) | valStr == "True"  = [CUDA.IArg 1]
                                       | valStr == "False" = [CUDA.IArg 0]
                                       | otherwise = [CUDA.IArg (read valStr :: Int)]
      toFunParams' t' = error $ shows t' " not supported yet as a const type"
  in  toFunParams' $ codeGenTupleType $ expType e
toFunParams e = error $
  "the expression, " ++ shows e ", could not be marshalled as a kernel parameter."

-- Types
-- -----

codeGenTupleType :: TupleType a -> CUDA.TySpec
codeGenTupleType (UnitTuple)             = error "not implemented yet"
codeGenTupleType (SingleTuple t)         = codeGenScalarType t
codeGenTupleType (PairTuple UnitTuple t) = codeGenTupleType t
codeGenTupleType (PairTuple _         _) = error "not implemented yet"

codeGenScalarType :: ScalarType a -> CUDA.TySpec
codeGenScalarType (NumScalarType t)    = codeGenNumType t
codeGenScalarType (NonNumScalarType t) = codeGenNonNumType t

codeGenNumType :: NumType a -> CUDA.TySpec
codeGenNumType (IntegralNumType t) = codeGenIntegralType t
codeGenNumType (FloatingNumType t) = codeGenFloatingType t

codeGenIntegralType :: IntegralType a -> CUDA.TySpec
codeGenIntegralType (TypeInt     _) = CUDA.Int      Nothing
codeGenIntegralType (TypeInt8    _) = CUDA.Char     Nothing
codeGenIntegralType (TypeInt16   _) = CUDA.Short    Nothing
codeGenIntegralType (TypeInt32   _) = CUDA.Long     Nothing
codeGenIntegralType (TypeInt64   _) = CUDA.LongLong Nothing
codeGenIntegralType (TypeWord    _) = CUDA.Int      $ Just CUDA.Unsigned
codeGenIntegralType (TypeWord8   _) = CUDA.Char     $ Just CUDA.Unsigned
codeGenIntegralType (TypeWord16  _) = CUDA.Short    $ Just CUDA.Unsigned
codeGenIntegralType (TypeWord32  _) = CUDA.Long     $ Just CUDA.Unsigned
codeGenIntegralType (TypeWord64  _) = CUDA.LongLong $ Just CUDA.Unsigned
codeGenIntegralType (TypeCShort  _) = CUDA.Short    Nothing
codeGenIntegralType (TypeCUShort _) = CUDA.Short    $ Just CUDA.Unsigned
codeGenIntegralType (TypeCInt    _) = CUDA.Int      Nothing
codeGenIntegralType (TypeCUInt   _) = CUDA.Int      $ Just CUDA.Unsigned
codeGenIntegralType (TypeCLong   _) = CUDA.Long     Nothing
codeGenIntegralType (TypeCULong  _) = CUDA.Long     $ Just CUDA.Unsigned
codeGenIntegralType (TypeCLLong  _) = CUDA.LongLong Nothing
codeGenIntegralType (TypeCULLong _) = CUDA.LongLong $ Just CUDA.Unsigned

codeGenFloatingType :: FloatingType a -> CUDA.TySpec
codeGenFloatingType (TypeFloat   _) = CUDA.Float
codeGenFloatingType (TypeDouble  _) = CUDA.Double
codeGenFloatingType (TypeCFloat  _) = CUDA.Float
codeGenFloatingType (TypeCDouble _) = CUDA.Double

codeGenNonNumType :: NonNumType a -> CUDA.TySpec
codeGenNonNumType (TypeBool   _) = CUDA.Char $ Just CUDA.Unsigned
codeGenNonNumType (TypeChar   _) = CUDA.Char Nothing
codeGenNonNumType (TypeCChar  _) = CUDA.Char Nothing
codeGenNonNumType (TypeCSChar _) = CUDA.Char $ Just CUDA.Signed
codeGenNonNumType (TypeCUChar _) = CUDA.Char $ Just CUDA.Unsigned

-- Scalar primitives
-- -----------------

codeGenPrimConst :: PrimConst a -> CUDA.Exp
codeGenPrimConst (PrimMinBound ty) = codeGenMinBound ty
codeGenPrimConst (PrimMaxBound ty) = codeGenMaxBound ty
codeGenPrimConst (PrimPi       ty) = codeGenPi ty

codeGenMinBound :: forall a . BoundedType a -> CUDA.Exp
codeGenMinBound (IntegralBoundedType ty)
  | IntegralDict <- integralDict ty =
    CUDA.Exp
      [CUDA.toAssignExp $ CUDA.IntegerConst $ fromIntegral (minBound :: a)]
codeGenMinBound (NonNumBoundedType ty@(TypeBool _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp [CUDA.toAssignExp $ CUDA.IntegerConst 1]
codeGenMinBound (NonNumBoundedType ty@(TypeChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (minBound :: CChar)]
codeGenMinBound (NonNumBoundedType ty@(TypeCChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (minBound :: CChar)]
codeGenMinBound (NonNumBoundedType ty@(TypeCSChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (minBound :: CSChar)]
codeGenMinBound (NonNumBoundedType ty@(TypeCUChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (minBound :: CUChar)]

codeGenMaxBound :: forall a . BoundedType a -> CUDA.Exp
codeGenMaxBound (IntegralBoundedType ty)
  | IntegralDict <- integralDict ty =
    CUDA.Exp
      [CUDA.toAssignExp $ CUDA.IntegerConst $ fromIntegral (maxBound :: a)]
codeGenMaxBound (NonNumBoundedType ty@(TypeBool _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp [CUDA.toAssignExp $ CUDA.IntegerConst 1]
codeGenMaxBound (NonNumBoundedType ty@(TypeChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (maxBound :: CChar)]
codeGenMaxBound (NonNumBoundedType ty@(TypeCChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (maxBound :: CChar)]
codeGenMaxBound (NonNumBoundedType ty@(TypeCSChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (maxBound :: CSChar)]
codeGenMaxBound (NonNumBoundedType ty@(TypeCUChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (maxBound :: CUChar)]

codeGenPi :: FloatingType a -> CUDA.Exp
codeGenPi ty@(TypeFloat _)   | FloatingDict <- floatingDict ty =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.FloatConst pi]
codeGenPi ty@(TypeDouble _)  | FloatingDict <- floatingDict ty =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.DoubleConst pi]
codeGenPi ty@(TypeCFloat _)  | FloatingDict <- floatingDict ty =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.FloatConst pi]
codeGenPi ty@(TypeCDouble _) | FloatingDict <- floatingDict ty =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.DoubleConst pi]

codeGenPrim :: PrimFun sig -> [CUDA.PrimaryExp] -> CUDA.Exp
codeGenPrim (PrimAdd         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Add (CUDA.toAddExp x) (CUDA.toMulExp y)]
codeGenPrim (PrimSub         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Sub (CUDA.toAddExp x) (CUDA.toMulExp y)]
codeGenPrim (PrimMul         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Mul (CUDA.toMulExp x) (CUDA.toCastExp y)]
codeGenPrim (PrimNeg         _) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Sub
    (CUDA.toAddExp $ CUDA.IntegerConst 0) (CUDA.toMulExp x)]
codeGenPrim (PrimAbs         _) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.Lt
      (CUDA.toRelExp x) (CUDA.toShftExp $ CUDA.IntegerConst 0))
    (CUDA.Exp [CUDA.toAssignExp $ CUDA.Sub
      (CUDA.toAddExp $ CUDA.IntegerConst 0) (CUDA.toMulExp x)])
    (CUDA.toCondExp x)]
codeGenPrim (PrimSig         _) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.Lt
      (CUDA.toRelExp x) (CUDA.toShftExp $ CUDA.IntegerConst 0))
    (CUDA.Exp [CUDA.toAssignExp $ CUDA.IntegerConst (-1)])
    (CUDA.Cond
      (CUDA.toLgcOrExp $ CUDA.Gt
        (CUDA.toRelExp x) (CUDA.toShftExp $ CUDA.IntegerConst 0))
      (CUDA.Exp [CUDA.toAssignExp $ CUDA.IntegerConst 1])
      (CUDA.toCondExp $ CUDA.IntegerConst 0))]
codeGenPrim (PrimQuot        _) [_, _] = error "the code generation of PrimQuot not implemented yet."
codeGenPrim (PrimRem         _) [_, _] = error "the code generation of PrimRem not implemented yet."
codeGenPrim (PrimIDiv        _) [_, _] = error "the code generation of PrimIDiv not implemented yet."
codeGenPrim (PrimMod         _) [_, _] = error "the code generation of PrimMod not implemented yet."
codeGenPrim (PrimBAnd        _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.And (CUDA.toAndExp x) (CUDA.toEqExp y)]
codeGenPrim (PrimBOr         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Or (CUDA.toOrExp x) (CUDA.toXorExp y)]
codeGenPrim (PrimBXor        _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Xor (CUDA.toXorExp x) (CUDA.toAndExp y)]
codeGenPrim (PrimBNot        _) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.BitNot (CUDA.toCastExp x)]
codeGenPrim (PrimFDiv        _) [_, _] = error "the code generation of PrimFDiv not implemented yet."
codeGenPrim (PrimRecip       _) [_]    = error "the code generation of PrimRecip not implemented yet."
codeGenPrim (PrimSin         _) [_]    = error "the code generation of PrimSin not implemented yet."
codeGenPrim (PrimCos         _) [_]    = error "the code generation of PrimCos not implemented yet."
codeGenPrim (PrimTan         _) [_]    = error "the code generation of PrimTan not implemented yet."
codeGenPrim (PrimAsin        _) [_]    = error "the code generation of PrimAsin not implemented yet."
codeGenPrim (PrimAcos        _) [_]    = error "the code generation of PrimAcos not implemented yet."
codeGenPrim (PrimAtan        _) [_]    = error "the code generation of PrimAtan not implemented yet."
codeGenPrim (PrimAsinh       _) [_]    = error "the code generation of PrimAsinh not implemented yet."
codeGenPrim (PrimAcosh       _) [_]    = error "the code generation of PrimAcosh not implemented yet."
codeGenPrim (PrimAtanh       _) [_]    = error "the code generation of PrimAtanh not implemented yet."
codeGenPrim (PrimExpFloating _) [_]    = error "the code generation of PrimExpFloating not implemented yet."
codeGenPrim (PrimSqrt        _) [_]    = error "the code generation of PrimSqrt not implemented yet."
codeGenPrim (PrimLog         _) [_]    = error "the code generation of PrimLog not implemented yet."
codeGenPrim (PrimFPow        _) [_, _] = error "the code generation of PrimFPow not implemented yet."
codeGenPrim (PrimLogBase     _) [_, _] = error "the code generation of PrimLogBase not implemented yet."
codeGenPrim (PrimLt          _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Lt (CUDA.toRelExp x) (CUDA.toShftExp y)]
codeGenPrim (PrimGt          _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Gt (CUDA.toRelExp x) (CUDA.toShftExp y)]
codeGenPrim (PrimLtEq        _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Le (CUDA.toRelExp x) (CUDA.toShftExp y)]
codeGenPrim (PrimGtEq        _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Ge (CUDA.toRelExp x) (CUDA.toShftExp y)]
codeGenPrim (PrimEq          _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Eq (CUDA.toEqExp x) (CUDA.toRelExp y)]
codeGenPrim (PrimNEq         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Neq (CUDA.toEqExp x) (CUDA.toRelExp y)]
codeGenPrim (PrimMax         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.Gt (CUDA.toRelExp x) (CUDA.toShftExp y))
    (CUDA.Exp [CUDA.toAssignExp x])
    (CUDA.toCondExp y)]
codeGenPrim (PrimMin         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.Lt (CUDA.toRelExp x) (CUDA.toShftExp y))
    (CUDA.Exp [CUDA.toAssignExp x])
    (CUDA.toCondExp y)]
codeGenPrim (PrimLAnd         ) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.LgcAnd
    (CUDA.toLgcAndExp x) (CUDA.toOrExp y)]
codeGenPrim (PrimLOr          ) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.LgcOr
    (CUDA.toLgcOrExp x) (CUDA.toLgcAndExp y)]
codeGenPrim (PrimLNot         ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.LgcNot (CUDA.toCastExp x)]
codeGenPrim (PrimOrd          ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.TyCast
    (CUDA.TyName [CUDA.SpecQualTySpec (CUDA.Int Nothing)] Nothing)
    (CUDA.toCastExp x)]
codeGenPrim (PrimChr          ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.TyCast
    (CUDA.TyName [CUDA.SpecQualTySpec (CUDA.Char Nothing)] Nothing)
    (CUDA.toCastExp x)]
codeGenPrim (PrimRoundFloatInt) [_] = error "the code generation of PrimRoundFloatInt not implemented yet."
codeGenPrim (PrimTruncFloatInt) [_] = error "the code generation of PrimTruncFloatInt not implemented yet."
codeGenPrim (PrimIntFloat     ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.TyCast
    (CUDA.TyName [CUDA.SpecQualTySpec CUDA.Float] Nothing)
    (CUDA.toCastExp x)]
codeGenPrim (PrimBoolToInt    ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.TyCast
    (CUDA.TyName [CUDA.SpecQualTySpec (CUDA.Int Nothing)] Nothing)
    (CUDA.toCastExp x)]
